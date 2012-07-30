(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Lwt
open Xs_packet
open Junk

let ( |> ) a b = b a
let ( ++ ) f g x = f (g x)

let debug fmt = Logging.debug "call" fmt
let error fmt = Logging.error "call" fmt

let strings data = String.split '\000' data

let one_string data =
    let args = String.split ~limit:2 '\000' data in
    match args with
	| x :: [] -> x
	| _       -> failwith (Printf.sprintf "one_string parse failure: [%s](%d)" data (String.length data))

let two_strings data =
    let args = String.split ~limit:2 '\000' data in
	match args with
	| a :: b :: [] -> a, b
	| _            -> failwith (Printf.sprintf "two_strings parse failure: [%s](%d)" data (String.length data))


let c_int_of_string s =
	let v = ref 0 in
	let is_digit c = c >= '0' && c <= '9' in
	let len = String.length s in
	let i = ref 0 in
	while !i < len && not (is_digit s.[!i]) do incr i done;
	while !i < len && is_digit s.[!i]
	do
        let x = (Char.code s.[!i]) - (Char.code '0') in
        v := !v * 10 + x;
        incr i
	done;
	!v

let create_implicit_path t perm path =
	let dirname = Store.Path.get_parent path in
	if not (Transaction.path_exists t dirname) then (
		let rec check_path p =
			match p with
				| []      -> []
				| h :: l  ->
					if Transaction.path_exists t h then
						check_path l
					else
						p in
		let ret = check_path (List.tl (Store.Path.get_hierarchy dirname)) in
		List.iter (fun s -> Transaction.mkdir ~with_watch:false t perm s) ret
	)


let reply_exn store c request =
	let connection_path = Store.Path.getdomainpath c.Connection.domid in
	let resolve data = Store.Path.create data connection_path in
	let data = get_data request in
	let tid = get_tid request in
	let t = Transaction.make tid store in
	match get_ty request with
	| Op.Read ->
		let path = data |> one_string |> resolve in
		let v = Transaction.read t c.Connection.perm path in
		Response.read request v
	| Op.Directory ->
		let path = data |> one_string |> resolve in
		let entries = Transaction.ls t c.Connection.perm path in
		Response.directory request entries
	| Op.Getperms ->
		let path = data |> one_string |> resolve in
		let v = Transaction.getperms t c.Connection.perm path in
		Response.getperms request v
	| Op.Getdomainpath ->
		(Response.getdomainpath request ++ Store.Path.getdomainpath ++ c_int_of_string ++ one_string) data
	| Op.Transaction_start ->
		Response.transaction_start request 1l
	| Op.Write ->
		let path, value = two_strings data in
		let path = resolve path in
		create_implicit_path t c.Connection.perm path;
		Transaction.write t c.Connection.perm path value;
		Response.write request
	| Op.Mkdir ->
		let path = data |> one_string |> resolve in
		create_implicit_path t c.Connection.perm path;
		begin
			try
				Transaction.mkdir t c.Connection.perm path
			with Store.Path.Already_exist -> ()
		end;
		Response.mkdir request
	| Op.Rm ->
		let path = data |> one_string |> resolve in
		begin
			try
				Transaction.rm t c.Connection.perm path
			with Store.Path.Doesnt_exist -> ()
		end;
		Response.rm request
	| Op.Setperms ->
		let path, perms = two_strings data in
		let path = resolve path in
		begin match Xs_packet.ACL.of_string perms with
		| Some x ->
			Transaction.setperms t c.Connection.perm path x;
			Response.setperms request
		| None ->
			Response.error request "failed to parse perms"
		end
	| Op.Watch ->
		Response.watch request
	| Op.Unwatch ->
		Response.unwatch request
	| Op.Transaction_end ->
		Connection.unregister_transaction c tid;
		begin match one_string data with
		| "F" ->
			(* Don't log an explicit abort *)
			Response.transaction_end request
		| "T" ->
			Logging.end_transaction ~tid ~con:c.Connection.domstr;
			if Transaction.commit ~con:c.Connection.domstr t then begin
				(* process_watch (List.rev (Transaction.get_ops t)) cons*)
				Response.transaction_end request
			end else Response.error request "EAGAIN"
		| _ ->
			Response.error request "EINVAL"
		end
	| Op.Debug ->
		Perms.has c.Connection.perm Perms.DEBUG;
		Response.debug request (
			try match strings data with
			| "print" :: msg :: _ ->
				Logging.xb_op ~tid:0l ~ty:Xs_packet.Op.Debug ~con:"=======>" msg;
				[]
			| "watches" :: _ ->
				let watches = (* Connections.debug cons *) "" in
				[ watches ]
			| _ -> []
			with _ -> [])
	| Op.Introduce ->
		Perms.has c.Connection.perm Perms.INTRODUCE;
		begin match strings data with
		| domid :: mfn :: port :: _ ->
			let _ = int_of_string domid in
			let _ = Nativeint.of_string mfn in
			let _ = int_of_string port in
			(* register domain *)
			(* @introduceDomain *)
			Response.introduce request
		| _ ->
			Response.error request "EINVAL"
		end
	| Op.Release ->
		Perms.has c.Connection.perm Perms.RELEASE;
		begin match strings data with
		| domid :: _ ->
			let _ = int_of_string domid in
			(* unregister domain *)
			(* @releaseDomain *)
			Response.release request
		| _ ->
			Response.error request "EINVAL"
		end
	| Op.Watchevent | Op.Error | Op.Isintroduced
	| Op.Resume | Op.Set_target ->
		Response.error request "Not implemented"

let reply store c request =
	try
		reply_exn store c request
	with e ->
		error "Caught: %s" (Printexc.to_string e);
		Response.error request (Printexc.to_string e)
