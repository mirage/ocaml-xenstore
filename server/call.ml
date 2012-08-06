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

exception Parse_failure

exception Transaction_again

exception Transaction_nested


let reply_exn store c request =
	let connection_path = Store.Path.getdomainpath c.Connection.domid in
	let resolve data = Store.Path.create data connection_path in
	let tid = get_tid request in
	let t =
		if tid = Transaction.none
		then Transaction.make tid store
		else Connection.get_transaction c tid in
	Logging.xb_op ~ty:(get_ty request) ~tid ~con:c.Connection.domstr (get_data request);

	let open Request in
	let reply = match parse request with
		| None ->
 			error "Failed to parse request: got %s" (hexify (Xs_packet.to_string request));
			raise Parse_failure
		| Some (Read path) ->
			let path = resolve path in
			let v = Transaction.read t c.Connection.perm path in
			Response.read request v
		| Some (Directory path) ->
			let path = resolve path in
			let entries = Transaction.ls t c.Connection.perm path in
			Response.directory request entries
		| Some (Getperms path) ->
			let path = resolve path in
			let v = Transaction.getperms t c.Connection.perm path in
			Response.getperms request v
		| Some (Getdomainpath domid) ->
			let v = Store.Path.getdomainpath domid |> Store.Path.to_string in
			Response.getdomainpath request v
		| Some (Transaction_start) ->
			if tid <> Transaction.none then raise Transaction_nested;
			let tid = Connection.register_transaction c store in
			Response.transaction_start request tid
		| Some (Write(path, value)) ->
(*			Printf.fprintf stderr "Write %s <- %s\n%!" path value; *)
			let path = resolve path in
			Transaction.mkdir_p t c.Connection.perm path;
			Transaction.write t c.Connection.perm path value;
(*			Printf.fprintf stderr "tid = %lu len(wops) = %d\n%!" tid (List.length t.Transaction.ops); *)
			Response.write request
		| Some (Mkdir path) ->
			let path = resolve path in
			Transaction.mkdir_p t c.Connection.perm path;
			begin
				try
					Transaction.mkdir t c.Connection.perm path
				with Store.Path.Already_exist -> ()
			end;
			Response.mkdir request
		| Some (Rm path) ->
			let path = resolve path in
			begin
				try
					Transaction.rm t c.Connection.perm path
				with Store.Path.Doesnt_exist -> ()
			end;
			Response.rm request
		| Some (Setperms(path, perms)) ->
			let path = resolve path in
			Transaction.setperms t c.Connection.perm path perms;
			Response.setperms request
		| Some (Watch(path, token)) ->
			let watch = Connection.add_watch c (Store.Name.of_string path) token in
			Connection.fire_one None watch;
			Response.watch request
		| Some (Unwatch(path, token)) ->
			Connection.del_watch c (Store.Name.of_string path) token;
			Response.unwatch request
		| Some (Transaction_end commit) ->
			Connection.unregister_transaction c tid;
			if commit then begin
				Logging.end_transaction ~tid ~con:c.Connection.domstr;
				if Transaction.commit ~con:c.Connection.domstr t then begin
					Transaction.get_ops t |> List.rev |> List.iter Connection.fire;
					Response.transaction_end request
				end else raise Transaction_again
			end else begin
				(* Don't log an explicit abort *)
				Response.transaction_end request
			end
		| Some (Debug cmd) ->
			Perms.has c.Connection.perm Perms.DEBUG;
			Response.debug request (
				try match cmd with
				| "print" :: msg :: _ ->
					Logging.xb_op ~tid:0l ~ty:Xs_packet.Op.Debug ~con:"=======>" msg;
					[]
				| "watches" :: _ ->
					let watches = (* Connections.debug cons *) "" in
					[ watches ]
				| _ -> []
				with _ -> [])
		| Some (Introduce(domid, mfn, port)) ->
			Perms.has c.Connection.perm Perms.INTRODUCE;
			(* register domain *)
			Connection.fire (Xs_packet.Op.Write, Store.Name.introduceDomain);
			Response.introduce request
		| Some (Resume(domid)) ->
			Perms.has c.Connection.perm Perms.RESUME;
			(* register domain *)
			Response.resume request
		| Some (Release(domid)) ->
			Perms.has c.Connection.perm Perms.RELEASE;
			(* unregister domain *)
			Connection.fire (Xs_packet.Op.Write, Store.Name.releaseDomain);
			Response.release request
		| Some (Set_target(mine, yours)) ->
			Perms.has c.Connection.perm Perms.SET_TARGET;
			if Hashtbl.mem Connection.domains mine then begin
				let c = Hashtbl.find Connection.domains mine in
				c.Connection.perm <- Perms.set_target c.Connection.perm yours;
				Response.set_target request
			end else begin
				error "set_target %d -> %d; domid %d is not connected" mine yours mine;
				Response.error request "EINVAL"
			end
		| Some (Restrict domid) ->
			Perms.has c.Connection.perm Perms.RESTRICT;
			c.Connection.perm <- Perms.restrict c.Connection.perm domid;
			Response.restrict request
		| Some (Isintroduced domid) ->
			Perms.has c.Connection.perm Perms.ISINTRODUCED;
			Response.isintroduced request false
		| Some (Error msg) ->
			error "client sent us an error: %s" (hexify msg);
			raise Parse_failure
		| Some (Watchevent msg) ->
			error "client sent us a watch event: %s" (hexify msg);
			raise Parse_failure in
	if tid = Transaction.none
	then Transaction.get_ops t |> List.rev |> List.iter Connection.fire;
	reply


let reply store c request =
	let reply =
		try
			reply_exn store c request
		with e ->
			let reply code =
				error "Caught: %s; returning %s" (Printexc.to_string e) code;
				Response.error request code in
			begin match e with
				| Store.Invalid_path               -> reply "EINVAL"
				| Store.Path.Already_exist         -> reply "EEXIST"
				| Store.Path.Doesnt_exist          -> reply "ENOENT"
				| Store.Path.Lookup_Doesnt_exist s -> reply "ENOENT"
				| Perms.Permission_denied          -> reply "EACCES"
				| Not_found                        -> reply "ENOENT"
				| Parse_failure                    -> reply "EINVAL"
				| Invalid_argument i               -> reply "EINVAL"
				| Transaction_again                -> reply "EAGAIN"
				| Transaction_nested               -> reply "EBUSY"
				| Quota.Limit_reached              -> reply "EQUOTA"
				| Quota.Data_too_big               -> reply "E2BIG"
				| Quota.Transaction_opened         -> reply "EQUOTA"
				| (Failure "int_of_string")        -> reply "EINVAL"
				| _                                -> reply "EIO"
			end in
	let tid = get_tid reply in
    Logging.xb_answer ~ty:(get_ty reply) ~tid ~con:c.Connection.domstr (get_data reply);
	reply

