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

let ( |> ) a b = b a
let ( ++ ) f g x = f (g x)

let store =
	let store = Store.create () in
	let localpath = Store.Path.of_string "/local" in
	if not (Store.path_exists store localpath)
	then Store.mkdir store (Perms.Connection.create 0) localpath;
	store

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

module type TRANSPORT = sig
  type server
  val listen: unit -> server Lwt.t

  type t
  val read: t -> string -> int -> int -> int Lwt.t
  val write: t -> string -> int -> int -> int Lwt.t
  val destroy: t -> unit Lwt.t

  val accept_forever: server -> (t -> unit Lwt.t) -> 'a Lwt.t
end

module Server = functor(T: TRANSPORT) -> struct
	module PS = PacketStream(T)
	open Xs_packet
	open Junk

	let perm' = ACL.( { owner = 0; other = NONE; acl = [] })
	let perm = Perms.Connection.full_rights

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


	let handle_connection t =
		let channel = PS.make t in
		let resolve data = Store.Path.create data "/connection_path" in
		let connection_perm = Perms.Connection.full_rights in
		lwt request = PS.recv channel in
		let reply =
			try
				let data = get_data request in
				let t = Transaction.make (Int32.to_int (get_tid request)) store in
				match get_ty request with
					| Op.Read ->
						let path = data |> one_string |> resolve in
						let v = Transaction.read t connection_perm path in
						Transaction.commit t;
						Response.read request v
					| Op.Directory ->
						let path = data |> one_string |> resolve in
						let entries = Transaction.ls t connection_perm path in
						Response.directory request entries
					| Op.Getperms ->
						let path = data |> one_string |> resolve in
						let v = Transaction.getperms t connection_perm path in
						Response.getperms request perm'
					| Op.Getdomainpath ->
						(Response.getdomainpath request ++ (Printf.sprintf "/local/domain/%u") ++ c_int_of_string ++ one_string) data
					| Op.Transaction_start ->
						Response.transaction_start request 1l
					| Op.Write ->
						let path, value = two_strings data in
						let path = resolve path in
						let t = Transaction.make (Int32.to_int (get_tid request)) store in
						create_implicit_path t connection_perm path;
						Transaction.write t connection_perm path value;
						Transaction.commit t;
						Response.write request
					| Op.Mkdir ->
						let path = data |> one_string |> resolve in
						create_implicit_path t connection_perm path;
						begin
							try
								Transaction.mkdir t connection_perm path
							with Store.Path.Already_exist -> ()
						end;
						Response.mkdir request
					| Op.Rm ->
						let path = data |> one_string |> resolve in
						begin
							try
								Transaction.rm t connection_perm path
							with Store.Path.Doesnt_exist -> ()
						end;
						Response.rm request
					| Op.Setperms ->
						let path, perms = two_strings data in
						let path = resolve path in
						Transaction.setperms t connection_perm path (Perms.Node.of_string perms);
						Response.setperms request
					| Op.Watch ->
						Response.watch request
					| Op.Unwatch ->
						Response.unwatch request
					| Op.Transaction_end ->
						Response.transaction_end request
					| Op.Debug
					| Op.Introduce | Op.Release
					| Op.Watchevent | Op.Error | Op.Isintroduced
					| Op.Resume | Op.Set_target ->
						Response.error request "Not implemented"
			with e ->
				Lwt_io.printf "Caught: %s\n" (Printexc.to_string e);
				Response.error request (Printexc.to_string e) in
		lwt () = PS.send channel reply in
		T.destroy t

	let serve_forever () =
		lwt server = T.listen () in
		T.accept_forever server handle_connection
end
