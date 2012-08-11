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

(* Perform a 'simple' operation (not a Transaction_start or Transaction_end)
   and create a response. *)
let op_exn store c t (payload: Request.payload) : Response.payload =
	let connection_path = Store.Path.getdomainpath c.Connection.domid in
	let resolve data = Store.Path.create data connection_path in
	let open Request in
	match payload with
		| Transaction_start
		| Transaction_end _
		| Watch(_, _)
		| Unwatch(_, _)
		| Debug _
		| Introduce(_, _, _)
		| Resume(_)
		| Release(_)
		| Set_target(_, _)
		| Restrict _
		| Isintroduced _
		| Error _
		| Watchevent _ -> assert false
		| Read path ->
			let path = resolve path in
			let v = Transaction.read t c.Connection.perm path in
			Response.Read v
		| Directory path ->
			let path = resolve path in
			let entries = Transaction.ls t c.Connection.perm path in
			Response.Directory entries
		| Getperms path ->
			let path = resolve path in
			let v = Transaction.getperms t c.Connection.perm path in
			Response.Getperms v
		| Getdomainpath domid ->
			let v = Store.Path.getdomainpath domid |> Store.Path.to_string in
			Response.Getdomainpath v
		| Write(path, value) ->
			let path = resolve path in
			Transaction.mkdir_p t c.Connection.perm path;
			Transaction.write t c.Connection.perm path value;
			Response.Write
		| Mkdir path ->
			let path = resolve path in
			Transaction.mkdir_p t c.Connection.perm path;
			begin
				try
					Transaction.mkdir t c.Connection.perm path
				with Store.Path.Already_exist -> ()
			end;
			Response.Mkdir
		| Rm path ->
			let path = resolve path in
			begin
				try
					Transaction.rm t c.Connection.perm path
				with Store.Path.Doesnt_exist -> ()
			end;
			Response.Rm
		| Setperms(path, perms) ->
			let path = resolve path in
			Transaction.setperms t c.Connection.perm path perms;
			Response.Setperms
			
(* Replay a stored transaction against a fresh store, check the responses are
   all equivalent: if so, commit the transaction. Otherwise send the abort to
   the client. *)
let transaction_replay store c t =
	let ops = Transaction.get_operations t in
	let tid = Connection.register_transaction c store in
	let t = Transaction.make tid store in
	let con = "replay request:" ^ c.Connection.domstr in
	let perform_exn (request, response) =
		Logging.request ~tid ~con:("replay request:" ^ c.Connection.domstr) request;
		Logging.response ~tid ~con:("replay reply1: " ^ c.Connection.domstr) response;
		let response' = op_exn store c t request in
		Logging.response ~tid ~con:("replay reply2: " ^ c.Connection.domstr) response';
		Logging.response ~tid ~con response';
		if response <> response' then begin
			Printf.fprintf stderr "EAGAIN\n%!";
			raise Transaction_again
		end in
	try
		Logging.start_transaction ~con ~tid;
		List.iter perform_exn ops;
		Logging.end_transaction ~tid ~con;

		Transaction.commit ~con t
	with e ->
		error "transaction_replay caught: %s" (Printexc.to_string e);
		false

let reply_exn store c (request: t) : Response.payload =
	let tid = get_tid request in
	let t =
		if tid = Transaction.none
		then Transaction.make tid store
		else Connection.get_transaction c tid in
	let payload : Xs_packet.Request.payload = match Xs_packet.Request.parse (request: t) with
		| None ->
 			error "Failed to parse request: got %s" (hexify (Xs_packet.to_string request));
			raise Parse_failure
		| Some x -> x in

	Logging.request ~tid ~con:c.Connection.domstr payload;

	let response_payload = match payload with
		| Request.Transaction_start ->
			if tid <> Transaction.none then raise Transaction_nested;
			let tid = Connection.register_transaction c store in
			Response.Transaction_start tid
		| Request.Transaction_end commit ->
			Connection.unregister_transaction c tid;
			if commit then begin
				Logging.end_transaction ~tid ~con:c.Connection.domstr;
				if true
					&& not(Transaction.commit ~con:c.Connection.domstr t)
					&& not(transaction_replay store c t)
				then raise Transaction_again;
				Transaction.get_paths t |> List.rev |> List.iter Connection.fire;
				Response.Transaction_end
			end else begin
				(* Don't log an explicit abort *)
				Response.Transaction_end
			end
		| Request.Watch(path, token) ->
			let watch = Connection.add_watch c (Store.Name.of_string path) token in
			Connection.fire_one None watch;
			Response.Watch
		| Request.Unwatch(path, token) ->
			Connection.del_watch c (Store.Name.of_string path) token;
			Response.Unwatch
		| Request.Debug cmd ->
			Perms.has c.Connection.perm Perms.DEBUG;
			Response.Debug (
				try match cmd with
				| "print" :: msg :: _ ->
					Logging.debug_print ~con:c.Connection.domstr msg;
					[]
				| "watches" :: _ ->
					let watches = (* Connections.debug cons *) "" in
					[ watches ]
				| "quota" :: domid :: [] ->
					let domid = int_of_string domid in
					let q = Quota.get_entry store.Store.quota domid in
					[ string_of_int q ]
				| _ -> []
				with _ -> [])
		| Request.Introduce(domid, mfn, port) ->
			Perms.has c.Connection.perm Perms.INTRODUCE;
			(* register domain *)
			Connection.fire (Xs_packet.Op.Write, Store.Name.introduceDomain);
			Response.Introduce
		| Request.Resume(domid) ->
			Perms.has c.Connection.perm Perms.RESUME;
			(* register domain *)
			Response.Resume
		| Request.Release(domid) ->
			Perms.has c.Connection.perm Perms.RELEASE;
			(* unregister domain *)
			Connection.fire (Xs_packet.Op.Write, Store.Name.releaseDomain);
			Response.Release
		| Request.Set_target(mine, yours) ->
			Perms.has c.Connection.perm Perms.SET_TARGET;
			if Hashtbl.mem Connection.domains mine then begin
				let c = Hashtbl.find Connection.domains mine in
				c.Connection.perm <- Perms.set_target c.Connection.perm yours;
				Response.Set_target
			end else begin
				error "set_target %d -> %d; domid %d is not connected" mine yours mine;
				Response.Error "EINVAL"
			end
		| Request.Restrict domid ->
			Perms.has c.Connection.perm Perms.RESTRICT;
			c.Connection.perm <- Perms.restrict c.Connection.perm domid;
			Response.Restrict
		| Request.Isintroduced domid ->
			Perms.has c.Connection.perm Perms.ISINTRODUCED;
			Response.Isintroduced false
		| Request.Error msg ->
			error "client sent us an error: %s" (hexify msg);
			raise Parse_failure
		| Request.Watchevent msg ->
			error "client sent us a watch event: %s" (hexify msg);
			raise Parse_failure
		| op ->
			let reply = op_exn store c t op in
			if tid <> Transaction.none then Transaction.add_operation t op reply;
			reply in

	if tid = Transaction.none
	then Transaction.get_paths t |> List.rev |> List.iter Connection.fire;
	response_payload

let reply store c request =
	let response_payload =
		try
			reply_exn store c request
		with e ->
			let reply code =
				error "Caught: %s; returning %s" (Printexc.to_string e) code;
				Response.Error code in
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
	Logging.response ~tid:(get_tid request) ~con:c.Connection.domstr response_payload;

	Response.print request response_payload


