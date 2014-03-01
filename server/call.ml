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
open Xenstore
open Protocol

let ( |> ) a b = b a
let ( ++ ) f g x = f (g x)

let debug fmt = Logging.debug "call" fmt
let error fmt = Logging.error "call" fmt

exception Parse_failure

exception Transaction_again

exception Transaction_nested

let get_namespace_implementation path = match Protocol.Path.to_string_list path with
	| "tool" :: "xenstored" :: "quota" :: rest ->
		Protocol.Path.of_string_list rest, (module Quota_interface: Namespace.IO)
	| "tool" :: "xenstored" :: "connection" :: rest ->
		Protocol.Path.of_string_list rest, (module Connection.Interface: Namespace.IO)
	| "tool" :: "xenstored" :: "log" :: rest ->
		Protocol.Path.of_string_list rest, (module Logging_interface: Namespace.IO)
	| "tool" :: "xenstored" :: "memory" :: rest ->
		Protocol.Path.of_string_list rest, (module Heap_debug_interface: Namespace.IO)
	| _ ->
		path, (module Transaction: Namespace.IO)

(* Perform a 'simple' operation (not a Transaction_start or Transaction_end)
   and create a response. *)
let op_exn store c t (payload: Request.payload) : Response.payload * Transaction.side_effects =
	let open Request in
        (* used when an operation has side-effects which should be visible
           immediately (if not in a transaction context) or upon commit (if
           in a transaction context) *)
        let has_side_effects () =
                if Transaction.get_id t = Transaction.none
                then Transaction.get_side_effects t
                else Transaction.no_side_effects () in
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
		| Getdomainpath domid ->
			let v = Store.getdomainpath domid |> Protocol.Name.to_string in
			Response.Getdomainpath v, Transaction.no_side_effects ()
		| PathOp(path, op) ->
			let path = Protocol.Name.(to_path (resolve (of_string path) c.Connection.domainpath)) in
			let path, m = get_namespace_implementation path in
			let module Impl = (val m: Namespace.IO) in

			begin match op with
			| Read ->
				let v = Impl.read t c.Connection.perm path in
				Response.Read v, Transaction.no_side_effects ()
			| Directory ->
				let entries = Impl.ls t c.Connection.perm path in
				Response.Directory entries, Transaction.no_side_effects ()
			| Getperms ->
				let v = Impl.getperms t c.Connection.perm path in
				Response.Getperms v, Transaction.no_side_effects ()
			| Write value ->
				Impl.write t c.Connection.domid c.Connection.perm path value;
				Response.Write, has_side_effects ()
			| Mkdir ->
				Impl.mkdir t c.Connection.domid c.Connection.perm path;
				Response.Mkdir, has_side_effects ()
			| Rm ->
				Impl.rm t c.Connection.perm path;
				Response.Rm, has_side_effects ()
			| Setperms perms ->
				Impl.setperms t c.Connection.perm path perms;
				Response.Setperms, Transaction.no_side_effects ()
			end

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
		let response', side_effects = op_exn store c t request in
		Logging.response ~tid ~con:("replay reply2: " ^ c.Connection.domstr) response';
		Logging.response ~tid ~con response';
		if response <> response' then begin
			raise Transaction_again
		end in
	try
		Logging.start_transaction ~con ~tid;
		List.iter perform_exn ops;
		Logging.end_transaction ~tid ~con;

		Transaction.commit t
	with e ->
		error "transaction_replay caught: %s" (Printexc.to_string e);
		false

let hexify s =
        let hexseq_of_char c = Printf.sprintf "%02x" (Char.code c) in
        let hs = String.create (String.length s * 2) in
        for i = 0 to String.length s - 1 do
                let seq = hexseq_of_char s.[i] in
                hs.[i * 2] <- seq.[0];
                hs.[i * 2 + 1] <- seq.[1];
        done;
        hs

let reply_exn store c (request: t) : Response.payload * Transaction.side_effects =
	let tid = get_tid request in
	let t =
		if tid = Transaction.none
		then Transaction.make tid store
		else Connection.get_transaction c tid in
	let payload : Request.payload = match Request.parse (request: t) with
		| None ->
 			error "Failed to parse request: got %s" (hexify (marshal request));
			raise Parse_failure
		| Some x -> x in

	Logging.request ~tid ~con:c.Connection.domstr payload;

	match payload with
		| Request.Transaction_start ->
			if tid <> Transaction.none then raise Transaction_nested;
			let tid = Connection.register_transaction c store in
			Response.Transaction_start tid, Transaction.no_side_effects ()
		| Request.Transaction_end commit ->
			Connection.unregister_transaction c tid;
			if commit then begin
				Logging.end_transaction ~tid ~con:c.Connection.domstr;
				if true
					&& not(Transaction.commit t)
					&& not(transaction_replay store c t)
				then begin
                                        Logging.conflict ~tid ~con:c.Connection.domstr;
                                        raise Transaction_again
                                end;
                                Logging.commit ~tid ~con:c.Connection.domstr;
				Response.Transaction_end, Transaction.get_side_effects t
			end else begin
				(* Don't log an explicit abort *)
				Response.Transaction_end, Transaction.no_side_effects ()
			end
		| Request.Watch(path, token) ->
			let watch = Connection.add_watch c (Protocol.Name.of_string path) token in
			Connection.fire_one None watch;
			Response.Watch, Transaction.no_side_effects ()
		| Request.Unwatch(path, token) ->
			Connection.del_watch c (Protocol.Name.of_string path) token;
			Response.Unwatch, Transaction.no_side_effects ()
		| Request.Debug cmd ->
			Perms.has c.Connection.perm Perms.DEBUG;
			Response.Debug (
				try match cmd with
				| "print" :: msg :: _ ->
					Logging.debug_print ~tid:0l ~con:c.Connection.domstr msg;
					[]
				| _ -> []
				with _ -> []), Transaction.no_side_effects ()
		| Request.Introduce(domid, mfn, remote_port) ->
			Perms.has c.Connection.perm Perms.INTRODUCE;
			Introduce.(introduce { domid = domid; mfn = mfn; remote_port = remote_port });
                        Connection.fire (Op.Write, Protocol.Name.(Predefined IntroduceDomain));
			Response.Introduce, Transaction.no_side_effects ()
		| Request.Resume(domid) ->
			Perms.has c.Connection.perm Perms.RESUME;
			(* register domain *)
			Response.Resume, Transaction.no_side_effects ()
		| Request.Release(domid) ->
			Perms.has c.Connection.perm Perms.RELEASE;
			(* unregister domain *)
			Connection.fire (Op.Write, Protocol.Name.(Predefined ReleaseDomain));
			Response.Release, Transaction.no_side_effects ()
		| Request.Set_target(mine, yours) ->
			Perms.has c.Connection.perm Perms.SET_TARGET;
			Hashtbl.iter
				(fun address c ->
					if c.Connection.domid = mine
					then c.Connection.perm <- Perms.set_target c.Connection.perm yours;
				) Connection.by_address;
			Response.Set_target, Transaction.no_side_effects ()
		| Request.Restrict domid ->
			Perms.has c.Connection.perm Perms.RESTRICT;
			c.Connection.perm <- Perms.restrict c.Connection.perm domid;
			Response.Restrict, Transaction.no_side_effects ()
		| Request.Isintroduced domid ->
			Perms.has c.Connection.perm Perms.ISINTRODUCED;
			Response.Isintroduced false, Transaction.no_side_effects ()
		| Request.Error msg ->
			error "client sent us an error: %s" (hexify msg);
			raise Parse_failure
		| Request.Watchevent msg ->
			error "client sent us a watch event: %s" (hexify msg);
			raise Parse_failure
		| op ->
			let reply, side_effects = op_exn store c t op in
			if tid <> Transaction.none then Transaction.add_operation t op reply;
			reply, side_effects

let gc store =
	if Symbol.created () > 1000 || Symbol.used () > 20000
	then begin
		debug "Started symbol GC";
		Symbol.mark_all_as_unused ();
		Store.mark_symbols store;
		Hashtbl.iter (fun _ c -> Connection.mark_symbols c) Connection.by_address;
		Symbol.garbage ()
	end

let reply store c request =
	gc store;
	c.Connection.stat_nb_ops <- c.Connection.stat_nb_ops + 1;
	let tid = get_tid request in
	let rid = get_rid request in
	let (response_payload, side_effects), info =
		try
			reply_exn store c request, None
		with e ->
			let default = Some (Printexc.to_string e) in
                        let reply code = Response.Error code, Transaction.no_side_effects () in
			begin match e with
				| Store.Already_exists p           -> reply "EEXIST", Some p
				| Node.Doesnt_exist p              -> reply "ENOENT", Some (Protocol.Path.to_string p)
                                | Protocol.Path.Invalid_path(p, reason) -> reply "EINVAL", Some (Printf.sprintf "%s: %s" p reason)
				| Perms.Permission_denied          -> reply "EACCES", default
				| Not_found                        -> reply "ENOENT", default
				| Parse_failure                    -> reply "EINVAL", default
				| Invalid_argument i               -> reply "EINVAL", Some i
				| Transaction_again                -> reply "EAGAIN", default
				| Transaction_nested               -> reply "EBUSY",  default
				| Quota.Limit_reached              -> reply "EQUOTA", default
				| Quota.Data_too_big               -> reply "E2BIG",  default
				| Quota.Transaction_opened         -> reply "EQUOTA", default
				| (Failure "int_of_string")        -> reply "EINVAL", default
				| Namespace.Unsupported            -> reply "ENOTSUP",default
				| _                                ->
                                                Printf.fprintf stderr "Uncaught exception: %s\n%!" (Printexc.to_string e);
                                                Printexc.print_backtrace stderr;
                                                reply "EIO",    default
			end in
	Logging.response ~tid ~con:c.Connection.domstr ?info response_payload;

	Response.marshal response_payload tid rid, side_effects
