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

  (* Perform a 'simple' operation (not a Transaction_start or Transaction_end)
   and create a response. *)
let op_exn store limits c t (payload: Request.t) : Response.t * Transaction.side_effects =
	let open Request in
        (* used when an operation has side-effects which should be visible
           immediately (if not in a transaction context) or upon commit (if
           in a transaction context) *)
        let has_side_effects () =
                if Transaction.get_immediate t
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
		| Isintroduced _ -> assert false
		| Getdomainpath domid ->
			let v = Store.getdomainpath domid |> Protocol.Name.to_string in
			Response.Getdomainpath v, Transaction.no_side_effects ()
		| PathOp(path, op) ->
			let path = Protocol.Name.(to_path (resolve (of_string path) c.Connection.domainpath)) in
                        let module Impl = Mount.Tree in
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
				Impl.write t limits c.Connection.domid c.Connection.perm path value;
				Response.Write, has_side_effects ()
			| Mkdir ->
				Impl.mkdir t limits c.Connection.domid c.Connection.perm path;
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
let transaction_replay store limits c t =
	let ops = Transaction.get_operations t in
	let con = "replay request:" ^ c.Connection.domstr in
	let perform_exn t (request, response) =
                let tid = Transaction.get_id t in
		Logging.request ~tid ~con:("replay request:" ^ c.Connection.domstr) request;
		Logging.response ~tid ~con:("replay reply1: " ^ c.Connection.domstr) response;
		let response', side_effects = op_exn store limits c t request in
		Logging.response ~tid ~con:("replay reply2: " ^ c.Connection.domstr) response';
		Logging.response ~tid ~con response';
		if response <> response' then begin
			raise Transaction_again
                end;
                side_effects in
        let t = Transaction.take_snapshot store in
        let tid = Transaction.none in
	try
                (* Perform a test replay on a throwaway copy of the store *)
		Logging.start_transaction ~con ~tid;
		List.iter (fun op -> let (_: Transaction.side_effects) = perform_exn t op in ()) ops;
                (* Replay on the live store *)
                let t = Transaction.make Transaction.none store in
                List.fold_left (fun side_effects op -> Transaction.merge side_effects (perform_exn t op)) (Transaction.no_side_effects ()) ops
	with e ->
		error "transaction_replay caught: %s" (Printexc.to_string e);
                Connection.unregister_transaction c tid;
		Logging.conflict ~tid ~con:c.Connection.domstr;
                raise Transaction_again

let gc store =
  if Symbol.created () > 1000 || Symbol.used () > 20000 then begin
    debug "Started symbol GC";
    Symbol.mark_all_as_unused ();
    Store.mark_symbols store;
    Hashtbl.iter (fun _ c -> Connection.mark_symbols c) Connection.by_address;
    Symbol.garbage ()
  end

(* Compute a reply and set of side effects, or fail *)
let reply_or_fail store limits c hdr (request: Request.t) : (Response.t * Transaction.side_effects) Lwt.t =
	let tid = hdr.Header.tid in
	let t =
		if tid = Transaction.none
		then Transaction.make tid store
		else Connection.get_transaction c tid in

	Logging.request ~tid ~con:c.Connection.domstr request;

	match request with
		| Request.Transaction_start ->
                        if tid <> Transaction.none
                        then return (Response.Error "EBUSY", Transaction.no_side_effects ())
                        else begin
			  Connection.register_transaction limits c store >>= fun tid ->
			  return (Response.Transaction_start tid, Transaction.no_side_effects ())
                        end
		| Request.Transaction_end commit ->
			Connection.unregister_transaction c tid;
			if commit then begin
				Logging.end_transaction ~tid ~con:c.Connection.domstr;
                                try
                                        let side_effects = transaction_replay store limits c t in
                                        Logging.commit ~tid ~con:c.Connection.domstr;
				        return (Response.Transaction_end, side_effects)
                                with e -> fail e
			end else begin
				(* Don't log an explicit abort *)
				return (Response.Transaction_end, Transaction.no_side_effects ())
			end
		| Request.Watch(path, token) ->
                        return (Response.Watch, Transaction.( { no_side_effects () with watch = [ Protocol.Name.of_string path, token ] } ))
		| Request.Unwatch(path, token) ->
                        return (Response.Unwatch, Transaction.( { no_side_effects () with unwatch = [ Protocol.Name.of_string path, token ] } ))
		| Request.Debug cmd ->
                        (try
		        	Perms.has c.Connection.perm Perms.DEBUG;
                                return (Response.Debug (try match cmd with
				        | "print" :: msg :: _ ->
				                Logging.debug_print ~tid:0l ~con:c.Connection.domstr msg;
				        	[]
			        	| _ ->
                                                []
			        	with _ -> []), Transaction.no_side_effects ())
                        with e -> fail e)
		| Request.Introduce(domid, mfn, remote_port) ->
                        (try
		        	Perms.has c.Connection.perm Perms.INTRODUCE;
                                let address = { Domain.domid; mfn; remote_port } in
                                let side_effects = {
                                  Transaction.no_side_effects () with
                                  Transaction.domains = [ address ];
                                  watches = [ Op.Write, Protocol.Name.(Predefined IntroduceDomain) ]
                                } in
			        return (Response.Introduce, side_effects)
                        with e -> fail e)
		| Request.Resume(domid) ->
                        (try
			        Perms.has c.Connection.perm Perms.RESUME;
			        (* register domain *)
			        return (Response.Resume, Transaction.no_side_effects ())
                        with e -> fail e)
		| Request.Release(domid) ->
                        (try Perms.has c.Connection.perm Perms.RELEASE; return () with e -> fail e) >>= fun () ->
			(* unregister domain *)
			Connection.fire limits (Op.Write, Protocol.Name.(Predefined ReleaseDomain)) >>= fun () ->
			return (Response.Release, Transaction.no_side_effects ())
		| Request.Set_target(mine, yours) ->
                        (try
			        Perms.has c.Connection.perm Perms.SET_TARGET;
			        Hashtbl.iter
				        (fun address c ->
				        	if c.Connection.domid = mine
				        	then c.Connection.perm <- Perms.set_target c.Connection.perm yours;
				        ) Connection.by_address;
			        return (Response.Set_target, Transaction.no_side_effects ())
                        with e -> fail e)
		| Request.Restrict domid ->
                        (try
			        Perms.has c.Connection.perm Perms.RESTRICT;
			        c.Connection.perm <- Perms.restrict c.Connection.perm domid;
			        return (Response.Restrict, Transaction.no_side_effects ())
                        with e -> fail e)
		| Request.Isintroduced domid ->
                        (try
			        Perms.has c.Connection.perm Perms.ISINTRODUCED;
			        return (Response.Isintroduced false, Transaction.no_side_effects ())
                        with e -> fail e)
		| op ->
                        (try
			        let reply, side_effects = op_exn store limits c t op in
			        if tid <> Transaction.none then Transaction.add_operation t op reply;
			        return (reply, side_effects)
                        with e -> fail e)

let reply store limits c hdr request : (Response.t * Transaction.side_effects) Lwt.t =
  gc store;
  c.Connection.stat_nb_ops <- c.Connection.stat_nb_ops + 1;
  Lwt.catch
    (fun () ->
      reply_or_fail store limits c hdr request >>= fun x ->
      return (x, None))
    (fun e ->
      let default = Some (Printexc.to_string e) in
      let reply code = Response.Error code, Transaction.no_side_effects () in
      match e with
      | Transaction_again                     -> return (reply "EAGAIN", default)
      | Limits.Limit_reached                  -> return (reply "EQUOTA", default)
      | Store.Already_exists p                -> return (reply "EEXIST", Some p)
      | Node.Doesnt_exist p                   -> return (reply "ENOENT", Some (Protocol.Path.to_string p))
      | Protocol.Path.Invalid_path(p, reason) -> return (reply "EINVAL", Some (Printf.sprintf "%s: %s" p reason))
      | Perms.Permission_denied               -> return (reply "EACCES", default)
      | Not_found                             -> return (reply "ENOENT", default)
      | Parse_failure                         -> return (reply "EINVAL", default)
      | Invalid_argument i                    -> return (reply "EINVAL", Some i)
      | Limits.Data_too_big                   -> return (reply "E2BIG",  default)
      | Limits.Transaction_opened             -> return (reply "EQUOTA", default)
      | (Failure "int_of_string")             -> return (reply "EINVAL", default)
      | Tree.Unsupported                      -> return (reply "ENOTSUP",default)
      | _ ->
        (* quirk: Write <string> (no value) is one of several parse
           failures where EINVAL is expected instead of EIO *)
        return (reply "EINVAL", default)
    ) >>= fun ((response_payload, side_effects), info) ->
    Logging.response ~tid:hdr.Header.tid ~con:c.Connection.domstr ?info response_payload;
    return (response_payload, side_effects)
