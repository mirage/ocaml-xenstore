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

let debug fmt = Logging.debug "server" fmt
let error fmt = Logging.error "server" fmt

let persist_request = Lwt_mvar.create_empty ()
let persist_response = Lwt_mvar.create_empty ()

let no_persistence () =
  let rec forever () =
    Lwt_mvar.take persist_request >>= fun _ ->
    Lwt_mvar.put persist_response `Ok >>= fun () ->
    forever () in
  forever ()

let git_persistence dir =
  let module DB = (val IrminGit.local ~bare:false dir) in
  let db_t = DB.create () in

  let dir_suffix = ".dir" in
  let value_suffix = ".value" in

  let value_of_filename path = match List.rev (Protocol.Path.to_string_list path) with
  | [] -> []
  | file :: dirs -> List.rev ((file ^ value_suffix) :: (List.map (fun x -> x ^ dir_suffix) dirs)) in

  let dir_of_filename path =
    List.rev (List.map (fun x -> x ^ dir_suffix) (List.rev (Protocol.Path.to_string_list path))) in

  let rec forever () =
    db_t >>= fun db ->
    Lwt_mvar.take persist_request >>= fun request ->
    (match request with
    | Store.Write(path, perm, value) ->
      Printf.fprintf stderr "+ %s\n%!" (Protocol.Path.to_string path);
      (try_lwt
        DB.update db (value_of_filename path) value
      with e -> (Printf.fprintf stderr "ERR %s\n%!" (Printexc.to_string e)); return ())
    | Store.Rm path ->
      Printf.fprintf stderr "- %s\n%!" (Protocol.Path.to_string path);
      (try_lwt
        DB.remove db (dir_of_filename path) >>= fun () ->
        DB.remove db (value_of_filename path)
      with e -> (Printf.fprintf stderr "ERR %s\n%!" (Printexc.to_string e)); return ()) ) >>= fun () ->
    Lwt_mvar.put persist_response `Ok >>= fun () ->
    forever () in
  forever ()

let persist side_effects =
  Lwt_list.iter_s (fun x ->
    Lwt_mvar.put persist_request x >>= fun () ->
    Lwt_mvar.take persist_response >>= fun _ ->
    return ()
  ) side_effects.Transaction.updates

let store =
	let store = Store.create () in
        let t = Transaction.make 1l store in
	List.iter
		(fun path ->
                        let path = Protocol.Path.of_string path in
			if not (Transaction.exists t (Perms.of_domain 0) path)
			then Transaction.mkdir t 0 (Perms.of_domain 0) path
		) [ "/local"; "/local/domain"; "/tool"; "/tool/xenstored"; "/tool/xenstored/quota"; "/tool/xenstored/connection"; "/tool/xenstored/log"; "/tool/xenstored/memory" ];
        assert (Transaction.commit t);
        (*
        persist (Transaction.get_side_effects t);
        *)
        store

module Make_namespace(T: S.TRANSPORT) = struct
  let namespace_of channel =
    let module Interface = struct
      include Namespace.Unsupported
      let read t (perms: Perms.t) (path: Protocol.Path.t) =
        Perms.has perms Perms.CONFIGURE;
        match T.Introspect.read channel (Protocol.Path.to_string_list path) with
        | Some x -> x
        | None -> raise (Node.Doesnt_exist path)
      let exists t perms path = try ignore(read t perms path); true with Node.Doesnt_exist _ -> false
      let ls t perms path =
        Perms.has perms Perms.CONFIGURE;
        T.Introspect.ls channel (Protocol.Path.to_string_list path)
      let write t _ perms path v =
        Perms.has perms Perms.CONFIGURE;
        if not(T.Introspect.write channel (Protocol.Path.to_string_list path) v)
        then raise Perms.Permission_denied
    end in
    Some (module Interface: Namespace.IO)
end

let fail_on_error = function
| `Ok x -> return x
| `Error x -> fail (Failure x)

module Make = functor(T: S.TRANSPORT) -> struct
        module NS = Make_namespace(T)

        include T

	let handle_connection t =
		lwt address = T.address_of t in
                let dom = T.domain_of t in
		let interface = NS.namespace_of t in
		let c = Connection.create (address, dom) interface in
		let m = Lwt_mutex.create () in
		let take_watch_events () =
			let q = List.rev (Queue.fold (fun acc x -> x :: acc) [] c.Connection.watch_events) in
			Queue.clear c.Connection.watch_events;
			q in
		let flush_watch_events header_buf payload_buf q =
			Lwt_list.iter_s
				(fun (path, token) ->
                                        let reply = Protocol.Response.Watchevent(path, token) in
                                        let next = Protocol.Response.marshal reply payload_buf in
                                        let len = next.Cstruct.off in
                                        let payload_buf' = Cstruct.sub payload_buf 0 len in
                                        let hdr = { Header.tid = 0l; rid = 0l; ty = Protocol.Op.Watchevent; len } in
                                        ignore (Protocol.Header.marshal hdr header_buf);
                                        T.write t header_buf >>= fun () ->
                                        T.write t payload_buf' >>= fun () ->
                                        return ()
				) q in
		let (background_watch_event_flusher: unit Lwt.t) =
			while_lwt true do
                                let header_buf = Cstruct.create Protocol.Header.sizeof in
                                let payload_buf = Cstruct.create Protocol.xenstore_payload_max in
				Lwt_mutex.with_lock m
					(fun () ->
						lwt () = while_lwt Queue.length c.Connection.watch_events = 0 do
							Lwt_condition.wait ~mutex:m c.Connection.cvar
						done in
						flush_watch_events header_buf payload_buf (take_watch_events ())
					)
			done in

		try_lwt
			lwt () =
                        let header_buf = Cstruct.create Protocol.Header.sizeof in
                        let payload_buf = Cstruct.create Protocol.xenstore_payload_max in
			while_lwt true do
                                T.read t header_buf >>= fun () ->
                                fail_on_error (Protocol.Header.unmarshal header_buf) >>= fun hdr ->
                                let payload_buf' = Cstruct.sub payload_buf 0 hdr.Protocol.Header.len in
                                T.read t payload_buf' >>= fun () ->
				let events = take_watch_events () in
				let reply, side_effects = match Protocol.Request.unmarshal hdr payload_buf' with
                                | `Ok request -> Call.reply store c hdr request
                                | `Error msg ->
					(* quirk: if this is a NULL-termination error then it should be EINVAL *)
					Protocol.Response.Error "EINVAL", Transaction.no_side_effects () in
                                Transaction.get_watches side_effects |> List.rev |> List.iter Connection.fire;
                                persist side_effects >>= fun () ->
				Lwt_mutex.with_lock m
					(fun () ->
						lwt () = flush_watch_events header_buf payload_buf events in
                                                let next = Protocol.Response.marshal reply payload_buf in
                                                let len = next.Cstruct.off in
                                                let payload_buf' = Cstruct.sub payload_buf 0 len in
                                                let ty = Protocol.Response.get_ty reply in
                                                let hdr = { hdr with Protocol.Header.ty; len } in
                                                ignore(Protocol.Header.marshal hdr header_buf);
                                                T.write t header_buf >>= fun () ->
                                                T.write t payload_buf'
					)
			done in
			T.destroy t
		with e ->
			Lwt.cancel background_watch_event_flusher;
			Connection.destroy address;
			T.destroy t

	let serve_forever persistence =
                let (_: unit Lwt.t) = match persistence with
                | S.NoPersistence -> no_persistence ()
                | S.Git filename -> git_persistence filename in
		lwt server = T.listen () in
		T.accept_forever server handle_connection
end
