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

let fail_on_error = function
| `Ok x -> return x
| `Error x -> fail (Failure x)

module Make = functor(T: S.TRANSPORT) -> struct

  include T

  let introspect channel =
    let module Interface = struct
      include Tree.Unsupported
      let read t (perms: Perms.t) (path: Protocol.Path.t) =
        Perms.has perms Perms.CONFIGURE;
        match T.Introspect.read channel (Protocol.Path.to_string_list path) with
        | Some x -> x
        | None -> raise (Node.Doesnt_exist path)
      let exists t perms path = try ignore(read t perms path); true with Node.Doesnt_exist _ -> false
      let ls t perms path =
        Perms.has perms Perms.CONFIGURE;
        T.Introspect.ls channel (Protocol.Path.to_string_list path)
      let write t _ _ perms path v =
        Perms.has perms Perms.CONFIGURE;
        if not(T.Introspect.write channel (Protocol.Path.to_string_list path) v)
        then raise Perms.Permission_denied
    end in
    (module Interface: Tree.S)

	let handle_connection t =
		lwt address = T.address_of t in
                let dom = T.domain_of t in
		let interface = introspect t in
		Connection.create (address, dom) >>= fun c ->
                let connection_path = Protocol.Path.of_string (Printf.sprintf "/tool/xenstored/transport/%s/%d" (match Uri.scheme address with Some x -> x | None -> "unknown") (Connection.index c)) in
                (* Hold this mutex only when marshalling to the buffers and writing
                   to the channel to prevent corruption *)
		let m = Lwt_mutex.create () in
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
                                Connection.pop_watch_events c >>= fun w ->
				Lwt_mutex.with_lock m
					(fun () ->
						flush_watch_events header_buf payload_buf w
					)
			done in

                Mount.mount connection_path interface >>= fun () ->
		try_lwt
			lwt () =
                        let header_buf = Cstruct.create Protocol.Header.sizeof in
                        let payload_buf = Cstruct.create Protocol.xenstore_payload_max in
			while_lwt true do
                                T.read t header_buf >>= fun () ->
                                fail_on_error (Protocol.Header.unmarshal header_buf) >>= fun hdr ->
                                let payload_buf' = Cstruct.sub payload_buf 0 hdr.Protocol.Header.len in
                                T.read t payload_buf' >>= fun () ->
				Connection.pop_watch_events_nowait c >>= fun events ->
                                Database.store >>= fun store ->
                                Quota.limits_of_domain dom >>= fun limits ->
                                Connection.PPerms.get (Connection.perm c) >>= fun perm ->
				( match Protocol.Request.unmarshal hdr payload_buf' with
                                  | `Ok request -> Call.reply store (Some limits) perm c hdr request
                                  | `Error msg ->
					(* quirk: if this is a NULL-termination error then it should be EINVAL *)
					return (Protocol.Response.Error "EINVAL", Transaction.no_side_effects ())
                                ) >>= fun (reply, side_effects) ->
                                Transaction.get_watch side_effects |> List.rev |> Lwt_list.iter_s (Connection.watch c (Some limits)) >>= fun () ->
                                Transaction.get_unwatch side_effects |> List.rev |> Lwt_list.iter_s (Connection.unwatch c) >>= fun () ->
                                Transaction.get_watches side_effects |> List.rev |> Lwt_list.iter_s (Connection.fire (Some limits)) >>= fun () ->
                                Lwt_list.iter_s Introduce.introduce (Transaction.get_domains side_effects) >>= fun () ->
                                (* XXX: need to update the store root *)
                                Database.persist side_effects >>= fun () ->
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
			return ()
		with e ->
			Lwt.cancel background_watch_event_flusher;
			Connection.destroy address >>= fun () ->
                        Mount.unmount connection_path >>= fun () ->
                        Quota.remove dom >>= fun () ->
                        T.destroy t

	let serve_forever persistence =
                let (_: unit Lwt.t) = match persistence with
                | S.NoPersistence -> Database.no_persistence ()
                | S.Git filename -> Database.git_persistence filename in
		lwt server = T.listen () in
		T.accept_forever server handle_connection
end
