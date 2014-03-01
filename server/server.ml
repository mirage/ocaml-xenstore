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

module DB = (val IrminGit.local ~bare:false "/tmp/xenstore/test")

let db_t = DB.create ()

let persist side_effects =
        db_t >>= fun db ->
        Lwt_list.iter_s (function
        | Store.Write(path, perm, value) ->
                        Printf.fprintf stderr "+ %s\n%!" (Protocol.Path.to_string path);
                        (try_lwt
                DB.update db (Protocol.Path.to_string_list path) value
                with e -> (Printf.fprintf stderr "ERR %s\n%!" (Printexc.to_string e); fail e))
        | Store.Rm path ->
                        Printf.fprintf stderr "- %s\n%!" (Protocol.Path.to_string path);
                        (try_lwt
                DB.remove db (Protocol.Path.to_string_list path)
                with e -> (Printf.fprintf stderr "ERR %s\n%!" (Printexc.to_string e); fail e))
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

module Make = functor(T: S.TRANSPORT) -> struct
	module PS = PacketStream(T)
        module NS = Make_namespace(T)

        include T

	let handle_connection t =
		lwt address = T.address_of t in
                let dom = T.domain_of t in
		let interface = NS.namespace_of t in
		let c = Connection.create (address, dom) interface in
		let channel = PS.make t in
		let m = Lwt_mutex.create () in
		let take_watch_events () =
			let q = List.rev (Queue.fold (fun acc x -> x :: acc) [] c.Connection.watch_events) in
			Queue.clear c.Connection.watch_events;
			q in
		let flush_watch_events q =
			Lwt_list.iter_s
				(fun (path, token) ->
					PS.send channel (Protocol.(Response.(marshal (Watchevent(path, token)) 0l 0l)))
				) q in
		let (background_watch_event_flusher: unit Lwt.t) =
			while_lwt true do
				Lwt_mutex.with_lock m
					(fun () ->
						lwt () = while_lwt Queue.length c.Connection.watch_events = 0 do
							Lwt_condition.wait ~mutex:m c.Connection.cvar
						done in
						flush_watch_events (take_watch_events ())
					)
			done in

		try_lwt
			lwt () =
			while_lwt true do
				lwt request = match_lwt (PS.recv channel) with
				| `Ok x -> return x
				| `Error e -> raise_lwt e in
				let events = take_watch_events () in
				let reply, side_effects = Call.reply store c request in
                                Transaction.get_watches side_effects |> List.rev |> List.iter Connection.fire;
                                persist side_effects >>= fun () ->
				Lwt_mutex.with_lock m
					(fun () ->
						lwt () = flush_watch_events events in	
						PS.send channel reply
					)
			done in
			T.destroy t
		with e ->
			Lwt.cancel background_watch_event_flusher;
			Connection.destroy address;
			T.destroy t

	let serve_forever () =
		lwt server = T.listen () in
		T.accept_forever server handle_connection
end
