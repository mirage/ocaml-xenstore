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
open Xs_protocol

let ( |> ) a b = b a
let ( ++ ) f g x = f (g x)

let debug fmt = Logging.debug "xs_server" fmt
let error fmt = Logging.error "xs_server" fmt

let store =
	let store = Store.create () in
	List.iter
		(fun path ->
			let p = Store.Path.create path (Store.Path.getdomainpath 0) in
			if not (Store.exists store p)
			then Store.mkdir store 0 (Perms.of_domain 0) p
		) [ "/local"; "/local/domain"; "/tool"; "/tool/xenstored"; "/tool/xenstored/quota"; "/tool/xenstored/connection"; "/tool/xenstored/log"; "/tool/xenstored/memory" ];
	store

module Server = functor(T: S.TRANSPORT) -> struct
	module PS = PacketStream(T)

	let handle_connection t =
		lwt address = T.address_of t in
		let interface = T.namespace_of t in
		let c = Connection.create address interface in
		let channel = PS.make t in
		let m = Lwt_mutex.create () in
		let take_watch_events () =
			let q = List.rev (Queue.fold (fun acc x -> x :: acc) [] c.Connection.watch_events) in
			Queue.clear c.Connection.watch_events;
			q in
		let flush_watch_events q =
			Lwt_list.iter_s
				(fun (path, token) ->
					PS.send channel (Xs_protocol.(Response.(print (Watchevent(path, token)) 0l 0l)))
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
					| Ok x -> return x
					| Exception e -> raise_lwt e in
				let events = take_watch_events () in
				let reply = Call.reply store c request in
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
