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

let debug fmt = Logging.debug "xs_server" fmt
let error fmt = Logging.error "xs_server" fmt

let store =
	let store = Store.create () in
	List.iter
		(fun path ->
			let p = Store.Path.create path (Store.Path.getdomainpath 0) in
			if not (Store.exists store p)
			then Store.mkdir store 0 (Perms.of_domain 0) p
		) [ "/local"; "/local/domain"; "/quota"; "/connection"; "/log" ];
	store

module type TRANSPORT = sig
  type server
  val listen: unit -> server Lwt.t

  type t
  val read: t -> string -> int -> int -> int Lwt.t
  val write: t -> string -> int -> int -> int Lwt.t
  val destroy: t -> unit Lwt.t
  val address_of: t -> Xs_packet.address

  val accept_forever: server -> (t -> unit Lwt.t) -> 'a Lwt.t
end

module Server = functor(T: TRANSPORT) -> struct
	module PS = PacketStream(T)

	let handle_connection t =
		debug "New connection";
		let address = T.address_of t in
		let c = Connection.create address in
		let channel = PS.make t in
		try_lwt
			lwt () = while_lwt true do
				lwt request = PS.recv channel in
				let reply = Call.reply store c request in
				PS.send channel reply
			done in
			T.destroy t
		with e ->
			error "Caught: %s" (Printexc.to_string e);
			Connection.destroy address;
			T.destroy t

	let serve_forever () =
		lwt server = T.listen () in
		T.accept_forever server handle_connection
end
