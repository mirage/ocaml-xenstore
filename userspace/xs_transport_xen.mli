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

type 'a t = 'a Lwt.t
val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
val return: 'a -> 'a t

type channel

val read: channel -> string -> int -> int -> int Lwt.t
val write: channel -> string -> int -> int -> unit Lwt.t
val destroy: channel -> unit Lwt.t
val address_of: channel -> Xs_protocol.address Lwt.t

type server

val listen: unit -> server Lwt.t
val accept_forever: server -> (channel -> unit Lwt.t) -> 'a Lwt.t

val namespace_of: channel -> (module Xenstore_server.Namespace.IO) option
