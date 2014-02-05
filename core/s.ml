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

module type TRANSPORT = sig
  type 'a t = 'a Lwt.t
  val return: 'a -> 'a Lwt.t
  val ( >>= ): 'a t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

  type server
  val listen: unit -> server Lwt.t

  type channel
  val read: channel -> string -> int -> int -> int Lwt.t
  val write: channel -> string -> int -> int -> unit Lwt.t
  val destroy: channel -> unit Lwt.t
  val address_of: channel -> Xs_protocol.address Lwt.t
  val accept_forever: server -> (channel -> unit Lwt.t) -> 'a Lwt.t

  module Introspect : sig
    val list: channel -> string list -> string list
    val read: channel -> string list -> string option
  end
end

