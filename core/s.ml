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

module type IO = sig
  type 'a t = 'a Lwt.t
  val return: 'a -> 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
end

module type TRANSPORT = sig
  include IO

  type server
  val listen: unit -> server t

  type channel
  val read: channel -> string -> int -> int -> int t
  val write: channel -> string -> int -> int -> unit t
  val destroy: channel -> unit t

  val address_of: channel -> Uri.t t
  val domain_of: channel -> int

  val accept_forever: server -> (channel -> unit t) -> 'a t

  module Introspect : sig
    val list: channel -> string list -> string list
    val read: channel -> string list -> string option
  end
end
