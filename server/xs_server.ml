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

module type TRANSPORT = sig
  type server
  val listen: unit -> server

  type t
  val read: t -> string -> int -> int -> int Lwt.t
  val write: t -> string -> int -> int -> int Lwt.t
  val destroy: t -> unit Lwt.t

  val accept_forever: server -> (t -> unit Lwt.t) -> 'a Lwt.t
end

module Server = functor(T: TRANSPORT) -> struct

  let handle_connection t =
    T.destroy t

  let serve_forever () =
    let server = T.listen () in
    T.accept_forever server handle_connection
end
