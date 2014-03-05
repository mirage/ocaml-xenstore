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
  val create: unit -> channel t
  val read: channel -> Cstruct.t -> unit t
  val write: channel -> Cstruct.t -> unit t
  val destroy: channel -> unit t

  val address_of: channel -> Uri.t t
  val domain_of: channel -> int

  val accept_forever: server -> (channel -> unit t) -> 'a t

  module Introspect : sig
    val ls: channel -> string list -> string list
    val read: channel -> string list -> string option
    val write: channel -> string list -> string -> bool
  end
end

module type CLIENT = sig
  include IO

  type client

  val make : unit -> client t
  val suspend : client -> unit t
  val resume : client -> unit t

  type handle

  val immediate : client -> (handle -> 'a t) -> 'a t
  val transaction : client -> (handle -> 'a t) -> 'a t
  val wait : client -> (handle -> 'a t) -> 'a t
  val directory : handle -> string -> string list t
  val read : handle -> string -> string t
  val write : handle -> string -> string -> unit t
  val rm : handle -> string -> unit t
  val mkdir : handle -> string -> unit t
  val setperms : handle -> string -> Protocol.ACL.t -> unit t
  val debug : handle -> string list -> string list t
  val restrict : handle -> int -> unit t
  val getdomainpath : handle -> int -> string t
  val watch : handle -> string -> Protocol.Token.t -> unit t
  val unwatch : handle -> string -> Protocol.Token.t -> unit t
  val introduce : handle -> int -> nativeint -> int -> unit t
  val set_target : handle -> int -> int -> unit t
end

type persistence =
| NoPersistence (** lose updates after a restart *)
| Git of string (** persist all updates to a git repo on disk *)

module type SERVER = sig
  include IO

  val serve_forever: persistence -> unit t
end
