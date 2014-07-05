(*
 * Copyright (C) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Sexplib

module type STRINGABLE = sig
  type t
  val to_string: t -> string
  val of_string: string -> t
end

module type SEXPABLE = sig
  type t
  val sexp_of_t: t -> Sexp.t
  val t_of_sexp: Sexp.t -> t
end

module type IO = sig
  type 'a t = 'a Lwt.t
  val return: 'a -> 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
end

module type CONNECTION = sig
  include IO

  type connection

  val create: unit -> connection t

  val destroy: connection -> unit t

  val address_of: connection -> Uri.t t

  val domain_of: connection -> int

  val read: connection -> Cstruct.t -> unit t

  val write: connection -> Cstruct.t -> unit t
end

module type SERVER = sig
  include IO
  include CONNECTION
    with type 'a t := 'a t

  type server

  val listen: unit -> server t

  val accept_forever: server -> (connection -> unit t) -> 'a t

  type offset with sexp

  val get_read_offset: connection -> offset t

  val get_write_offset: connection -> offset t

  val flush: connection -> offset -> unit t

  val enqueue: connection -> Protocol.Header.t -> Protocol.Response.t -> offset t

  val recv: connection -> offset -> (offset * [ `Ok of (Protocol.Header.t * Protocol.Request.t) | `Error of string ]) t

  module Introspect : sig
    val ls: connection -> string list -> string list
    val read: connection -> string list -> string option
    val write: connection -> string list -> string -> bool
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
