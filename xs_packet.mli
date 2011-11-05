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


module Op : sig
  type t =
    | Debug | Directory | Read | Getperms
    | Watch | Unwatch | Transaction_start
    | Transaction_end | Introduce | Release
    | Getdomainpath | Write | Mkdir | Rm
    | Setperms | Watchevent | Error | Isintroduced
    | Resume | Set_target | Restrict
  (** The type of xenstore operation *)

  val to_string: t -> string
end

module Partial : sig
  (** Help unmarshal whole packets by buffering fragments *)

  type pkt

  type buf =
    | HaveHdr of pkt 
    | NoHdr of int * string

  val empty: unit -> buf

  val header_size: int
  val of_string: string -> pkt
  val append: pkt -> string -> int -> unit
  val to_complete: pkt -> int
end


type t = { tid : int; rid : int; ty : Op.t; data : string; }
exception Error of string
exception DataError of string
external string_of_header : int -> int -> int -> int -> string = "stub_string_of_header"
val create : int -> int -> Op.t -> string -> t
val of_partialpkt : Partial.pkt -> t
val to_string : t -> string
val unpack : t -> int * int * Op.t * string
val get_tid : t -> int
val get_ty : t -> Op.t
val get_data : t -> string
val get_rid : t -> int

type token
(** A token is associated with every watch and returned in the callback *)

val token_to_string: token -> string
(** [token_to_string token] returns a debug-printable version of [token] *)

val create_token: string -> token
(** [create_token x] transforms [x] into a fresh watch token *)

val user_string_of_token: token -> string
(** [user_string_of_token token] returns the user-supplied part of [token] *)

val parse_token: string -> token
(** [parse_token x] parses the marshalled token [x] *)

val data_concat : string list -> string
val with_path : Op.t -> int -> string -> t
val directory : int -> string -> t
val read : int -> string -> t
val getperms : int -> string -> t
val debug : string list -> t
val watch : string -> token -> t
val unwatch : string -> token -> t
val transaction_start : unit -> t
val transaction_end : int -> bool -> t
val introduce : int -> nativeint -> int -> t
val release : int -> t
val resume : int -> t
val getdomainpath : int -> t
val write : int -> string -> string -> t
val mkdir : int -> string -> t
val rm : int -> string -> t
val setperms : int -> string -> string -> t
