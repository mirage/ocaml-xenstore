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
    | Resume | Set_target
  (** The type of xenstore operation *)

  val to_string: t -> string
end

type t = {
  tid : int32;
  rid : int32;
  ty : Op.t;
  len: int;
  data : Buffer.t;
}

module Parser : sig
  (** Incrementally parse packets *)

  type state =
    | Unknown_operation of int32 (** received an unexpected message type *)
    | Parser_failed              (** we failed to parse a header *)
    | Need_more_data of int      (** we still need 'n' bytes *)
    | Packet of t                (** successfully decoded a packet *)

  type parse (** the internal state of the parser *)

  val start: unit -> parse
  (** create a parser set to the initial state *)

  val state: parse -> state
  (** query the state of the parser *)

  val input: parse -> string -> parse
  (** input some bytes into the parser. Must be no more than needed
      (see Need_more_data above) *)
end


exception Error of string
exception DataError of string

val create : int32 -> int32 -> Op.t -> string -> t
val to_string : t -> string
val get_tid : t -> int32
val get_ty : t -> Op.t
val get_data : t -> string
val get_rid : t -> int32

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
val with_path : Op.t -> int32 -> string -> t
val directory : int32 -> string -> t
val read : int32 -> string -> t
val getperms : int32 -> string -> t
val debug : string list -> t
val watch : string -> token -> t
val unwatch : string -> token -> t
val transaction_start : unit -> t
val transaction_end : int32 -> bool -> t
val introduce : int -> nativeint -> int -> t
val release : int -> t
val resume : int -> t
val getdomainpath : int -> t
val write : int32 -> string -> string -> t
val mkdir : int32 -> string -> t
val rm : int32 -> string -> t
val setperms : int32 -> string -> string -> t
