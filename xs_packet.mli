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

type t (** a valid packet *)

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

module ACL : sig
    (** Access control lists *)

  type perm =
    | NONE
    | READ
    | WRITE
    | RDWR

  type t = int * perm * (int * perm) list
  (** owner domid * default for others * access control list *)

  val of_string: string -> t option

  val to_string: t -> string

end

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

module Request : sig
  val directory : int32 -> string -> t option
  val read : int32 -> string -> t option
  val getperms : int32 -> string -> t option
  val debug : string list -> t option
  val watch : string -> token -> t option
  val unwatch : string -> token -> t option
  val transaction_start : unit -> t option
  val transaction_end : int32 -> bool -> t option
  val introduce : int -> nativeint -> int -> t option
  val release : int -> t option
  val resume : int -> t option
  val getdomainpath : int -> t option
  val write : int32 -> string -> string -> t option
  val mkdir : int32 -> string -> t option
  val rm : int32 -> string -> t option
  val setperms : int32 -> string -> string -> t option
end

module Response : sig
  val string : t -> string option
  val list : t -> string list option
  val acl : t -> ACL.t option
  val int : t -> int option
end
