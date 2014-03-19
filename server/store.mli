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
open Xenstore

exception Already_exists of string
(** thrown when a (watch) name already exists *)

type t =
{
	mutable stat_transaction_abort: int;
	mutable root: Node.t;
        created: (int, int) Hashtbl.t;
} with sexp

type update =
| Write of Protocol.Path.t * Protocol.ACL.t * string
| Rm of Protocol.Path.t
with sexp

val getdomainpath: int -> Protocol.Name.t
(** [getdomainpath domid] returns the default directory for [domid] *)

val create: unit -> t

val copy: t -> t

val exists: t -> Protocol.Path.t -> bool

val write: t -> Limits.t option -> int -> Perms.t -> Protocol.Path.t -> string -> update

val mkdir: t -> Limits.t option -> int -> Perms.t -> Protocol.Path.t -> update

val setperms: t -> Perms.t -> Protocol.Path.t -> Protocol.ACL.t -> update

val rm: t -> Perms.t -> Protocol.Path.t -> update list

val ls: t -> Perms.t -> Protocol.Path.t -> string list

val read: t -> Perms.t -> Protocol.Path.t -> string

val getperms: t -> Perms.t -> Protocol.Path.t -> Protocol.ACL.t

val mark_symbols: t -> unit
