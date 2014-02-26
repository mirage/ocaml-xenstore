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
	mutable quota: Quota.t;
}

val dump_stdout: t -> unit

val getdomainpath: int -> Protocol.Name.t
(** [getdomainpath domid] returns the default directory for [domid] *)

val set_root: t -> Node.t -> unit
val set_quota: t -> Quota.t -> unit

val create: unit -> t

val copy: t -> t

val exists: t -> Protocol.Path.t -> bool

val write: t -> int -> Perms.t -> Protocol.Path.t -> string -> unit

val mkdir: t -> int -> Perms.t -> Protocol.Path.t -> unit

val setperms: t -> Perms.t -> Protocol.Path.t -> Protocol.ACL.t -> unit

val rm: t -> Perms.t -> Protocol.Path.t -> unit

val ls: t -> Perms.t -> Protocol.Path.t -> string list

val read: t -> Perms.t -> Protocol.Path.t -> string

val getperms: t -> Perms.t -> Protocol.Path.t -> Protocol.ACL.t


val replace: t -> Protocol.Path.t -> Node.t -> Quota.t -> Quota.t -> unit
(** [replace t path node original_quota new_quota]: replaces the node
    at [path] in [t] with [node], updating the quote at the same time. *)

val mark_symbols: t -> unit
