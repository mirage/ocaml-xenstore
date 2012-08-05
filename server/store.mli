(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 * Author Thomas Gazagnaire <thomas.gazagnaire@eu.citrix.com>
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

module Node : sig

type t = {
	name: Symbol.t;
	perms: Xs_packet.ACL.t;
	value: string;
	children: t list;
}

val create : string -> Xs_packet.ACL.t -> string -> t

val set_value: t -> string -> t
val set_perms: t -> Xs_packet.ACL.t -> t

end

exception Invalid_path

module Name : sig
	type t

	val introduceDomain: t
	val releaseDomain: t

	val of_string: string -> t
	val to_string: t -> string

	val is_relative: t -> bool

	val to_key: t -> string list
end

module Path : sig

exception Lookup_Doesnt_exist of string

exception Doesnt_exist

exception Already_exist

type t


val getdomainpath: int -> t

val create: string -> t -> t

val to_name: t -> Name.t

val to_string: t -> string

val get_hierarchy: t -> t list

val get_node: Node.t -> t -> Node.t option

val get_common_prefix: t -> t -> t

val get_parent: t -> t

val make_relative: t -> Name.t -> Name.t

end

type t =
{
	mutable stat_transaction_coalesce: int;
	mutable stat_transaction_abort: int;
	mutable root: Node.t;
	mutable quota: Quota.t;
}

val set_root: t -> Node.t -> unit
val set_quota: t -> Quota.t -> unit

val create: unit -> t

val copy: t -> t

val path_exists: t -> Path.t -> bool

val write: t -> Perms.t -> Path.t -> string -> unit

val mkdir: t -> Perms.t -> Path.t -> unit

val setperms: t -> Perms.t -> Path.t -> Xs_packet.ACL.t -> unit

val rm: t -> Perms.t -> Path.t -> unit

val ls: t -> Perms.t -> Path.t -> string list

val read: t -> Perms.t -> Path.t -> string

val getperms: t -> Perms.t -> Path.t -> Xs_packet.ACL.t

val get_node: t -> Path.t -> Node.t option
val set_node: t -> Path.t -> Node.t -> unit

val mark_symbols: t -> unit
