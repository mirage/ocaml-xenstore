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

type t
(** A Node in the main xenstore tree *)

val create : string -> int -> Protocol.ACL.t -> string -> t
(** [create name creator perms value] returns fresh Node.t *)

val fold: ('a -> t -> 'a) -> t -> 'a -> 'a
(** [fold f t initial] folds [f] over the tree [t] in a depth-first
    fashion. *)

val get_name: t -> string
(** [get_name t] returns the string name associated with [t] *)

val get_symbol: t -> Symbol.t
(** [get_symbol t] returns the symbol representing the name associated with [t] *)

val get_creator: t -> int
(** [get_creator t] returns the domain id which created [t] *)

val get_perms: t -> Protocol.ACL.t
(** [get_perms t] returns the permissions attached to [t] *)

val set_perms: t -> Protocol.ACL.t -> t
(** [set_perms t newperms] returns [t] with the permissions set to
    [newperms] *)

val get_value: t -> string
(** [get_value t] returns the value associated with [t] *)

val set_value: t -> string -> t
(** [set_value t newvalue] returns [t] with the value set to [newvalue].
    Note if the newvalue is equal to the old value according to String.compare
    then we guarantee that [t == t] *)

val get_children: t -> t list
(** [get_children t] returns all children of [t] as a list *)

val exists: t -> string -> bool
(** [exists t childname]: true if [t] has a child called [childname] *)

val find: t -> string -> t
(** [find t childname]: returns the child of [t] with name [childname]
    or raises Not_found *)

val replace_child: t -> Symbol.t -> t -> t
(** [replace_child t symbol new_child] returns a copy of [t] where the
    child named [symbol] has been replaced by [new_child] *)

val add_child: t -> t -> t
(** [add_child t new_child] returns [t] with [new_child] added *)


val del_childname: t -> string -> t
(** [del_childname t childname] returns [t] without the child named
    [childname] *)

val del_all_children: t -> t
(** [del_all_children t] returns [t] with no children *)

exception Doesnt_exist of Protocol.Path.t

val with_parent: t -> Protocol.Path.t -> (t -> Protocol.Path.Element.t -> 'a) -> 'a
(** [with_parent t path f] returns [f t' element] where [t'] is the node which
    would contain a child calle [element] which would be found by following
    [path] from [t]. Raises Not_found if the parent node does not exist. *)

val modify: t -> Protocol.Path.t -> (t -> string -> t) -> t
(** [modify t path f] constructs a new tree where the node at [path] in [t]
    has been replaced by [f parent name] *)

val replace: t -> Protocol.Path.t -> t -> t
(** [replace t path t'] replaces the node at [path] with [t'] *)

val lookup: t -> Protocol.Path.t -> t option
(** [lookup t path] returns [Some t] where [t] can be found at the end of
    [path] from [t], or [None] otherwise. *)
