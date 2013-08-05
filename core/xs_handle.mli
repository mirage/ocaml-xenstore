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

module StringSet : module type of Set.Make(struct type t = string let compare = compare end)

(** XenStore handles. *)

type 'a t
(** A 'handle' is a sub-connection used for a particular purpose.
    The handle is a convenient place to store sub-connection state. *)

(** Functions to create handles. *)

val no_transaction: 'a -> 'a t
(** Handle used for 'immediate' non-transactional read/writes. *)

val transaction: 'a -> int32 -> 'a t
(** Handle used for transactional read/writes. *)

val watching: 'a -> 'a t
(** Handle used to store watch-related information. *)

(** Accessors for the handle type. *)

val tid: 'a t -> int32
(** [tid h] is the transaction id (typically for debug printing). *)

val client: 'a t -> 'a
(** [client h] is the client instance wrapped in a [h]. *)

val add_accessed_path: 'a t -> string -> 'a t
(** [set_accessed_path h path] sets [path] as an accessed path iff the
    set of accessed path contained in [h] is not empty. *)

val accessed_paths: 'a t -> StringSet.t
(** [get_accessed_path h] is the set of paths that we have
    accessed. *)

val watch: 'a t -> string -> 'a t
(** Declare that we are watching a path. *)

val unwatch: 'a t -> string -> 'a t
(** Declare that we are no longer watching a path. *)

val watched_paths: 'a t -> StringSet.t
(** [get_watched_paths h] is the list of paths we're currently
    watching. *)

