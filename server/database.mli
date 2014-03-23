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

val persist: Transaction.side_effects -> unit Lwt.t
(** Persists the given side-effects. Make sure you start exactly one
    persistence thread *)

val store: Store.t Lwt.t
(** The in-memory copy of the database *)

val initialise: S.persistence -> unit Lwt.t
(** [initialise persistence-policy] initialises the database. If
    [persistence-policy] is [NoPersistence] then all updates are discarded.
    If [persistence-policy] is [Git path] then all updates are
    stored in a git database located at [path] *)
