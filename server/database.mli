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

val no_persistence: unit -> unit Lwt.t
(** a thread which throws all side-effects away *)

val git_persistence: string -> unit Lwt.t
(** [git_persistence filename]: a thread which persists all side-effects
    to the database at [filename] *)

val persist: Transaction.side_effects -> unit Lwt.t
(** Persists the given side-effects. Make sure you start exactly one
    persistence thread *)

val store: Store.t
(** The in-memory copy of the database *)
