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

val mount: Xenstore.Protocol.Path.t -> (module Tree.S) -> unit Lwt.t
(** [mount mountpoint implementation] attaches the [implementation] tree
    at the given [mountpoint] path such that accessing paths for which
    [mountpoint] is a prefix, are performed within [implementation] *)

val unmount: Xenstore.Protocol.Path.t -> unit Lwt.t
(** [unmount mountpoint] removes any attached implementation from
    [mountpoint] and deletes [mountpoint] if it is now empty. *)

module Tree: Tree.S
(** A tree with all the 'mounts' overlayed on top of the store *)
