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

module Watch : sig
  type t = Protocol.Name.t * string with sexp
end

module Watch_events: PQueue.S with type v := Watch.t

module Watch_registrations: PSet.S with type v := Watch.t

module PInt32: PRef.S with type v := int32

module PPerms: PRef.S with type v := Perms.t

type t
(** A currently-open connection *)

type w
(** A watch registered by a connection *)

val by_address: (Uri.t, t) Hashtbl.t
(** an index of all currently open connections *)

val index: t -> int
(** a unique id associated with a connection *)

val domainpath: t -> Protocol.Name.t
(** The default domain directory associated with a connection *)

val domid: t -> int
(** The domain id associated with a connection *)

val address: t -> Uri.t
(** The address associated with a connection *)

val perm: t -> PPerms.t
(** The premissions associated with a connection *)

val watch_events: t -> Watch_events.t
(** A connection's watch event queue *)

val pop_watch_events: t -> Watch.t list Lwt.t
(** Dequeues a list of watch events *)

val pop_watch_events_nowait: t -> Watch.t list Lwt.t
(** Dequeues any pending watch events without blocking *)

val fire_one: Limits.t option -> Protocol.Name.t option -> w -> unit Lwt.t
(** [fire_one limits name w] enqueues single watch event provided
    the watch event queue hasn't reached its limit. If the watch was
    triggered because some name changed then that name is provided. *)

val watch: t -> Limits.t option -> Watch.t -> unit Lwt.t
(** [watch t limits watch] registered [watch] on connection [t] *)

val unwatch: t -> Watch.t -> unit Lwt.t
(** [unwatch t watch] removes the registration associated with [watch].
    Raises Enoent if the watch registration does not exist. *)

val fire: Limits.t option -> (Protocol.Op.t * Protocol.Name.t) -> unit Lwt.t
(** [fire limits (op, name)] fires watches associated with [name] after
    operatino [op] *)

val register_transaction: Limits.t option -> t -> Store.t -> int32 Lwt.t
(** [register_transaction limits t store] allocates and returns a fresh
    transaction id *)

val unregister_transaction: t -> int32 -> unit
(** [unregister_transaction t tid] forgets about the transaction with
    id [tid]. *)

val get_transaction: t -> int32 -> Transaction.t
(** [get_transactino t tid] retrieves the transaction associated with [tid],
    throwing Not_found if it does not exist. *)

val incr_nb_ops: t -> unit
(** Increment the per-connection operations counter *)

val mark_symbols: t -> unit

val destroy: Uri.t -> unit Lwt.t
(** [destroy address] destroys any connection associated with [address] *)

val create: (Uri.t * int) -> t Lwt.t
(** [create (address, domid)] creates a connection associated with [address]
    and [domid] *)

module Introspect: Tree.S
