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

module Make(K: S.STRINGABLE)(V: S.SEXPABLE) : sig
  type t
  (** A persistent queue mapping stringable things to V.t *)

  val create: string list -> t Lwt.t
  (** [create name]: loads the map at [name] *)

  val name: t -> string list
  (** [name t]: returns the [name] associated with the map *)

  val cardinal: t -> int
  (** [cardinal t]: the number of bindings in the map *)

  val add: K.t -> V.t -> t -> unit Lwt.t
  (** [add k v t]: binds [k] to [v] in the map [t].
      When the thread completes the update will be in the persistent
      store and will survive a crash. *)

  val clear: t -> unit Lwt.t
  (** [clear t]: deletes all bindings from map [t] *)

  val fold: ('b -> K.t -> V.t -> 'b) -> 'b -> t -> 'b
  (** [fold f initial t]: folds [f] across [t] starting with [initial] *)

  val max_binding: t -> K.t * V.t
  (** [max_binding t]: returns the largest binding of the given map *)
end
(** Create a persistent map which associates strings with values of
    type V.t. This implementation caches the queue in the heap so reads
    are fast. Note this means you must not write to the underlying
    disk structure directly. *)
