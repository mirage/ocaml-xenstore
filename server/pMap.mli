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

  val cardinal: t -> int Lwt.t
  (** [cardinal t]: the number of bindings in the map *)

  val add: K.t -> V.t -> t -> unit Lwt.t
  (** [add k v t]: binds [k] to [v] in the map [t].
      When the thread completes the update will be in the persistent
      store and will survive a crash. *)

  val remove: K.t -> t -> unit Lwt.t
  (** [remove k t]: removes the binding [k] from [t] if it exists.
      If there is no binding then return ().
      When the thread completes the update will be in the persistent
      store and will survive a crash. *)

  val find: K.t -> t -> V.t Lwt.t
  (** [find k t]: returns the current binding of [k] in [t] or raises Not_found *)

  val mem: K.t -> t -> bool Lwt.t
  (** [mem k t]: true if [k] is bound in [t], false otherwise *)

  val clear: t -> unit Lwt.t
  (** [clear t]: deletes all bindings from map [t] *)

  val fold: ('b -> K.t -> V.t -> 'b) -> 'b -> t -> 'b Lwt.t
  (** [fold f initial t]: folds [f] across [t] starting with [initial] *)

  val max_binding: t -> (K.t * V.t) option Lwt.t
  (** [max_binding t]: returns the largest binding of the given map *)

  val min_binding: t -> (K.t * V.t) option Lwt.t
  (** [min_binding t]: returns the smallest binding of the given map *)
end
(** Create a persistent map which associates strings with values of
    type V.t. *)
