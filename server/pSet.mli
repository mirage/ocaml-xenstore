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

module Make(V: S.SEXPABLE) : sig
  type t
  (** A persistent set of values of type V.t *)

  val create: string list -> t Lwt.t
  (** [create name]: loads the set at [name] *)

  val name: t -> string list
  (** [name t]: returns the [name] associated with the set *)

  val cardinal: t -> int Lwt.t
  (** [cardinal t]: the number of elements in the set *)

  val add: V.t -> t -> unit Lwt.t
  (** [add v t]: adds [v] to set [t].
      When the thread completes the update will be in the persistent
      store and will survive a crash. *)

  val remove: V.t -> t -> unit Lwt.t
  (** [remove v t]: removes [v] from the set [t].
      When the thread completes the update will be in the persistent
      store and will survive a crash. *)

  val mem: V.t -> t -> bool Lwt.t
  (** [mem v t]: true if [v] is in [t], false otherwise *)

  val clear: t -> unit Lwt.t
  (** [clear t]: deletes all bindings from map [t] *)

  val fold: ('b -> V.t -> 'b) -> 'b -> t -> 'b Lwt.t
  (** [fold f initial t]: folds [f] across all the elements in
      [t] starting with [initial] *)
end
(** Create a persistent set which contains values of type V.t *)
