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

module Make(T: S.SEXPABLE) : sig
  type t
  (** A persistent queue *)

  val create: string list -> t Lwt.t
  (** [create name]: loads the queue at [name] *)

  val length: t -> int Lwt.t
  (** [length t]: the number of elements in queue *)

  val add: T.t -> t -> unit Lwt.t
  (** [add elem t]: adds the element [elem] to the queue [t].
      When the thread completes the element will be in the persistent
      store and will survive a crash. *)

  val clear: t -> unit Lwt.t
  (** [clear t]: deletes all elements in queue [t] *)

  val fold: ('b -> T.t -> 'b) -> 'b -> t -> 'b Lwt.t
  (** [fold f initial t]: folds [f] across [t] starting with [initial] *)

end
(** Create a persistent FIFO queue which stores T.t. This implementation
    caches the queue in memory so reads are fast. Note this means you
    must not write to the underlying queue structure directly. *)
