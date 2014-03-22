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

module type S = sig
  type v
  (** An item in the persistent queue *)

  type t
  (** A persistent queue *)

  val create: string list -> t Lwt.t
  (** [create name]: loads the queue at [name] *)

  val length: t -> int Lwt.t
  (** [length t]: the number of elements in queue *)

  val add: v -> t -> unit Lwt.t
  (** [add elem t]: adds the element [elem] to the queue [t].
      When the thread completes the element will be in the persistent
      store and will survive a crash. *)

  val clear: t -> unit Lwt.t
  (** [clear t]: deletes all elements in queue [t] *)

  val fold: ('b -> v -> 'b) -> 'b -> t -> 'b Lwt.t
  (** [fold f initial t]: folds [f] across [t] starting with [initial] *)

end
(** A persistent FIFO queue which stores v. *)

open Sexplib
open Xenstore

let debug fmt = Logging.debug "pqueue" fmt
let info  fmt = Logging.info  "pqueue" fmt
let error fmt = Logging.debug "pqueue" fmt

open Lwt

module Make(T: S.SEXPABLE) = struct
  module M = PMap.Make(Int64)(T)

  type v = T.t

  type t = {
    mutable next_id: int64;
    mutable root: M.t;
  }

  let create name =
    M.create name >>= fun root ->
    M.max_binding root >>= fun x ->
    let next_id = match x with
    | None -> 0L
    | Some (id, _) -> Int64.succ id in
    return { next_id; root }

  let length t = M.cardinal t.root

  let add item t =
    let id = t.next_id in
    t.next_id <- Int64.succ t.next_id;
    M.add id item t.root

  let clear t =
    t.next_id <- 0L;
    M.clear t.root

  let fold f initial t = M.fold (fun acc _ v -> f acc v) initial t.root
end
