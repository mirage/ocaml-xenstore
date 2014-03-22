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
  (** The value stored in the cell *)

  type t
  (** A persistent reference cell holding values of type v *)

  val create: string list -> v -> t Lwt.t
  (** [create name default]: loads the reference cell at [name].
      If the cell doesn't already exist, one is created with value
      [default] *)

  val destroy: t -> unit Lwt.t
  (** [destroy t]: removes the persistent cell *)

  val name: t -> string list
  (** [name t]: returns the [name] associated with the cell *)

  val get: t -> v Lwt.t
  (** [get t]: returns the current value *)

  val set: v -> t -> unit Lwt.t
  (** [set v t] sets the current value to [v]. When the thread completes
      the value is guaranteed to be in the persistent store and will
      survive a crash. *)
end
(** Create a persistent reference cell holding values of a given type. *)

open Sexplib
open Xenstore

let debug fmt = Logging.debug "pref" fmt
let info  fmt = Logging.info  "pref" fmt
let error fmt = Logging.debug "pref" fmt

open Lwt

module Make(V: S.SEXPABLE) = struct
  type v = V.t

  type t = {
    name: string list;
    default: v;
  }

  let recreate t =
    Database.store >>= fun db ->
    let tr = Transaction.make Transaction.none db in
    let path = Protocol.Path.of_string_list t.name in
    let perms = Perms.of_domain 0 in
    let v =
      if Transaction.exists tr perms path
      then try V.t_of_sexp (Sexp.of_string (Transaction.read tr perms path)) with _ -> t.default
      else t.default in
    Transaction.write tr None 0 (Perms.of_domain 0) path (Sexp.to_string (V.sexp_of_t v));
    Database.persist (Transaction.get_side_effects tr) >>= fun () ->
    return v

  let create name default =
    let t = { name; default } in
    recreate t >>= fun _ ->
    return t

  let destroy t =
    Database.store >>= fun db ->
    let tr = Transaction.make Transaction.none db in
    let path = Protocol.Path.of_string_list t.name in
    let perms = Perms.of_domain 0 in
    Transaction.rm tr perms path;
    Database.persist (Transaction.get_side_effects tr)

  let name t = t.name

  let set v t =
    Database.store >>= fun db ->
    let tr = Transaction.make Transaction.none db in
    Transaction.write tr None 0 (Perms.of_domain 0) (Protocol.Path.of_string_list t.name) (Sexp.to_string (V.sexp_of_t v));
    Database.persist (Transaction.get_side_effects tr)

  let get t = recreate t
end

open Sexplib.Std

module Int = Make(struct type t = int with sexp end)
