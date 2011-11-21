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

module type TRANSPORT = sig
  type t
  val create: unit -> t Lwt.t
  val destroy: t -> unit Lwt.t
  val read: t -> string -> int -> int -> int Lwt.t
  val write: t -> string -> int -> int -> int Lwt.t
end

exception Unknown_xenstore_operation of int32
exception Response_parser_failed
exception Malformed_watch_event
exception Unexpected_rid of int32
exception Dispatcher_failed

module Client : functor(T: TRANSPORT) -> sig
  type client
  (** A multiplexing xenstore client *)

  val make : unit -> client Lwt.t
  (** [make ()] initialises and returns a xenstore client *)

  type handle
  (** A handle represents a single thread's xenstore access *)

  val with_xs : client -> (handle -> 'a Lwt.t) -> 'a Lwt.t
  (** Access xenstore with individual operations *)

  val with_xst : client -> (handle -> 'a Lwt.t) -> 'a Lwt.t
  (** Access xenstore with a single transaction.
      On conflict the operation will be repeated. *)

  val wait : client -> (handle -> 'a Lwt.t) -> 'a Lwt.t
  (** Wait for some condition to become true and return a value.
      The function argument should throw Eagain if the condition
      is not met, and the condition will be re-evaluated when paths
      change. *)

  val directory : handle -> string -> string list Lwt.t
  (** [directory h path] returns the directory listing of [path] *)

  val read : handle -> string -> string Lwt.t
  (** [read h path] returns the value at [path] or raises Enoent *)

  val write : handle -> string -> string -> unit Lwt.t
  (** [write h k v] writes [v] at [k] *)

  val watch : handle -> string -> Xs_packet.Token.t -> unit Lwt.t
  (** [watch h path token] registers a manual watch at [path] with [token] *)

  val unwatch : handle -> string -> Xs_packet.Token.t -> unit Lwt.t
  (** [unwatch h path token] unregisters a manual watch at [path] with [token] *)

end
