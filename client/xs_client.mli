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
  val write: t -> string -> int -> int -> unit Lwt.t
end

exception Malformed_watch_event
exception Unexpected_rid of int32
exception Dispatcher_failed

module Client : functor(T: TRANSPORT) -> sig
  type client
  (** A multiplexing xenstore client *)

  val make : unit -> client Lwt.t
  (** [make ()] initialises and returns a xenstore client *)

  val suspend : client -> unit Lwt.t
  (** [suspend ()] suspends the client, waiting for outstanding
      RPCs to be completed, cancelling all watches
      and causing new requests to be queued *)

  val resume : client -> unit Lwt.t
  (** [resume ()] resumes the client. The connection must
      be up and running again before using this function. *)

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

  val rm : handle -> string -> unit Lwt.t
  (** [rm h k] removes the node [k] *)

  val mkdir : handle -> string -> unit Lwt.t
  (** [mkdir h k] creates the node [k] with an empty value *)

  val setperms : handle -> string -> Xs_protocol.ACL.t -> unit Lwt.t
  (** [setperms h k acl] sets the permissions of [k] to [acl] *)

  val debug : handle -> string list -> string list Lwt.t
  (** [debug cmd_args] invokes a debug command *)

  val restrict : handle -> int -> unit Lwt.t
  (** [restrict h domid] restricts the current connection to have only
	  the priviledges associated with domain [domid] *)

  val getdomainpath : handle -> int -> string Lwt.t
  (** [getdomainpath domid] returns the local directory of domain [domid] *)

  val watch : handle -> string -> Xs_protocol.Token.t -> unit Lwt.t
  (** [watch h path token] registers a manual watch at [path] with [token] *)

  val unwatch : handle -> string -> Xs_protocol.Token.t -> unit Lwt.t
  (** [unwatch h path token] unregisters a manual watch at [path] with [token] *)

end
