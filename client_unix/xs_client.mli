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

module type IO = sig
  type 'a t
  val return: 'a -> 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t

  type channel
  val create: unit -> channel t
  val destroy: channel -> unit t
  val read: channel -> string -> int -> int -> int t
  val write: channel -> string -> int -> int -> unit t
end

exception Malformed_watch_event
exception Unexpected_rid of int32
exception Dispatcher_failed

module Client : functor(IO: IO) -> sig
  type client
  (** A multiplexing xenstore client *)

  val make : unit -> client IO.t
  (** [make ()] initialises and returns a xenstore client *)

  type handle
  (** A handle represents a single thread's xenstore access *)

  val with_xs : client -> (handle -> 'a IO.t) -> 'a IO.t
  (** Access xenstore with individual operations *)

  val with_xst : client -> (handle -> 'a IO.t) -> 'a IO.t
  (** Access xenstore with a single transaction.
      On conflict the operation will be repeated. *)

  val wait : client -> (handle -> 'a IO.t) -> 'a IO.t
  (** Wait for some condition to become true and return a value.
      The function argument should throw Eagain if the condition
      is not met, and the condition will be re-evaluated when paths
      change. *)

  val directory : handle -> string -> string list IO.t
  (** [directory h path] returns the directory listing of [path] *)

  val read : handle -> string -> string IO.t
  (** [read h path] returns the value at [path] or raises Enoent *)

  val write : handle -> string -> string -> unit IO.t
  (** [write h k v] writes [v] at [k] *)

  val rm : handle -> string -> unit IO.t
  (** [rm h k] removes the node [k] *)

  val mkdir : handle -> string -> unit IO.t
  (** [mkdir h k] creates the node [k] with an empty value *)

  val setperms : handle -> string -> Xs_protocol.ACL.t -> unit IO.t
  (** [setperms h k acl] sets the permissions of [k] to [acl] *)

  val debug : handle -> string list -> string list IO.t
  (** [debug cmd_args] invokes a debug command *)

  val restrict : handle -> int -> unit IO.t
  (** [restrict h domid] restricts the current connection to have only
	  the priviledges associated with domain [domid] *)

  val getdomainpath : handle -> int -> string IO.t
  (** [getdomainpath domid] returns the local directory of domain [domid] *)

  val watch : handle -> string -> Xs_protocol.Token.t -> unit IO.t
  (** [watch h path token] registers a manual watch at [path] with [token] *)

  val unwatch : handle -> string -> Xs_protocol.Token.t -> unit IO.t
  (** [unwatch h path token] unregisters a manual watch at [path] with [token] *)

end
