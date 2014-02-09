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

(** A multiplexing XenStore protocol client over a byte-level transport, using Lwt. *)

exception Malformed_watch_event
exception Unexpected_rid of int32
exception Dispatcher_failed

module Make : functor(IO: S.TRANSPORT) -> S.CLIENT
