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

val reply: Store.t -> Limits.t option -> Connection.t -> Xenstore.Protocol.Header.t -> Xenstore.Protocol.Request.t -> Xenstore.Protocol.Response.t * Transaction.side_effects
(** [reply store limits con request] modifies [store] according to the request in
    [request] from connection [con] and returns the response to be sent to the client
    together with a description of the side-effects (watches, path writes, deletes). *)
