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

val debug: string -> ('b, unit, string, unit) format4 -> 'b
val info: string -> ('b, unit, string, unit) format4 -> 'b
val warn: string -> ('b, unit, string, unit) format4 -> 'b
val error: string -> ('b, unit, string, unit) format4 -> 'b

type logger
(** An in-memory non-blocking logger with a fixed size circular buffer.
    If the buffer is full then some messages may be dropped. The logger
    will replace runs of dropped messages with a single message with
    a count of how many messages were dropped. *)

val logger: logger
(** The application logger *)

val get: logger -> string list Lwt.t
(** [get logger] returns any available log lines or, if none are available,
    blocks until some are available. *)
