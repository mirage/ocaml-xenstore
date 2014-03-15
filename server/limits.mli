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
exception Limit_reached
(** The maximum number of entries has been created *)

exception Data_too_big
(** The value to be written is too big *)

exception Transaction_opened
(** Too many transactions have been started *)

type t = {
  number_of_entries: int;
  entry_length: int;
  number_of_registered_watches: int;
  number_of_active_transactions: int;
  number_of_queued_watch_events: int;
} with sexp
