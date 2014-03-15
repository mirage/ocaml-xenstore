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
open Sexplib.Std
open Lwt

let debug fmt = Logging.debug "quota" fmt
let info  fmt = Logging.info  "quota" fmt
let warn  fmt = Logging.warn  "quota" fmt

module PerDomain = PMap.Make(struct
  type t = int
  let of_string = int_of_string
  let to_string = string_of_int
end)(struct type t = int with sexp end)

let prefix = [ "tool"; "xenstored"; "quota" ]

(* Global default quotas: *)
let maxent         = PRef.Int.create (prefix @ [ "default"; "number-of-entries" ]) 10000
let maxsize        = PRef.Int.create (prefix @ [ "default"; "entry-length" ]) 4096
let maxwatch       = PRef.Int.create (prefix @ [ "default"; "number-of-registered-watches" ]) 50
let maxtransaction = PRef.Int.create (prefix @ [ "default"; "number-of-active-transactions" ]) 20
let maxwatchevent  = PRef.Int.create (prefix @ [ "default"; "number-of-queued-watch-events" ]) 256

(* Per-domain quota overrides: *)
let maxent_overrides         = PerDomain.create (prefix @ [ "number-of-entries" ])
let maxwatch_overrides       = PerDomain.create (prefix @ [ "number-of-registered-watches" ])
let maxtransaction_overrides = PerDomain.create (prefix @ [ "number-of-active-transactions" ])

let remove domid =
  maxent_overrides >>= fun maxent_overrides ->
  maxwatch_overrides >>= fun maxwatch_overrides ->
  maxtransaction_overrides >>= fun maxtransaction_overrides ->
  PerDomain.remove domid maxent_overrides >>= fun () ->
  PerDomain.remove domid maxwatch_overrides >>= fun () ->
  PerDomain.remove domid maxtransaction_overrides

(* A snapshot of the current state for a given domid, needed to check
   for quota violations during a transaction. *)
let limits_of_domain domid =
  maxent >>= fun maxent ->
  maxsize >>= fun maxsize ->
  maxwatch >>= fun maxwatch ->
  maxwatchevent >>= fun maxwatchevent ->
  maxtransaction >>= fun maxtransaction ->
  maxent_overrides >>= fun maxent_overrides ->
  maxwatch_overrides >>= fun maxwatch_overrides ->
  maxtransaction_overrides >>= fun maxtransaction_overrides ->
  PerDomain.mem domid maxent_overrides >>= fun b ->
  (if b then
          PerDomain.find domid maxent_overrides 
  else PRef.Int.get maxent) >>= fun number_of_entries ->
  PRef.Int.get maxsize >>= fun entry_length ->
  PerDomain.mem domid maxwatch_overrides >>= fun b ->
  (if b then PerDomain.find domid maxwatch_overrides else PRef.Int.get maxwatch) >>= fun number_of_registered_watches ->
  PerDomain.mem domid maxtransaction_overrides >>= fun b ->
  (if b then PerDomain.find domid maxtransaction_overrides else PRef.Int.get maxtransaction) >>= fun number_of_active_transactions ->
  PRef.Int.get maxwatchevent >>= fun number_of_queued_watch_events ->
  return { Limits.number_of_entries; entry_length; number_of_registered_watches; number_of_active_transactions; number_of_queued_watch_events }
