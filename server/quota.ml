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

let debug fmt = Logging.debug "quota" fmt
let info  fmt = Logging.info  "quota" fmt
let warn  fmt = Logging.warn  "quota" fmt

exception Limit_reached
exception Data_too_big
exception Transaction_opened

type domid = int

(* Global defaults *)
let maxent = ref (10000)
let maxsize = ref (4096)

(* Per-domain maxent overrides *)
let maxent_overrides = Hashtbl.create 10

let maxent_of_domain domid =
	if Hashtbl.mem maxent_overrides domid
	then Hashtbl.find maxent_overrides domid
	else !maxent

type t = {
	cur: (domid, int) Hashtbl.t; (* current domains entry usage *)
}

let create () =
	{ cur = Hashtbl.create 100; }

let copy quota = { cur = (Hashtbl.copy quota.cur) }

let del quota id = Hashtbl.remove quota.cur id

let check quota id size =
	if size > !maxsize then (
		warn "domain %u err create entry: data too big %d" id size;
		raise Data_too_big
	);
	if Hashtbl.mem quota.cur id then
		let entry = Hashtbl.find quota.cur id in
		let maxent = maxent_of_domain id in
		if entry >= maxent then (
			warn "domain %u cannot create entry: quota reached (%d)" id maxent;
			raise Limit_reached
		)

let list quota =
	Hashtbl.fold (fun domid x acc -> (domid, x) :: acc) quota.cur []

let get quota id =
	if Hashtbl.mem quota.cur id
	then Hashtbl.find quota.cur id
	else 0

let set quota id nb =
	if nb = 0
	then Hashtbl.remove quota.cur id
	else begin
	if Hashtbl.mem quota.cur id then
		Hashtbl.replace quota.cur id nb
	else
		Hashtbl.add quota.cur id nb
	end

let decr quota id =
	let nb = get quota id in
	if nb > 0
	then set quota id (nb - 1)

let incr quota id =
	let nb = get quota id in
	set quota id (nb + 1)

let union quota diff =
	Hashtbl.iter (fun id nb -> set quota id (get quota id + nb)) diff.cur


