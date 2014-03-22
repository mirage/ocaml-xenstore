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

let info fmt = Logging.info "perms" fmt
open Xenstore

exception Permission_denied

type domid = int

(* permission of connections *)
open Protocol.ACL
open Sexplib.Std

type elt = domid * (perm list) with sexp
type t = {
  main: elt;
  target: elt option
} with sexp

let superuser : t =
	{ main = 0, [READ; WRITE];
	  target = None }

let of_domain domid : t =
	{ main = (domid, [READ; WRITE]);
	  target = None }

let set_target (connection:t)  domid =
	{ connection with target = Some (domid, [READ; WRITE]) }

let get_owners (connection:t) =
	match connection.main, connection.target with
	| c1, Some c2 -> [ fst c1; fst c2 ]
	| c1, None    -> [ fst c1 ]

let is_owner (connection:t) id =
	match connection.target with
	| Some target -> fst connection.main = id || fst target = id
	| None        -> fst connection.main = id

let is_dom0 (connection:t) =
	is_owner connection 0

let restrict (connection:t) domid =
	match connection.target, connection.main with
	| None, (0, perms) ->
		info "restricting connection from domid %d to domid %d" 0 domid;
		{ connection with main = (domid, perms) }
	| _                -> raise Permission_denied

type permission =
	| READ
	| WRITE
	| CHANGE_ACL
	| DEBUG
	| INTRODUCE
	| ISINTRODUCED
	| RESUME
	| RELEASE
	| SET_TARGET
	| RESTRICT
	| CONFIGURE

let has (t: t) p =
	if not(is_dom0 t) then raise Permission_denied

(* check if owner of the current connection and of the current node are the same *)
let check_owner (connection:t) (node:Protocol.ACL.t) =
	if not (is_dom0 connection)
	then is_owner connection node.Protocol.ACL.owner
	else true

(* check if the current connection has the requested perm on the current node *)
let check (connection:t) request (node:Protocol.ACL.t) =
	let check_acl domainid =
		let perm =
			if List.mem_assoc domainid node.Protocol.ACL.acl
			then List.assoc domainid node.Protocol.ACL.acl
			else node.Protocol.ACL.other
		in
		match perm, request with
		| Protocol.ACL.NONE, _ ->
			info "Permission denied: Domain %d has no permission" domainid;
			false
		| Protocol.ACL.RDWR, _ -> true
		| Protocol.ACL.READ, READ -> true
		| Protocol.ACL.WRITE, WRITE -> true
		| Protocol.ACL.READ, _ ->
			info "Permission denied: Domain %d has read only access" domainid;
			false
		| Protocol.ACL.WRITE, _ ->
			info "Permission denied: Domain %d has write only access" domainid;
			false
	in
	if true
	&& not (is_dom0 connection)
	&& not (check_owner connection node)
	&& not (List.exists check_acl (get_owners connection))
	then raise Permission_denied


