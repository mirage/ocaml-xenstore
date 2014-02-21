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
let debug fmt = Logging.debug "store" fmt
let error fmt = Logging.debug "error" fmt
open Xenstore
open Junk

type t = {
	name: Symbol.t;
	creator: int;
	perms: Protocol.ACL.t;
	value: string;
	children: t list;
}

let create _name _creator _perms _value =
	{ name = Symbol.of_string _name; creator = _creator; perms = _perms; value = _value; children = []; }

let get_creator node = node.creator

let get_name node = Symbol.to_string node.name

let get_symbol node = node.name

let get_value node = node.value

let set_value node nvalue = 
	if node.value = nvalue
	then node
	else { node with value = nvalue }

let set_perms node nperms = { node with perms = nperms }
let get_perms node = node.perms

let get_children node = node.children

let add_child node child =
	{ node with children = child :: node.children }

let exists node childname =
	let childname = Symbol.of_string childname in
	List.exists (fun n -> n.name = childname) node.children

let find node childname =
	let childname = Symbol.of_string childname in
	List.find (fun n -> n.name = childname) node.children

let replace_child node child nchild =
	(* this is the on-steroid version of the filter one-replace one *)
	let rec replace_one_in_list l =
		match l with
		| []                               -> []
		| h :: tl when h.name = child.name -> nchild :: tl
		| h :: tl                          -> h :: replace_one_in_list tl
		in
	{ node with children = (replace_one_in_list node.children) }

let del_childname node childname =
	let sym = Symbol.of_string childname in
	let rec delete_one_in_list l =
		match l with
		| []                        -> raise Not_found
		| h :: tl when h.name = sym -> tl
		| h :: tl                   -> h :: delete_one_in_list tl
		in
	{ node with children = (delete_one_in_list node.children) }

let del_all_children node =
	{ node with children = [] }

let rec fold f node acc =
  List.fold_left (fun acc node -> fold f node acc) (f acc node) node.children

let unpack node = (Symbol.to_string node.name, node.perms, node.value)

