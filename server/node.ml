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
open Xenstore

type contents = {
  creator: int;
  perms: Protocol.ACL.t;
  value: string;
} with sexp

type t = {
  name: Symbol.t;
  contents: contents;
  children: t list;
} with sexp

let create _name _creator _perms _value =
        { name = Symbol.of_string _name; contents = { creator = _creator; perms = _perms; value = _value }; children = []; }

let get_contents node = node.contents

let get_creator node = node.contents.creator

let get_name node = Symbol.to_string node.name

let get_symbol node = node.name

let get_value node = node.contents.value

let set_value node nvalue = 
	if node.contents.value = nvalue
	then node
        else { node with contents = { node.contents with value = nvalue } }

let set_perms node nperms = { node with contents = { node.contents with perms = nperms } }
let get_perms node = node.contents.perms

let get_children node = node.children

let add_child node child =
	{ node with children = child :: node.children }

let exists node childname =
	let childname = Symbol.of_string childname in
	List.exists (fun n -> n.name = childname) node.children

let find node childname =
	let childname = Symbol.of_string childname in
	List.find (fun n -> n.name = childname) node.children

let replace_child node symbol nchild =
	(* this is the on-steroid version of the filter one-replace one *)
	let rec replace_one_in_list l =
		match l with
		| []                           -> [ nchild ]
		| h :: tl when h.name = symbol -> nchild :: tl
		| h :: tl                      -> h :: replace_one_in_list tl
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

let unpack node = (Symbol.to_string node.name, node.contents.perms, node.contents.value)

open Protocol.Path

exception Doesnt_exist of t

let doesnt_exist t = raise (Doesnt_exist t)

let lookup node path =
  Protocol.Path.walk (fun e node -> match node with
    | None -> None
    | Some node ->
      let e = Protocol.Path.Element.to_string e in
      if exists node e then Some (find node e) else None
  ) path (Some node)

(* read | ls | getperms use this *)
let with_parent node path fct =
  match lookup node (Protocol.Path.dirname path) with
  | None -> raise Not_found
  | Some parent -> fct parent (Protocol.Path.basename path)

let modify node path f =
  let rec aux node = function
  | []      -> raise Not_found
  | h :: [] -> f node h
  | h :: l  ->
    if not (exists node h) then raise (Doesnt_exist path);
    let new_h = aux (find node h) l in
    replace_child node (Symbol.of_string h) new_h in
  aux node (Protocol.Path.to_string_list path)

let replace node path node' =
  if path = Protocol.Path.empty
  then node'
  else modify node path (fun node name -> replace_child node (Symbol.of_string name) node')
