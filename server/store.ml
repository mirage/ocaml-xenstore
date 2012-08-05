(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 * Author Thomas Gazagnaire <thomas.gazagnaire@eu.citrix.com>
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

open Junk

module Node = struct

type t = {
	name: Symbol.t;
	perms: Xs_packet.ACL.t;
	value: string;
	children: t list;
}

let create _name _perms _value =
	{ name = Symbol.of_string _name; perms = _perms; value = _value; children = []; }

let get_owner node = node.perms.Xs_packet.ACL.owner
let get_children node = node.children
let get_value node = node.value
let get_perms node = node.perms
let get_name node = Symbol.to_string node.name

let set_value node nvalue = 
	if node.value = nvalue
	then node
	else { node with value = nvalue }

let set_perms node nperms = { node with perms = nperms }

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

let rec recurse fct node = fct node; List.iter (recurse fct) node.children

let unpack node = (Symbol.to_string node.name, node.perms, node.value)

end

let char_is_valid c =
	(c >= 'a' && c <= 'z') ||
	(c >= 'A' && c <= 'Z') ||
	(c >= '0' && c <= '9') ||
	c = '_' || c = '-' || c = '@'

let name_is_valid name =
	name <> "" && String.fold_left (fun accu c -> accu && char_is_valid c) true name

let is_valid = List.for_all name_is_valid

type path = string list

exception Invalid_path

let path_of_string = function
	| "/" -> []
	| path ->
		if String.length path > 1024 then begin
			error "paths larger than 1024 bytes are invalid";
			raise Invalid_path
		end;
		begin match String.split '/' path with
		| "" :: path ->
			if not(is_valid path) then begin
				error "valid paths contain only ([a-z]|[A-Z]|[0-9]|-|_|@])+";
				raise Invalid_path
			end;
			path
		| path ->
			raise Invalid_path
		end

let path_to_string path = String.concat "/" ("" :: path)

module Name = struct

	type t =
		| IntroduceDomain
		| ReleaseDomain
		| Absolute of path
		| Relative of path

	let is_relative = function
		| Relative _ -> true
		| _ -> false

	let introduceDomain = IntroduceDomain
	let releaseDomain = ReleaseDomain

	let of_string = function
        | "@introduceDomain" -> IntroduceDomain
        | "@releaseDomain"   -> ReleaseDomain
        | "" ->
                error "zero-length paths are invalid";
                raise Invalid_path
		| path when path.[0] <> '/' ->
			if String.length path > 1024 then begin
				error "paths larger than 1024 bytes are invalid";
				raise Invalid_path
			end;
			let path = String.split '/' path in
            if not(is_valid path) then begin
                error "valid paths contain only ([a-z]|[A-Z]|[0-9]|-|_|@])+";
                raise Invalid_path
            end;
            Relative path
		| path -> Absolute (path_of_string path)

	let to_string = function
		| IntroduceDomain -> "@introduceDomain"
		| ReleaseDomain -> "@releaseDomain"
		| Absolute path -> path_to_string path
		| Relative path -> String.concat "/" path

	let to_key = function
		| IntroduceDomain -> [ "@introduceDomain" ]
		| ReleaseDomain -> [ "@releaseDomain" ]
		| Absolute p -> "" :: p
		| Relative p -> "" :: p

end

module Path = struct

exception Lookup_Doesnt_exist of string

exception Doesnt_exist

exception Already_exist

type t = string list

let getdomainpath domid = [ "local"; "domain"; Printf.sprintf "%u" domid ]


let create path connection_path =
	let open Name in
	match of_string path with
	| Absolute path -> path
	| Relative x -> connection_path @ x
	| _ -> raise Invalid_path

let to_name x = Name.Absolute x

let of_string = path_of_string
let to_string = path_to_string

let get_parent t = match t with
	| [] -> t
	| t -> List.rev (List.tl (List.rev t))

let make_relative base t =
	let open Name in
	match t with
	| IntroduceDomain
	| ReleaseDomain
	| Relative _ -> t
	| Absolute t ->
		(* base should be a prefix of t *)
		let rec f x y = match x, y with
			| x :: xs, y :: ys when x = y -> f xs ys
			| [], y -> Relative y
			| _, _ -> Absolute t in
		f base t

let list_tl_multi n l =
	let rec do_tl i x =
		if i = 0 then x else do_tl (i - 1) (List.tl x)
		in
	do_tl n l

(* string utils *)
let get_hierarchy path =
	let l = List.length path in
	let revpath = List.rev path in
	let rec sub i =
		let x = List.rev (list_tl_multi (l - i) revpath) in
		if i = l then [ x ] else x :: sub (i + 1)
	in
	sub 0

let get_common_prefix p1 p2 =
	let rec compare l1 l2 =
		match l1, l2 with
		| h1 :: tl1, h2 :: tl2 ->
			if h1 = h2 then h1 :: (compare tl1 tl2) else []
		| _, [] | [], _ ->
				(* if l1 or l2 is empty, we found the equal part already *)
			[]
	in
	compare p1 p2

let rec lookup_modify node path fct =
	match path with
	| []      -> raise Invalid_path
	| h :: [] -> fct node h
	| h :: l  ->
		let (n, c) =
			if not (Node.exists node h) then
				raise (Lookup_Doesnt_exist h)
			else
				(node, Node.find node h) in
		let nc = lookup_modify c l fct in
		Node.replace_child n c nc

let apply_modify rnode path fct =
	lookup_modify rnode path fct

let rec lookup_get node path =
	match path with
	| []      -> raise (Invalid_path)
	| h :: [] ->
		(try
			Node.find node h
		with Not_found ->
			raise Doesnt_exist)
	| h :: l  -> let cnode = Node.find node h in lookup_get cnode l

let get_node rnode path =
	if path = [] then
		Some rnode
	else (
		try Some (lookup_get rnode path) with Doesnt_exist -> None
	)

(* get the deepest existing node for this path *)
let rec get_deepest_existing_node node path =
	let rec f node = function
		| [] -> node
		| h :: t ->
			try f (Node.find node h) t 
			with Not_found -> node in
	f node path

let set_node rnode path nnode =
	let quota = Quota.create () in
	if !Quota.activate then Node.recurse (fun node -> Quota.add_entry quota (Node.get_owner node)) nnode;
	if path = [] then
		nnode, quota
	else
		let set_node node name =
			try
				let ent = Node.find node name in
				if !Quota.activate then Node.recurse (fun node -> Quota.del_entry quota (Node.get_owner node)) ent;
				Node.replace_child node ent nnode
			with Not_found ->
				Node.add_child node nnode
			in
		apply_modify rnode path set_node, quota

(* read | ls | getperms use this *)
let rec lookup node path fct =
	match path with
	| []      -> raise (Invalid_path)
	| h :: [] -> fct node h
	| h :: l  -> let cnode = Node.find node h in lookup cnode l fct

let apply rnode path fct =
	lookup rnode path fct
end

type t =
{
	mutable stat_transaction_coalesce: int;
	mutable stat_transaction_abort: int;
	mutable root: Node.t;
	mutable quota: Quota.t;
}

let get_root store = store.root
let set_root store root =
	debug "Updating root of store";
	store.root <- root

let get_quota store = store.quota
let set_quota store quota = store.quota <- quota

(* modifying functions *)
let path_mkdir store perm path =
	let do_mkdir node name =
		try
			let ent = Node.find node name in
			Perms.check perm Perms.WRITE ent.Node.perms;
			raise Path.Already_exist
		with Not_found ->
			Perms.check perm Perms.WRITE node.Node.perms;
			Node.add_child node (Node.create name node.Node.perms "") in
	if path = [] then
		store.root
	else
		Path.apply_modify store.root path do_mkdir

let path_write store perm path value =
	let node_created = ref false in
	let do_write node name =
		try
			let ent = Node.find node name in
			Perms.check perm Perms.WRITE ent.Node.perms;
			let nent = Node.set_value ent value in
			Node.replace_child node ent nent
		with Not_found ->
			node_created := true;
			Perms.check perm Perms.WRITE node.Node.perms;
			Node.add_child node (Node.create name node.Node.perms value) in
	if path = [] then (
		Perms.check perm Perms.WRITE store.root.Node.perms;
		Node.set_value store.root value, false
	) else
		Path.apply_modify store.root path do_write, !node_created

let path_rm store perm path =
	let do_rm node name =
		try
			let ent = Node.find node name in
			Perms.check perm Perms.WRITE ent.Node.perms;
			Node.del_childname node name
		with Not_found ->
			raise Path.Doesnt_exist in
	if path = [] then
		Node.del_all_children store.root
	else
		Path.apply_modify store.root path do_rm

let path_setperms store perm path perms =
	if path = [] then
		Node.set_perms store.root perms
	else
		let do_setperms node name =
			let c = Node.find node name in
			Perms.check perm Perms.CHANGE_ACL c.Node.perms;
			Perms.check perm Perms.WRITE c.Node.perms;
			let nc = Node.set_perms c perms in
			Node.replace_child node c nc
		in
		Path.apply_modify store.root path do_setperms

(* accessing functions *)
let get_node store path =
	Path.get_node store.root path

let get_deepest_existing_node store path =
	Path.get_deepest_existing_node store.root path

let read store perm path =
	let do_read node name =
		let ent = Node.find node name in
		Perms.check perm Perms.READ ent.Node.perms;
		ent.Node.value
	in
	if path = [] then (
		let ent = store.root in
		Perms.check perm Perms.READ ent.Node.perms;
		ent.Node.value
	) else
		Path.apply store.root path do_read

let ls store perm path =
	let children =
		if path = [] then
			(Node.get_children store.root)
		else
			let do_ls node name =
				let cnode = Node.find node name in
				Perms.check perm Perms.READ cnode.Node.perms;
				cnode.Node.children in
			Path.apply store.root path do_ls in
	List.rev (List.map (fun n -> Symbol.to_string n.Node.name) children)

let getperms store perm path =
	if path = [] then
		(Node.get_perms store.root)
	else
		let fct n name =
			let c = Node.find n name in
			Perms.check perm Perms.READ c.Node.perms;
			c.Node.perms in
		Path.apply store.root path fct

let path_exists store path =
	if path = [] then
		true
	else
		try
			let check_exist node name =
				ignore(Node.find node name);
				true in
			Path.apply store.root path check_exist
		with Not_found -> false

(* others utils *)
let traversal root_node f =
	let rec _traversal path node =
		f path node;
		List.iter (_traversal (path @ [ Symbol.to_string node.Node.name ])) node.Node.children
		in
	_traversal [] root_node
		
let dump_store_buf root_node =
	let buf = Buffer.create 8192 in
	let dump_node path node =
		let pathstr = String.concat "/" path in
		Printf.bprintf buf "%s/%s{%s}" pathstr (Symbol.to_string node.Node.name)
		               (String.escaped (Xs_packet.ACL.to_string (Node.get_perms node)));
		if String.length node.Node.value > 0 then
			Printf.bprintf buf " = %s\n" (String.escaped node.Node.value)
		else
			Printf.bprintf buf "\n";
		in
	traversal root_node dump_node;
	buf

let dump_store chan root_node =
	let buf = dump_store_buf root_node in
	output_string chan (Buffer.contents buf);
	Buffer.reset buf

let dump_fct store f = traversal store.root f
let dump store out_chan = dump_store out_chan store.root
let dump_stdout store = dump_store stdout store.root
let dump_buffer store = dump_store_buf store.root


(* modifying functions with quota udpate *)
let set_node store path node =
	let root, quota_diff = Path.set_node store.root path node in
	store.root <- root;
	Quota.add store.quota quota_diff

let write store perm path value =
	let owner = Node.get_owner (get_deepest_existing_node store path) in
	Quota.check store.quota owner (String.length value);
	let root, node_created = path_write store perm path value in
	store.root <- root;
	if node_created
	then Quota.add_entry store.quota owner

let mkdir store perm path =
	let owner = Node.get_owner (get_deepest_existing_node store path) in
	Quota.check store.quota owner 0;
	store.root <- path_mkdir store perm path;
	Quota.add_entry store.quota owner

let rm store perm path =
	let rmed_node = Path.get_node store.root path in
	match rmed_node with
	| None -> raise Path.Doesnt_exist
	| Some rmed_node ->
		store.root <- path_rm store perm path;
		Node.recurse (fun node -> Quota.del_entry store.quota (Node.get_owner node)) rmed_node
		
let setperms store perm path nperms =
	match Path.get_node store.root path with
	| None -> raise Path.Doesnt_exist
	| Some node ->
		let old_owner = Node.get_owner node in
		let new_owner = nperms.Xs_packet.ACL.owner in
		Quota.check store.quota new_owner 0;
		store.root <- path_setperms store perm path nperms;
		Quota.del_entry store.quota old_owner;
		Quota.add_entry store.quota new_owner
(*
type ops = {
	store: t;
	write: Path.t -> string -> unit;
	mkdir: Path.t -> unit;
	rm: Path.t -> unit;
	setperms: Path.t -> Xs_packet.ACL.t -> unit;
	ls: Path.t -> string list;
	read: Path.t -> string;
	getperms: Path.t -> Xs_packet.ACL.t;
	path_exists: Path.t -> bool;
}

let get_ops store perms = {
	store = store;
	write = write store perms;
	mkdir = mkdir store perms;
	rm = rm store perms;
	setperms = setperms store perms;
	ls = ls store perms;
	read = read store perms;
	getperms = getperms store perms;
	path_exists = path_exists store;
}
*)
let create () = {
	stat_transaction_coalesce = 0;
	stat_transaction_abort = 0;
	root = Node.create "" (Xs_packet.ACL.({ owner = 0; other = NONE; acl = [] })) "";
	quota = Quota.create ();
}
let copy store = {
	stat_transaction_coalesce = store.stat_transaction_coalesce;
	stat_transaction_abort = store.stat_transaction_abort;
	root = store.root;
	quota = Quota.copy store.quota;
}

let mark_symbols store =
	Node.recurse (fun node -> Symbol.mark_as_used node.Node.name) store.root

let incr_transaction_coalesce store =
	store.stat_transaction_coalesce <- store.stat_transaction_coalesce + 1
let incr_transaction_abort store =
	store.stat_transaction_abort <- store.stat_transaction_abort + 1

let stats store =
	let nb_nodes = ref 0 in
	traversal store.root (fun path node ->
		incr nb_nodes
	);
	!nb_nodes, store.stat_transaction_abort, store.stat_transaction_coalesce
