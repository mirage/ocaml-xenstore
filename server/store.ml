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

exception Already_exists of string

let getdomainpath domid =
  Protocol.Name.Absolute (Protocol.Path.of_string (Printf.sprintf "/local/domain/%u" domid))

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

let exists store path = match Node.lookup store.root path with
| None -> false
| Some _ -> true

let mkdir store creator perm path =
  try
    store.root <- Node.modify store.root path
      (fun parent name ->
        if Node.exists parent name then begin
          Perms.check perm Perms.WRITE (Node.get_perms (Node.find parent name));
          raise (Already_exists (Protocol.Path.to_string path))
        end else begin
          Perms.check perm Perms.WRITE (Node.get_perms parent);
          Node.add_child parent (Node.create name creator (Node.get_perms parent) "")
        end
      );
    Quota.incr store.quota creator
  with Already_exists _ -> ()

let write store creator perm path value =
  Quota.check store.quota creator (String.length value);
  let root, node_created = match Node.lookup store.root path with
    | None ->
      Node.modify store.root path
        (fun parent name ->
          Perms.check perm Perms.WRITE (Node.get_perms parent);
          Node.add_child parent (Node.create name creator (Node.get_perms parent) value)
        ), true
    | Some existing ->
      Node.modify store.root path
        (fun parent name ->
          Perms.check perm Perms.WRITE (Node.get_perms existing);
          Node.replace_child parent (Symbol.of_string name) (Node.set_value existing value)
        ), false in
  if node_created
  then Quota.incr store.quota creator;
  store.root <- root

let rm store perm path = match Node.lookup store.root path with
| None ->
  (* If the parent node doesn't exist then we fail *)
  let parent = Protocol.Path.dirname path in
  if not(exists store parent) then raise (Node.Doesnt_exist parent);
  (* Otherwise this is not an error *)
  ()
| Some node when node == store.root ->
  invalid_arg "removing the root node is forbidden"
| Some node ->
  (* cannot be the root node, therefore must have a parent,
     so this cannot fail: *)
  let root' = Node.modify store.root path
    (fun parent name ->
      Perms.check perm Perms.WRITE (Node.get_perms parent);
      Node.del_childname parent name
    ) in
  (* For each node that was just deleted, adjust the quota
     of the domain which created it *)
  let rec traverse node =
    Quota.decr store.quota (Node.get_creator node);
    List.iter traverse (Node.get_children node) in
  traverse node;
  store.root <- root'

let setperms store perm path nperms = match Node.lookup store.root path with
| None -> raise (Node.Doesnt_exist path)
| Some node ->
  store.root <- Node.modify store.root path
    (fun parent name ->
      let c = Node.find parent name in
      Perms.check perm Perms.CHANGE_ACL (Node.get_perms c);
      Perms.check perm Perms.WRITE (Node.get_perms c);
      let c' = Node.set_perms c nperms in
      Node.replace_child parent (Symbol.of_string name) c'
    )

let read store perm path = match Node.lookup store.root path with
| None -> raise (Node.Doesnt_exist path)
| Some node ->
  Perms.check perm Perms.READ (Node.get_perms node);
  Node.get_value node

let ls store perm path = match Node.lookup store.root path with
| None -> raise (Node.Doesnt_exist path)
| Some node ->
  Perms.check perm Perms.READ (Node.get_perms node);
  List.rev (List.map Node.get_name (Node.get_children node))

let getperms store perm path = match Node.lookup store.root path with
| None -> raise (Node.Doesnt_exist path)
| Some node ->
  Perms.check perm Perms.READ (Node.get_perms node);
  Node.get_perms node

(* others utils *)
let traversal root_node f =
	let rec _traversal path node =
		f path node;
		List.iter (_traversal (path @ [ Node.get_name node ])) (Node.get_children node)
		in
	_traversal [] root_node
		
let dump_store_buf root_node =
	let buf = Buffer.create 8192 in
	let dump_node path node =
		let pathstr = String.concat "/" path in
		Printf.bprintf buf "%s/%s{%s}" pathstr (Node.get_name node)
		               (String.escaped (Protocol.ACL.to_string (Node.get_perms node)));
		if String.length (Node.get_value node) > 0 then
			Printf.bprintf buf " = %s\n" (String.escaped (Node.get_value node))
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

(* Used in transaction merging *)
let replace store path node orig_quota mod_quota =
  store.root <- Node.replace store.root path node;
  Quota.merge orig_quota mod_quota store.quota



let create () = {
	stat_transaction_coalesce = 0;
	stat_transaction_abort = 0;
	root = Node.create "" 0 (Protocol.ACL.({ owner = 0; other = NONE; acl = [] })) "";
	quota = Quota.create ();
}
let copy store = {
	stat_transaction_coalesce = store.stat_transaction_coalesce;
	stat_transaction_abort = store.stat_transaction_abort;
	root = store.root;
	quota = Quota.copy store.quota;
}

let mark_symbols store =
	Node.fold (fun () node -> Symbol.mark_as_used (Node.get_symbol node)) store.root ()

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
