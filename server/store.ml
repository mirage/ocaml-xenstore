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

let debug fmt = Logging.debug "store" fmt
let error fmt = Logging.debug "error" fmt

exception Already_exists of string

let getdomainpath domid =
  Protocol.Name.Absolute (Protocol.Path.of_string (Printf.sprintf "/local/domain/%u" domid))

type t =
{
	mutable stat_transaction_abort: int;
	mutable root: Node.t;
	mutable quota: Quota.t;
} with sexp

type update =
| Write of Protocol.Path.t * Protocol.ACL.t * string
| Rm of Protocol.Path.t
with sexp

let get_root store = store.root
let set_root store root =
	debug "Updating root of store";
	store.root <- root

let get_quota store = store.quota
let set_quota store quota = store.quota <- quota

let exists store path = match Node.lookup store.root path with
| None -> false
| Some _ -> true

let update_of_path store path = match Node.lookup store.root path with
| Some n -> Write(path, Node.get_perms n, Node.get_value n)
| None -> assert false

let mkdir store creator perm path =
  store.root <- Node.modify store.root path
    (fun parent name ->
      if Node.exists parent name then begin
        Perms.check perm Perms.WRITE (Node.get_perms (Node.find parent name));
        parent
      end else begin
        Perms.check perm Perms.WRITE (Node.get_perms parent);
        Node.add_child parent (Node.create name creator (Node.get_perms parent) "")
      end
    );
    Quota.incr store.quota creator;
  update_of_path store path

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
  store.root <- root;
  update_of_path store path

let rm store perm path = match Node.lookup store.root path with
| None ->
  (* If the parent node doesn't exist then we fail *)
  let parent = Protocol.Path.dirname path in
  if not(exists store parent) then raise (Node.Doesnt_exist parent);
  (* Otherwise this is not an error *)
  []
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
  (* Deletes are recursive *)
  let rec traverse path acc node =
    Quota.decr store.quota (Node.get_creator node);
    let path = Node.get_name node :: path in 
    let acc = Rm (Protocol.Path.of_string_list (List.rev path)) :: acc in
    List.fold_left (traverse path) acc (Node.get_children node) in
  let updates = traverse (List.rev (Protocol.Path.(to_string_list(dirname path)))) [] node in
  store.root <- root';
  updates 

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
    );
  update_of_path store path

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

let create () = {
	stat_transaction_abort = 0;
	root = Node.create "" 0 (Protocol.ACL.({ owner = 0; other = NONE; acl = [] })) "";
	quota = Quota.create ();
}
let copy store = {
	stat_transaction_abort = store.stat_transaction_abort;
	root = store.root;
	quota = Quota.copy store.quota;
}

let mark_symbols store =
	Node.fold (fun () node -> Symbol.mark_as_used (Node.get_symbol node)) store.root ()

let incr_transaction_abort store =
	store.stat_transaction_abort <- store.stat_transaction_abort + 1
