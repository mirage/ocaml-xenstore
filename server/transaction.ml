(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 * Author Thomas Gazagnaire <thomas.gazagnaire@citrix.com>
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

let debug fmt = Logging.debug "transaction" fmt

open Junk

let none = 0l
let test_eagain = ref false
let do_coalesce = ref true

let check_parents_perms_identical root1 root2 path =
	let hierarch = Store.Path.get_hierarchy path in
	let permdiff = List.fold_left (fun acc path ->
		let n1 = Store.Path.get_node root1 path
		and n2 = Store.Path.get_node root2 path in
		match n1, n2 with
		| Some n1, Some n2 ->
			n1.Store.Node.perms <> n2.Store.Node.perms || acc
		| _ ->
			true || acc
	) false hierarch in
	(not permdiff)

let get_lowest path1 path2 =
	match path2 with
	| None       -> Some path1
	| Some path2 -> Some (Store.Path.get_common_prefix path1 path2)

let test_coalesce oldroot currentroot optpath =
	match optpath with
	| None      -> true
	| Some path ->
		let oldnode = Store.Path.get_node oldroot path
		and currentnode = Store.Path.get_node currentroot path in
		
		match oldnode, currentnode with
		| (Some oldnode), (Some currentnode) ->
			if oldnode == currentnode then (
				check_parents_perms_identical oldroot currentroot path
			) else (
				false
			)
		| None, None -> (
			(* ok then it doesn't exists in the old version and the current version,
			   just sneak it in as a child of the parent node if it exists, or else fail *)
			let pnode = Store.Path.get_node currentroot (Store.Path.get_parent path) in
			match pnode with
			| None       -> false (* ok it doesn't exists, just bail out. *)
			| Some pnode -> true
			)
		| _ ->
			false

let can_coalesce oldroot currentroot path =
	if !do_coalesce then
		try test_coalesce oldroot currentroot path with _ -> false
	else
		false

type ty = No | Full of (int32 * Store.Node.t * Store.t)

type t = {
	ty: ty;
	store: Store.t;
	mutable ops: (Xs_packet.Op.t * Store.Name.t) list;
	mutable read_lowpath: Store.Path.t option;
	mutable write_lowpath: Store.Path.t option;
}

let make id store =
	let ty = if id = none then No else Full(id, store.Store.root, store) in
	{
		ty = ty;
		store = if id = none then store else Store.copy store;
		ops = [];
		read_lowpath = None;
		write_lowpath = None;
	}

let get_id t = match t.ty with No -> none | Full (id, _, _) -> id
let get_store t = t.store
let get_ops t = t.ops

let add_wop t ty path = t.ops <- (ty, Store.Path.to_name path) :: t.ops
let set_read_lowpath t path = t.read_lowpath <- get_lowest path t.read_lowpath
let set_write_lowpath t path = t.write_lowpath <- get_lowest path t.write_lowpath

let path_exists t path = Store.path_exists t.store path

let write t perm path value =
	let path_exists = path_exists t path in
	Store.write t.store perm path value;
	if path_exists
	then set_write_lowpath t path
	else set_write_lowpath t (Store.Path.get_parent path);
	add_wop t Xs_packet.Op.Write path

let mkdir ?(with_watch=true) t perm path =
	Store.mkdir t.store perm path;
	set_write_lowpath t path;
	if with_watch then
		add_wop t Xs_packet.Op.Mkdir path

let mkdir_p t perm path =
	let dirname = Store.Path.get_parent path in
	if not (path_exists t dirname) then (
		let rec check_path p =
			match p with
				| []      -> []
				| h :: l  ->
					if path_exists t h then
						check_path l
					else
						p in
		let ret = check_path (List.tl (Store.Path.get_hierarchy dirname)) in
		List.iter (fun s -> mkdir ~with_watch:false t perm s) ret
	)

let setperms t perm path perms =
	Store.setperms t.store perm path perms;
	set_write_lowpath t path;
	add_wop t Xs_packet.Op.Setperms path

let rm t perm path =
	Store.rm t.store perm path;
	set_write_lowpath t (Store.Path.get_parent path);
	add_wop t Xs_packet.Op.Rm path

let ls t perm path =	
	let r = Store.ls t.store perm path in
	set_read_lowpath t path;
	r

let read t perm path =
	let r = Store.read t.store perm path in
	set_read_lowpath t path;
	r

let getperms t perm path =
	let r = Store.getperms t.store perm path in
	set_read_lowpath t path;
	r

let commit ~con t =
	let has_write_ops = List.length t.ops > 0 in
	debug "committing with %d write ops" (List.length t.ops);
	let has_coalesced = ref false in
	let has_commited =
	match t.ty with
	| No                         -> true
	| Full (id, oldroot, cstore) ->
		let commit_partial oldroot cstore store =
			(* get the lowest path of the query and verify that it hasn't
			   been modified by others transactions. *)
			if can_coalesce oldroot cstore.Store.root t.read_lowpath
			&& can_coalesce oldroot cstore.Store.root t.write_lowpath then (
				maybe (fun p ->
					let n = Store.get_node store p in

					(* it has to be in the store, otherwise it means bugs
					   in the lowpath registration. we don't need to handle none. *)
					maybe (fun n -> Store.set_node cstore p n) n;
					Logging.write_coalesce ~tid:(get_id t) ~con (Store.Path.to_string p);
				) t.write_lowpath;
				maybe (fun p ->
					Logging.read_coalesce ~tid:(get_id t) ~con (Store.Path.to_string p)
					) t.read_lowpath;
				has_coalesced := true;
				cstore.Store.stat_transaction_coalesce <- cstore.Store.stat_transaction_coalesce + 1;
				true
			) else (
				(* cannot do anything simple, just discard the queries,
				   and the client need to redo it later *)
				cstore.Store.stat_transaction_abort <- cstore.Store.stat_transaction_abort + 1;
				false
			)
			in
		let try_commit oldroot cstore store =
			if oldroot == cstore.Store.root then (
				(* move the new root to the current store, if the oldroot
				   has not been modified *)
				if has_write_ops then (
					Store.set_root cstore store.Store.root;
					Store.set_quota cstore store.Store.quota
				);
				true
			) else
				(* we try a partial commit if possible *)
				commit_partial oldroot cstore store
			in
		if !test_eagain && Random.int 3 = 0 then
			false
		else
			try_commit oldroot cstore t.store
	in
(*
	if has_commited && has_write_ops then
		Disk.write t.store;
*)
	if not has_commited 
	then Logging.conflict ~tid:(get_id t) ~con
	else if not !has_coalesced 
	then Logging.commit ~tid:(get_id t) ~con;
	has_commited
