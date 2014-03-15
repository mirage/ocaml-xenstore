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

let debug fmt = Logging.debug "transaction" fmt
open Xenstore

let none = 0l
let test_eagain = ref false

type ty = No | Full of (int32 * Node.t * Store.t)

type side_effects = {
        (* A log of all the store updates in this transaction. When the transaction
           is committed, these paths need to be committed to stable storage. *) 
        mutable updates: Store.update list;
        (* A log of updates which should generate a watch events. Note this can't
           be derived directly from [updates] above because implicit directory
           creates don't generate watches (for no good reason) *)
	mutable watches: (Protocol.Op.t * Protocol.Name.t) list;
        (* A list of introduced domains *)
        mutable domains: Domain.address list
} with sexp

let no_side_effects () = { updates = []; watches = []; domains = [] }

let get_watches side_effects = side_effects.watches
let get_updates side_effects = side_effects.updates
let get_domains side_effects = side_effects.domains

type t = {
	ty: ty;
	store: Store.t;
        (* Side-effects which should be generated when the transaction is committed. *)
        side_effects: side_effects;
        (* A log of all the requests and responses during this transaction. When
           committing a transaction to a modified store, we replay the requests and
           abort the transaction if any of the responses would now be different. *)
	mutable operations: (Protocol.Request.t * Protocol.Response.t) list;
}

let make id store =
	let ty = if id = none then No else Full(id, store.Store.root, store) in
	{
		ty = ty;
		store = if id = none then store else Store.copy store;
                side_effects = no_side_effects ();
		operations = [];
	}

let get_id t = match t.ty with No -> none | Full (id, _, _) -> id
let get_store t = t.store
let get_side_effects t = t.side_effects

let add_watch t ty path = t.side_effects.watches <- (ty, Protocol.Name.Absolute path) :: t.side_effects.watches
let add_operation t request response = t.operations <- (request, response) :: t.operations
let get_operations t = List.rev t.operations

let mkdir t creator perm path =
        if not (Store.exists t.store path) then (
                Protocol.Path.iter (fun prefix ->
                        if not(Store.exists t.store prefix) then begin
                                let update = Store.mkdir t.store creator perm prefix in
                                t.side_effects.updates <- update :: t.side_effects.updates;
                                (* no watches for implicitly created directories *)
                        end
                ) path;
                add_watch t Protocol.Op.Mkdir path
        )

let write t creator perm path value =
        mkdir t creator perm (Protocol.Path.dirname path);
        let update = Store.write t.store creator perm path value in
        t.side_effects.updates <- update :: t.side_effects.updates;
        add_watch t Protocol.Op.Write path

let setperms t perm path perms =
        let update = Store.setperms t.store perm path perms in
        t.side_effects.updates <- update :: t.side_effects.updates;
	add_watch t Protocol.Op.Setperms path

let rm t perm path =
        let updates = Store.rm t.store perm path in
        t.side_effects.updates <- updates @ t.side_effects.updates;
	add_watch t Protocol.Op.Rm path

let exists t perms path = Store.exists t.store path
let ls t perm path = Store.ls t.store perm path
let read t perm path = Store.read t.store perm path
let getperms t perm path = Store.getperms t.store perm path
