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
open Sexplib
open Xenstore

let debug fmt = Logging.debug "pqueue" fmt
let info  fmt = Logging.info  "pqueue" fmt
let error fmt = Logging.debug "pqueue" fmt

open Lwt

module Make(T: S.SEXPABLE) = struct
  module IntMap = Map.Make(Int64)

  type t = {
    mutable next_id: int64;
    mutable root: T.t IntMap.t;
    name: string list;
  }

  let create name =
    Database.store >>= fun db ->
    let t = Transaction.make 1l db in
    let ls = try Transaction.ls t (Perms.of_domain 0) (Protocol.Path.of_string_list name) with Node.Doesnt_exist _ -> [] in
    let root = List.fold_left (fun acc id ->
      try
        let id' = Int64.of_string id in
        let v = T.t_of_sexp (Sexp.of_string (Transaction.read t (Perms.of_domain 0) (Protocol.Path.of_string_list (name @ [ id ])))) in
        IntMap.add id' v acc
      with _ -> acc
    ) IntMap.empty ls in
    let next_id = Int64.succ (try fst (IntMap.max_binding root) with Not_found -> (-1L)) in
    return { next_id; root; name }

  let length t = IntMap.cardinal t.root

  let add item t =
    Database.store >>= fun db ->
    let tr = Transaction.make 1l db in
    Transaction.write tr 0 (Perms.of_domain 0) (Protocol.Path.of_string_list (t.name @ [ Int64.to_string t.next_id ])) (Sexp.to_string (T.sexp_of_t item));
    t.root <- IntMap.add t.next_id item t.root;
    t.next_id <- Int64.succ t.next_id;
    assert(Transaction.commit tr);
    Database.persist (Transaction.get_side_effects tr)

  let clear t =
    Database.store >>= fun db ->
    let tr = Transaction.make 1l db in
    let path = Protocol.Path.of_string_list t.name in
    if Transaction.exists tr (Perms.of_domain 0) path then Transaction.rm tr (Perms.of_domain 0) path;
    assert(Transaction.commit tr);
    Database.persist (Transaction.get_side_effects tr) >>= fun () ->
    t.next_id <- 0L;
    t.root <- IntMap.empty;
    return ()

  let fold f initial t = IntMap.fold (fun _ v acc -> f acc v) t.root initial
end
