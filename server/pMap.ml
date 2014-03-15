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

let debug fmt = Logging.debug "pmap" fmt
let info  fmt = Logging.info  "pmap" fmt
let error fmt = Logging.debug "pmap" fmt

open Lwt

module Make(K: S.STRINGABLE)(T: S.SEXPABLE) = struct
  module StringMap = Map.Make(String)

  type t = {
    mutable root: T.t StringMap.t;
    name: string list;
  }

  let create name =
    Database.store >>= fun db ->
    let t = Transaction.make Transaction.none db in
    let ls = try Transaction.ls t (Perms.of_domain 0) (Protocol.Path.of_string_list name) with Node.Doesnt_exist _ -> [] in
    let root = List.fold_left (fun acc id ->
      try
        let v = T.t_of_sexp (Sexp.of_string (Transaction.read t (Perms.of_domain 0) (Protocol.Path.of_string_list (name @ [ id ])))) in
        StringMap.add id v acc
      with _ -> acc
    ) StringMap.empty ls in
    return { root; name }

  let name t = t.name

  let cardinal t = StringMap.cardinal t.root

  let add key item t =
    Database.store >>= fun db ->
    let tr = Transaction.make Transaction.none db in
    let key = K.to_string key in
    Transaction.write tr 0 (Perms.of_domain 0) (Protocol.Path.of_string_list (t.name @ [ key ])) (Sexp.to_string (T.sexp_of_t item));
    t.root <- StringMap.add key item t.root;
    Database.persist (Transaction.get_side_effects tr)

  let clear t =
    Database.store >>= fun db ->
    let tr = Transaction.make Transaction.none db in
    let path = Protocol.Path.of_string_list t.name in
    if Transaction.exists tr (Perms.of_domain 0) path then Transaction.rm tr (Perms.of_domain 0) path;
    Database.persist (Transaction.get_side_effects tr) >>= fun () ->
    t.root <- StringMap.empty;
    return ()

  let fold f initial t = StringMap.fold (fun k v acc -> f acc (K.of_string k) v) t.root initial

  let max_binding t =
    let k, v = StringMap.max_binding t.root in
    K.of_string k, v
end
