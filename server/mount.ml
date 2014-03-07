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

open Lwt
open Xenstore
open Protocol

let mounts : (string, (module Tree.S)) Trie.t ref = ref (Trie.create ())

let debug fmt = Logging.debug "mount" fmt
let error fmt = Logging.error "mount" fmt

let lookup path =
  (* Find the longest prefix patch between [path] and the keys of [mounts] *)
  match Path.fold
    (fun prefix acc ->
      let key = Path.to_string_list prefix in
      let relative = Name.(to_path(relative (Absolute path) (Absolute prefix))) in
      if Trie.mem !mounts key then Some(relative, Trie.find !mounts key) else acc
    ) path None with
  | Some (a, b) -> a, b
  | None -> path, (module Transaction: Tree.S)

let read_through mountpoint (module Base: Tree.S) (module T: Tree.S) =
  let module M = struct
    let write = T.write
    let mkdir = T.mkdir
    let ls t perms path =
      let underneath = try Base.ls t perms (Path.concat mountpoint path) with Node.Doesnt_exist _ -> [] in
      let this = T.ls t perms path in
      List.filter (fun x -> not(List.mem x underneath)) this @ underneath
    let read t perms path =
      if T.exists t perms path
      then T.read t perms path
      else Base.read t perms (Path.concat mountpoint path)
    let exists t perms path = try ignore(read t perms path); true with Node.Doesnt_exist _ -> false
    let rm t perms path =
      if T.exists t perms path
      then T.rm t perms path
      else Base.rm t perms (Path.concat mountpoint path)
    let getperms t perms path =
      if T.exists t perms path
      then T.getperms t perms path
      else Base.getperms t perms (Path.concat mountpoint path)
    let setperms t perms path acl =
      if T.exists t perms path
      then T.setperms t perms path acl
      else Base.setperms t perms (Path.concat mountpoint path) acl
  end in
  (module M: Tree.S)

let mount path implementation =
  mounts := Trie.set !mounts (Path.to_string_list path) (read_through path (module Transaction) implementation);
  Database.store >>= fun store ->
  let t = Transaction.make 1l store in
  Transaction.mkdir t 0 (Perms.of_domain 0) path;
  assert (Transaction.commit t);
  Database.persist (Transaction.get_side_effects t)

let unmount path =
  let key = Path.to_string_list path in
  if not(Trie.mem !mounts key)
  then return ()
  else begin
    mounts := Trie.unset !mounts key;
    Database.store >>= fun store ->
    let t = Transaction.make 1l store in
    let ls = Transaction.ls t (Perms.of_domain 0) path in
    if ls = [] then Transaction.rm t (Perms.of_domain 0) path;
    if Transaction.commit t
    then Database.persist (Transaction.get_side_effects t)
    else return () (* conflict means we'll leave it alone *)
  end

