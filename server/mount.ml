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

(* Find all pairs [mountpoint, implementation] where [mountpoint] is a prefix
   of [path], ordered longest-prefix first. *)
let all path =
  (* Find the longest prefix patch between [path] and the keys of [mounts] *)
  Path.fold
    (fun prefix acc ->
      let key = Path.to_string_list prefix in
      let relative = Name.(to_path(relative (Absolute path) (Absolute prefix))) in
      if Trie.mem !mounts key then (relative, Trie.find !mounts key) :: acc else acc
    ) path []
  @ [ path, (module Transaction: Tree.S) ]

(* Return the longest-prefix match, i.e. the most specific implementation *)
let bottom_layer path = match all path with
| [] -> path, (module Transaction: Tree.S)
| x :: _ -> x

(* Return the longest-prefix match where the path actually exists *)
let lowest_existing t path =
  let exists (path, m) =
    let module Impl = (val m: Tree.S) in
    Impl.exists t (Perms.of_domain 0) path in
  match List.filter exists (all path) with
  | [] -> raise (Node.Doesnt_exist path)
  | x :: _ -> x

module Tree = struct
  let write t creator perms path v =
    let path, m = bottom_layer path in
    let module Impl = (val m: Tree.S) in
    Impl.write t creator perms path v
  let mkdir t creator perms path =
    let path, m = bottom_layer path in
    let module Impl = (val m: Tree.S) in
    Impl.mkdir t creator perms path
  let read t perms path =
    let path, m = lowest_existing t path in
    let module Impl = (val m: Tree.S) in
    Impl.read t perms path
  let getperms t perms path =
    let path, m = lowest_existing t path in
    let module Impl = (val m: Tree.S) in
    Impl.getperms t perms path
  let setperms t perms path acl =
    let path, m = lowest_existing t path in
    let module Impl = (val m: Tree.S) in
    Impl.setperms t perms path acl
  let exists t perms path = try ignore(read t perms path); true with Node.Doesnt_exist _ -> false
  let rm t perms path =
    if not(exists t perms (Path.dirname path)) then raise (Node.Doesnt_exist (Path.dirname path));
    if exists t perms path then begin
      let path, m = lowest_existing t path in
      let module Impl = (val m: Tree.S) in
      Impl.rm t perms path
    end
  let ls t perms path =
    if not(exists t perms path) then raise (Node.Doesnt_exist path);
    List.fold_left (fun acc (path, m) ->
      let module Impl = (val m: Tree.S) in
      let extra = try Impl.ls t perms path with Node.Doesnt_exist _ -> [] in
      List.filter (fun x -> not(List.mem x acc)) extra @ acc
    ) [] (all path)
end

let mount path implementation =
  mounts := Trie.set !mounts (Path.to_string_list path) implementation;
  Database.store >>= fun store ->
  let t = Transaction.make Transaction.none store in
  Transaction.mkdir t 0 (Perms.of_domain 0) path;
  Database.persist (Transaction.get_side_effects t)

let unmount path =
  let key = Path.to_string_list path in
  if not(Trie.mem !mounts key)
  then return ()
  else begin
    mounts := Trie.unset !mounts key;
    Database.store >>= fun store ->
    let t = Transaction.make Transaction.none store in
    let ls = Transaction.ls t (Perms.of_domain 0) path in
    if ls = [] then Transaction.rm t (Perms.of_domain 0) path;
    Database.persist (Transaction.get_side_effects t)
  end
