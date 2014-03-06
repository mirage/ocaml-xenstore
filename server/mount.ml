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

let mount path implementation =
  mounts := Trie.set !mounts (Path.to_string_list path) implementation

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

