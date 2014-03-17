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
open Xenstore
open Domain

let (stream: address Lwt_stream.t), introduce_fn = Lwt_stream.create ()

let root = "/tool/xenstored/connection/domain"
let mfn_path domid = Protocol.Path.of_string (Printf.sprintf "%s/%d/mfn" root domid)
let remote_port_path domid = Protocol.Path.of_string (Printf.sprintf "%s/%d/remote-port" root domid)
let pairs x = [
  mfn_path x.domid, Nativeint.to_string x.mfn;
  remote_port_path x.domid, string_of_int x.remote_port;
]

open Lwt

let write x =
  let pairs = pairs x in
  Database.store >>= fun store ->
  let t = Transaction.make Transaction.none store in
  List.iter (fun (k, v) -> Transaction.write t None 0 (Perms.of_domain 0) k v) pairs;
  Database.persist (Transaction.get_side_effects t)

let introduce x =
  (* Persist the domain address immediately *)
  write x >>= fun () ->
  introduce_fn (Some x);
  return ()

let forget x =
  let pairs = pairs x in
  Database.store >>= fun store ->
  let t = Transaction.make Transaction.none store in
  List.iter (fun (key, _) -> Transaction.rm t (Perms.of_domain 0) key) pairs;
  Database.persist (Transaction.get_side_effects t)

let read domid =
  Database.store >>= fun store ->
  let t = Transaction.make Transaction.none store in
  try
    let domid = int_of_string domid in
    let mfn = Nativeint.of_string (Transaction.read t (Perms.of_domain 0) (mfn_path domid)) in
    let remote_port = int_of_string (Transaction.read t (Perms.of_domain 0) (remote_port_path domid)) in
    return (Some { domid; mfn; remote_port })
  with _ ->
    return None

let ls () =
  Database.store >>= fun store ->
  let t = Transaction.make 1l store in
  let domids = Transaction.ls t (Perms.of_domain 0) (Protocol.Path.of_string root) in
  Lwt_list.map_s read domids >>= fun x ->
  return (List.fold_left (fun acc x -> match x with None -> acc | Some x -> x :: acc) [] x)

(* Automatically reconnect to all existing rings *)
let _ =
  ls () >>= fun existing ->
  List.iter (fun x -> introduce_fn (Some x)) existing;
  return ()

