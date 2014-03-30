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
open Lwt
open Xenstore
open Protocol

let ( |> ) a b = b a
let ( ++ ) f g x = f (g x)

let debug fmt = Logging.debug "database" fmt
let error fmt = Logging.error "database" fmt

let store, store_wakener = Lwt.task ()
let persister, persister_wakener = Lwt.task ()

let persist side_effects =
  persister >>= fun p ->
  Lwt_list.iter_s p (List.rev side_effects.Transaction.updates)

let initialise = function
| S.NoPersistence ->
  let s = Store.create () in
  Lwt.wakeup persister_wakener (fun u ->
    debug "Not persisting %s" (Sexp.to_string (Store.sexp_of_update u));
    return ()
  );
  Lwt.wakeup store_wakener s;
  return ()
| S.Git filename ->
  let module DB = (val IrminGit.local ~bare:false filename) in
  DB.create () >>= fun db ->

  let dir_suffix = ".dir" in
  let value_suffix = ".value" in

  let value_of_filename path = match List.rev (Protocol.Path.to_string_list path) with
  | [] -> []
  | file :: dirs -> List.rev ((file ^ value_suffix) :: (List.map (fun x -> x ^ dir_suffix) dirs)) in

  let dir_of_filename path =
    List.rev (List.map (fun x -> x ^ dir_suffix) (List.rev (Protocol.Path.to_string_list path))) in

  let remove_suffix suffix x =
    let suffix' = String.length suffix and x' = String.length x in
    String.sub x 0 (x' - suffix') in
  let endswith suffix x =
    let suffix' = String.length suffix and x' = String.length x in
    suffix' <= x' && (String.sub x (x' - suffix') suffix' = suffix) in

  (* These must all be idempotent *)
  let p = function
    | Store.Write(path, contents) ->
      Printf.fprintf stderr "+ %s\n%!" (Protocol.Path.to_string path);
      (try_lwt
        DB.update db (value_of_filename path) (Sexp.to_string (Node.sexp_of_contents contents))
      with e -> (Printf.fprintf stderr "ERR %s\n%!" (Printexc.to_string e)); return ())
    | Store.Rm path ->
      Printf.fprintf stderr "- %s\n%!" (Protocol.Path.to_string path);
      (try_lwt
        DB.remove db (dir_of_filename path) >>= fun () ->
        DB.remove db (value_of_filename path)
      with e -> (Printf.fprintf stderr "ERR %s\n%!" (Printexc.to_string e)); return ()) in
  let store = Store.create () in
  let t = Transaction.make Transaction.none store in
  DB.contents db >>= fun contents ->
  (* Sort into order of path length, so we create directories before we need them *)
  let contents = List.sort (fun (path, _) (path', _) -> compare (List.length path) (List.length path')) contents in
  List.iter
    (fun (path, value) ->
     debug "%s <- %s" (String.concat "/" path) value;
     (* The keys are always of the form foo.dir/bar.dir/baz.value
        The List.sort guarantees that foo.value will have been processed before foo.dir/bar.value *)
     let path = List.map (fun element ->
        if endswith dir_suffix element then remove_suffix dir_suffix element
        else if endswith value_suffix element then remove_suffix value_suffix element
        else element (* should never happen *)
     ) path in
     let contents = Node.contents_of_sexp (Sexp.of_string value) in
     let path = Protocol.Path.of_string_list path in
     Transaction.write t None contents.Node.creator (Perms.of_domain 0) path contents.Node.value;
     Transaction.setperms t (Perms.of_domain 0) path contents.Node.perms
    ) contents;
  Lwt.wakeup persister_wakener p;
  Lwt.wakeup store_wakener store;
  return ()
