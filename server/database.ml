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

let ( |> ) a b = b a
let ( ++ ) f g x = f (g x)

let debug fmt = Logging.debug "database" fmt
let error fmt = Logging.error "database" fmt

let persist_request = Lwt_mvar.create_empty ()
let persist_response : [`Ok of unit] Lwt_mvar.t = Lwt_mvar.create_empty ()

let no_persistence () =
  let rec forever () =
    Lwt_mvar.take persist_request >>= fun _ ->
    Lwt_mvar.put persist_response (`Ok ()) >>= fun () ->
    forever () in
  forever ()

let git_persistence dir =
  let module DB = (val IrminGit.local ~bare:false dir) in
  let db_t = DB.create () in

  let dir_suffix = ".dir" in
  let value_suffix = ".value" in

  let value_of_filename path = match List.rev (Protocol.Path.to_string_list path) with
  | [] -> []
  | file :: dirs -> List.rev ((file ^ value_suffix) :: (List.map (fun x -> x ^ dir_suffix) dirs)) in

  let dir_of_filename path =
    List.rev (List.map (fun x -> x ^ dir_suffix) (List.rev (Protocol.Path.to_string_list path))) in

  let rec forever () =
    db_t >>= fun db ->
    Lwt_mvar.take persist_request >>= fun request ->
    (match request with
    | Store.Write(path, perm, value) ->
      Printf.fprintf stderr "+ %s\n%!" (Protocol.Path.to_string path);
      (try_lwt
        DB.update db (value_of_filename path) value
      with e -> (Printf.fprintf stderr "ERR %s\n%!" (Printexc.to_string e)); return ())
    | Store.Rm path ->
      Printf.fprintf stderr "- %s\n%!" (Protocol.Path.to_string path);
      (try_lwt
        DB.remove db (dir_of_filename path) >>= fun () ->
        DB.remove db (value_of_filename path)
      with e -> (Printf.fprintf stderr "ERR %s\n%!" (Printexc.to_string e)); return ()) ) >>= fun () ->
    Lwt_mvar.put persist_response (`Ok ()) >>= fun () ->
    forever () in
  forever ()

let persist side_effects =
  Lwt_list.iter_s (fun x ->
    Lwt_mvar.put persist_request x >>= fun () ->
    Lwt_mvar.take persist_response >>= fun _ ->
    return ()
  ) side_effects.Transaction.updates

let store =
  let store = Store.create () in
  let t = Transaction.make 1l store in
  List.iter
    (fun path ->
      let path = Protocol.Path.of_string path in
      if not (Transaction.exists t (Perms.of_domain 0) path)
      then Transaction.mkdir t 0 (Perms.of_domain 0) path
    ) [ "/local"; "/local/domain"; "/tool"; "/tool/xenstored"; "/tool/xenstored/quota"; "/tool/xenstored/connection"; "/tool/xenstored/log"; "/tool/xenstored/memory" ];
  assert (Transaction.commit t);
  persist (Transaction.get_side_effects t) >>= fun () ->
  return store
