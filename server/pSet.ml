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

let debug fmt = Logging.debug "pset" fmt
let info  fmt = Logging.info  "pset" fmt
let error fmt = Logging.debug "pset" fmt

open Lwt

module Make(T: S.SEXPABLE) = struct

  type t = {
    name: string list;
    m: Lwt_mutex.t;
  }

  let recreate t =
    Database.store >>= fun db ->
    let tr = Transaction.make Transaction.none db in
    let path = Protocol.Path.of_string_list t.name in
    if not(Transaction.exists tr (Perms.of_domain 0) path) then Transaction.mkdir tr None 0 (Perms.of_domain 0) path;
    Database.persist (Transaction.get_side_effects tr)

  (* fold over keys and values: for internal use only. *)
  let fold f initial t =
    recreate t >>= fun () ->
    Database.store >>= fun db ->
    let tr = Transaction.make Transaction.none db in
    let path = Protocol.Path.of_string_list t.name in
    let ls = Transaction.ls tr (Perms.of_domain 0) path in
    return (List.fold_left (fun acc k ->
      let v = Transaction.read tr (Perms.of_domain 0) (Protocol.Path.of_string_list (t.name @ [k])) in
      f acc k (T.t_of_sexp (Sexp.of_string v))
    ) initial ls)

  let mem v t = fold (fun acc _ v' -> acc || v = v') false t

  (* Everything below here is in the public API and should consider
   * holding the mutex over the operation. *)

  let create name =
    let m = Lwt_mutex.create () in
    let t = { name; m } in
    Lwt_mutex.with_lock t.m
      (fun () ->
        recreate t >>= fun () ->
        return t
      )

  let name t = t.name

  let cardinal t = Lwt_mutex.with_lock t.m (fun () -> fold (fun acc _ _ -> acc + 1) 0 t)

  let add v t =
    Lwt_mutex.with_lock t.m
      (fun () ->
        mem v t >>= function
        | true -> return ()
        | false ->
          fold (fun acc k _ -> try max acc (int_of_string k) with _ -> acc) (-1) t >>= fun max_id ->
          Database.store >>= fun db ->
          let tr = Transaction.make Transaction.none db in
          Transaction.write tr None 0 (Perms.of_domain 0) (Protocol.Path.of_string_list (t.name @ [ string_of_int (max_id + 1) ])) (Sexp.to_string (T.sexp_of_t v));
          Database.persist (Transaction.get_side_effects tr)
      )

  let remove v t =
    Lwt_mutex.with_lock t.m
      (fun () ->
        fold (fun acc k v' -> if v' = v then Some k else None) None t >>= function
        | None -> return ()
        | Some key ->
          Database.store >>= fun db ->
          let tr = Transaction.make Transaction.none db in
          Transaction.rm tr (Perms.of_domain 0) (Protocol.Path.of_string_list (t.name @ [ key ]));
          Database.persist (Transaction.get_side_effects tr)
      )

  let clear t =
    Lwt_mutex.with_lock t.m
      (fun () ->
        Database.store >>= fun db ->
        let tr = Transaction.make Transaction.none db in
        let path = Protocol.Path.of_string_list t.name in
        if Transaction.exists tr (Perms.of_domain 0) path then Transaction.rm tr (Perms.of_domain 0) path;
        Transaction.mkdir tr None 0 (Perms.of_domain 0) path;
        Database.persist (Transaction.get_side_effects tr) >>= fun () ->
        return ()
      )

  let fold f i t = Lwt_mutex.with_lock t.m (fun () -> fold (fun acc _ v -> f acc v) i t)

  let mem v t = Lwt_mutex.with_lock t.m (fun () -> mem v t)
end
