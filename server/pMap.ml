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

  type t = {
    name: string list;
  }

  let recreate t =
    Database.store >>= fun db ->
    let tr = Transaction.make Transaction.none db in
    let path = Protocol.Path.of_string_list t.name in
    if not(Transaction.exists tr (Perms.of_domain 0) path) then Transaction.mkdir tr None 0 (Perms.of_domain 0) path;
    Database.persist (Transaction.get_side_effects tr)

  let create name =
    let t = { name } in
    recreate t >>= fun () ->
    return t

  let name t = t.name

  let cardinal t =
    recreate t >>= fun () ->
    Database.store >>= fun db ->
    let tr = Transaction.make Transaction.none db in
    let path = Protocol.Path.of_string_list t.name in
    let ls = Transaction.ls tr (Perms.of_domain 0) path in
    return (List.length ls)

  let add key item t =
    Database.store >>= fun db ->
    let tr = Transaction.make Transaction.none db in
    let key = K.to_string key in
    Transaction.write tr None 0 (Perms.of_domain 0) (Protocol.Path.of_string_list (t.name @ [ key ])) (Sexp.to_string (T.sexp_of_t item));
    Database.persist (Transaction.get_side_effects tr)

  let remove key t =
    recreate t >>= fun () ->
    Database.store >>= fun db ->
    let tr = Transaction.make Transaction.none db in
    let key = K.to_string key in
    Transaction.rm tr (Perms.of_domain 0) (Protocol.Path.of_string_list (t.name @ [ key ]));
    Database.persist (Transaction.get_side_effects tr)

  let mem key t =
    Database.store >>= fun db ->
    let tr = Transaction.make Transaction.none db in
    let key = K.to_string key in
    return (Transaction.exists tr (Perms.of_domain 0) (Protocol.Path.of_string_list (t.name @ [ key ])))

  let find key t =
    mem key t >>= function
    | false -> (* fail Not_found *) fail (Failure "find")
    | true ->
      Database.store >>= fun db ->
      let tr = Transaction.make Transaction.none db in
      let key = K.to_string key in
      return (T.t_of_sexp (Sexp.of_string (Transaction.read tr (Perms.of_domain 0) (Protocol.Path.of_string_list (t.name @ [ key ])))))

  let clear t =
    Database.store >>= fun db ->
    let tr = Transaction.make Transaction.none db in
    let path = Protocol.Path.of_string_list t.name in
    if Transaction.exists tr (Perms.of_domain 0) path then Transaction.rm tr (Perms.of_domain 0) path;
    Transaction.mkdir tr None 0 (Perms.of_domain 0) path;
    Database.persist (Transaction.get_side_effects tr) >>= fun () ->
    return ()

  let fold f initial t =
    recreate t >>= fun () ->
    Database.store >>= fun db ->
    let tr = Transaction.make Transaction.none db in
    let path = Protocol.Path.of_string_list t.name in
    let ls = Transaction.ls tr (Perms.of_domain 0) path in
    return (List.fold_left (fun acc k ->
      let v = Transaction.read tr (Perms.of_domain 0) (Protocol.Path.of_string_list (t.name @ [k])) in
      f acc (K.of_string k) (T.t_of_sexp (Sexp.of_string v))
    ) initial ls)

  let max_binding t =
    fold (fun best k v -> match best with
          | None -> Some(k, v)
          | Some (k', v') when k' < k -> Some (k, v)
          | x -> x) None t

  let min_binding t =
    fold (fun best k v -> match best with
          | None -> Some(k, v)
          | Some (k', v') when k' > k -> Some (k, v)
          | x -> x) None t
end
