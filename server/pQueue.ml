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
  module M = PMap.Make(Int64)(T)

  type t = {
    mutable next_id: int64;
    mutable root: M.t;
  }

  let create name =
    M.create name >>= fun root ->
    let next_id = Int64.succ (try fst(M.max_binding root) with Not_found -> (-1L)) in
    return { next_id; root }

  let length t = M.cardinal t.root

  let add item t =
    let id = t.next_id in
    t.next_id <- Int64.succ t.next_id;
    M.add id item t.root

  let clear t =
    t.next_id <- 0L;
    M.clear t.root

  let fold f initial t = M.fold (fun acc _ v -> f acc v) initial t.root
end
