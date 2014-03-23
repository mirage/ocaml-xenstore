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
open Sexplib.Std
open Xenstore
open Lwt
open Logging

module POpBoolMap = PMap.Make(Protocol.Op)(struct type t = bool with sexp end)

let requests  = POpBoolMap.create [ "tool"; "xenstored"; "log"; "request" ]
let responses = POpBoolMap.create [ "tool"; "xenstored"; "log"; "response" ]

let request r =
  requests >>= fun requests ->
  POpBoolMap.mem (Protocol.Request.get_ty r) requests >>= function
  | false -> return false
  | true -> POpBoolMap.find (Protocol.Request.get_ty r) requests

let response r =
  responses >>= fun responses ->
  POpBoolMap.mem (Protocol.Response.get_ty r) responses >>= function
  | false -> return false
  | true -> POpBoolMap.find (Protocol.Response.get_ty r) responses

let _ =
  (* Populate every missing key with an explicit 'false' so that we can
     see what the keys are supposed to be *)
  let missing_becomes_false map =
    Lwt_list.iter_s (fun x ->
      POpBoolMap.mem x map >>= function
      | false -> POpBoolMap.add x false map
      | true -> return ()
    ) Protocol.Op.all in
  requests >>= fun requests ->
  missing_becomes_false requests >>= fun () ->
  responses >>= fun responses ->
  missing_becomes_false responses

