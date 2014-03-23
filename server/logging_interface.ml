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

module Introspect = struct
  include Tree.Unsupported

  let ( |> ) a b = b a

  let general_params = [
    "conflict", disable_conflict;
    "commit", disable_conflict;
    "newconn", disable_newconn;
    "endconn", disable_endconn;
    "transaction", disable_transaction;
  ]

  let op_of_string path op =
    try Protocol.Op.t_of_sexp (Sexplib.Sexp.of_string op) with _ -> raise (Node.Doesnt_exist path)

  let read t (perms: Perms.t) (path: Protocol.Path.t) =
    Perms.has perms Perms.CONFIGURE;
    match Protocol.Path.to_string_list path with
    | [] -> ""
    | "request" :: [] -> ""
    | "reply-ok" :: [] -> ""
    | "reply-err" :: [] -> ""
    | "request" :: x :: [] -> if List.mem (op_of_string path x) !disable_request then "1" else raise (Node.Doesnt_exist path)
    | "reply-ok" :: x :: [] -> if List.mem (op_of_string path x) !disable_reply_ok then "1" else raise (Node.Doesnt_exist path)
    | "reply-err" :: x :: [] -> if List.mem x !disable_reply_err then "1" else raise (Node.Doesnt_exist path)
    | x :: [] ->
      if List.mem_assoc x general_params
      then if !(List.assoc x general_params) then "1" else raise (Node.Doesnt_exist path)
      else raise (Node.Doesnt_exist path)
    | _ -> raise (Node.Doesnt_exist path)

  let exists t perms path = try ignore(read t perms path); true with Node.Doesnt_exist _ -> false

  let write t limits creator perms path value =
    Perms.has perms Perms.CONFIGURE;
    let f list value key =
      match value with
      | "1" -> if not(List.mem key !list) then list := key :: !list
      | _ -> raise (Invalid_argument value)  in
    match Protocol.Path.to_string_list path with
    | "request" :: x :: [] -> f disable_request value (op_of_string path x)
    | "reply-ok" :: x :: [] -> f disable_reply_ok value (op_of_string path x)
    | "reply-err" :: x :: [] -> f disable_reply_err value x
    | x :: [] ->
      begin
        if List.mem_assoc x general_params then
          (List.assoc x general_params) := match value with
          | "1" -> true
          | _ -> raise (Invalid_argument value)
      end
    | _ -> raise (Node.Doesnt_exist path)

  let ls t perms path =
    Perms.has perms Perms.CONFIGURE;
    let string_of_op op = Sexplib.Sexp.to_string (Protocol.Op.sexp_of_t op) in
    match Protocol.Path.to_string_list path with
    | [] -> [ "request"; "reply-ok"; "reply-err" ] @ (List.map fst (List.filter (fun (_, b) -> !b) general_params))
    | "request" :: [] -> List.map string_of_op !disable_request
    | "reply-ok" :: [] -> List.map string_of_op !disable_reply_ok
    | "reply-err" :: [] -> !disable_reply_err
    | _ -> []

  let rm t perms path =
    Perms.has perms Perms.CONFIGURE; 
    let f list key = list := List.filter (fun x -> x <> key) !list in
    match Protocol.Path.to_string_list path with
    | "request" :: x :: [] -> f disable_request (op_of_string path x)
    | "reply-ok" :: x :: [] -> f disable_reply_ok (op_of_string path x)
    | "reply-err" :: x :: [] -> f disable_reply_err x
    | x :: [] ->
      if List.mem_assoc x general_params
      then (List.assoc x general_params) := false
      else raise (Node.Doesnt_exist path)
    | _ -> raise (Node.Doesnt_exist path)
end

let _ = Mount.mount (Protocol.Path.of_string "/tool/xenstore/logging") (module Introspect: Tree.S)
