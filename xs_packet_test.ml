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

open OUnit

let acl_parser _ =
  let open Xs_packet.ACL in
  let ts = [
    5, READ, [ 2, WRITE; 3, RDWR ];
    1, WRITE, [];
  ] in
  let ss = List.map to_string ts in
  let ts' = List.map of_string ss in
  let printer = function
    | None -> "None"
    | Some x -> "Some " ^ to_string x in
  List.iter
    (fun (x, y) -> assert_equal ~msg:"acl" ~printer x y)
    (List.combine (List.map (fun x -> Some x) ts) ts')

(*
let error_unmarshal _ =
  let open Xs_packet.Response in
  let enoent = 
*)
let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test xenstore protocol code";

  let suite = "xenstore" >:::
    [
      "acl_parser" >:: acl_parser;
    ] in
  run_test_tt ~verbose:!verbose suite
