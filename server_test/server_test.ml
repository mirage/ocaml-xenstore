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

let ( |> ) a b = b a

let empty_store () = Store.create ()


type result =
	| OK
	| Err of string
let check_result reply =
	let ty = Xs_packet.get_ty reply in
	let data = Xs_packet.get_data reply in
	function
	| OK ->
		if ty = Xs_packet.Op.Error
		then failwith (Printf.sprintf "Error: %s" data)
	| Err which ->
		if ty <> Xs_packet.Op.Error
		then failwith (Printf.sprintf "Expected %s got success" which)
		else if data <> which
		then failwith (Printf.sprintf "Expected %s got %s" which data)

let run store (payloads: (Connection.t * Xs_packet.Request.payload * result) list) =
	let one (c, payload, expected_result) =
		let request = Xs_packet.Request.print payload 0l in
		let reply = Call.reply store c request in
		check_result reply expected_result in
	List.iter one payloads

let test_implicit_create () =
	(* Write a path and check the parent nodes can be read *)
	let dom0 = Connection.create 0 in
	let domU = Connection.create 1 in
	let store = empty_store () in
	let open Xs_packet.Request in
	run store [
		(* If a node doesn't exist, everyone gets ENOENT: *)
		dom0, Read "/a", Err "ENOENT";
		domU, Read "/a", Err "ENOENT";
		(* If dom0 makes a node, suddenly domU gets EACCES: *)
		dom0, Write ("/a/b", "hello"), OK;
		domU, Read "/a/b", Err "EACCES";
		(* dom0 can also see the implicit path created: *)
		dom0, Read "/a", OK;
		(* domU gets EACCES: *)
		domU, Read "/a", Err "EACCES";
	]

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test xenstore server code";

  let suite = "xenstore" >:::
    [
		"test_implicit_create" >:: test_implicit_create;
	] in
  run_test_tt ~verbose:!verbose suite
