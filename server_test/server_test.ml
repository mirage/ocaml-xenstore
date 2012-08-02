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
	| StringList of (string list -> unit)
	| Perms of (Xs_packet.ACL.t -> unit)

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
	| StringList f ->
		if ty = Xs_packet.Op.Error
		then failwith (Printf.sprintf "Error: %s" data)
		else begin match Xs_packet.Unmarshal.list reply with
		| Some x -> f x
		| None -> failwith "Failed to unmarshal string list"
		end
	| Perms f ->
		if ty = Xs_packet.Op.Error
		then failwith (Printf.sprintf "Error: %s" data)
		else begin match Xs_packet.Unmarshal.acl reply with
		| Some x -> f x
		| None -> failwith "Failed to unmarshal ACLs"
		end

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

let test_directory_order () =
	(* Create nodes in a particular order and check 'directory'
	   preserves the ordering *)
	let dom0 = Connection.create 0 in
	let store = empty_store () in
	let open Xs_packet.Request in
	run store [
		dom0, Write ("/a/1", ""), OK;
		dom0, Write ("/a/2/foo", ""), OK;
		dom0, Write ("/a/3", ""), OK;
		dom0, Directory "/a", StringList (fun x -> assert_equal ~msg:"directory /a" ~printer:(String.concat ", ") ["1"; "2"; "3"] x);
	]

let example_acl =
	let open Xs_packet.ACL in
    { owner = 5; other = READ; acl = [ 2, WRITE; 3, RDWR ] }

let test_setperms_getperms () =
	(* Check that getperms(setperms(x)) = x *)
	let dom0 = Connection.create 0 in
	let store = empty_store () in
	let open Xs_packet.Request in
	run store [
		dom0, Write ("/foo", ""), OK;
		dom0, Setperms("/foo", example_acl), OK;
		dom0, Getperms "/foo", Perms (fun x -> assert_equal ~msg:"perms /foo" ~printer:Xs_packet.ACL.to_string x example_acl);
	]

let test_setperms_owner () =
	(* Check that only the owner of a node can setperms even
	   if another domain has read/write access *)
	()

let test_restrict () =
	(* Check that only dom0 can restrict to another domain
	   and that it loses access to dom0-only nodes. *)
	()

let test_set_target () =
	(* Check that dom0 can grant dom1 access to dom2's nodes,
	   without which it wouldn't have access. *)
	()

let test_transactions_are_isolated () =
	(* Check that other connections cannot see the nodes created
	   within an uncommitted transaction *)
	()

let test_independent_transactions_coalesce () =
	(* Check that two parallel, unrelated transactions can be
	   coalesced properly *)
	()

let test_device_create_coalesce () =
	(* Check that two parallel, device-creating transactions can coalesce *)
	()

let test_transaction_watches () =
	(* Check that watches only appear on transaction commit *)
	()



let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test xenstore server code";

  let suite = "xenstore" >:::
    [
		"test_implicit_create" >:: test_implicit_create;
		"test_directory_order" >:: test_directory_order;
		"getperms(setperms)" >:: test_setperms_getperms;
	] in
  run_test_tt ~verbose:!verbose suite
