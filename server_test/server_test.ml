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

let none = Transaction.none

type result =
	| OK
	| Err of string
	| StringList of (string list -> unit)
	| Perms of (Xs_packet.ACL.t -> unit)
	| Tid of (int32 -> unit)

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
	| Tid f ->
		if ty = Xs_packet.Op.Error
		then failwith (Printf.sprintf "Error: %s" data)
		else begin match Xs_packet.Unmarshal.int32 reply with
		| Some x -> f x
		| None -> failwith "Failed to unmarshal transaction id"
		end

let run store (payloads: (Connection.t * int32 * Xs_packet.Request.payload * result) list) =
	let one (c, tid, payload, expected_result) =
		let request = Xs_packet.Request.print payload tid in
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
		dom0, none, Read "/a", Err "ENOENT";
		domU, none, Read "/a", Err "ENOENT";
		(* If dom0 makes a node, suddenly domU gets EACCES: *)
		dom0, none, Write ("/a/b", "hello"), OK;
		domU, none, Read "/a/b", Err "EACCES";
		(* dom0 can also see the implicit path created: *)
		dom0, none, Read "/a", OK;
		(* domU gets EACCES: *)
		domU, none, Read "/a", Err "EACCES";
	]

let test_directory_order () =
	(* Create nodes in a particular order and check 'directory'
	   preserves the ordering *)
	let dom0 = Connection.create 0 in
	let store = empty_store () in
	let open Xs_packet.Request in
	run store [
		dom0, none, Write ("/a/1", ""), OK;
		dom0, none, Write ("/a/2/foo", ""), OK;
		dom0, none, Write ("/a/3", ""), OK;
		dom0, none, Directory "/a", StringList (fun x -> assert_equal ~msg:"directory /a" ~printer:(String.concat ", ") ["1"; "2"; "3"] x);
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
		dom0, none, Write ("/foo", ""), OK;
		dom0, none, Setperms("/foo", example_acl), OK;
		dom0, none, Getperms "/foo", Perms (fun x -> assert_equal ~msg:"perms /foo" ~printer:Xs_packet.ACL.to_string x example_acl);
	]

let test_setperms_owner () =
	(* Check that only the owner of a node can setperms even
	   if another domain has read/write access *)
	let dom0 = Connection.create 0 in
	let dom2 = Connection.create 2 in
	let dom5 = Connection.create 5 in
	let store = empty_store () in
	let open Xs_packet.Request in
	run store [
		dom0, none, Write ("/foo", ""), OK;
		dom0, none, Setperms("/foo", example_acl), OK;
		(* owned by dom5, so dom2 can't setperms *)
		dom2, none, Setperms("/foo", { example_acl with Xs_packet.ACL.owner = 2 }), Err "EACCES";
		(* dom5 sets the owner to dom2 *)
		dom5, none, Setperms("/foo", { example_acl with Xs_packet.ACL.owner = 2 }), OK;
		(* dom2 sets the owner back to dom5 *)
		dom2, none, Setperms("/foo", { example_acl with Xs_packet.ACL.owner = 5 }), OK;
	]

let test_restrict () =
	(* Check that only dom0 can restrict to another domain
	   and that it loses access to dom0-only nodes. *)
	let dom0 = Connection.create 0 in
	let dom3 = Connection.create 3 in
	let dom7 = Connection.create 7 in
	let store = empty_store () in
	let open Xs_packet.Request in
	run store [
		dom0, none, Write("/foo", "bar"), OK;
		dom0, none, Setperms("/foo", example_acl), OK;
		dom3, none, Write("/foo", "bar"), OK;
		dom7, none, Write("/foo", "bar"), Err "EACCES";
		dom0, none, Restrict 7, OK;
		dom0, none, Write("/foo", "bar"), Err "EACCES";
	]

let test_set_target () =
	(* Check that dom0 can grant dom1 access to dom2's nodes,
	   without which it wouldn't have access. *)
	let dom0 = Connection.create 0 in
	let dom3 = Connection.create 3 in
	let dom7 = Connection.create 7 in
	let store = empty_store () in
	let open Xs_packet.Request in
	run store [
		dom0, none, Write("/foo", "bar"), OK;
		dom0, none, Setperms("/foo", example_acl), OK;
		dom7, none, Write("/foo", "bar"), Err "EACCES";
		dom0, none, Set_target(7, 5), OK;
		dom7, none, Write("/foo", "bar"), OK;
	]

let test_transactions_are_isolated () =
	(* Check that other connections cannot see the nodes created
	   within an uncommitted transaction *)
	let dom0 = Connection.create 0 in
	let store = empty_store () in
	let open Xs_packet.Request in
	let tid = ref none in
	run store [
		dom0, none, Transaction_start, Tid(fun x -> tid := x);
		dom0, !tid, Write("/foo", "bar"), OK;
		dom0, none, Read "/foo", Err "ENOENT"
	]

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
		"test_setperms_owner" >:: test_setperms_owner;
		"test_restrict" >:: test_restrict;
		"test_set_target" >:: test_set_target;
		"transactions_are_isolated" >:: test_transactions_are_isolated;
	] in
  run_test_tt ~verbose:!verbose suite
