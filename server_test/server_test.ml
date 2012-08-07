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
let ( ++ ) a b x = a (b x)
let id x = x

let empty_store () = Store.create ()

let none = Transaction.none

let success f reply =
	match Xs_packet.get_ty reply with
		| Xs_packet.Op.Error ->
			failwith (Printf.sprintf "Error: %s" (Xs_packet.get_data reply))
		| _ -> f reply

let failure f reply =
	match Xs_packet.get_ty reply with
		| Xs_packet.Op.Error -> f reply
		| _ ->
			failwith (Printf.sprintf "Expected failure, got success: %s" (Junk.hexify(Xs_packet.to_string reply)))

let list f reply = match Xs_packet.Unmarshal.list reply with
	| Some x -> f x
	| None -> failwith "Failed to unmarshal string list"

let string f reply = match Xs_packet.Unmarshal.string reply with
	| Some x -> f x
	| None -> failwith "Failed to unmarshal string"

let acl f reply = match Xs_packet.Unmarshal.acl reply with
	| Some x -> f x
	| None -> failwith "Failed to unmarshal acl"

let int32 f reply = match Xs_packet.Unmarshal.int32 reply with
	| Some x -> f x
	| None -> failwith "Failed to unmarshal int32"

let equals expected got =
	if expected <> got
	then failwith (Printf.sprintf "Expected %s got %s" expected got)

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
		success ignore reply
	| Err which ->
		(failure ++ string ++ equals) which reply
	| StringList f ->
		(success ++ list) f reply
	| Perms f ->
		(success ++ acl) f reply
	| Tid f ->
		(success ++ int32) f reply

let rpc store c tid payload =
	let request = Xs_packet.Request.print payload tid in
	Call.reply store c request

let run store (payloads: (Connection.t * int32 * Xs_packet.Request.payload * result) list) =
	List.iter
		(fun (c, tid, payload, expected_result) ->
			check_result (rpc store c tid payload) expected_result
		) payloads

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

let test_mkdir () =
	(* Check that mkdir creates usable nodes *)
	let dom0 = Connection.create 0 in
	let store = empty_store () in
	let open Xs_packet.Request in
	run store [
		dom0, none, Read "/a/b", Err "ENOENT";
		dom0, none, Read "/a", Err "ENOENT";
	];
	let tid = (success ++ int32) id (rpc store dom0 none Transaction_start) in
	run store [
		dom0, tid, Mkdir "/bench/local/domain/0", OK;
		dom0, tid, Setperms("/bench/local/domain/0", example_acl), OK;
		dom0, tid, Read "/bench/local/domain/0", OK;
		dom0, tid, Transaction_end true, OK;
	]

let test_empty () =
	(* Check that I can read an empty value *)
	let dom0 = Connection.create 0 in
	let store = empty_store () in
	let open Xs_packet.Request in
	run store [
		dom0, none, Write("/a", ""), OK;
		dom0, none, Read "/a", OK;
	]

let test_directory () =
	()

let test_rm () =
	(* rm of a missing node from an existing parent should succeed *)
	(* rm of a missing node from a missing parent should ENOENT *)
	let dom0 = Connection.create 0 in
	let store = empty_store () in
	let open Xs_packet.Request in
	run store [
		dom0, none, Rm "/a", OK;
		dom0, none, Rm "/a/b", Err "ENOENT";
		dom0, none, Write ("/a", "hello"), OK;
		dom0, none, Rm "/a/b", OK;
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

	let tid = (success ++ int32) id (rpc store dom0 none Transaction_start) in

	run store [
		dom0, tid, Write("/foo", "bar"), OK;
		dom0, none, Read "/foo", Err "ENOENT";
		dom0, tid, Transaction_end true, OK;
		dom0, none, Read "/foo", OK;
	]

let test_independent_transactions_coalesce () =
	(* Check that two parallel, unrelated transactions can be
	   coalesced properly *)
	let dom0 = Connection.create 0 in
	let store = empty_store () in
	let open Xs_packet.Request in

	run store [
		dom0, none, Mkdir "/a/b", OK;
		dom0, none, Mkdir "/1/2", OK;
	];
	let tid_1 = (success ++ int32) id (rpc store dom0 none Transaction_start) in
	let tid_2 = (success ++ int32) id (rpc store dom0 none Transaction_start) in
	run store [
		dom0, tid_1, Write("/a/b", "foo"), OK;
		dom0, tid_2, Write("/1/2", "foo"), OK;
		dom0, tid_1, Transaction_end true, OK;
		dom0, tid_2, Transaction_end true, OK;
	]

let test_device_create_coalesce () =
	(* Check that two parallel, device-creating transactions can coalesce *)
	let dom0 = Connection.create 0 in
	let store = empty_store () in
	let open Xs_packet.Request in
	run store [
		dom0, none, Mkdir "/local/domain/0/backend/vbd", OK;
		dom0, none, Mkdir "/local/domain/1/device/vbd", OK;
		dom0, none, Mkdir "/local/domain/2/device/vbd", OK;
	];
	let tid_1 = (success ++ int32) id (rpc store dom0 none Transaction_start) in
	let tid_2 = (success ++ int32) id (rpc store dom0 none Transaction_start) in
	run store [
		dom0, tid_1, Write("/local/domain/0/backend/vbd/1/51712", "hello"), OK;
		dom0, tid_1, Write("/local/domain/1/device/vbd/51712", "there"), OK;
		dom0, tid_2, Write("/local/domain/0/backend/vbd/2/51712", "hello"), OK;
		dom0, tid_2, Write("/local/domain/2/device/vbd/51712", "there"), OK;
		dom0, tid_1, Transaction_end true, OK;
		dom0, tid_2, Transaction_end true, OK;
	]

let string_of_watch_events watch_events =
	String.concat "; " (List.map (fun (k, v) -> k ^ ", " ^ v) watch_events)

let assert_watches c expected =
	let got = List.rev (Queue.fold (fun acc x -> x :: acc) [] c.Connection.watch_events) in
	assert_equal ~msg:"watches" ~printer:string_of_watch_events expected got

let test_simple_watches () =
	(* Check that writes generate watches and reads do not *)
	let dom0 = Connection.create 0 in
	let dom1 = Connection.create 1 in
	let store = empty_store () in
	let open Xs_packet.Request in
	(* No watch events are generated without registering *)
	run store [
		dom0, none, Mkdir "/a", OK;
		dom0, none, Setperms("/a", Xs_packet.ACL.({ owner = 0; other = RDWR; acl = []})), OK;
	];
	assert_watches dom0 [];
	run store [
		dom0, none, Watch ("/a", "token"), OK;
	];
	assert_watches dom0 [ ("/a", "token") ];
	Queue.clear dom0.Connection.watch_events;
	assert_watches dom0 [];
	(* dom0 can see its own write via watches *)
	run store [
		dom0, none, Write("/a", "foo"), OK;
	];
	assert_watches dom0 [ ("/a", "token") ];
	Queue.clear dom0.Connection.watch_events;
	assert_watches dom0 [];
	(* dom0 can see dom1's writes via watches *)
	run store [
		dom1, none, Write("/a", "foo"), OK;
	];
	assert_watches dom0 [ ("/a", "token") ];
	Queue.clear dom0.Connection.watch_events;
	assert_watches dom0 [];
	(* reads don't generate watches *)
	run store [
		dom0, none, Read "/a", OK;
		dom0, none, Read "/a/1", Err "ENOENT";
		dom1, none, Read "/a", OK;
		dom1, none, Read "/a/1", Err "ENOENT";
	];
	assert_watches dom0 []

let test_watches_read_perm () =
	(* Check that a connection only receives a watch if it
       can read the node that was modified. *)
	let dom0 = Connection.create 0 in
	let dom1 = Connection.create 1 in
	let store = empty_store () in
	let open Xs_packet.Request in
	run store [
		dom1, none, Watch ("/a", "token"), OK;
	];
	assert_watches dom1 [ ("/a", "token") ];
	Queue.clear dom1.Connection.watch_events;
	assert_watches dom1 [];
	run store [
		dom0, none, Write ("/a", "hello"), OK;
		dom1, none, Read "/a", Err "EACCES";
	];
	assert_watches dom1 []

let test_transaction_watches () =
	(* Check that watches only appear on transaction commit
	   and not at all in the case of abort *)
	let dom0 = Connection.create 0 in
	let store = empty_store () in
	let open Xs_packet.Request in
	run store [
		dom0, none, Watch ("/a", "token"), OK;
	];
	assert_watches dom0 [ ("/a", "token") ];
	Queue.clear dom0.Connection.watch_events;
	assert_watches dom0 [];
	(* Writes in a transaction don't generate watches immediately *)
	let tid = (success ++ int32) id (rpc store dom0 none Transaction_start) in
	run store [
		dom0, tid, Write ("/a", "hello"), OK;
	];
	assert_watches dom0 [];
	(* If the transaction is aborted then no watches are generated *)
	run store [
		dom0, tid, Transaction_end false, OK
	];
	assert_watches dom0 [];
	(* If the transaction successfully commits then the watches appear *)
	let tid = (success ++ int32) id (rpc store dom0 none Transaction_start) in
	run store [
		dom0, tid, Write ("/a", "hello"), OK;
		dom0, tid, Transaction_end true, OK
	];
	assert_watches dom0 [ ("/a", "token") ]


let test_introduce_watches () =
	(* Check that @introduceDomain watches appear on introduce *)
	let dom0 = Connection.create 0 in
	let store = empty_store () in
	let open Xs_packet.Request in
	run store [
		dom0, none, Watch ("@introduceDomain", "token"), OK;
	];
	assert_watches dom0 [ ("@introduceDomain", "token") ];
	Queue.clear dom0.Connection.watch_events;
	assert_watches dom0 [];
	run store [
		dom0, none, Introduce(5, 5n, 5), OK;
	];
	assert_watches dom0 [ ("@introduceDomain", "token") ]

let test_release_watches () =
	(* Check that @releaseDomain watches appear on introduce *)
	()

let test_recursive_rm_watch () =
	(* Check that rm generates recursive watches *)
	()

let test_no_watch_no_error () =
	(* Check that a write failure doesn't generate a watch *)
	()

let test_bounded_watch_events () =
	(* Check that the per-connection watch event queue is bounded *)
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
		"test_mkdir" >:: test_mkdir;
		"test_empty" >:: test_empty;
		"test_rm" >:: test_rm;
		"test_restrict" >:: test_restrict;
		"test_set_target" >:: test_set_target;
		"transactions_are_isolated" >:: test_transactions_are_isolated;
		"independent_transactions_coalesce" >:: test_independent_transactions_coalesce;
(*		"device_create_coalesce" >:: test_device_create_coalesce; *)
		"test_simple_watches" >:: test_simple_watches;
(*		"test_watches_read_perm" >:: test_watches_read_perm; *)
		"test_transaction_watches" >:: test_transaction_watches;
		"test_introduce_watches" >:: test_introduce_watches;
	] in
  run_test_tt ~verbose:!verbose suite
