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
open Xenstore
open OUnit
open Sexplib

let ( |> ) a b = b a
let ( ++ ) a b x = a (b x)
let id x = x

module Q = Quota_interface (* make sure the filesystem is mounted *)

let empty_store () = Store.create ()

let none = Transaction.none

let rpc store c tid request =
        let open Lwt in
        let hdr = { Protocol.Header.tid; rid = 0l; ty = Protocol.Request.get_ty request; len = 0 } in
        let response, side_effects = Call.reply store c hdr request in
        Transaction.get_watches side_effects |> List.rev |> Lwt_list.iter_s Connection.fire >>= fun () ->
        return response

let run store (sequence: (Connection.t * int32 * Protocol.Request.t * Protocol.Response.t) list) =
        let open Lwt in
	Lwt_main.run (Lwt_list.iter_s
		(fun (c, tid, request, expected_result) ->
                        rpc store c tid request >>= fun actual ->
                        (* Store.dump_stdout store; *)
                        assert_equal ~printer:(fun x -> Sexp.to_string (Protocol.Response.sexp_of_t x)) expected_result actual;
                        return ()
		) sequence)

let interdomain domid = Uri.make ~scheme:"domain" ~path:(string_of_int domid) (), domid

let connect domid = Lwt_main.run (Connection.create (interdomain domid))

let test_implicit_create () =
	(* Write a path and check the parent nodes can be read *)
	let dom0 = connect 0 in
	let domU = connect 1 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		(* If a node doesn't exist, everyone gets ENOENT: *)
		dom0, none, PathOp("/a", Read), Response.Error "ENOENT";
		domU, none, PathOp("/a", Read), Response.Error "ENOENT";
		(* If dom0 makes a node, suddenly domU gets EACCES: *)
		dom0, none, PathOp("/a/b", Write "hello"), Response.Write;
		domU, none, PathOp("/a/b", Read), Response.Error "EACCES";
		(* dom0 can also see the implicit path created: *)
		dom0, none, PathOp("/a", Read), Response.Read "";
		(* domU gets EACCES: *)
		domU, none, PathOp("/a", Read), Response.Error "EACCES";
	]

let test_directory_order () =
	(* Create nodes in a particular order and check 'directory'
	   preserves the ordering *)
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom0, none, PathOp("/a/1", Write ""), Response.Write;
		dom0, none, PathOp("/a/2/foo", Write ""), Response.Write;
		dom0, none, PathOp("/a/3", Write ""), Response.Write;
		dom0, none, PathOp("/a", Directory), Response.Directory ["1"; "2"; "3"];
	]

let example_acl =
	let open Protocol.ACL in
    { owner = 5; other = READ; acl = [ 2, WRITE; 3, RDWR ] }

let test_setperms_getperms () =
	(* Check that getperms(setperms(x)) = x *)
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom0, none, PathOp("/foo", Write ""), Response.Write;
		dom0, none, PathOp("/foo", Setperms example_acl), Response.Setperms;
                dom0, none, PathOp("/foo", Getperms), Response.Getperms example_acl;
	]

let test_setperms_owner () =
	(* Check that only the owner of a node can setperms even
	   if another domain has read/write access *)
        let dom0 = connect 0 in
        let dom2 = connect 2 in
        let dom5 = connect 5 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom0, none, PathOp("/foo", Write ""), Response.Write;
		dom0, none, PathOp("/foo", Setperms example_acl), Response.Setperms;
		(* owned by dom5, so dom2 can't setperms *)
		dom2, none, PathOp("/foo", Setperms { example_acl with Protocol.ACL.owner = 2 }), Response.Error "EACCES";
		(* dom5 sets the owner to dom2 *)
		dom5, none, PathOp("/foo", Setperms { example_acl with Protocol.ACL.owner = 2 }), Response.Setperms;
		(* dom2 sets the owner back to dom5 *)
		dom2, none, PathOp("/foo", Setperms { example_acl with Protocol.ACL.owner = 5 }), Response.Setperms;
	]

let begin_transaction store c =
        let open Lwt in
        Lwt_main.run
                (rpc store c none Protocol.Request.Transaction_start >>= function
                | Protocol.Response.Transaction_start tid -> return tid
                | _ -> failwith "begin_transaction")

let test_mkdir () =
	(* Check that mkdir creates usable nodes *)
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom0, none, PathOp("/a/b", Read), Response.Error "ENOENT";
		dom0, none, PathOp("/a", Read), Response.Error "ENOENT";
	];
        let tid = begin_transaction store dom0 in
	run store [
		dom0, tid, PathOp("/bench/local/domain/0", Mkdir), Response.Mkdir;
		dom0, tid, PathOp("/bench/local/domain/0", Setperms example_acl), Response.Setperms;
		dom0, tid, PathOp("/bench/local/domain/0", Read), Response.Read "";
		dom0, tid, Transaction_end true, Response.Transaction_end;
	]

let test_empty () =
	(* Check that I can read an empty value *)
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom0, none, PathOp("/a", Write ""), Response.Write;
		dom0, none, PathOp("/a", Read), Response.Read "";
	]

let test_directory () =
	()

let test_rm () =
	(* rm of a missing node from an existing parent should succeed *)
	(* rm of a missing node from a missing parent should ENOENT *)
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom0, none, PathOp("/a", Rm), Response.Rm;
		dom0, none, PathOp("/a/b", Rm), Response.Error "ENOENT";
		dom0, none, PathOp("/a", Write "hello"), Response.Write;
		dom0, none, PathOp("/a/b", Rm), Response.Rm;
	]

let test_restrict () =
	(* Check that only dom0 can restrict to another domain
	   and that it loses access to dom0-only nodes. *)
        let dom0 = connect 0 in
        let dom3 = connect 3 in
        let dom7 = connect 7 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom0, none, PathOp("/foo", Write "bar"), Response.Write;
		dom0, none, PathOp("/foo", Setperms example_acl), Response.Setperms;
		dom3, none, PathOp("/foo", Write "bar"), Response.Write;
		dom7, none, PathOp("/foo", Write "bar"), Response.Error "EACCES";
		dom0, none, Restrict 7, Response.Restrict;
		dom0, none, PathOp("/foo", Write "bar"), Response.Error "EACCES";
	]

let test_set_target () =
	(* Check that dom0 can grant dom1 access to dom2's nodes,
	   without which it wouldn't have access. *)
        let dom0 = connect 0 in
        let dom7 = connect 7 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom0, none, PathOp("/foo", Write "bar"), Response.Write;
		dom0, none, PathOp("/foo", Setperms example_acl), Response.Setperms;
		dom7, none, PathOp("/foo", Write "bar"), Response.Error "EACCES";
		dom0, none, Set_target(7, 5), Response.Set_target;
		dom7, none, PathOp("/foo", Write "bar"), Response.Write;
	]

let test_transactions_are_isolated () =
	(* Check that other connections cannot see the nodes created
	   within an uncommitted transaction *)
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
        let tid = begin_transaction store dom0 in

	run store [
		dom0, tid, PathOp("/foo", Write "bar"), Response.Write;
		dom0, none, PathOp("/foo", Read), Response.Error "ENOENT";
		dom0, tid, Transaction_end true, Response.Transaction_end;
		dom0, none, PathOp("/foo", Read), Response.Read "bar";
	]

let test_independent_transactions_coalesce () =
	(* Check that two parallel, unrelated transactions can be
	   coalesced properly *)
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in

	run store [
		dom0, none, PathOp("/a/b", Mkdir), Response.Mkdir;
		dom0, none, PathOp("/1/2", Mkdir), Response.Mkdir;
	];
        let tid_1 = begin_transaction store dom0 in
        let tid_2 = begin_transaction store dom0 in
	run store [
		dom0, tid_1, PathOp("/a/b", Write "foo"), Response.Write;
		dom0, tid_2, PathOp("/1/2", Write "foo"), Response.Write;
		dom0, tid_1, Transaction_end true, Response.Transaction_end;
		dom0, tid_2, Transaction_end true, Response.Transaction_end;
		dom0, none, PathOp("/a/b", Read), Response.Read "foo";
		dom0, none, PathOp("/1/2", Read), Response.Read "foo";
	]

let test_device_create_coalesce () =
	(* Check that two parallel, device-creating transactions can coalesce *)
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom0, none, PathOp("/local/domain/0/backend/vbd", Mkdir), Response.Mkdir;
		dom0, none, PathOp("/local/domain/1/device/vbd", Mkdir), Response.Mkdir;
		dom0, none, PathOp("/local/domain/2/device/vbd", Mkdir), Response.Mkdir;
	];
        let tid_1 = begin_transaction store dom0 in
        let tid_2 = begin_transaction store dom0 in
	run store [
		dom0, tid_1, PathOp("/local/domain/0/backend/vbd/1/51712", Write "hello"), Response.Write;
		dom0, tid_1, PathOp("/local/domain/1/device/vbd/51712", Write "there"), Response.Write;
		dom0, tid_2, PathOp("/local/domain/0/backend/vbd/2/51712", Write "hello"), Response.Write;
		dom0, tid_2, PathOp("/local/domain/2/device/vbd/51712", Write "there"), Response.Write;
		dom0, tid_1, Transaction_end true, Response.Transaction_end;
		dom0, tid_2, Transaction_end true, Response.Transaction_end;
		dom0, none, PathOp("/local/domain/0/backend/vbd/1/51712", Read), Response.Read "hello";
		dom0, none, PathOp("/local/domain/0/backend/vbd/2/51712", Read), Response.Read "hello";
	]

let test_transactions_really_do_conflict () =
	(* Check that transactions that really can't interleave are aborted *)
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom0, none, PathOp("/a", Mkdir), Response.Mkdir;
	];
        let tid = begin_transaction store dom0 in
	run store [
                dom0, tid, PathOp("/a", Directory), Response.Directory [];
		dom0, none, PathOp("/a/b", Write "hello"), Response.Write;
		dom0, tid, PathOp("/a/b", Write "there"), Response.Write;
		dom0, tid, Transaction_end true, Response.Error "EAGAIN";
		dom0, none, PathOp("/a/b", Read), Response.Read "hello"
	]


let string_of_watch_events watch_events =
	String.concat "; " (List.map (fun (k, v) -> k ^ ", " ^ v) watch_events)

let assert_watches c expected =
	let got = List.rev (Connection.Watch_events.fold (fun acc x -> x :: acc) [] c.Connection.watch_events) in
	assert_equal ~msg:"watches" ~printer:string_of_watch_events expected got

let test_watch_event_quota () =
	(* Check that we can't exceed the per-domain watch event quota *)
        let dom0 = connect 0 in
        let dom1 = connect 1 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	(* No watch events are generated without registering *)
	run store [
		dom0, none, PathOp("/tool/xenstored/quota/number-of-queued-watch-events/1", Write "1"), Response.Write;
		dom0, none, PathOp("/a", Mkdir), Response.Mkdir;
		dom0, none, PathOp("/a", Setperms Protocol.ACL.({ owner = 0; other = RDWR; acl = []})), Response.Setperms;
	];
	assert_watches dom1 [];
	run store [
		dom1, none, Watch ("/a", "token"), Response.Watch;
	];
	assert_watches dom1 [ ("/a", "token") ];
	assert_equal ~msg:"nb_dropped_watches" ~printer:string_of_int 0 dom1.Connection.nb_dropped_watches;
	(* This watch will be dropped *)
	run store [
		dom0, none, PathOp("/a", Write "hello"), Response.Write;
	];
	assert_watches dom1 [ ("/a", "token") ];
	assert_equal ~msg:"nb_dropped_watches" ~printer:string_of_int 1 dom1.Connection.nb_dropped_watches;
	run store [
		dom0, none, PathOp("/tool/xenstored/quota/number-of-queued-watch-events/1", Write "2"), Response.Write;
		dom0, none, PathOp("/a", Write "there"), Response.Write;
	];
	assert_watches dom1 [ ("/a", "token"); ("/a", "token") ];
	assert_equal ~msg:"nb_dropped_watches" ~printer:string_of_int 1 dom1.Connection.nb_dropped_watches

let test_simple_watches () =
	(* Check that writes generate watches and reads do not *)
        let dom0 = connect 0 in
        let dom1 = connect 1 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	(* No watch events are generated without registering *)
	run store [
		dom0, none, PathOp("/a", Mkdir), Response.Mkdir;
		dom0, none, PathOp("/a", Setperms Protocol.ACL.({ owner = 0; other = RDWR; acl = []})), Response.Setperms;
	];
	assert_watches dom0 [];
	run store [
		dom0, none, Watch ("/a", "token"), Response.Watch;
	];
	assert_watches dom0 [ ("/a", "token") ];
        Lwt_main.run (Connection.Watch_events.clear dom0.Connection.watch_events);
	assert_watches dom0 [];
	(* dom0 can see its own write via watches *)
	run store [
		dom0, none, PathOp("/a", Write "foo"), Response.Write;
	];
	assert_watches dom0 [ ("/a", "token") ];
        Lwt_main.run (Connection.Watch_events.clear dom0.Connection.watch_events);
	assert_watches dom0 [];
	(* dom0 can see dom1's writes via watches *)
	run store [
		dom1, none, PathOp("/a", Write "foo"), Response.Write;
	];
	assert_watches dom0 [ ("/a", "token") ];
        Lwt_main.run (Connection.Watch_events.clear dom0.Connection.watch_events);
	assert_watches dom0 [];
	(* reads don't generate watches *)
	run store [
		dom0, none, PathOp("/a", Read), Response.Read "foo";
		dom0, none, PathOp("/a/1", Read), Response.Error "ENOENT";
		dom1, none, PathOp("/a", Read), Response.Read "foo";
		dom1, none, PathOp("/a/1", Read), Response.Error "ENOENT";
	];
	assert_watches dom0 []

let test_relative_watches () =
	(* Check that watches for relative paths *)
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	(* No watch events are generated without registering *)
	run store [
		dom0, none, PathOp("/local/domain/0/name", Write ""), Response.Write;
		dom0, none, PathOp("/local/domain/0/device", Write ""), Response.Write;
		dom0, none, Watch("device", "token"), Response.Watch;
	];
	assert_watches dom0 [ "device", "token" ];
        Lwt_main.run (Connection.Watch_events.clear dom0.Connection.watch_events);
	assert_watches dom0 [];
	run store [
		dom0, none, PathOp("/local/domain/0/device/vbd", Write "hello"), Response.Write;
	];
	assert_watches dom0 [ "device/vbd", "token" ]

let test_watches_read_perm () =
	(* Check that a connection only receives a watch if it
       can read the node that was modified. *)
        let dom0 = connect 0 in
        let dom1 = connect 1 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom1, none, Watch ("/a", "token"), Response.Watch;
	];
	assert_watches dom1 [ ("/a", "token") ];
        Lwt_main.run (Connection.Watch_events.clear dom1.Connection.watch_events);
	assert_watches dom1 [];
	run store [
		dom0, none, PathOp("/a", Write "hello"), Response.Write;
		dom1, none, PathOp("/a", Read), Response.Error "EACCES";
	];
	assert_watches dom1 []

let test_transaction_watches () =
	(* Check that watches only appear on transaction commit
	   and not at all in the case of abort *)
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom0, none, Watch ("/a", "token"), Response.Watch;
	];
	assert_watches dom0 [ ("/a", "token") ];
        Lwt_main.run (Connection.Watch_events.clear dom0.Connection.watch_events);
	assert_watches dom0 [];
	(* PathOp( Writes in a transaction don't generate watches immediately *)
        let tid = begin_transaction store dom0 in
	run store [
		dom0, tid, PathOp("/a", Write "hello"), Response.Write;
	];
	assert_watches dom0 [];
	(* If the transaction is aborted then no watches are generated *)
	run store [
		dom0, tid, Transaction_end false, Response.Transaction_end
	];
	assert_watches dom0 [];
	(* If the transaction successfully commits then the watches appear *)
        let tid = begin_transaction store dom0 in
	run store [
		dom0, tid, PathOp("/a", Write "hello"), Response.Write;
		dom0, tid, Transaction_end true, Response.Transaction_end
	];
	assert_watches dom0 [ ("/a", "token") ]

let test_introduce_watches () =
	(* Check that @introduceDomain watches appear on introduce *)
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom0, none, Watch ("@introduceDomain", "token"), Response.Watch;
	];
	assert_watches dom0 [ ("@introduceDomain", "token") ];
        Lwt_main.run (Connection.Watch_events.clear dom0.Connection.watch_events);
	assert_watches dom0 [];
	run store [
		dom0, none, Introduce(5, 5n, 5), Response.Introduce;
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

let test_rm_root () =
        (* Check that deleting / fails *)
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		(* Removing the root node is forbidden *)
		dom0, none, PathOp("/", Rm), Response.Error "EINVAL";
	]


let test_quota () =
	(* Check that node creation and destruction changes a quota *)
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in

	run store [
(*		dom0, none, PathOp("/quota/entries-per-domain/0", Read), StringList (fun x -> start := int_of_string (List.hd x)); *)
		dom0, none, PathOp("/a", Write "hello"), Response.Write;
                dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/0", Read), Response.Read "1";
		(* Implicit creation of 2 elements *)
		dom0, none, PathOp("/a/b/c", Write "hello"), Response.Write;
                dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/0", Read), Response.Read "3";
		(* Remove one element *)
		dom0, none, PathOp("/a/b/c", Rm), Response.Rm;
                dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/0", Read), Response.Read "2";
		(* Recursive remove of 2 elements *)
		dom0, none, PathOp("/a", Rm), Response.Rm;
                dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/0", Read), Response.Read "0";
		(* Remove an already removed element *)
		dom0, none, PathOp("/a", Rm), Response.Rm;
                dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/0", Read), Response.Read "0";
		(* Remove a missing element: *)
		dom0, none, PathOp("/a", Rm), Response.Rm;
		dom0, none, PathOp("/a", Rm), Response.Rm;
		dom0, none, PathOp("/a", Rm), Response.Rm;
                dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/0", Read), Response.Read "0";
		(* Removing the root node is forbidden *)
		dom0, none, PathOp("/", Rm), Response.Error "EINVAL";
                dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/0", Read), Response.Read "0";
	]

let test_quota_ls () =
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in

	run store [
                dom0, none, PathOp("/tool/xenstored/quota", Directory), Response.Directory
                        [
                                "default";
                                "entries-per-domain";
                                "number-of-entries";
                                "number-of-registered-watches";
                                "number-of-active-transactions";
                                "number-of-queued-watch-events";
                        ]
        ]

let test_quota_transaction () =
	(* Check that node creation and destruction changes a quota *)
        let dom0 = connect 0 in
        let dom1 = connect 1 in
        let dom2 = connect 2 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in

	run store [
		dom0, none, PathOp("/local/domain/1", Write ""), Response.Write;
		dom0, none, PathOp("/local/domain/1", Setperms { example_acl with Protocol.ACL.owner = 1 }), Response.Setperms;
		dom0, none, PathOp("/local/domain/2", Write ""), Response.Write;
		dom0, none, PathOp("/local/domain/2", Setperms { example_acl with Protocol.ACL.owner = 2 }), Response.Setperms;
		dom1, none, PathOp("/local/domain/1/data/test", Write ""), Response.Write;
                dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/1", Read), Response.Read "2";
		dom1, none, PathOp("/local/domain/1/data/test/node0", Write "node0"), Response.Write;
                dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/1", Read), Response.Read "3";
		dom2, none, PathOp("/local/domain/2/data/test", Write ""), Response.Write;
                dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/2", Read), Response.Read "2";
	];
        let tid = begin_transaction store dom1 in
	run store [
		dom1, tid, PathOp("/local/domain/1/data/test", Rm), Response.Rm;
		dom2, none, PathOp("/local/domain/2/data/test/node0", Write "node0"), Response.Write;
		dom1, tid, Transaction_end true, Response.Transaction_end;
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/1", Read), Response.Read "1";
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/2", Read), Response.Read "3";
	]

let test_quota_setperms () =
	(* Check that one connection cannot exhaust another's quota *)
        let dom0 = connect 0 in
        let dom1 = connect 1 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom0, none, PathOp("/local/domain/1", Mkdir), Response.Mkdir;
		dom0, none, PathOp("/local/domain/1", Setperms Protocol.ACL.({owner = 1; other = NONE; acl = []})), Response.Setperms;
		dom1, none, PathOp("/local/domain/1/private", Mkdir), Response.Mkdir;
                dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/1", Read), Response.Read "1";
                dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/2", Read), Response.Read "0";
		dom1, none, PathOp("/local/domain/1/private/foo", Write "hello"), Response.Write;
                dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/1", Read), Response.Read "2";
                dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/2", Read), Response.Read "0";
		(* Hand this node to domain 2 (who doesn't want it) *)
		dom1, none, PathOp("/local/domain/1/private/foo", Setperms Protocol.ACL.({owner = 2; other = NONE; acl = []})), Response.Setperms;
		(* Domain 2's quota shouldn't be affected: *)
                dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/2", Read), Response.Read "0";
	]

let test_quota_maxsize () =
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom0, none, PathOp("/tool/xenstored/quota/default/entry-length", Write "5"), Response.Write;
		dom0, none, PathOp("/a", Write "hello"), Response.Write;
		dom0, none, PathOp("/a", Write "hello2"), Response.Error "E2BIG";
		dom0, none, PathOp("/tool/xenstored/quota/default/entry-length", Write "6"), Response.Write;
		dom0, none, PathOp("/a", Write "hello2"), Response.Write;
	]

let test_quota_maxent () =
        let dom0 = connect 0 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		(* Side effect creates the quota entry *)
		dom0, none, PathOp("/first", Write "post"), Response.Write;
		dom0, none, PathOp("/tool/xenstored/quota/default/number-of-entries", Write "1"), Response.Write;
		dom0, none, PathOp("/a", Write "hello"), Response.Error "EQUOTA";
		dom0, none, PathOp("/tool/xenstored/quota/number-of-entries/0", Write "2"), Response.Write;
		dom0, none, PathOp("/a", Write "hello"), Response.Write;
		dom0, none, PathOp("/a", Write "there"), Response.Write;
		dom0, none, PathOp("/b", Write "hello"), Response.Error "EQUOTA";
	]

let test_control_perms () =
        let dom1 = connect 1 in
	let store = empty_store () in
        let open Protocol in
	let open Protocol.Request in
	run store [
		dom1, none, PathOp("/quota/default/number-of-entries", Write "1"), Response.Error "EACCES";
		dom1, none, PathOp("/tool/xenstored/log/reply-err/ENOENT", Write "1"), Response.Error "EACCES";
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
		"device_create_coalesce" >:: test_device_create_coalesce;
		"test_transactions_really_do_conflict" >:: test_transactions_really_do_conflict;
		"test_simple_watches" >:: test_simple_watches;
		"test_relative_watches" >:: test_relative_watches;
(*		"test_watches_read_perm" >:: test_watches_read_perm; *)
		"test_transaction_watches" >:: test_transaction_watches;
		"test_introduce_watches" >:: test_introduce_watches;
                "test_rm_root" >:: test_rm_root;
		"test_quota" >:: test_quota;
                "test_quota_ls" >:: test_quota_ls;
		"test_quota_transaction" >:: test_quota_transaction;
		"test_quota_setperms" >:: test_quota_setperms;
		"test_quota_maxsize" >:: test_quota_maxsize;
		"test_quota_maxent" >:: test_quota_maxent;
		"test_watch_event_quota" >:: test_watch_event_quota;
		"test_control_perms" >:: test_control_perms;
	] in
  run_test_tt ~verbose:!verbose suite
