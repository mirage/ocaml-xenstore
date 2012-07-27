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
let id x = x

let op_ids _ =
  let open Xs_packet.Op in
  for i = 0 to 100 do (* higher than the highest ID *)
    let i' = Int32.of_int i in
    match of_int32 i' with
      | None -> ()
      | Some x -> assert (to_int32 x = i')
  done

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

let test_packet_parser choose pkt () =
    let open Xs_packet in
    let p = ref (Parser.start ()) in
    let s = to_string pkt in
    let i = ref 0 in
    let finished = ref false in
    while not !finished do
      match Parser.state !p with
	| Parser.Need_more_data x ->
	  let n = choose x in
	  p := Parser.input !p (String.sub s !i n);
	  i := !i + n
	| Parser.Packet pkt' ->
	  assert(get_tid pkt = (get_tid pkt'));
	  assert(get_ty pkt = (get_ty pkt'));
	  assert(get_data pkt = (get_data pkt'));
	  assert(get_rid pkt = (get_rid pkt'));
	  finished := true
	| _ ->
	  failwith (Printf.sprintf "parser failed for %s" (pkt |> get_ty |> Op.to_string))
    done


open Lwt


let test _ =
  let t = return () in
  Lwt_main.run t

type example_packet = {
	description: string;
	packet: Xs_packet.t;
	wire_fmt: string;
}
let make_example_request description pkt_opt wire_fmt = match pkt_opt with
	| None -> failwith (Printf.sprintf "make_example_request:%s" description)
	| Some x -> {
		description = description;
		packet = x;
		wire_fmt = wire_fmt;
	}

let example_request_packets =
    let open Xs_packet.Request in [
		make_example_request "directory" (directory "/whatever/whenever" 5l)
			"\x01\x00\x00\x00\x0f\x00\x00\x00\x05\x00\x00\x00\x13\x00\x00\x00\x2f\x77\x68\x61\x74\x65\x76\x65\x72\x2f\x77\x68\x65\x6e\x65\x76\x65\x72\x00";
		make_example_request "read" (read "/a/b/c" 6l)
			"\x02\x00\x00\x00\x0e\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x2f\x61\x2f\x62\x2f\x63\x00";
		make_example_request "getperms" (getperms "/a/b" 7l)
			"\x03\x00\x00\x00\x0d\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x2f\x61\x2f\x62\x00";
		make_example_request "rm" (rm "/" 0l)
			"\x0d\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x2f\x00";
		make_example_request "setperms" (setperms "/" "someperms" 1l)
			"\x0e\x00\x00\x00\x0b\x00\x00\x00\x01\x00\x00\x00\x0c\x00\x00\x00\x2f\x00\x73\x6f\x6d\x65\x70\x65\x72\x6d\x73\x00";
		make_example_request "write" (write "/key" "value" 1l)
			"\x0b\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x2f\x6b\x65\x79\x00\x76\x61\x6c\x75\x65";
		make_example_request "mkdir" (mkdir "/" 1024l)
			"\x0c\x00\x00\x00\x09\x00\x00\x00\x00\x04\x00\x00\x02\x00\x00\x00\x2f\x00";
		make_example_request "transaction_start" (transaction_start ())
			"\x06\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00";
		make_example_request "transaction_end" (transaction_end true 1l)
			"\x07\x00\x00\x00\x07\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x54\x00";
		make_example_request "introduce" (introduce 4 5n 1)
			"\x08\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x34\x00\x35\x00\x31\x00";
		make_example_request "release" (release 2)
			"\x09\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x32\x00";
		make_example_request "resume" (resume 3)
			"\x12\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x33\x00";
		make_example_request "getdomainpath" (getdomainpath 3)
			"\x0a\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x33\x00";
		make_example_request "watch" (watch "/foo/bar" (Xs_packet.Token.of_user_string "something"))
			"\x04\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x15\x00\x00\x00\x2f\x66\x6f\x6f\x2f\x62\x61\x72\x00\x31\x3a\x73\x6f\x6d\x65\x74\x68\x69\x6e\x67\x00";
		make_example_request "unwatch" (unwatch "/foo/bar" (Xs_packet.Token.of_user_string "somethinglse"))
			"\x05\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x2f\x66\x6f\x6f\x2f\x62\x61\x72\x00\x30\x3a\x73\x6f\x6d\x65\x74\x68\x69\x6e\x67\x6c\x73\x65\x00";
		make_example_request "debug" (debug [ "a"; "b"; "something" ])
			"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x61\x00\x62\x00\x73\x6f\x6d\x65\x74\x68\x69\x6e\x67\x00"
	]


let rec ints first last =
	if first > last then [] else first :: (ints (first + 1) last)

let hexstring x =
	String.concat "" ([
		"\"";
	] @ (
		List.map (fun i -> Printf.sprintf "\\x%02x" (int_of_char x.[i])) (ints 0 (String.length x - 1))
	) @ [
		"\"";
	])

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

  let request_parsing choose =
    let f = test_packet_parser choose in
    let open Xs_packet.Request in
    "request parsing" >:::
		(List.map (fun example ->
			example.description >:: f example.packet
		) example_request_packets) in
  let request_printing =
	  "request_printing" >:::
		  (List.map (fun example ->
			  example.description >:: (fun () -> assert_equal ~msg:example.description ~printer:hexstring (Xs_packet.to_string example.packet) example.wire_fmt)
		  ) example_request_packets) in
  let suite = "xenstore" >:::
    [
      "op_ids" >:: op_ids;
      "acl_parser" >:: acl_parser;
      request_parsing id;
      request_parsing (fun _ -> 1);
	  request_printing;
      "test" >:: test;
    ] in
  run_test_tt ~verbose:!verbose suite
