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

let test_packet_parser choose pkt () = match pkt with
  | None -> failwith "Failed to generate packet"
  | Some pkt ->
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

  let packet_parsing choose =
    let f = test_packet_parser choose in
    let open Xs_packet.Request in
    "packet parsing" >:::
      [
	"parse_directory" >:: f (directory "/whatever/whenever" 5l);
	"parse_read" >:: f (read "/a/b/c" 6l);
	"getperms" >:: f (getperms "/a/b" 7l);
	"rm" >:: f (rm "/" 0l);
	"setperms" >:: f (setperms "/" "someperms" 1l);
	"write" >:: f (write "/key" "value" 1l);
	"mkdir" >:: f (mkdir "/" 1024l);
	"transaction_start" >:: f (transaction_start ());
	"transaction_end" >:: f (transaction_end true 1l);
	"introduce" >:: f (introduce 4 5n 1);
	"release" >:: f (release 2);
	"resume" >:: f (resume 3);
	"getdomainpath" >:: f (getdomainpath 3);
	"watch" >:: f (watch "/foo/bar" (Xs_packet.Token.of_user_string "something"));
	"unwatch" >:: f (watch "/foo/bar" (Xs_packet.Token.of_user_string "something"));
	"debug" >:: f (debug ["a"; "b"; "something" ]);
      ] in
  let suite = "xenstore" >:::
    [
      "op_ids" >:: op_ids;
      "acl_parser" >:: acl_parser;
      packet_parsing id;
      packet_parsing (fun _ -> 1);
      "test" >:: test;
    ] in
  run_test_tt ~verbose:!verbose suite
