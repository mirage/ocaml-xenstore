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
open Sexplib
open OUnit

let ( |> ) a b = b a
let id x = x

let failure_on_error = function
  | `Ok x -> x
  | `Error x -> failwith x

let unbox = function
  | None -> failwith "unbox"
  | Some x -> x

let op_ids _ =
  let open Protocol.Op in
  for i = 0 to 100 do (* higher than the highest ID *)
    let i' = Int32.of_int i in
    match of_int32 i' with
    | `Error _ -> ()
    | `Ok x -> assert (to_int32 x = i')
  done

let example_acl =
  let open Protocol.ACL in
  { owner = 5; other = READ; acl = [ 2, WRITE; 3, RDWR ] }

let acl_parser _ =
  let open Protocol.ACL in
  let ts = [
    { owner = 5; other = READ; acl = [ 2, WRITE; 3, RDWR ] };
    { owner = 1; other = WRITE; acl = [] };
  ] in
  let buf = Cstruct.create 1024 in
  List.iter
    (fun t ->
       let next = marshal t buf in
       let data = Cstruct.sub buf 0 next.Cstruct.off in
       let t' = failure_on_error (unmarshal data) in
       let printer x = Sexp.to_string_hum (sexp_of_t x) in
       assert_equal ~msg:"acl" ~printer t t'
    ) ts

open Lwt


let test _ =
  let t = return () in
  Lwt_main.run t

let cstruct_of_string x =
  let c = Cstruct.create (String.length x) in
  Cstruct.blit_from_string x 0 c 0 (Cstruct.len c);
  c

module Example_request_packet = struct
  type t = {
    op: Protocol.Op.t;
    tid: int32;
    request: Protocol.Request.t;
    expected: string;
  }

  let test_parse t () =
    let buf = cstruct_of_string t.expected in
    let hdr = failure_on_error (Protocol.Header.unmarshal buf) in
    let payload = Cstruct.shift buf Protocol.Header.sizeof in
    assert_equal ~printer:(fun x -> Sexp.to_string (Protocol.Op.sexp_of_t x)) t.op hdr.Protocol.Header.ty;
    assert_equal ~printer:Int32.to_string t.tid hdr.Protocol.Header.tid;
    let request = failure_on_error (Protocol.Request.unmarshal hdr payload) in
    assert_equal ~printer:(fun x -> Sexp.to_string (Protocol.Request.sexp_of_t x)) t.request request

  let test_print t () =
    let buf = Cstruct.create (Protocol.xenstore_payload_max + Protocol.Header.sizeof) in
    let payload = Cstruct.shift buf Protocol.Header.sizeof in
    let next = Protocol.Request.marshal t.request payload in
    let len = next.Cstruct.off - Protocol.Header.sizeof in
    let hdr = { Protocol.Header.tid = t.tid; rid = 0l; ty = t.op; len } in
    ignore(Protocol.Header.marshal hdr buf);
    let all = Cstruct.sub buf 0 (Protocol.Header.sizeof + len) in
    let txt = Cstruct.to_string all in
    assert_equal ~printer:String.escaped t.expected txt
end

let make_example_request op request tid expected =
  { Example_request_packet.op; tid; request; expected }

(* Test that we can parse unexpected packets the same way as the
   previous oxenstored version *)
let unexpected_request_packets =
  let open Protocol in
  let open Protocol.Request in [
    (* client sends a single NULL as the argument to Getdomaimpath:
       assume they meant 0 *)
    make_example_request Op.Getdomainpath (Getdomainpath 0) 0l
      "\n\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000";
  ]

let example_request_packets =
  let open Protocol in
  let open Protocol.Request in [
    make_example_request Op.Directory (PathOp("/whatever/whenever", Directory)) 5l
      "\x01\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x13\x00\x00\x00\x2f\x77\x68\x61\x74\x65\x76\x65\x72\x2f\x77\x68\x65\x6e\x65\x76\x65\x72\x00";
    make_example_request Op.Read (PathOp("/a/b/c", Read)) 6l
      "\x02\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x2f\x61\x2f\x62\x2f\x63\x00";
    make_example_request Op.Getperms (PathOp("/a/b", Getperms)) 7l
      "\x03\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x2f\x61\x2f\x62\x00";
    make_example_request Op.Rm (PathOp("/", Rm)) 0l
      "\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x2f\x00";
    make_example_request Op.Setperms (PathOp("/", Setperms example_acl)) 1l
      "\x0e\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x2f\x00\x72\x35\x00\x77\x32\x00\x62\x33\x00";
    make_example_request Op.Write (PathOp("/key", Write "value")) 1l
      "\x0b\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x2f\x6b\x65\x79\x00\x76\x61\x6c\x75\x65";
    make_example_request Op.Mkdir (PathOp("/", Mkdir)) 1024l
      "\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x02\x00\x00\x00\x2f\x00";
    make_example_request Op.Transaction_start Transaction_start 0l
      "\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00";
    make_example_request Op.Transaction_end (Transaction_end true) 1l
      "\x07\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x54\x00";
    make_example_request Op.Introduce (Introduce(4, 5n, 1)) 0l
      "\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x34\x00\x35\x00\x31\x00";
    make_example_request Op.Release (Release 2) 0l
      "\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x32\x00";
    make_example_request Op.Resume (Resume 3) 0l
      "\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x33\x00";
    make_example_request Op.Getdomainpath (Getdomainpath 3) 0l
      "\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x33\x00";
    make_example_request Op.Watch (Watch("/foo/bar", (Protocol.Token.(marshal(unmarshal "something"))))) 0l
      "\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x00\x00\x2f\x66\x6f\x6f\x2f\x62\x61\x72\x00\x73\x6f\x6d\x65\x74\x68\x69\x6e\x67\x00";
    make_example_request Op.Unwatch (Unwatch("/foo/bar", (Protocol.Token.(marshal(unmarshal "somethinglse"))))) 0l
      "\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x2f\x66\x6f\x6f\x2f\x62\x61\x72\x00\x73\x6f\x6d\x65\x74\x68\x69\x6e\x67\x6c\x73\x65\x00";
    make_example_request Op.Debug (Debug [ "a"; "b"; "something" ]) 0l
      "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x61\x00\x62\x00\x73\x6f\x6d\x65\x74\x68\x69\x6e\x67\x00"
  ]

module Example_response_packet = struct
  type t = {
    op: Protocol.Op.t;
    tid: int32;
    rid: int32;
    response: Protocol.Response.t;
    expected: string;
  }
  let test_parse t () =
    let buf = cstruct_of_string t.expected in
    let hdr = failure_on_error (Protocol.Header.unmarshal buf) in
    let payload = Cstruct.shift buf Protocol.Header.sizeof in
    assert_equal ~printer:(fun x -> Sexp.to_string (Protocol.Op.sexp_of_t x)) t.op hdr.Protocol.Header.ty;
    assert_equal ~printer:Int32.to_string t.tid hdr.Protocol.Header.tid;
    let response = failure_on_error (Protocol.Response.unmarshal hdr payload) in
    assert_equal ~printer:(fun x -> Sexp.to_string (Protocol.Response.sexp_of_t x)) t.response response

  let test_print t () =
    let buf = Cstruct.create (Protocol.xenstore_payload_max + Protocol.Header.sizeof) in
    let payload = Cstruct.shift buf Protocol.Header.sizeof in
    let next = Protocol.Response.marshal t.response payload in
    let len = next.Cstruct.off - Protocol.Header.sizeof in
    let hdr = { Protocol.Header.tid = t.tid; rid = t.rid; ty = t.op; len } in
    ignore(Protocol.Header.marshal hdr buf);
    let all = Cstruct.sub buf 0 (Protocol.Header.sizeof + len) in
    let txt = Cstruct.to_string all in
    assert_equal ~printer:String.escaped t.expected txt
end

let make_example_response op response expected =
  let request = List.find (fun x -> x.Example_request_packet.op = op) example_request_packets in
  let open Protocol in
  let buf = Cstruct.create Header.sizeof in
  Cstruct.blit_from_string request.Example_request_packet.expected 0 buf 0 Header.sizeof;
  let hdr = failure_on_error (Header.unmarshal buf) in
  { Example_response_packet.op; tid = hdr.Header.tid; rid = hdr.Header.rid; response; expected }

(* We use the example requests to generate example responses *)
let example_response_packets =
  let open Protocol in
  let open Protocol.Response in [
    make_example_response Op.Read (Read "theresult")
      "\x02\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x09\x00\x00\x00\x74\x68\x65\x72\x65\x73\x75\x6c\x74";
    make_example_response Op.Read (Read "")
      "\x02\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00";
    make_example_response Op.Getperms (Getperms (Protocol.ACL.( { owner = 2; other = READ; acl = [ 4, NONE ] } )))
      "\x03\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x06\x00\x00\x00\x72\x32\x00\x6e\x34\x00";
    make_example_response Op.Getdomainpath (Getdomainpath "/local/domain/4")
      "\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x2f\x6c\x6f\x63\x61\x6c\x2f\x64\x6f\x6d\x61\x69\x6e\x2f\x34\x00";
    make_example_response Op.Transaction_start (Transaction_start 3l)
      "\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x33\x00";
    make_example_response Op.Directory (Directory [ "a"; "b"; "c"; "aseasyas"; "1"; "2"; "3" ])
      "\x01\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x15\x00\x00\x00\x61\x00\x62\x00\x63\x00\x61\x73\x65\x61\x73\x79\x61\x73\x00\x31\x00\x32\x00\x33\x00";
    make_example_response Op.Write Write
      "\x0b\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x4f\x4b\x00";
    make_example_response Op.Mkdir Mkdir
      "\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x03\x00\x00\x00\x4f\x4b\x00";
    make_example_response Op.Rm Rm
      "\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x4f\x4b\x00";
    make_example_response Op.Setperms Setperms
      "\x0e\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x4f\x4b\x00";
    make_example_response Op.Watch Watch
      "\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x4f\x4b\x00";
    make_example_response Op.Unwatch Unwatch
      "\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x4f\x4b\x00";
    make_example_response Op.Transaction_end Transaction_end
      "\x07\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x4f\x4b\x00";
    {
      Example_response_packet.tid = 0x2l;
      rid = 10l;
      op = Op.Error;
      response = Protocol.Response.Error "whatyoutalkingabout";
      expected = 
        "\x10\x00\x00\x00\n\x00\x00\x00\x02\x00\x00\x00\x14\x00\x00\x00\x77\x68\x61\x74\x79\x6f\x75\x74\x61\x6c\x6b\x69\x6e\x67\x61\x62\x6f\x75\x74\x00"
    }
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

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test xenstore protocol code";

  let request_parsing =
    "request_parsing" >:::
    (List.map (fun t ->
         let description = Sexp.to_string (Protocol.Request.sexp_of_t t.Example_request_packet.request) in
         description >:: Example_request_packet.test_parse t
       ) (unexpected_request_packets @ example_request_packets)) in

  let response_parsing =
    "response_parsing" >:::
    (List.map (fun t ->
         let description = Sexp.to_string (Protocol.Response.sexp_of_t t.Example_response_packet.response) in
         description >:: Example_response_packet.test_parse t
       ) example_response_packets) in

  let request_printing =
    "request_printing" >:::
    (List.map (fun t ->
         let description = Sexp.to_string (Protocol.Request.sexp_of_t t.Example_request_packet.request) in
         description >:: Example_request_packet.test_print t
       ) example_request_packets) in

  let response_printing =
    "response_printing" >:::
    (List.map (fun t ->
         let description = Sexp.to_string (Protocol.Response.sexp_of_t t.Example_response_packet.response) in
         description >:: Example_response_packet.test_print t
       ) example_response_packets) in

  let suite = "xenstore" >:::
              [
                "op_ids" >:: op_ids;
                "acl_parser" >:: acl_parser;
                request_parsing;
                response_parsing;
                request_printing;
                response_printing;
                "test" >:: test;
              ] in
  run_test_tt ~verbose:!verbose suite
