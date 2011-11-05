(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
open Pervasives

module Op = struct
  type t =
    | Debug | Directory | Read | Getperms
    | Watch | Unwatch | Transaction_start
    | Transaction_end | Introduce | Release
    | Getdomainpath | Write | Mkdir | Rm
    | Setperms | Watchevent | Error | Isintroduced
    | Resume | Set_target | Restrict

(* There are two sets of XB operations: the one coming from open-source and *)
(* the one coming from our private patch queue. These operations            *)
(* in two differents arrays for make easier the forward compatibility       *)
let operation_c_mapping =
	[| Debug; Directory; Read; Getperms;
           Watch; Unwatch; Transaction_start;
           Transaction_end; Introduce; Release;
           Getdomainpath; Write; Mkdir; Rm;
           Setperms; Watchevent; Error; Isintroduced;
           Resume; Set_target |]
let size = Array.length operation_c_mapping

(* [offset_pq] has to be the same as in <xen/io/xs_wire.h> *)
external get_internal_offset: unit -> int = "stub_get_internal_offset"
let offset_pq = get_internal_offset ()

let operation_c_mapping_pq =
	[| Restrict |]
let size_pq = Array.length operation_c_mapping_pq

let array_search el a =
	let len = Array.length a in
	let rec search i =
		if i > len then raise Not_found;
		if a.(i) = el then i else search (i + 1) in
	search 0

let of_cval i =
  let i = Int32.to_int i in
	if i >= 0 && i < size
	then operation_c_mapping.(i)
	else if i >= offset_pq && i < offset_pq + size_pq
	then operation_c_mapping_pq.(i-offset_pq)
	else raise Not_found

let to_cval op =
  let i =
	try
	array_search op operation_c_mapping
	with _ -> offset_pq + array_search op operation_c_mapping_pq in
  Int32.of_int i

let to_string ty =
	match ty with
	| Debug			-> "DEBUG"
	| Directory		-> "DIRECTORY"
	| Read			-> "READ"
	| Getperms		-> "GET_PERMS"
	| Watch			-> "WATCH"
	| Unwatch		-> "UNWATCH"
	| Transaction_start	-> "TRANSACTION_START"
	| Transaction_end	-> "TRANSACTION_END"
	| Introduce		-> "INTRODUCE"
	| Release		-> "RELEASE"
	| Getdomainpath		-> "GET_DOMAIN_PATH"
	| Write			-> "WRITE"
	| Mkdir			-> "MKDIR"
	| Rm			-> "RM"
	| Setperms		-> "SET_PERMS"
	| Watchevent		-> "WATCH_EVENT"
	| Error			-> "ERROR"
	| Isintroduced		-> "IS_INTRODUCED"
	| Resume		-> "RESUME"
	| Set_target		-> "SET_TARGET"
	| Restrict		-> "RESTRICT"
end

module Partial = struct
type pkt =
{
	tid: int32;
	rid: int32;
	ty: Op.t;
	len: int;
	buf: Buffer.t;
}

let header_size = 16

type buf = HaveHdr of pkt | NoHdr of int * string

let empty () = NoHdr (header_size, String.make header_size '\000')

let header_of_string x =
  let bits = Bitstring.bitstring_of_string x in
  bitmatch bits with
    | { ty: 32: littleendian;
	req_id: 32: littleendian;
	tx_id: 32: littleendian;
	len: 32: littleendian } ->
      tx_id, req_id, ty, Int32.to_int len
    | { _ } -> failwith "Failed to parse header"

let of_string s =
	let tid, rid, opint, dlen = header_of_string s in
	{
		tid = tid;
		rid = rid;
		ty = (Op.of_cval opint);
		len = dlen;
		buf = Buffer.create dlen;
	}

let append pkt s sz =
	Buffer.add_string pkt.buf (String.sub s 0 sz)

let to_complete pkt =
	pkt.len - (Buffer.length pkt.buf)

end



type t =
{
	tid: int32;
	rid: int32;
	ty: Op.t;
	data: string;
}

exception Error of string
exception DataError of string

let string_of_header tid rid ty len =
  let len = Int32.of_int len in
  let bs = BITSTRING {
    ty: 32: littleendian;
    rid: 32: littleendian;
    tid: 32: littleendian;
    len: 32: littleendian
  } in
  Bitstring.string_of_bitstring bs

let create tid rid ty data = { tid = tid; rid = rid; ty = ty; data = data; }

let of_partialpkt ppkt =
	create ppkt.Partial.tid ppkt.Partial.rid ppkt.Partial.ty (Buffer.contents ppkt.Partial.buf)

let to_string pkt =
	let header = string_of_header pkt.tid pkt.rid (Op.to_cval pkt.ty) (String.length pkt.data) in
	header ^ pkt.data

let unpack pkt =
	pkt.tid, pkt.rid, pkt.ty, pkt.data

let get_tid pkt = pkt.tid
let get_ty pkt = pkt.ty
let get_data pkt =
	let l = String.length pkt.data in
	if l > 0 && pkt.data.[l - 1] = '\000' then
		String.sub pkt.data 0 (l - 1)
	else
		pkt.data
let get_rid pkt = pkt.rid

let unique_id () =
    let last = ref 0l in
    fun () ->
        let result = !last in
	last := Int32.succ !last;
        result

(** [next_rid ()] returns a fresh request id, used to associate replies
    with responses. *)
let next_rid = unique_id ()

type token = string

(** [create_token x] takes a user-supplied watch token [x] and wraps it
    with a unique integer so we can demux watch events to the appropriate
    watcher. Note watch events are always transmitted with rid = 0 *)
let create_token =
    let next = unique_id () in
    fun x -> Printf.sprintf "%ld:%s" (next ()) x

(** [user_string_of_token x] returns the user-supplied part of the watch token *)
let user_string_of_token x = Scanf.sscanf x "%d:%s" (fun _ x -> x)

let token_to_string x = x

let parse_token x = x

let data_concat ls = (String.concat "\000" ls) ^ "\000"
let with_path ty (tid: int32) (path: string) =
	let data = data_concat [ path; ] in
	create tid (next_rid ()) ty data

(* operations *)
let directory tid path = with_path Op.Directory tid path
let read tid path = with_path Op.Read tid path

let getperms tid path = with_path Op.Getperms tid path

let debug commands =
	create 0l (next_rid ()) Op.Debug (data_concat commands)

let watch path data =
	let data = data_concat [ path; data; ] in
	create 0l (next_rid ()) Op.Watch data

let unwatch path data =
	let data = data_concat [ path; data; ] in
	create 0l (next_rid ()) Op.Unwatch data

let transaction_start () =
	create 0l (next_rid ()) Op.Transaction_start (data_concat [])

let transaction_end tid commit =
	let data = data_concat [ (if commit then "T" else "F"); ] in
	create tid (next_rid ()) Op.Transaction_end data

let introduce domid mfn port =
	let data = data_concat [ Printf.sprintf "%u" domid;
	                         Printf.sprintf "%nu" mfn;
	                         string_of_int port; ] in
	create 0l (next_rid ()) Op.Introduce data

let release domid =
	let data = data_concat [ Printf.sprintf "%u" domid; ] in
	create 0l (next_rid ()) Op.Release data

let resume domid =
	let data = data_concat [ Printf.sprintf "%u" domid; ] in
	create 0l (next_rid ()) Op.Resume data

let getdomainpath domid =
	let data = data_concat [ Printf.sprintf "%u" domid; ] in
	create 0l (next_rid ()) Op.Getdomainpath data

let write tid path value =
	let data = path ^ "\000" ^ value (* no NULL at the end *) in
	create tid (next_rid ()) Op.Write data

let mkdir tid path = with_path Op.Mkdir tid path
let rm tid path = with_path Op.Rm tid path

let setperms tid path perms =
	let data = data_concat [ path; perms ] in
	create tid (next_rid ()) Op.Setperms data
