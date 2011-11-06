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
    | Resume | Set_target

(* The index of the value in the array is the integer representation used
   by the wire protocol. Every element of t exists exactly once in the array. *)
let on_the_wire =
  [| Debug; Directory; Read; Getperms;
     Watch; Unwatch; Transaction_start;
     Transaction_end; Introduce; Release;
     Getdomainpath; Write; Mkdir; Rm;
     Setperms; Watchevent; Error; Isintroduced;
     Resume; Set_target |]

let of_int32 i =
  let i = Int32.to_int i in
  if i >= 0 && i < Array.length on_the_wire then Some (on_the_wire.(i)) else None

let to_int32 x =
  match snd (Array.fold_left
    (fun (idx, result) v -> if x = v then (idx + 1, Some idx) else (idx + 1, result))
    (0, None) on_the_wire) with
    | None -> assert false (* impossible since on_the_wire contains each element *)
    | Some i -> Int32.of_int i

let to_string = function
  | Debug             -> "DEBUG"
  | Directory         -> "DIRECTORY"
  | Read              -> "READ"
  | Getperms          -> "GET_PERMS"
  | Watch             -> "WATCH"
  | Unwatch           -> "UNWATCH"
  | Transaction_start -> "TRANSACTION_START"
  | Transaction_end   -> "TRANSACTION_END"
  | Introduce         -> "INTRODUCE"
  | Release           -> "RELEASE"
  | Getdomainpath     -> "GET_DOMAIN_PATH"
  | Write             -> "WRITE"
  | Mkdir             -> "MKDIR"
  | Rm                -> "RM"
  | Setperms          -> "SET_PERMS"
  | Watchevent        -> "WATCH_EVENT"
  | Error             -> "ERROR"
  | Isintroduced      -> "IS_INTRODUCED"
  | Resume            -> "RESUME"
  | Set_target        -> "SET_TARGET"
end

let rec split_string ?limit:(limit=(-1)) c s =
  let i = try String.index s c with Not_found -> -1 in
  let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
  if i = -1 || nlimit = 0 then
    [ s ]
  else
    let a = String.sub s 0 i
    and b = String.sub s (i + 1) (String.length s - i - 1) in
    a :: (split_string ~limit: nlimit c b)

module ACL = struct
  type perm =
    | NONE
    | READ
    | WRITE
    | RDWR

  type t = int * perm * (int * perm) list

  let to_string perms =
    let owner, other, acl = perms in
    let char_of_perm = function
      | NONE -> 'n'
      | READ -> 'r'
      | WRITE -> 'w'
      | RDWR -> 'b' in
    let string_of_perm (id, perm) = Printf.sprintf "%c%u" (char_of_perm perm) id in
    String.concat "\000" (List.map string_of_perm ((owner,other) :: acl))

  let of_string s =
    let perm_of_char = function
      | 'n' -> NONE
      | 'r' -> READ
      | 'w' -> WRITE
      | 'b' -> RDWR
      | c -> invalid_arg (Printf.sprintf "Unknown permission: '%c'" c) in
    (* A perm is stored as '<c>domid' *)
    let perm_of_string s =
      if String.length s < 2
      then invalid_arg (Printf.sprintf "Permission string too short: '%s'" s);
      int_of_string (String.sub s 1 (String.length s - 1)), perm_of_char s.[0] in
    try
      let l = List.map perm_of_string (split_string '\000' s) in
      match l with
	| h :: l -> Some (fst h, snd h, l)
	| [] -> Some (0, NONE, [])
    with e ->
      None
end

type t = {
  tid: int32;
  rid: int32;
  ty: Op.t;
  len: int;
  data: Buffer.t;
}

let to_string pkt =
  let len = Int32.of_int (Buffer.length pkt.data) in
  let ty = Op.to_int32 pkt.ty in
  let header = BITSTRING {
    ty: 32: littleendian;
    pkt.rid: 32: littleendian;
    pkt.tid: 32: littleendian;
    len: 32: littleendian
  } in
  Bitstring.string_of_bitstring header ^ (Buffer.contents pkt.data)

let get_tid pkt = pkt.tid
let get_ty pkt = pkt.ty
let get_data pkt =
  if pkt.len > 0 && Buffer.nth pkt.data (pkt.len - 1) = '\000' then
    Buffer.sub pkt.data 0 (pkt.len - 1)
  else
    Buffer.contents pkt.data
let get_rid pkt = pkt.rid

module Parser = struct
  (** Incrementally parse packets *)

  let header_size = 16

  type state =
    | Unknown_operation of int32
    | Parser_failed
    | Need_more_data of int
    | Packet of t

  type parse =
    | ReadingHeader of int * string
    | ReadingBody of t
    | Finished of state

  let start () = ReadingHeader (0, String.make header_size '\000')

  let state = function
    | ReadingHeader(got_already, _) -> Need_more_data (header_size - got_already)
    | ReadingBody pkt -> Need_more_data (pkt.len - (Buffer.length pkt.data))
    | Finished r -> r

  let parse_header str =
    bitmatch (Bitstring.bitstring_of_string str) with
      | { ty: 32: littleendian;
	  rid: 32: littleendian;
	  tid: 32: littleendian;
	  len: 32: littleendian } ->
	let len = Int32.to_int len in
	      (* TODO: detect anamalous 'len' values and abort early *)
	begin match Op.of_int32 ty with
	  | Some ty ->
	    ReadingBody {
	      tid = tid;
	      rid = rid;
	      ty = ty;
	      len = len;
	      data = Buffer.create len;
	    }
	  | None -> Finished (Unknown_operation ty)
	end
      | { _ } -> Finished Parser_failed

  let input state bytes =
    match state with
      | ReadingHeader(got_already, str) ->
	String.blit bytes 0 str got_already (String.length bytes);
	let got_already = got_already + (String.length bytes) in
	if got_already < header_size
	then ReadingHeader(got_already, str)
	else parse_header str
      | ReadingBody x ->
	Buffer.add_string x.data bytes;
	let needed = x.len - (Buffer.length x.data) in
	if needed > 0
	then ReadingBody x
	else Finished (Packet x)
      | Finished f -> Finished f
end

let unique_id () =
    let last = ref 0l in
    fun () ->
        let result = !last in
	last := Int32.succ !last;
        result

(** Check paths are suitable for read/write/mkdir/rm/directory etc (NOT watches)  *)
let is_valid_path path =
  (* Paths shouldn't have a "//" in the middle *)
  let result = ref true in
  let bad = "//" in
  for offset = 0 to String.length path - (String.length bad) do
    if String.sub path offset (String.length bad) = bad then result := false
  done;
  (* Paths shouldn't have a "/" at the end, except for the root *)
  if path <> "/" && path <> "" && path.[String.length path - 1] = '/'
  then result := false;
  !result

(** Check to see if a path is suitable for watches *)
let is_valid_watch_path path =
  (* Check for stuff like @releaseDomain etc first *)
  (path <> "" && path.[0] = '@') || (is_valid_path path)

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

let create tid rid ty data =
  let len = String.length data in
  let b = Buffer.create len in
  Buffer.add_string b data;
  {
    tid = tid;
    rid = rid;
    ty = ty;
    len = len;
    data = b;
  }

let with_path ty (tid: int32) (path: string) =
	let data = data_concat [ path; ] in
	create tid (next_rid ()) ty data

(* operations *)
let directory tid path =
  if is_valid_path path then Some (with_path Op.Directory tid path) else None

let read tid path =
  if is_valid_path path then Some (with_path Op.Read tid path) else None

let getperms tid path =
  if is_valid_path path then Some (with_path Op.Getperms tid path) else None

let debug commands =
  Some(create 0l (next_rid ()) Op.Debug (data_concat commands))

let watch path data =
  if is_valid_watch_path path
  then Some (
    let data = data_concat [ path; data; ] in
    create 0l (next_rid ()) Op.Watch data
  ) else None

let unwatch path data =
  if is_valid_watch_path path
  then Some (
    let data = data_concat [ path; data; ] in
    create 0l (next_rid ()) Op.Unwatch data
  ) else None

let transaction_start () =
  Some(create 0l (next_rid ()) Op.Transaction_start (data_concat []))

let transaction_end tid commit =
  let data = data_concat [ (if commit then "T" else "F"); ] in
  Some(create tid (next_rid ()) Op.Transaction_end data)

let introduce domid mfn port =
  let data = data_concat [ Printf.sprintf "%u" domid;
	                   Printf.sprintf "%nu" mfn;
	                   string_of_int port; ] in
  Some(create 0l (next_rid ()) Op.Introduce data)

let release domid =
  let data = data_concat [ Printf.sprintf "%u" domid; ] in
  Some(create 0l (next_rid ()) Op.Release data)

let resume domid =
  let data = data_concat [ Printf.sprintf "%u" domid; ] in
  Some(create 0l (next_rid ()) Op.Resume data)

let getdomainpath domid =
  let data = data_concat [ Printf.sprintf "%u" domid; ] in
  Some(create 0l (next_rid ()) Op.Getdomainpath data)

let write tid path value =
  let data = path ^ "\000" ^ value (* no NULL at the end *) in
  if is_valid_path path then Some(create tid (next_rid ()) Op.Write data) else None

let mkdir tid path =
  if is_valid_path path then Some(with_path Op.Mkdir tid path) else None
let rm tid path =
  if is_valid_path path then Some(with_path Op.Rm tid path) else None

let setperms tid path perms =
  let data = data_concat [ path; perms ] in
  if is_valid_path path then Some(create tid (next_rid ()) Op.Setperms data) else None
