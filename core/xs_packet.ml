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

let ( |> ) f g = g f
let ( ++ ) f g x = f (g x)

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

  type domid = int

  type t = {
	  owner: domid;             (** domain which "owns", has full access *)
	  other: perm;              (** default permissions for all others... *)
	  acl: (domid * perm) list; (** ... unless overridden in the ACL *)
  }

  let to_string perms =
    let char_of_perm = function
      | NONE -> 'n'
      | READ -> 'r'
      | WRITE -> 'w'
      | RDWR -> 'b' in
    let string_of_perm (id, perm) = Printf.sprintf "%c%u" (char_of_perm perm) id in
    String.concat "\000" (List.map string_of_perm ((perms.owner,perms.other) :: perms.acl))

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
	| h :: l -> Some { owner = fst h; other = snd h; acl = l }
	| [] -> Some { owner = 0; other = NONE; acl = [] }
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

(* Should we switch to an explicit stream abstraction here? *)
module type CHANNEL = sig
  type t
  val read: t -> string -> int -> int -> int Lwt.t
  val write: t -> string -> int -> int -> int Lwt.t    
end

exception Unknown_xenstore_operation of int32
exception Response_parser_failed


module PacketStream = functor(C: CHANNEL) -> struct
  open Lwt
  type stream = {
    channel: C.t;
    mutable incoming_pkt: Parser.parse; (* incrementally parses the next packet *)
    outgoing_mutex: Lwt_mutex.t;        (* held to serialise outgoing packets *)
  }

  let make t = {
    channel = t;
    incoming_pkt = Parser.start ();
    outgoing_mutex = Lwt_mutex.create ();    
  }

  (* [recv client] returns a single Packet, or fails *)
  let rec recv t =
    let open Parser in match Parser.state t.incoming_pkt with
    | Packet pkt ->
      t.incoming_pkt <- start ();
      return pkt
    | Need_more_data x ->
      let buf = String.make x '\000' in
      lwt n = C.read t.channel buf 0 x in
      if n = 0
      then raise_lwt Response_parser_failed
      else let fragment = String.sub buf 0 n in
	   t.incoming_pkt <- input t.incoming_pkt fragment;
	   recv t
    | Unknown_operation x -> raise_lwt (Unknown_xenstore_operation x)
    | Parser_failed -> raise_lwt Response_parser_failed

  (* [send client pkt] sends [pkt] and returns (), or fails *)
  let send t request =
    let req = to_string request in
    lwt n = Lwt_mutex.with_lock t.outgoing_mutex
        (fun () -> C.write t.channel req 0 (String.length req)) in
    return ()
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

module Token = struct
  type t = string

  (** [of_user_string x] takes a user-supplied watch token [x] and wraps it
      with a unique integer so we can demux watch events to the appropriate
      watcher. Note watch events are always transmitted with rid = 0 *)
  let of_user_string =
    let next = unique_id () in
    fun x -> Printf.sprintf "%ld:%s" (next ()) x

  (** [to_user_string x] returns the user-supplied part of the watch token *)
  let to_user_string x = Scanf.sscanf x "%d:%s" (fun _ x -> x)

  let to_debug_string x = x

  let of_string x = x
end

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

let set_data pkt (data: string) =
  let len = String.length data in
  let b = Buffer.create len in
  Buffer.add_string b data;
  { pkt with len = len; data = b }

module Response = struct

  let read request x = set_data request (data_concat [ x ])
  let getperms request perms = set_data request (data_concat [ ACL.to_string perms ])
  let getdomainpath request x = set_data request (data_concat [ x ])
  let transaction_start request tid = set_data request (data_concat [ Int32.to_string tid ])
  let directory request ls = set_data request (if ls = [] then "" else data_concat ls)

  let error request x = set_data request (data_concat [ x ])

  let ack request = set_data request "OK\000"

  let write = ack
  let mkdir = ack
  let rm = ack
  let setperms = ack
  let watch = ack
  let unwatch = ack
  let transaction_end = ack
end

module Request = struct

  let directory path tid =
    if is_valid_path path then Some (with_path Op.Directory tid path) else None

  let read path tid =
    if is_valid_path path then Some (with_path Op.Read tid path) else None

  let getperms path tid =
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

  let transaction_end commit tid =
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

  let write path value tid =
    let data = path ^ "\000" ^ value (* no NULL at the end *) in
    if is_valid_path path then Some(create tid (next_rid ()) Op.Write data) else None

  let mkdir path tid =
    if is_valid_path path then Some(with_path Op.Mkdir tid path) else None

  let rm path tid =
    if is_valid_path path then Some(with_path Op.Rm tid path) else None

  let setperms path perms tid =
    let data = data_concat [ path; perms ] in
    if is_valid_path path then Some(create tid (next_rid ()) Op.Setperms data) else None
end

module Unmarshal = struct
  let some x = Some x
  let int_of_string_opt x = try Some(int_of_string x) with _ -> None
  let int32_of_string_opt x = try Some(Int32.of_string x) with _ -> None
  let unit_of_string_opt x = if x = "" then Some () else None
  let ok x = if x = "OK" then Some () else None

  let string = some ++ get_data
  let list = some ++ split_string '\000' ++ get_data
  let acl = ACL.of_string ++ get_data
  let int = int_of_string_opt ++ get_data
  let int32 = int32_of_string_opt ++ get_data
  let unit = unit_of_string_opt ++ get_data
  let ok = ok ++ get_data
end

exception Enoent of string
exception Eagain
exception Invalid
exception Error of string

let response hint sent received f = match get_ty sent, get_ty received with
  | _, Op.Error ->
    begin match get_data received with
      | "ENOENT" -> raise (Enoent hint)
      | "EAGAIN" -> raise Eagain
      | "EINVAL" -> raise Invalid
      | s -> raise (Error s)
    end
  | x, y when x = y ->
    begin match f received with
      | None -> raise (Error (Printf.sprintf "failed to parse response (hint:%s) (payload:%s)" hint (get_data received)))
      | Some z -> z
    end
  | x, y ->
    raise (Error (Printf.sprintf "unexpected packet: expected %s; got %s" (Op.to_string x) (Op.to_string y)))

