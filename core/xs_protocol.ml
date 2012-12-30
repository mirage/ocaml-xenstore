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
  | Restrict

(* The index of the value in the array is the integer representation used
   by the wire protocol. Every element of t exists exactly once in the array. *)
let on_the_wire =
  [| Debug; Directory; Read; Getperms;
     Watch; Unwatch; Transaction_start;
     Transaction_end; Introduce; Release;
     Getdomainpath; Write; Mkdir; Rm;
     Setperms; Watchevent; Error; Isintroduced;
     Resume; Set_target;
   Restrict
  |]

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
  | Debug             -> "debug"
  | Directory         -> "directory"
  | Read              -> "read"
  | Getperms          -> "getperms"
  | Watch             -> "watch"
  | Unwatch           -> "unwatch"
  | Transaction_start -> "transaction_start"
  | Transaction_end   -> "transaction_end"
  | Introduce         -> "introduce"
  | Release           -> "release"
  | Getdomainpath     -> "getdomainpath"
  | Write             -> "write"
  | Mkdir             -> "mkdir"
  | Rm                -> "rm"
  | Setperms          -> "setperms"
  | Watchevent        -> "watchevent"
  | Error             -> "error"
  | Isintroduced      -> "isintroduced"
  | Resume            -> "resume"
  | Set_target        -> "set_target"
  | Restrict          -> "restrict"
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

  let char_of_perm = function
    | READ -> 'r'
    | WRITE -> 'w'
    | RDWR -> 'b'
    | NONE -> 'n'

  let perm_of_char = function
    | 'r' -> Some READ
    | 'w' -> Some WRITE
    | 'b' -> Some RDWR
    | 'n' -> Some NONE
    | _ -> None

  type domid = int

  type t = {
    owner: domid;             (** domain which "owns", has full access *)
    other: perm;              (** default permissions for all others... *)
    acl: (domid * perm) list; (** ... unless overridden in the ACL *)
  }

  let to_string perms =
    let string_of_perm (id, perm) = Printf.sprintf "%c%u" (char_of_perm perm) id in
    String.concat "\000" (List.map string_of_perm ((perms.owner,perms.other) :: perms.acl))

  let of_string s =
      (* A perm is stored as '<c>domid' *)
    let perm_of_char_exn x = match (perm_of_char x) with Some y -> y | None -> raise Not_found in
    try
      let perm_of_string s =
        if String.length s < 2
        then invalid_arg (Printf.sprintf "Permission string too short: '%s'" s);
        int_of_string (String.sub s 1 (String.length s - 1)), perm_of_char_exn s.[0] in
      let l = List.map perm_of_string (split_string '\000' s) in
      match l with
        | (owner, other) :: l -> Some { owner = owner; other = other; acl = l }
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

cstruct header {
  uint32_t ty;
  uint32_t rid;
  uint32_t tid;
  uint32_t len
} as little_endian

let to_string pkt =
  let header = Cstruct.create sizeof_header in
  let len = Int32.of_int (Buffer.length pkt.data) in
  let ty = Op.to_int32 pkt.ty in
  set_header_ty header ty;
  set_header_rid header pkt.rid;
  set_header_tid header pkt.tid;
  set_header_len header len;
  Cstruct.to_string header ^ (Buffer.contents pkt.data)

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
    | Parser_failed of string
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
    let header = Cstruct.create sizeof_header in
    Cstruct.blit_from_string str 0 header 0 sizeof_header;
    let ty = get_header_ty header in
    let rid = get_header_rid header in
    let tid = get_header_tid header in
    let len = get_header_len header in

    let len = Int32.to_int len in
    (* TODO: detect anamalous 'len' values and abort early *)
    begin match Op.of_int32 ty with
    | Some ty ->
      let t = {
        tid = tid;
        rid = rid;
        ty = ty;
        len = len;
        data = Buffer.create len;
        } in
      if len = 0
      then Finished (Packet t)
      else ReadingBody t
    | None -> Finished (Unknown_operation ty)
    end

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
module type IO = sig
  type 'a t
  val return: 'a -> 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t

  type channel
  val read: channel -> string -> int -> int -> int t
  val write: channel -> string -> int -> int -> unit t
end

exception Unknown_xenstore_operation of int32
exception Response_parser_failed of string
exception EOF

type ('a, 'b) result =
	| Ok of 'a
	| Exception of 'b

module PacketStream = functor(IO: IO) -> struct
  let ( >>= ) = IO.( >>= )
  let return = IO.return

  type stream = {
    channel: IO.channel;
    mutable incoming_pkt: Parser.parse; (* incrementally parses the next packet *)
  }

  let make t = {
    channel = t;
    incoming_pkt = Parser.start ();
  }

  (* [recv client] returns a single Packet, or fails *)
  let rec recv t =
    let open Parser in match Parser.state t.incoming_pkt with
    | Packet pkt ->
      t.incoming_pkt <- start ();
      return (Ok pkt)
    | Need_more_data x ->
      let buf = String.make x '\000' in
      IO.read t.channel buf 0 x
      >>= (function
      | 0 -> return (Exception EOF)
      | n ->
        let fragment = String.sub buf 0 n in
	    t.incoming_pkt <- input t.incoming_pkt fragment;
	    recv t)
    | Unknown_operation x -> return (Exception (Unknown_xenstore_operation x))
    | Parser_failed x -> return (Exception (Response_parser_failed x))

  (* [send client pkt] sends [pkt] and returns (), or fails *)
  let send t request =
    let req = to_string request in
	IO.write t.channel req 0 (String.length req)
end

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

module Token = struct
  type t = string

  (** [to_user_string x] returns the user-supplied part of the watch token *)
  let to_user_string x = Scanf.sscanf x "%d:%s" (fun _ x -> x)

  let to_debug_string x = x

  let of_string x = x
  let to_string x = x
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

let set_data pkt (data: string) =
  let len = String.length data in
  let b = Buffer.create len in
  Buffer.add_string b data;
  { pkt with len = len; data = b }

module Response = struct

  type payload =
  | Read of string
  | Directory of string list
  | Getperms of ACL.t
  | Getdomainpath of string
  | Transaction_start of int32
  | Write
  | Mkdir
  | Rm
  | Setperms
  | Watch
  | Unwatch
  | Transaction_end
  | Debug of string list
  | Introduce
  | Resume
  | Release
  | Set_target
  | Restrict
  | Isintroduced of bool
  | Error of string
  | Watchevent of string * string

  let prettyprint_payload =
    let open Printf in function
      | Read x -> sprintf "Read %s" x
      | Directory xs -> sprintf "Directory [ %s ]" (String.concat "; " xs)
      | Getperms acl -> sprintf "Getperms %s" (ACL.to_string acl)
      | Getdomainpath p -> sprintf "Getdomainpath %s" p
      | Transaction_start x -> sprintf "Transaction_start %ld" x
      | Write -> "Write"
      | Mkdir -> "Mkdir"
      | Rm -> "Rm"
      | Setperms -> "Setperms"
      | Watch -> "Watch"
      | Unwatch -> "Unwatch"
      | Transaction_end -> "Transaction_end"
      | Debug xs -> sprintf "Debug [ %s ]" (String.concat "; " xs)
      | Introduce -> "Introduce"
      | Resume -> "Resume"
      | Release -> "Release"
      | Set_target -> "Set_target"
      | Restrict -> "Restrict"
      | Isintroduced x -> sprintf "Isintroduced %b" x
      | Error x -> sprintf "Error %s" x
      | Watchevent (x, y) -> sprintf "Watchevent %s %s" x y

  let ty_of_payload = function
    | Read _ -> Op.Read
    | Directory _ -> Op.Directory
    | Getperms perms -> Op.Getperms
    | Getdomainpath _ -> Op.Getdomainpath
    | Transaction_start _ -> Op.Transaction_start
    | Debug _ -> Op.Debug
    | Isintroduced _ -> Op.Isintroduced
    | Watchevent (_, _) -> Op.Watchevent
    | Error _ -> Op.Error
    | Write -> Op.Write
    | Mkdir -> Op.Mkdir
    | Rm -> Op.Rm
    | Setperms -> Op.Setperms
    | Watch -> Op.Watch
    | Unwatch -> Op.Unwatch
    | Transaction_end -> Op.Transaction_end
    | Introduce -> Op.Introduce
    | Resume -> Op.Resume
    | Release -> Op.Release
    | Set_target -> Op.Set_target
    | Restrict -> Op.Restrict

  let ok = "OK\000"

  let data_of_payload = function
    | Read x                   -> x
    | Directory ls             -> if ls = [] then "" else data_concat ls
    | Getperms perms           -> data_concat [ ACL.to_string perms ]
    | Getdomainpath x          -> data_concat [ x ]
    | Transaction_start tid    -> data_concat [ Int32.to_string tid ]
    | Debug items              -> data_concat items
    | Isintroduced b           -> data_concat [ if b then "T" else "F" ]
    | Watchevent (path, token) -> data_concat [ path; token ]
    | Error x                  -> data_concat [ x ]
    | _                        -> ok

  let print x tid rid =
    create tid rid (ty_of_payload x) (data_of_payload x)
end

module Request = struct

	type path_op =
	| Read
	| Directory
	| Getperms
	| Write of string
	| Mkdir
	| Rm
	| Setperms of ACL.t

	type payload =
	| PathOp of string * path_op
	| Getdomainpath of int
	| Transaction_start
	| Watch of string * string
	| Unwatch of string * string
	| Transaction_end of bool
	| Debug of string list
	| Introduce of int * Nativeint.t * int
	| Resume of int
	| Release of int
	| Set_target of int * int
	| Restrict of int
	| Isintroduced of int
	| Error of string
	| Watchevent of string

	open Printf

	let prettyprint_pathop x = function
		| Read -> sprintf "Read %s" x
		| Directory -> sprintf "Directory %s" x
		| Getperms -> sprintf "Getperms %s" x
		| Write v -> sprintf "Write %s %s" x v
		| Mkdir -> sprintf "Mkdir %s" x
		| Rm -> sprintf "Rm %s" x
		| Setperms acl -> sprintf "Setperms %s %s" x (ACL.to_string acl)

	let prettyprint_payload = function
		| PathOp (path, op) -> prettyprint_pathop path op
		| Getdomainpath x -> sprintf "Getdomainpath %d" x
		| Transaction_start -> "Transaction_start"
		| Watch (x, y) -> sprintf "Watch %s %s" x y
		| Unwatch (x, y) -> sprintf "Unwatch %s %s" x y
		| Transaction_end x -> sprintf "Transaction_end %b" x
		| Debug xs -> sprintf "Debug [ %s ]" (String.concat "; " xs)
		| Introduce (x, n, y) -> sprintf "Introduce %d %nu %d" x n y
		| Resume x -> sprintf "Resume %d" x
		| Release x -> sprintf "Release %d" x
		| Set_target (x, y) -> sprintf "Set_target %d %d" x y
		| Restrict x -> sprintf "Restrict %d" x
		| Isintroduced x -> sprintf "Isintroduced %d" x
		| Error x -> sprintf "Error %s" x
		| Watchevent x -> sprintf "Watchevent %s" x

	exception Parse_failure

	let strings data = split_string '\000' data

	let one_string data =
		let args = split_string ~limit:2 '\000' data in
		match args with
		| x :: [] -> x
		| _       ->
			raise Parse_failure

	let two_strings data =
		let args = split_string ~limit:2 '\000' data in
		match args with
		| a :: b :: [] -> a, b
		| a :: [] -> a, "" (* terminating NULL removed by get_data *)
		| _            ->
			raise Parse_failure

	let acl x = match ACL.of_string x with
		| Some x -> x
		| None ->
			raise Parse_failure

	let domid s =
		let v = ref 0 in
		let is_digit c = c >= '0' && c <= '9' in
		let len = String.length s in
		let i = ref 0 in
		while !i < len && not (is_digit s.[!i]) do incr i done;
		while !i < len && is_digit s.[!i]
		do
			let x = (Char.code s.[!i]) - (Char.code '0') in
			v := !v * 10 + x;
			incr i
		done;
		!v

	let bool = function
		| "F" -> false
		| "T" -> true
		| data ->
			raise Parse_failure

	let parse_exn request =
		let data = get_data request in
		match get_ty request with
		| Op.Read -> PathOp (data |> one_string, Read)
		| Op.Directory -> PathOp (data |> one_string, Directory)
		| Op.Getperms -> PathOp (data |> one_string, Getperms)
		| Op.Getdomainpath -> Getdomainpath (data |> one_string |> domid)
		| Op.Transaction_start -> Transaction_start
		| Op.Write ->
			let path, value = two_strings data in
			PathOp (path, Write value)
		| Op.Mkdir -> PathOp (data |> one_string, Mkdir)
		| Op.Rm -> PathOp (data |> one_string, Rm)
		| Op.Setperms ->
			let path, perms = two_strings data in
			let perms = acl perms in
			PathOp(path, Setperms perms)
		| Op.Watch ->
			let path, token = two_strings data in
			Watch(path, token)
		| Op.Unwatch ->
			let path, token = two_strings data in
			Unwatch(path, token)
		| Op.Transaction_end -> Transaction_end(data |> one_string |> bool)
		| Op.Debug -> Debug (strings data)
		| Op.Introduce ->
			begin match strings data with
			| d :: mfn :: port :: _ ->
				let d = domid d in
				let mfn = Nativeint.of_string mfn in
				let port = int_of_string port in
				Introduce (d, mfn, port)
			| _ ->
				raise Parse_failure
			end
		| Op.Resume -> Resume (data |> one_string |> domid)
		| Op.Release -> Release (data |> one_string |> domid)
		| Op.Set_target ->
			let mine, yours = two_strings data in
			let mine = domid mine and yours = domid yours in
			Set_target(mine, yours)
		| Op.Restrict -> Restrict (data |> one_string |> domid)
		| Op.Isintroduced -> Isintroduced (data |> one_string |> domid)
		| Op.Error -> Error(data |> one_string)
		| Op.Watchevent -> Watchevent(data |> one_string)

	let parse request =
		try
			Some (parse_exn request)
		with _ -> None

	let prettyprint request =
		Printf.sprintf "tid = %ld; rid = %ld; payload = %s"
			(get_tid request) (get_rid request)
			(match parse request with
				| None -> "None"
				| Some x -> "Some " ^ (prettyprint_payload x))

	let ty_of_payload = function
		| PathOp(_, Directory) -> Op.Directory
		| PathOp(_, Read) -> Op.Read
		| PathOp(_, Getperms) -> Op.Getperms
		| Debug _ -> Op.Debug
		| Watch (_, _) -> Op.Watch
		| Unwatch (_, _) -> Op.Unwatch
		| Transaction_start -> Op.Transaction_start
		| Transaction_end _ -> Op.Transaction_end
		| Introduce(_, _, _) -> Op.Introduce
		| Release _ -> Op.Release
		| Resume _ -> Op.Resume
		| Getdomainpath _ -> Op.Getdomainpath
		| PathOp(_, Write _) -> Op.Write
		| PathOp(_, Mkdir) -> Op.Mkdir
		| PathOp(_, Rm) -> Op.Rm
		| PathOp(_, Setperms _) -> Op.Setperms
		| Set_target (_, _) -> Op.Set_target
		| Restrict _ -> Op.Restrict
		| Isintroduced _ -> Op.Isintroduced

	let transactional_of_payload = function
		| PathOp(_, _)
		| Transaction_end _ -> true
		| _ -> false

	let data_of_payload = function
		| PathOp(path, Write value) ->
			path ^ "\000" ^ value (* no NULL at the end *)
		| PathOp(path, Setperms perms) ->
			data_concat [ path; ACL.to_string perms ]
		| PathOp(path, _) -> data_concat [ path ]
		| Debug commands -> data_concat commands
		| Watch (path, token)
		| Unwatch (path, token) -> data_concat [ path; token ]
		| Transaction_start -> data_concat []
		| Transaction_end commit -> data_concat [ if commit then "T" else "F" ]
		| Introduce(domid, mfn, port) ->
			data_concat [
				Printf.sprintf "%u" domid;
				Printf.sprintf "%nu" mfn;
				string_of_int port;
			]
		| Release domid
		| Resume domid
		| Getdomainpath domid
		| Restrict domid
		| Isintroduced domid ->
			data_concat [ Printf.sprintf "%u" domid; ]
		| Set_target (mine, yours) ->
			data_concat [ Printf.sprintf "%u" mine; Printf.sprintf "%u" yours; ]

	let print x tid rid =
		create
			(if transactional_of_payload x then tid else 0l)
			rid
			(ty_of_payload x)
			(data_of_payload x)
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

type address =
| Unix of string
| Domain of int

let string_of_address = function
| Unix x -> x
| Domain x -> string_of_int x

let domain_of_address = function
| Unix _ -> 0
| Domain x -> x

