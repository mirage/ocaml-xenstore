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
open Sexplib
open Sexplib.Std

(* The packet contains a Cstruct.t *)
module Cstruct = struct
  include Cstruct
  type _t = string with sexp
  let t_of_sexp s =
    let _t = _t_of_sexp s in
    let c = Cstruct.create (String.length _t) in
    Cstruct.blit_from_string _t 0 c 0 (Cstruct.len c);
    c
  let sexp_of_t t =
    let _t = Cstruct.to_string t in
    sexp_of__t _t
end

(* The IntroduceDomain message includes a Nativeint.t *)
module Nativeint = struct
  include Nativeint

  type _t = string with sexp
  let t_of_sexp s =
    let _t = _t_of_sexp s in
    Nativeint.of_string _t
  let sexp_of_t t =
    let _t = Nativeint.to_string t in
    sexp_of__t _t
end

let ( |> ) f g = g f
let ( ++ ) f g x = f (g x)

type ('a, 'b) result = [
| `Ok of 'a
| `Error of 'b
] with sexp

let ( >>= ) m f = match m with
| `Ok x -> f x
| `Error x -> `Error x

let return x = `Ok x

let xenstore_payload_max = 4096 (* xen/include/public/io/xs_wire.h *)

module Op = struct
  type t =
  | Debug | Directory | Read | Getperms
  | Watch | Unwatch | Transaction_start
  | Transaction_end | Introduce | Release
  | Getdomainpath | Write | Mkdir | Rm
  | Setperms | Watchevent | Error | Isintroduced
  | Resume | Set_target
  | Restrict
  with sexp

  (* The index of the value in the array is the integer representation used
     by the wire protocol. Every element of t exists exactly once in the array. *)
  let on_the_wire =
    [| Debug; Directory; Read; Getperms;
       Watch; Unwatch; Transaction_start;
       Transaction_end; Introduce; Release;
       Getdomainpath; Write; Mkdir; Rm;
       Setperms; Watchevent; Error; Isintroduced;
       Resume; Set_target;
       Restrict |]

  let of_int32 i =
    let i = Int32.to_int i in
    if i >= 0 && i < Array.length on_the_wire
    then `Ok (on_the_wire.(i))
    else `Error (Printf.sprintf "Unknown xenstore operation id: %d. Possible new protocol version? Or malfunctioning peer?" i)

  let to_int32 x =
    match snd (Array.fold_left
      (fun (idx, result) v -> if x = v then (idx + 1, Some idx) else (idx + 1, result))
      (0, None) on_the_wire) with
      | None -> assert false (* impossible since on_the_wire contains each element *)
      | Some i -> Int32.of_int i

  let all = Array.to_list on_the_wire
end


module Header = struct
  type t = {
    tid: int32;
    rid: int32;
    ty: Op.t;
    len: int;
  } with sexp

  cstruct hdr {
    uint32_t ty;
    uint32_t rid;
    uint32_t tid;
    uint32_t len
  } as little_endian

  let sizeof = sizeof_hdr

  let marshal t buf =
    set_hdr_ty buf (Op.to_int32 t.ty);
    set_hdr_rid buf t.rid;
    set_hdr_tid buf t.tid;
    set_hdr_len buf (Int32.of_int t.len);
    Cstruct.shift buf sizeof_hdr

  let unmarshal buf =
    let ty = get_hdr_ty buf in
    Op.of_int32 ty >>= fun ty ->
    let rid = get_hdr_rid buf in
    let tid = get_hdr_tid buf in
    let len = get_hdr_len buf in
    let len = Int32.to_int len in
    if len > xenstore_payload_max
    then `Error(Printf.sprintf "Packet is too large: %d > %d (see xen/include/public/io/xs_wire.h)" len xenstore_payload_max)
    else `Ok { tid; rid; ty; len }
end

module Token = struct
  type t = string with sexp

  (** [to_user_string x] returns the user-supplied part of the watch token *)
  let to_user_string x = Scanf.sscanf x "%d:%s" (fun _ x -> x)

  let to_debug_string x = x

  let unmarshal x = x
  let marshal x = x
end

module Path = struct
  module Element = struct
    type t = string with sexp

    let char_is_valid c =
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9') ||
      c = '_' || c = '-' || c = '@'

    exception Invalid_char of char

    let assert_valid x =
      for i = 0 to String.length x - 1 do
        if not(char_is_valid x.[i])
        then raise (Invalid_char x.[i])
      done

    let of_string x = assert_valid x; x
    let to_string x = x

  end

  type t = Element.t list with sexp

  let empty = []

  exception Invalid_path of string * string

  let rec split_string ?limit:(limit=(-1)) c s =
    let i = try String.index s c with Not_found -> -1 in
    let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
    if i = -1 || nlimit = 0
    then [ s ]
    else
      let a = String.sub s 0 i
      and b = String.sub s (i + 1) (String.length s - i - 1) in
      a :: (split_string ~limit: nlimit c b)

  let of_string path =
    if path = ""
    then raise (Invalid_path (path, "paths may not be empty"));
    if String.length path > 1024
    then raise (Invalid_path (path, "paths may not be larger than 1024 bytes"));
    let absolute, fragments = match split_string '/' path with
    | "" :: "" :: [] -> true, []
    | "" :: path -> true, path (* preceeding '/' *)
    | path -> false, path in
    List.map (fun fragment ->
      try Element.of_string fragment
      with Element.Invalid_char c -> raise (Invalid_path(path, Printf.sprintf "valid paths contain only ([a-z]|[A-Z]|[0-9]|-|_|@])+ but this contained '%c'" c))
    ) fragments

  let to_list t = t

  let to_string_list t = t

  let of_string_list xs = List.map Element.of_string xs

  let to_string t = String.concat "/" (List.map Element.to_string t)

  let dirname = function
  | [] -> []
  | x -> List.(rev (tl (rev x)))

  let basename x = List.(hd (rev x))

  let walk f path initial = List.fold_left (fun x y -> f y x) initial path

  let fold f path initial =
    let rec loop acc prefix = function
    | [] -> acc
    | x :: xs ->
      let prefix = prefix @ [x] in
      loop (f prefix acc) prefix xs in
    loop initial [] path

  let iter f path = fold (fun prefix () -> f prefix) path ()

  let common_prefix (p1: t) (p2: t) =
    let rec compare l1 l2 = match l1, l2 with
    | h1 :: tl1, h2 :: tl2 ->
      if h1 = h2 then h1 :: (compare tl1 tl2) else []
    | _, [] | [], _ ->
      (* if l1 or l2 is empty, we found the equal part already *)
      [] in
    compare p1 p2
end

module Name = struct
  type predefined =
  | IntroduceDomain
  | ReleaseDomain
  with sexp

  type t =
  | Predefined of predefined
  | Absolute of Path.t
  | Relative of Path.t
  with sexp

  let of_string = function
  | "@introduceDomain" -> Predefined IntroduceDomain
  | "@releaseDomain" -> Predefined ReleaseDomain
  | path when path <> "" && path.[0] = '/' -> Absolute (Path.of_string path)
  | path -> Relative (Path.of_string path)

  let to_string = function
  | Predefined IntroduceDomain -> "@introduceDomain"
  | Predefined ReleaseDomain -> "@releaseDomain"
  | Absolute path -> "/" ^ (Path.to_string path)
  | Relative path ->        Path.to_string path

  let is_relative = function
  | Relative _ -> true
  | _ -> false

  let resolve t relative_to = match t, relative_to with
  | Relative path, Absolute dir -> Absolute (dir @ path)
  | t, _ -> t

  let relative t base = match t, base with
  | Absolute t, Absolute base ->
    (* If [base] is a prefix of [t], strip it off *)
    let rec f x y = match x, y with
    | x :: xs, y :: ys when x = y -> f xs ys
    | [], y -> Relative y
    | _, _ -> Absolute t in
    f base t
  | _, _ -> t

  let to_path x = match x with
  | Predefined _ -> raise (Path.Invalid_path(to_string x, "not a valid path"))
  | Absolute p -> p
  | Relative p -> p
end

module Marshal = struct

  let null t =
    Cstruct.set_uint8 t 0 0;
    Cstruct.shift t 1
  let ok t =
    Cstruct.set_char t 0 'O';
    Cstruct.set_char t 1 'K';
    Cstruct.set_char t 2 '\000';
    Cstruct.shift t 3
  let rec list f xs t = match xs with
  | [] -> t
  | x :: xs -> list f xs (null (f x t))

  let string x t =
    Cstruct.blit_from_string x 0 t 0 (String.length x);
    Cstruct.shift t (String.length x)
  let int32 x t = string (Int32.to_string x) t
  let bool x t = string (if x then "T" else "F") t
  let char x t =
    Cstruct.set_char t 0 x;
    Cstruct.shift t 1
  let int x t = string (string_of_int x) t

  let finished f t =
    let t' = f t in
    let len = t'.Cstruct.off - t.Cstruct.off in
    Cstruct.sub t 0 len, t'
end

module Unmarshal = struct
  let expect_int x = try `Ok (int_of_string x) with _ -> `Error(Printf.sprintf "Failed to parse integer: \"%s\"" (String.escaped x))
  let expect_int32 x = try `Ok (Int32.of_string x) with _ -> `Error(Printf.sprintf "Failed to parse int32: \"%s\"" (String.escaped x))
  let expect_nativeint x = try `Ok (Nativeint.of_string x) with _ -> `Error(Printf.sprintf "Failed to parse nativeint: \"%s\"" (String.escaped x))
  let expect_unit x = if x = "" then `Ok () else `Error(Printf.sprintf "Expected an empty string, got: \"%s\"" (String.escaped x))
  let expect_ok x = if x = "OK" then `Ok () else `Error(Printf.sprintf "Expected the string \"OK\", got: \"%s\"" (String.escaped x))
  let expect_bool = function
  | "T" -> return true
  | "F" -> return false
  | x -> `Error (Printf.sprintf "Expected either T or F, got: \"%s\"" (String.escaped x))

  let expect_null_termination f t =
    if Cstruct.len t > 0 && (Cstruct.get_uint8 t (Cstruct.len t - 1) = 0)
    then f t
    else `Error (Printf.sprintf "Expected a NULL-termination, got: \"%s\"" (String.escaped (Cstruct.to_string t)))

  let remove_trailing_data t =
    (* remove data beyond the last NULL *)
    let rec to_remove = function
    | -1 -> Cstruct.len t
    | i -> if Cstruct.get_char t i = '\000'
           then Cstruct.len t - i - 1
           else to_remove (i - 1) in
    let n = to_remove (Cstruct.len t - 1) in
    Cstruct.sub t 0 (Cstruct.len t - n)

  let null t =
    if Cstruct.len t > 0 && (Cstruct.get_uint8 t (Cstruct.len t - 1) = 0)
    then Cstruct.sub t 0 (Cstruct.len t - 1)
    else t
  let return x = `Ok x
  let find buf c =
    let rec loop n =
      if n = Cstruct.len buf then raise Not_found;
      if Cstruct.get_char buf n = c then n else loop (n + 1) in
    loop 0
  let rec split ?limit:(limit=(-1)) c buf =
    let i = try find buf c with Not_found -> -1 in
    let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
    if i = -1 || nlimit = 0
    then [ buf ]
    else
      let a = Cstruct.sub buf 0 i
      and b = Cstruct.sub buf (i + 1) (Cstruct.len buf - i - 1) in
      a :: (split ~limit: nlimit c b)

  let rec join acc = function
  | [] -> return (List.rev acc)
  | x :: xs ->
    x >>= fun x ->
    join (x :: acc) xs

  let list f x = x |> null |> split '\000' |> List.filter (fun x -> Cstruct.len x <> 0) |> List.map f |> join []

  let cons a b x = x |> split ~limit:2 '\000' |> (function
  | a' :: b' :: [] ->
    a a' >>= fun a'' ->
    b b' >>= fun b'' ->
    return (a'', b'')
  | _ -> `Error(Printf.sprintf "Failed to unmarshal a cons: got \"%s\"" (String.escaped (Cstruct.to_string x))))
  let triple a b c x = x |> null |> split '\000' |> (function
  | a' :: b' :: c' :: [] ->
    a a' >>= fun a'' ->
    b b' >>= fun b'' ->
    c c' >>= fun c'' ->
    return (a'', b'', c'')
  | _ -> `Error(Printf.sprintf "Failed to unmarshal a triple: got \"%s\"" (String.escaped (Cstruct.to_string x))))
  
  let string    x = x |> null |> Cstruct.to_string |> return
  let int       x = x |> null |> Cstruct.to_string |> expect_int
  let int32     x = x |> null |> Cstruct.to_string |> expect_int32
  let nativeint x = x |> null |> Cstruct.to_string |> expect_nativeint
  let unit      x = x |> null |> Cstruct.to_string |> expect_unit
  let ok        x = x |> null |> Cstruct.to_string |> expect_ok
  let bool      x = x |> null |> Cstruct.to_string |> expect_bool
end

module ACL = struct
  type perm =
    | NONE
    | READ
    | WRITE
    | RDWR
  with sexp

  let char_of_perm = function
    | READ -> 'r'
    | WRITE -> 'w'
    | RDWR -> 'b'
    | NONE -> 'n'

  let perm_of_char = function
    | 'r' -> `Ok READ
    | 'w' -> `Ok WRITE
    | 'b' -> `Ok RDWR
    | 'n' -> `Ok NONE
    | c -> `Error (Printf.sprintf "Unknown permission character '%c'" c)

  type domid = int with sexp

  type t = {
    owner: domid;             (** domain which "owns", has full access *)
    other: perm;              (** default permissions for all others... *)
    acl: (domid * perm) list; (** ... unless overridden in the ACL *)
  } with sexp

  let marshal perms buf =
    Marshal.list (fun (domid, perm) buf -> buf
      |> Marshal.char (char_of_perm perm)
      |> Marshal.int domid
    ) ( (perms.owner, perms.other) :: perms.acl ) buf

  let unmarshal buf =
    buf
    (* quirk: ignore data following the last NULL *)
    |> Unmarshal.remove_trailing_data
    |> (Unmarshal.list (fun buf ->
        perm_of_char (Cstruct.get_char buf 0) >>= fun p ->
        Cstruct.shift buf 1 |> Cstruct.to_string |> Unmarshal.expect_int >>= fun domid ->
        return (domid, p)
      ))  >>= function
    | (owner, other) :: l -> return { owner = owner; other = other; acl = l }
    | [] -> `Error (Printf.sprintf "Invalid ACL: %s" (String.escaped (Cstruct.to_string buf)))
end

exception Enoent of string
exception Eagain
exception Invalid
exception Error of string

module Response = struct

  type t =
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
  with sexp

  let get_ty = function
  | Read _ -> Op.Read
  | Directory _ -> Op.Directory
  | Getperms _ -> Op.Getperms
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

  let marshal x buf = let open Marshal in match x with
  | Read x                  -> buf |> string x
  | Directory ls            -> buf |> list string ls
  | Getperms perms          -> buf |> ACL.marshal perms
  | Getdomainpath x         -> buf |> string x                    |> null
  | Transaction_start x     -> buf |> int32 x                     |> null
  | Debug items             -> buf |> list string items
  | Isintroduced b          -> buf |> bool b                      |> null
  | Watchevent(path, token) -> buf |> list string [ path; token ]
  | Error x                 -> buf |> string x                    |> null
  | _                       -> buf |> ok

  let unmarshal hdr payload =
    let open Unmarshal in
    let ok op = ok payload >>= fun () -> return op in
  match hdr.Header.ty with
  | Op.Read ->
    string payload >>= fun x ->
    return (Read x)
  | Op.Directory ->
    list string payload >>= fun ls ->
    return (Directory ls)
  | Op.Getperms ->
    ACL.unmarshal payload >>= fun perms ->
    return (Getperms perms)
  | Op.Getdomainpath ->
    string payload >>= fun path ->
    return (Getdomainpath path)
  | Op.Transaction_start ->
    int32 payload >>= fun tid ->
    return (Transaction_start tid)
  | Op.Debug ->
    list string payload >>= fun debug ->
    return (Debug debug)
  | Op.Isintroduced ->
    bool payload >>= fun b ->
    return (Isintroduced b)
  | Op.Watchevent ->
    cons string string payload >>= fun (path, token) ->
    return (Watchevent(path, token))
  | Op.Error ->
    string payload >>= fun x ->
    return (Error x)
  | Op.Write           -> ok Write
  | Op.Mkdir           -> ok Mkdir
  | Op.Rm              -> ok Rm
  | Op.Setperms        -> ok Setperms
  | Op.Watch           -> ok Watch
  | Op.Unwatch         -> ok Unwatch
  | Op.Transaction_end -> ok Transaction_end
  | Op.Introduce       -> ok Introduce
  | Op.Resume          -> ok Resume
  | Op.Release         -> ok Release
  | Op.Set_target      -> ok Set_target
  | Op.Restrict        -> ok Restrict

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
  with sexp

  type t =
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
  with sexp

  let get_ty = function
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

  let unmarshal hdr payload =
    let open Unmarshal in
    let pathop op =
      (* quirk: fail if a string NULL terminator not present *)
      expect_null_termination string payload >>= fun p ->
      return (PathOp (p, op)) in
    match hdr.Header.ty with
    | Op.Read -> pathop Read
    | Op.Directory -> pathop Directory
    | Op.Getperms -> pathop Getperms
    | Op.Mkdir -> pathop Mkdir
    | Op.Rm -> pathop Rm
    | Op.Getdomainpath ->
      (* quirk: if the string is empty, assume domain 0 *)
      string payload >>= fun txt ->
      if txt = ""
      then return (Getdomainpath 0)
      else expect_int txt >>= fun domid ->
      return (Getdomainpath domid)
    | Op.Transaction_start -> return Transaction_start
    | Op.Write ->
      cons string string payload >>= fun (path, value) ->
      return (PathOp(path, Write value))
    | Op.Setperms ->
      cons string ACL.unmarshal payload >>= fun (path, perms) ->
      return (PathOp (path, Setperms perms))
    | Op.Watch ->
      (* quirk: remove data beyond the last NULL *)
      payload
      |> remove_trailing_data
      |> cons string string >>= fun (path, token) ->
      return (Watch (path, token))
    | Op.Unwatch ->
      cons string string payload >>= fun (path, token) ->
      return (Unwatch (path, token))
    | Op.Transaction_end ->
      bool payload >>= fun b ->
      return (Transaction_end b)
    | Op.Debug ->
      list string payload >>= fun lines ->
      return (Debug lines)
    | Op.Introduce ->
      triple int nativeint int payload >>= fun (domid, mfn, port) ->
      return (Introduce (domid, mfn, port))
    | Op.Resume ->
      int payload >>= fun domid ->
      return (Resume domid)
    | Op.Release ->
      int payload >>= fun domid ->
      return (Release domid)
    | Op.Set_target ->
      cons int int payload >>= fun (mine, yours) ->
      return (Set_target (mine, yours))
    | Op.Restrict ->
      int payload >>= fun domid ->
      return (Restrict domid)
    | Op.Isintroduced ->
      int payload >>= fun domid ->
      return (Isintroduced domid)
    | Op.Watchevent ->
      `Error "It is illegal to send a Watchevent request"
    | Op.Error ->
      `Error "It is illegal to send an Error request"

  let marshal x buf = let open Marshal in match x with
    | PathOp(path, Write value)    -> buf |> string path |> null |> string value (* quirk: no NULL at the end *)
    | PathOp(path, Setperms perms) -> buf |> string path |> null |> ACL.marshal perms
    | PathOp(path, _)              -> buf |> string path |> null
    | Debug commands               -> buf |> list string commands
    | Watch (path, token)
    | Unwatch (path, token)        -> buf |> list string [ path; token ]
    | Transaction_start            -> buf |> null
    | Transaction_end commit       -> buf |> bool commit |> null
    | Introduce (domid, mfn, port) -> buf |> list string [ string_of_int domid; Nativeint.to_string mfn; string_of_int port ]
    | Release domid
    | Resume domid
    | Getdomainpath domid
    | Restrict domid
    | Isintroduced domid           -> buf |> list string [ string_of_int domid ]
    | Set_target (mine, yours)     -> buf |> list string [ string_of_int mine; string_of_int yours ]
end
