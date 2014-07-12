(*
 * Copyright (C) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Lwt
open Sexplib.Std
open Xenstore

(** A byte-level transport over the xenstore Unix domain socket *)

type 'a t = 'a Lwt.t
let return x = return x
let ( >>= ) m f = m >>= f

let debug fmt = Logging.debug "userspace" fmt
let error fmt = Logging.error "userspace" fmt

(* If we get a connection refused error it will be because the server
 * is still starting up. *)
let initial_retry_interval = 0.1 (* seconds *)
let max_retry_interval = 5.0 (* seconds *)
let retry_max = 100 (* attempts *)

let max_packet_size = Protocol.xenstore_payload_max + Protocol.Header.sizeof

exception Connection_timeout

module FDReader = struct
  type t = {
    fd: Lwt_unix.file_descr;
    buffer: Cstruct.t;
    mutable offset: int64;
    mutable length: int;
  }
  type offset = int64
  type item = Cstruct.t

  let make fd =
    let buffer = Cstruct.create 1024 in
    let offset = 0L in
    let length = 0 in
    { fd; buffer; offset; length }

  let next t = match t.length with
    | 0 ->
      Lwt_cstruct.read t.fd t.buffer >>= fun n ->
      t.length <- n;
      return (t.offset, Cstruct.sub t.buffer 0 n)
    | n ->
      return (t.offset, Cstruct.sub t.buffer 0 n)

  let ack t offset =
    let delta = Int64.(to_int (sub offset t.offset)) in
    let length = max 0 (t.length - delta) in
    if length > 0
    then Cstruct.blit t.buffer (t.length - length) t.buffer 0 length;
    t.offset <- offset;
    t.length <- length;
    return ()
end

let complete op fd buf =
  let ofs = buf.Cstruct.off in
  let len = buf.Cstruct.len in
  let buf = buf.Cstruct.buffer in
  let rec loop acc fd buf ofs len =
    op fd buf ofs len >>= fun n ->
    let len' = len - n in
    let acc' = acc + n in
    if len' = 0 || n = 0
    then return acc'
    else loop acc' fd buf (ofs + n) len' in
  loop 0 fd buf ofs len >>= fun n ->
  if n = 0 && len <> 0
  then fail End_of_file
  else return ()

module FDWriter = struct
  type t = {
    fd: Lwt_unix.file_descr;
    buffer: Cstruct.t;
    mutable offset: int64;
  }
  type offset = int64
  type item = Cstruct.t

  let make fd =
    let buffer = Cstruct.create 1024 in
    let offset = 0L in
    { fd; buffer; offset }

  let next t =
    return (t.offset, t.buffer )

  let ack t offset =
    let delta = Int64.(to_int (sub offset t.offset)) in
    complete Lwt_bytes.write t.fd (Cstruct.sub t.buffer 0 delta) >>= fun () ->
    let unwritten = Cstruct.len t.buffer - delta in
    if unwritten > 0
    then Cstruct.blit t.buffer (Cstruct.len t.buffer - unwritten) t.buffer 0 unwritten;
    t.offset <- offset;
    return ()
end

module BufferedReader = BufferedReader.Make(FDReader)
module BufferedWriter = BufferedWriter.Make(FDWriter)

module Request = struct
  module Reader = PacketReader.Make(BufferedReader)
  module Writer = PacketWriter.Make(BufferedWriter)
end

module Response = struct
  module Reader = PacketReader.Make(BufferedReader)
  module Writer = PacketWriter.Make(BufferedWriter)
end

(* Individual connections *)
type connection = {
  fd: Lwt_unix.file_descr;
  sockaddr: Lwt_unix.sockaddr;
  reader: BufferedReader.t;
  writer: BufferedWriter.t;
}

let alloc (fd, sockaddr) =
  let read_buffer = Cstruct.create max_packet_size in
  let reader = BufferedReader.create (FDReader.make fd) read_buffer in
  let write_buffer = Cstruct.create max_packet_size in
  let writer = BufferedWriter.create (FDWriter.make fd) write_buffer in
  return { fd; sockaddr; reader; writer }

let xenstored_socket = ref "/var/run/xenstored/socket"

(* We'll look for these paths in order: *)
let get_xenstore_paths () =
  let default = [
    !xenstored_socket;
    "/proc/xen/xenbus"; (* Linux *)
    "/dev/xen/xenstore"; (* FreeBSD *)
  ] in
  try
    Sys.getenv "XENSTORED_PATH" :: default
  with Not_found -> default

let choose_xenstore_path () =
  List.fold_left (fun acc possibility -> match acc with
      | Some x -> Some x
      | None ->
        if Sys.file_exists possibility then Some possibility else None
    ) None (get_xenstore_paths ())

exception Could_not_find_xenstore

let create () =
  ( match choose_xenstore_path () with
    | None ->
      error "Failed to find xenstore socket. I tried the following:";
      List.iter (fun x -> error "  %s" x) (get_xenstore_paths ());
      error "On linux you might not have xenfs mounted:";
      error "   sudo mount -t xenfs xenfs /proc/xen";
      error "Or perhaps you just need to set the XENSTORED_PATH environment variable.";
      fail Could_not_find_xenstore
    | Some x -> return x ) >>= fun path ->
  Lwt_unix.stat path >>= fun stats ->
  let sockaddr = Lwt_unix.ADDR_UNIX(path) in
  match stats.Lwt_unix.st_kind with
  | Lwt_unix.S_SOCK ->
    let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
    let start = Unix.gettimeofday () in
    let rec retry n interval =
      if n > retry_max then begin
        error "Failed to connect after %.0f seconds" (Unix.gettimeofday () -. start);
        fail Connection_timeout
      end else
        try_lwt
          Lwt_unix.connect fd sockaddr
        with Unix.Unix_error(Unix.ECONNREFUSED, _, _) ->
          Lwt_unix.sleep interval >>= fun () ->
          retry (n + 1) (interval +. 0.1) in
    retry 0 initial_retry_interval >>= fun () ->
    alloc (fd, sockaddr)
  | _ ->
    let fd = Unix.openfile path [ Lwt_unix.O_RDWR ] 0o0 in
    (* It looks like a file but behaves like a pipe: *)
    alloc (Lwt_unix.of_unix_file_descr ~blocking:false fd, sockaddr)

let destroy { fd } = Lwt_unix.close fd

let read { fd } = complete Lwt_bytes.read fd
let write { fd } = complete Lwt_bytes.write fd

let int_of_file_descr fd =
  let fd = Lwt_unix.unix_file_descr fd in
  let (fd: int) = Obj.magic fd in
  fd

let uri_of { fd } =
  let creds = Lwt_unix.get_credentials fd in
  let pid = creds.Lwt_unix.cred_pid in
  lwt cmdline =
    Lwt_io.with_file ~mode:Lwt_io.input
      (Printf.sprintf "/proc/%d/cmdline" pid)
      (fun ic ->
         lwt cmdline = Lwt_io.read_line_opt ic in
         match cmdline with
         | Some x -> return x
         | None -> return "unknown") in
  (* Take only the binary name, stripped of directories *)
  let filename =
    try
      let i = String.index cmdline '\000' in
      String.sub cmdline 0 i
    with Not_found -> cmdline in
  let basename = Filename.basename filename in
  let name = Printf.sprintf "%d:%s:%d" pid basename (int_of_file_descr fd) in
  return (Uri.make ~scheme:"unix" ~path:name ())

let domain_of _ = 0

(* Servers which accept connections *)
type server = Lwt_unix.file_descr

let _ =
  (* Make sure a write to a closed fd doesn't cause us to quit
     	   with SIGPIPE *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let listen () =
  let sockaddr = Lwt_unix.ADDR_UNIX(!xenstored_socket) in
  let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
  lwt () = try_lwt Lwt_unix.unlink !xenstored_socket with _ -> return () in
  Lwt_unix.bind fd sockaddr;
  Lwt_unix.listen fd 5;
  return fd

let rec accept_forever fd process =
  lwt conns, _ (*exn_option*) = Lwt_unix.accept_n fd 16 in
  let (_: unit Lwt.t list) = List.map (fun x -> alloc x >>= process) conns in
  accept_forever fd process

(*
type offset = unit with sexp

let get_read_offset _ = return ()
let get_write_offset _ = return ()

let flush _ _ = return ()

let enqueue t hdr response =
  let reply_buf = t.write_buffer in
  let payload_buf = Cstruct.shift reply_buf Protocol.Header.sizeof in
  let next = Protocol.Response.marshal response payload_buf in
  let length = next.Cstruct.off - payload_buf.Cstruct.off in
  let hdr = Protocol.Header.({ hdr with len = length }) in
  ignore (Protocol.Header.marshal hdr reply_buf);
  write t (Cstruct.sub t.write_buffer 0 (Protocol.Header.sizeof + length))

let recv t _ =
  let hdr = Cstruct.sub t.read_buffer 0 Protocol.Header.sizeof in
  read t hdr >>= fun () ->
  match Protocol.Header.unmarshal hdr with
  | `Error x -> return ((), `Error x)
  | `Ok x ->
    let payload = Cstruct.sub t.read_buffer Protocol.Header.sizeof x.Protocol.Header.len in
    read t payload >>= fun () ->
    begin match Protocol.Request.unmarshal x payload with
      | `Error y -> return ((), `Error y)
      | `Ok y -> return ((), `Ok (x, y))
    end
*)

module Introspect = struct
  type t = connection

  let read { fd } = function
    | [ "readable" ] -> Some (string_of_bool (Lwt_unix.readable fd))
    | [ "writable" ] -> Some (string_of_bool (Lwt_unix.writable fd))
    | _ -> None

  let ls t = function
    | [] -> [ "readable"; "writable" ]
    | _ -> []

  let write _ _ _ = false
end
