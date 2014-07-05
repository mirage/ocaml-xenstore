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
open Sexplib.Std
open Xenstore

(** A byte-level transport over the xenstore Unix domain socket *)

(* The unix domain socket may not exist, or we may get a connection
   refused error if the server is in the middle of restarting. *)
let initial_retry_interval = 0.1 (* seconds *)
let max_retry_interval = 5.0 (* seconds *)
let retry_max = 100 (* attempts *)

let xenstored_socket = ref "/var/run/xenstored/socket"

open Lwt

type 'a t = 'a Lwt.t
let return x = return x
let ( >>= ) m f = m >>= f

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

(* Individual connections *)
type connection = {
  fd: Lwt_unix.file_descr;
  sockaddr: Lwt_unix.sockaddr;
  read_buffer: Cstruct.t;
  write_buffer: Cstruct.t;
}

let alloc (fd, sockaddr) =
  let read_buffer = Cstruct.create (Protocol.Header.sizeof + Protocol.xenstore_payload_max) in
  let write_buffer = Cstruct.create (Protocol.Header.sizeof + Protocol.xenstore_payload_max) in
  return { fd; sockaddr; read_buffer; write_buffer }

let create () =
  let sockaddr = Lwt_unix.ADDR_UNIX(!xenstored_socket) in
  let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
  let start = Unix.gettimeofday () in
  let rec retry n interval =
    if n > retry_max
    then fail (Failure (Printf.sprintf "Failed to connect to xenstore after %.0f seconds" (Unix.gettimeofday () -. start)))
    else
      try_lwt
        Lwt_unix.connect fd sockaddr
      with _ ->
        lwt () = Lwt_unix.sleep interval in
        retry (n + 1) (interval +. 0.1) in
  retry 0 initial_retry_interval >>= fun () ->
  alloc (fd, sockaddr)

let destroy { fd } = Lwt_unix.close fd

let read { fd } = complete Lwt_bytes.read fd
let write { fd } = complete Lwt_bytes.write fd

let int_of_file_descr fd =
	let fd = Lwt_unix.unix_file_descr fd in
	let (fd: int) = Obj.magic fd in
	fd

let address_of { fd } =
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

module Introspect = struct
  let read { fd } = function
    | [ "readable" ] -> Some (string_of_bool (Lwt_unix.readable fd))
    | [ "writable" ] -> Some (string_of_bool (Lwt_unix.writable fd))
    | _ -> None

  let ls t = function
    | [] -> [ "readable"; "writable" ]
    | _ -> []

  let write _ _ _ = false
end
