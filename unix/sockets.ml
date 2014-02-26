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

(* Individual connections *)
type channel = Lwt_unix.file_descr * Lwt_unix.sockaddr
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
  lwt () = retry 0 initial_retry_interval in
  return (fd, sockaddr)
let destroy (fd, _) = Lwt_unix.close fd
let read (fd, _) = Lwt_unix.read fd
let write (fd, _) bufs ofs len =
	lwt n = Lwt_unix.write fd bufs ofs len in
	if n <> len
        then fail End_of_file
	else return ()

let int_of_file_descr fd =
	let fd = Lwt_unix.unix_file_descr fd in
	let (fd: int) = Obj.magic fd in
	fd

let address_of (fd, _) =
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
  let (_: unit Lwt.t list) = List.map process conns in
  accept_forever fd process

module Introspect = struct
  let read (fd, _) = function
    | [ "readable" ] -> Some (string_of_bool (Lwt_unix.readable fd))
    | [ "writable" ] -> Some (string_of_bool (Lwt_unix.writable fd))
    | _ -> None

  let ls t = function
    | [] -> [ "readable"; "writable" ]
    | _ -> []

  let write _ _ _ = false
end
