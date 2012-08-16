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

open Lwt

let xenstored_socket = ref "/var/run/xenstored/socket"

(* Individual connections *)
type t = Lwt_unix.file_descr * Lwt_unix.sockaddr
let create () =
  let sockaddr = Lwt_unix.ADDR_UNIX(!xenstored_socket) in
  let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
  lwt () = Lwt_unix.connect fd sockaddr in
  return (fd, sockaddr)
let destroy (fd, _) = Lwt_unix.close fd
let read (fd, _) = Lwt_unix.read fd
let write (fd, _) = Lwt_unix.write fd

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
			String.sub cmdline 0 (i - 1)
		with Not_found -> cmdline in
	let basename = Filename.basename filename in
	let padto x y = String.make (y - (String.length x)) ' ' in
	return (Xs_packet.Unix(Printf.sprintf "%d:%s%s" pid basename (padto basename 10)))

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



