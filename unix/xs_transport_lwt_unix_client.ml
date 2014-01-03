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

open Lwt

(* Individual connections *)
type channel = Lwt_unix.file_descr * Lwt_unix.sockaddr
let create () =
  let sockaddr = Lwt_unix.ADDR_UNIX(!Xs_transport.xenstored_socket) in
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
	if n <> len then begin
		fail End_of_file
	end else return ()

type 'a t = 'a Lwt.t
let return = Lwt.return
let ( >>= ) = Lwt.bind

type backend = [`unix | `xen]
let backend = `unix
