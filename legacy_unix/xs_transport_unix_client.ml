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

(* Individual connections *)
type channel = Unix.file_descr * Unix.sockaddr
let create () =
  let sockaddr = Unix.ADDR_UNIX(!Xs_transport.xenstored_socket) in
  let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.connect fd sockaddr;
  fd, sockaddr
let destroy (fd, _) = Unix.close fd
let read (fd, _) = Unix.read fd
let write (fd, _) bufs ofs len =
	let n = Unix.write fd bufs ofs len in
	if n <> len then begin
		raise End_of_file
	end

type 'a t = 'a
let return x = x
let ( >>= ) x f = f x

type backend = [`unix | `xen]
let backend = `unix
