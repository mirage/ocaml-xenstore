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

open Lwt

let xenstored_proc_port = "/proc/xen/xsd_port"
let xenstored_proc_kva  = "/proc/xen/xsd_kva"

let read_port () =
	Lwt_io.with_file ~mode:Lwt_io.input xenstored_proc_port
		(fun ic ->
			lwt line = Lwt_io.read_line ic in
			return (int_of_string line)
		)

let map_page () =
	lwt fd = Lwt_unix.openfile xenstored_proc_kva [ Lwt_unix.O_RDWR ] 0o0 in
	try_lwt
		let a = Bigarray.Array1.map_file (Lwt_unix.unix_file_descr fd) Bigarray.char Bigarray.c_layout true (-1) in
		return a
	finally
		Lwt_unix.close fd
