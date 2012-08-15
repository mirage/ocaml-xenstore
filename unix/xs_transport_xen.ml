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

open Introduce

type t = {
	address: address;
    page: Cstruct.buf;
    port: int;
}

let domains : (int, t) Hashtbl.t = Hashtbl.create 128

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

let eventchn = ref None

let get_eventchn () =
	match !eventchn with
		| Some e -> e
		| None ->
			let e = Xeneventchn.init () in
			eventchn := Some e;

			let virq_thread () =
				let virq_port = Xeneventchn.bind_dom_exc_virq e in

				let fd = Lwt_unix.of_unix_file_descr ~set_flags:false (Xeneventchn.fd e) in
				while_lwt true do
					lwt () = Lwt_unix.wait_read fd in
					let port = Xeneventchn.pending e in
					Xeneventchn.unmask e port;
					if port = virq_port then begin
						(* signal someone *)
						return ()
					end else return ()
				done in

			let (_: unit Lwt.t) = virq_thread () in
			e


let create_dom0 () =
	let eventchn = get_eventchn () in
	lwt remote_port = read_port () in
	lwt page = map_page () in
	let port = Xeneventchn.bind_interdomain eventchn 0 remote_port in
	Xeneventchn.notify eventchn port;
	let d = {
		address = {
			domid = 0;
			mfn = Nativeint.zero;
			remote_port = remote_port;
		};
		page = page;
		port = port;
	} in
	Hashtbl.add domains 0 d;
	return d

let create_domU address =
	let eventchn = get_eventchn () in
	lwt page = Xenstore.map_foreign address.domid address.mfn in
	let port = Xeneventchn.bind_interdomain eventchn address.domid address.remote_port in
	let d = {
		address = address;
		page = page;
		port = port;
	} in
	Hashtbl.add domains address.domid d;
	return d

let rec read t buf ofs len =
	let n = Xenstore.unsafe_read t.page buf ofs len in
	if n = 0
	then (* wait t >> read t buf len *) return 0
	else begin
		Xeneventchn.notify (get_eventchn ()) t.port;
		return n
	end

let write t buf ofs len =
	let n = Xenstore.unsafe_write t.page buf ofs len in
	if n > 0 then Xeneventchn.notify (get_eventchn ()) t.port;
	return n

let destroy t =
	let eventchn = get_eventchn () in
	Xeneventchn.unbind eventchn t.port;
	Hashtbl.remove domains t.address.domid;
	return ()

let address_of t =
	return (Xs_packet.Domain t.address.domid)

type server = address Lwt_stream.t

let listen () =
	return stream

let rec accept_forever stream process =
	lwt address = Lwt_stream.next stream in
	lwt d = create_domU address in
	let (_: unit Lwt.t) = process d in
	accept_forever stream process
