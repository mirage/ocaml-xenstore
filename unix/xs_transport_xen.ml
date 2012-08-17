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

let debug fmt = Logging.debug "xs_transport_xen" fmt

type t = {
	address: address;
    page: Cstruct.buf;
    port: int;
	c: unit Lwt_condition.t;
}

let domains : (int, t) Hashtbl.t = Hashtbl.create 128
let by_port : (int, t) Hashtbl.t = Hashtbl.create 128

let xenstored_proc_port = "/proc/xen/xsd_port"
let xenstored_proc_kva  = "/proc/xen/xsd_kva"

let read_port () =
	Lwt_io.with_file ~mode:Lwt_io.input xenstored_proc_port
		(fun ic ->
			lwt line = Lwt_io.read_line ic in
			return (int_of_string line)
		)

let map_page () =
	let fd = Unix.openfile xenstored_proc_kva [ Lwt_unix.O_RDWR ] 0o0 in
	try
		let page = Xenstore.map_fd fd 4096 in
		Unix.close fd;
		page
	with e ->
		debug "map_page error: %s" (Printexc.to_string e);
		Unix.close fd;
		raise e

let eventchn =
	let e = Xeneventchn.init () in

	let virq_thread () =
		let virq_port = Xeneventchn.bind_dom_exc_virq e in
		debug "Bound virq_port = %d" virq_port;
		let fd = Lwt_unix.of_unix_file_descr ~set_flags:false (Xeneventchn.fd e) in
		while_lwt true do
            lwt () = Lwt_unix.wait_read fd in
			let port = Xeneventchn.pending e in
			debug "Event on port %d" port;
			Xeneventchn.unmask e port;
			if Hashtbl.mem by_port port then begin
				let d = Hashtbl.find by_port port in
				debug "Waking domid %d" d.address.domid;
				Lwt_condition.signal d.c ()
			end;
			if port = virq_port then begin
				(* signal someone *)
				return ()
			end else return ()
		done in

	let rec wake_everyone_thread () =
		lwt () = Lwt_unix.sleep 5. in
		debug "heartbeat";
		Hashtbl.iter
			(fun domid t ->
				debug "Waking domid %d" domid;
				Lwt_condition.signal t.c ()
			) domains;
		wake_everyone_thread () in
	let (_: unit Lwt.t) = wake_everyone_thread () in

	let (_: unit Lwt.t) = virq_thread () in
	e


let create_dom0 () =
	debug "read_port";
	lwt remote_port = read_port () in
	debug "map_page";
	let page = map_page () in
	let port = Xeneventchn.bind_interdomain eventchn 0 remote_port in
	debug "create_dom0 remote_port = %d; port = %d" remote_port port;
	Xeneventchn.notify eventchn port;
	let d = {
		address = {
			domid = 0;
			mfn = Nativeint.zero;
			remote_port = remote_port;
		};
		page = page;
		port = port;
		c = Lwt_condition.create ();
	} in
	Hashtbl.add domains 0 d;
	Hashtbl.add by_port port d;
	return d

let create_domU address =
	lwt page = Xenstore.map_foreign address.domid address.mfn in
	let port = Xeneventchn.bind_interdomain eventchn address.domid address.remote_port in
	debug "create_domU remote_port = %d; port = %d" address.remote_port port;
	let d = {
		address = address;
		page = page;
		port = port;
		c = Lwt_condition.create ();
	} in
	Hashtbl.add domains address.domid d;
	Hashtbl.add by_port port d;
	return d

let rec read t buf ofs len =
	debug "read ofs=%d len=%d" ofs len;
	let n = Xenstore.unsafe_read t.page buf ofs len in
	if n = 0
	then begin
		debug "  0 bytes ready; blocking on port %d" t.port;
		lwt () = Lwt_condition.wait t.c in
		read t buf ofs len
	end else begin
		debug "  %d bytes read: [%s]" n (Junk.hexify (String.sub buf ofs n));
		Xeneventchn.notify eventchn t.port;
		return n
	end

let rec write t buf ofs len =
	debug "write ofs=%d len=%d: [%s]" ofs len (Junk.hexify (String.sub buf ofs len));
	let n = Xenstore.unsafe_write t.page buf ofs len in
	if n > 0 then Xeneventchn.notify eventchn t.port;
	if n < len then begin
		debug "  %d more bytes needed; blocking on port %d" (len - n) t.port;
		lwt () = Lwt_condition.wait t.c in
		write t buf (ofs + n) (len - n)
	end else return ()


let destroy t =
	Xeneventchn.unbind eventchn t.port;
	Hashtbl.remove domains t.address.domid;
	return ()

let address_of t =
	return (Xs_packet.Domain t.address.domid)

type server = address Lwt_stream.t

let listen () =
	return stream

let rec accept_forever stream process =
	debug "xen: accept forever";
	lwt address = Lwt_stream.next stream in
	debug "xen got domid %d" address.domid;
	lwt d = if address.domid = 0 then create_dom0 () else create_domU address in
	let (_: unit Lwt.t) = process d in
	accept_forever stream process
