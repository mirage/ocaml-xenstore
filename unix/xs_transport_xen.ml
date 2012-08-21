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
	mutable shutdown: bool;
}

(* Thrown when an attempt is made to read or write to a closed ring *)
exception Ring_shutdown

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
	let e = Xenstore.xc_evtchn_open () in

	let virq_thread () =
		let virq_port = Xenstore.xc_evtchn_bind_virq_dom_exc e in
		debug "Bound virq_port = %d" virq_port;
		let fd = Lwt_unix.of_unix_file_descr ~set_flags:false (Xenstore.xc_evtchn_fd e) in
		while_lwt true do
            lwt () = Lwt_unix.wait_read fd in
			let port = Xenstore.xc_evtchn_pending e in
			debug "Event on port %d: %s" port
				(if port = virq_port then "DOM_ESC VIRQ"
				else if Hashtbl.mem by_port port
				then Printf.sprintf "ring for domid %d" (Hashtbl.find by_port port).address.domid
				else "UNKNOWN");

			Xenstore.xc_evtchn_unmask e port;
			if Hashtbl.mem by_port port then begin
				let d = Hashtbl.find by_port port in
				debug "Waking domid %d" d.address.domid;
				Lwt_condition.broadcast d.c ()
			end;
			if port = virq_port then begin
				(* Check to see if any of our domains have shutdown *)
				lwt dis = Xenstore.domain_infolist () in
				begin match dis with
					| None ->
						debug "xc_domain_getinfolist failed"
					| Some dis ->
						debug "valid domain ids: [%s]" (String.concat ", " (List.fold_left (fun acc di -> string_of_int di.Xenstore.domid :: acc) [] dis));
						List.iter (fun di ->
							if di.Xenstore.dying || di.Xenstore.shutdown
							then debug "domid %d: %s%s%s" di.Xenstore.domid
								(if di.Xenstore.dying then "dying" else "")
								(if di.Xenstore.dying && di.Xenstore.shutdown then " and " else "")
								(if di.Xenstore.shutdown then "shutdown" else "")
						) dis;
						let dis_by_domid = Hashtbl.create 128 in
						List.iter (fun di -> Hashtbl.add dis_by_domid di.Xenstore.domid di) dis;
						(* Connections to domains which are missing or 'dying' should be closed *)
						let to_close = Hashtbl.fold (fun domid _ acc ->
							if not(Hashtbl.mem dis_by_domid domid) || (Hashtbl.find dis_by_domid domid).Xenstore.dying
							then domid :: acc else acc) domains [] in
						(* If any domain is missing, shutdown or dying then we should send @releaseDomain *)
						let release_domain = Hashtbl.fold (fun domid _ acc ->
							acc || (not(Hashtbl.mem dis_by_domid domid) ||
								(let di = Hashtbl.find dis_by_domid domid in
								di.Xenstore.shutdown || di.Xenstore.dying))
						) domains false in
						(* Set the connections to "closing", wake up any readers/writers *)
						List.iter
							(fun domid ->
								debug "closing connection to domid: %d" domid;
								let t = Hashtbl.find domains domid in
								t.shutdown <- true;
								Lwt_condition.broadcast t.c ()
							) to_close;
						if release_domain
						then Connection.fire (Xs_packet.Op.Write, Store.Name.releaseDomain);                                               
				end;
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
	let port = Xenstore.xc_evtchn_bind_interdomain eventchn 0 remote_port in
	debug "create_dom0 remote_port = %d; port = %d" remote_port port;
	Xenstore.xc_evtchn_notify eventchn port;
	let d = {
		address = {
			domid = 0;
			mfn = Nativeint.zero;
			remote_port = remote_port;
		};
		page = page;
		port = port;
		c = Lwt_condition.create ();
		shutdown = false;
	} in
	Hashtbl.add domains 0 d;
	Hashtbl.add by_port port d;
	return d

let create_domU address =
	lwt page = Xenstore.map_foreign address.domid address.mfn in
	let port = Xenstore.xc_evtchn_bind_interdomain eventchn address.domid address.remote_port in
	debug "create_domU remote_port = %d; port = %d" address.remote_port port;
	let d = {
		address = address;
		page = page;
		port = port;
		c = Lwt_condition.create ();
		shutdown = false;
	} in
	Hashtbl.add domains address.domid d;
	Hashtbl.add by_port port d;
	return d

let rec read t buf ofs len =
	debug "read ofs=%d len=%d" ofs len;
	if t.shutdown
	then fail Ring_shutdown
	else
		let n = Xenstore.unsafe_read t.page buf ofs len in
		if n = 0
		then begin
			debug "  0 bytes ready; blocking on port %d" t.port;
			lwt () = Lwt_condition.wait t.c in
			read t buf ofs len
		end else begin
			debug "  %d bytes read: [%s]" n (Junk.hexify (String.sub buf ofs n));
			Xenstore.xc_evtchn_notify eventchn t.port;
			return n
		end

let rec write t buf ofs len =
	debug "write ofs=%d len=%d: [%s]" ofs len (Junk.hexify (String.sub buf ofs len));
	if t.shutdown
	then fail Ring_shutdown
	else
		let n = Xenstore.unsafe_write t.page buf ofs len in
		if n > 0 then Xenstore.xc_evtchn_notify eventchn t.port;
		if n < len then begin
			debug "  %d more bytes needed; blocking on port %d" (len - n) t.port;
			lwt () = Lwt_condition.wait t.c in
			write t buf (ofs + n) (len - n)
		end else return ()

let destroy t =
	Xenstore.xc_evtchn_unbind eventchn t.port;
	Xenstore.unmap_foreign t.page;
	Hashtbl.remove domains t.address.domid;
	Hashtbl.remove by_port t.port;
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

let namespace_of t =
	let module Interface = struct
		include Namespace.Unsupported

	let read _ (perms: Perms.t) (path: Store.Path.t) =
		Perms.has perms Perms.CONFIGURE;
		match Store.Path.to_string_list path with
		| [] -> ""
		| [ "mfn" ] -> Nativeint.to_string t.address.mfn
		| [ "local-port" ] -> string_of_int t.port
		| [ "remote-port" ] -> string_of_int t.address.remote_port
		| [ "shutdown" ] -> string_of_bool t.shutdown
		| [ "wakeup" ]
		| [ "request" ]
		| [ "response" ] -> ""
		| [ "request"; "cons" ] -> string_of_int (Xenstore.((get_ring_state t.page).request.cons))
		| [ "request"; "prod" ] -> string_of_int (Xenstore.((get_ring_state t.page).request.prod))
		| [ "request"; "data" ] -> string_of_int (Xenstore.((get_ring_state t.page).request.data))
		| [ "response"; "cons" ] -> string_of_int (Xenstore.((get_ring_state t.page).response.cons))
		| [ "response"; "prod" ] -> string_of_int (Xenstore.((get_ring_state t.page).response.prod))
		| [ "response"; "data" ] -> string_of_int (Xenstore.((get_ring_state t.page).response.data))
		| _ -> Store.Path.doesnt_exist path

	let write _ _ perms path v =
		Perms.has perms Perms.CONFIGURE;
		match Store.Path.to_string_list path with
		| [ "wakeup" ] ->
			Lwt_condition.broadcast t.c ()
		| _ -> raise Perms.Permission_denied

	let exists t perms path = try ignore(read t perms path); true with Store.Path.Doesnt_exist _ -> false

	let list t perms path =
		Perms.has perms Perms.CONFIGURE;
		match Store.Path.to_string_list path with
		| [] -> [ "mfn"; "local-port"; "remote-port"; "shutdown"; "wakeup"; "request"; "response" ]
		| [ "request" ]
		| [ "response" ] -> [ "cons"; "prod"; "data" ]
		| _ -> []

	end in
	Some (module Interface: Namespace.IO)

