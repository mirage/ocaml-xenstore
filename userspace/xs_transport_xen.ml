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

type 'a t = 'a Lwt.t
let return x = return x
let ( >>= ) m f = m >>= f

open Xenstore_server.Introduce

let debug fmt = Xenstore_server.Logging.debug "xs_transport_xen" fmt
let error fmt = Xenstore_server.Logging.error "xs_transport_xen" fmt

type channel = {
	address: address;
	page: Xenstore.buf;
	ring: Cstruct.t;
	port: int;
	c: unit Lwt_condition.t;
	mutable shutdown: bool;
}

(* Thrown when an attempt is made to read or write to a closed ring *)
exception Ring_shutdown

let domains : (int, channel) Hashtbl.t = Hashtbl.create 128
let by_port : (int, channel) Hashtbl.t = Hashtbl.create 128

let xenstored_proc_port = "/proc/xen/xsd_port"
let xenstored_proc_kva  = "/proc/xen/xsd_kva"

let read_port () =
  try_lwt
    Lwt_io.with_file ~mode:Lwt_io.input xenstored_proc_port
      (fun ic ->
        lwt line = Lwt_io.read_line ic in
        return (int_of_string line)
      )
  with Unix.Unix_error(Unix.EACCES, _, _) as e->
    error "Failed to open %s (EACCES)" xenstored_proc_port;
    error "Ensure this program is running as root and try again.";
    fail e
  | Unix.Unix_error(Unix.ENOENT, _, _) as e ->
    error "Failed to open %s (ENOENT)" xenstored_proc_port;
    error "Ensure this system is running xen and try again.";
    fail e

let map_page () =
	let fd = Unix.openfile xenstored_proc_kva [ Lwt_unix.O_RDWR ] 0o0 in
	let page_opt = Xenstore.map_fd fd 4096 in
	Unix.close fd;
	page_opt

let open_eventchn () =
	let e = match Xenstore.xc_evtchn_open () with
		| None -> failwith "xc_evtchn_open failed"
		| Some e -> e in

	let virq_thread () =
		let virq_port = match Xenstore.xc_evtchn_bind_virq_dom_exc e with
			| Some x -> x
			| None ->
				debug "Failed to bind VIRQ DOM_EXC";
				failwith "DOM_EXC VIRQ" in
		debug "Bound virq_port = %d" virq_port;
		let fd = match Xenstore.xc_evtchn_fd e with
			| Some x -> x
			| None ->
				debug "Failed to extract evtchn fd";
				failwith "xc_evtchn_fd" in
		let fd = Lwt_unix.of_unix_file_descr ~set_flags:false fd in
		while_lwt true do
            lwt () = Lwt_unix.wait_read fd in
			let port = match Xenstore.xc_evtchn_pending e with
				| Some x -> x
				| None ->
					debug "xc_evtchn_pending failed";
					failwith "xc_evtchn_pending" in
(*
			debug "Event on port %d: %s" port
				(if port = virq_port then "DOM_ESC VIRQ"
				else if Hashtbl.mem by_port port
				then Printf.sprintf "ring for domid %d" (Hashtbl.find by_port port).address.domid
				else "UNKNOWN");
*)
			Xenstore.xc_evtchn_unmask e port;
			if Hashtbl.mem by_port port then begin
				let d = Hashtbl.find by_port port in
(*				debug "Waking domid %d" d.address.domid; *)
				Lwt_condition.broadcast d.c ()
			end;
			if port = virq_port then begin
				(* Check to see if any of our domains have shutdown *)
				lwt dis = Xenstore.domain_infolist () in
				begin match dis with
					| None ->
						debug "xc_domain_getinfolist failed"
					| Some dis ->
(*						debug "valid domain ids: [%s]" (String.concat ", " (List.fold_left (fun acc di -> string_of_int di.Xenstore.domid :: acc) [] dis)); *)
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
						then Xenstore_server.Connection.fire (Xs_protocol.Op.Write, Xenstore_server.Store.Name.releaseDomain);                                               
				end;
				return ()
			end else return ()
		done in

	let (_: unit Lwt.t) = virq_thread () in
	e

let singleton_eventchn = ref None
let get_eventchn () = match !singleton_eventchn with
  | Some e -> e
  | None ->
    let e = open_eventchn () in
    singleton_eventchn := Some e;
    e

let create_dom0 () =
	lwt remote_port = read_port () in
	let eventchn = get_eventchn () in
	match map_page () with
		| Some page ->
			let port = match Xenstore.xc_evtchn_bind_interdomain eventchn 0 remote_port with
				| Some x -> x
				| None ->
					debug "xc_evtchn_bind_interdomain failed";
					failwith "xc_evtchn_bind_interdomain" in
			Xenstore.xc_evtchn_notify eventchn port;
			let d = {
				address = {
					domid = 0;
					mfn = Nativeint.zero;
					remote_port = remote_port;
				};
				page = page;
				ring = Cstruct.of_bigarray page;
				port = port;
				c = Lwt_condition.create ();
				shutdown = false;
			} in
			Hashtbl.add domains 0 d;
			Hashtbl.add by_port port d;
			return (Some d)
		| None ->
			debug "failed to create connection to dom0";
			return None

let create_domU address =
	lwt page = Xenstore.map_foreign address.domid address.mfn in
	let eventchn = get_eventchn () in
	let port = match Xenstore.xc_evtchn_bind_interdomain eventchn address.domid address.remote_port with
		| Some x -> x
		| None ->
			debug "xc_evtchn_bind_interdomain failed";
			failwith "xc_evtchn_bind_interdomain" in
	let d = {
		address = address;
		page = page;
		ring = Cstruct.of_bigarray page;
		port = port;
		c = Lwt_condition.create ();
		shutdown = false;
	} in
	Hashtbl.add domains address.domid d;
	Hashtbl.add by_port port d;
	return (Some d)

let rec read t buf ofs len =
	if t.shutdown
	then fail Ring_shutdown
	else
		let n = Xenstore_ring.Ring.Back.unsafe_read t.ring buf ofs len in
		if n = 0
		then begin
			lwt () = Lwt_condition.wait t.c in
			read t buf ofs len
		end else begin
			let eventchn = get_eventchn () in
			Xenstore.xc_evtchn_notify eventchn t.port;
			return n
		end

let rec write t buf ofs len =
	if t.shutdown
	then fail Ring_shutdown
	else
		let n = Xenstore_ring.Ring.Back.unsafe_write t.ring buf ofs len in
		let eventchn = get_eventchn () in
		if n > 0 then Xenstore.xc_evtchn_notify eventchn t.port;
		if n < len then begin
			lwt () = Lwt_condition.wait t.c in
			write t buf (ofs + n) (len - n)
		end else return ()

let destroy t =
	let eventchn = get_eventchn () in
	Xenstore.xc_evtchn_unbind eventchn t.port;
	Xenstore.unmap_foreign t.page;
	Hashtbl.remove domains t.address.domid;
	Hashtbl.remove by_port t.port;
	return ()

let address_of t =
	return (Xs_protocol.Domain t.address.domid)

type server = address Lwt_stream.t

let listen () =
	return stream

let rec accept_forever stream process =
	lwt address = Lwt_stream.next stream in
	lwt d = if address.domid = 0 then create_dom0 () else create_domU address in
	begin match d with
		| Some d ->
			let (_: unit Lwt.t) = process d in
			()
		 | None ->
			()
	end;
	accept_forever stream process

let namespace_of t =
	let module Interface = struct
		include Xenstore_server.Namespace.Unsupported

	let read _ (perms: Xenstore_server.Perms.t) (path: Xenstore_server.Store.Path.t) =
		Xenstore_server.Perms.has perms Xenstore_server.Perms.CONFIGURE;
		let pairs = Xenstore_ring.Ring.to_debug_map t.ring in
		match Xenstore_server.Store.Path.to_string_list path with
		| [] -> ""
		| [ "mfn" ] -> Nativeint.to_string t.address.mfn
		| [ "local-port" ] -> string_of_int t.port
		| [ "remote-port" ] -> string_of_int t.address.remote_port
		| [ "shutdown" ] -> string_of_bool t.shutdown
		| [ "wakeup" ]
		| [ "request" ]
		| [ "response" ] -> ""
		| [ x ] when List.mem_assoc x pairs -> List.assoc x pairs
		| _ -> Xenstore_server.Store.Path.doesnt_exist path

	let write _ _ perms path v =
		Xenstore_server.Perms.has perms Xenstore_server.Perms.CONFIGURE;
		match Xenstore_server.Store.Path.to_string_list path with
		| [ "wakeup" ] ->
			Lwt_condition.broadcast t.c ()
		| _ -> raise Xenstore_server.Perms.Permission_denied

	let exists t perms path = try ignore(read t perms path); true with Xenstore_server.Store.Path.Doesnt_exist _ -> false

	let list t perms path =
		Xenstore_server.Perms.has perms Xenstore_server.Perms.CONFIGURE;
		match Xenstore_server.Store.Path.to_string_list path with
		| [] -> [ "mfn"; "local-port"; "remote-port"; "shutdown"; "wakeup"; "request"; "response" ]
		| [ "request" ]
		| [ "response" ] -> [ "cons"; "prod"; "data" ]
		| _ -> []

	end in
	Some (module Interface: Xenstore_server.Namespace.IO)

