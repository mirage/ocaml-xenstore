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

open OS

open Lwt
open Gnt
open Xenstore_server
open Introduce

let debug fmt = Logging.debug "xs_transport_domain" fmt
let warn  fmt = Logging.warn  "xs_transport_domain" fmt
let error fmt = Logging.error "xs_transport_domain" fmt

type channel = {
	address: address;
	ring: Cstruct.t;
	port: Eventchn.t;
	c: unit Lwt_condition.t;
	mutable closing: bool;
}
type 'a t = 'a Lwt.t
let ( >>= ) m f = m >>= f
let return = return

(* Thrown when an attempt is made to read or write to a closed ring *)
exception Ring_shutdown

let domains : (int, channel) Hashtbl.t = Hashtbl.create 128
let threads : (Eventchn.t, unit Lwt.t) Hashtbl.t = Hashtbl.create 128

let grant_handles : (int, Gnt.Gnttab.Local_mapping.t) Hashtbl.t = Hashtbl.create 128

let interface = Gnttab.interface_open ()
let eventchn = Eventchn.init ()

(*
(* Handle the DOM_EXC VIRQ *)
let rec virq_thread port =
	lwt () = Activations.wait port in
	(* Check to see if any of our domains have shutdown *)
    (* It would be more efficient to call getdomaininfolist but only getdomaininfo
	   is permitted, and only then with an XSM policy. *)
	let open Domctl.Xen_domctl_getdomaininfo in
	(* Check whether getdomaininfo works first *)
	if Domctl.getdomaininfo 0 = None then begin
		warn "Fix the XSM policy so I can call getdomaininfo"
	end else begin
		let dis_by_domid = Hashtbl.create 128 in
		Hashtbl.iter
			(fun domid _ ->
				match Domctl.getdomaininfo domid with
				| None ->
					debug "getdomaininfo %d failed" domid
				| Some di ->
					if di.dying || di.shutdown
					then debug "domid %d: %s%s%s" di.domid
						(if di.dying then "dying" else "")
						(if di.dying && di.shutdown then " and " else "")
						(if di.shutdown then "shutdown" else "");
					Hashtbl.add dis_by_domid domid di
			) domains;
		(* Connections to domains which are missing or 'dying' should be closed *)
		let to_close = Hashtbl.fold (fun domid _ acc ->
			if not(Hashtbl.mem dis_by_domid domid) || (Hashtbl.find dis_by_domid domid).dying
			then domid :: acc else acc) domains [] in
		(* If any domain is missing, shutdown or dying then we should send @releaseDomain *)
		let release_domain = Hashtbl.fold (fun domid _ acc ->
			acc || (not(Hashtbl.mem dis_by_domid domid) ||
						(let di = Hashtbl.find dis_by_domid domid in
						 di.shutdown || di.dying))
		) domains false in
		(* Set the connections to "closing", wake up any readers/writers *)
		List.iter
			(fun domid ->
				debug "closing connection to domid: %d" domid;
				let t = Hashtbl.find domains domid in
				t.closing <- true;
				Lwt_condition.broadcast t.c ()
			) to_close;
		if release_domain
		then Connection.fire (Xs_protocol.Op.Write, Store.Name.releaseDomain);
	end;

	virq_thread port
*)
(*
let (_: 'a Lwt.t) =
	let port = Evtchn.Virq.(bind Dom_exc) in
	debug "Bound DOM_EXC VIRQ to port %d" port;
	virq_thread port
*)

cstruct xenstore_ring{
	uint8_t req[1024];
	uint8_t rsp[1024];
	uint32_t req_cons;
	uint32_t req_prod;
	uint32_t rsp_cons;
	uint32_t rsp_prod
} as little_endian

let create_domain address =
	match Gnttab.map interface { Gnttab.domid = address.domid; ref = Gnt.xenstore } true with
	| Some h ->
		let page = Cstruct.of_bigarray (Gnttab.Local_mapping.to_buf h) in
		Hashtbl.replace grant_handles address.domid h;
		let port = Eventchn.bind_interdomain eventchn address.domid address.remote_port in
		let d = {
			address = address;
			ring = page;
			port = port;
			c = Lwt_condition.create ();
			closing = false;
		} in
		let (background_thread: unit Lwt.t) =
			while_lwt true do
				debug "Waiting for signal from domid %d on local port %d (remote port %d)" address.domid (Eventchn.to_int port) address.remote_port;
				lwt () = Activations.wait port in
				debug "Waking domid %d" d.address.domid;

				debug "req_cons = %ld; req_prod = %ld; rsp_cons = %ld; rsp_prod = %ld"
					(get_xenstore_ring_req_cons page)
					(get_xenstore_ring_req_prod page)
					(get_xenstore_ring_rsp_cons page)
					(get_xenstore_ring_rsp_prod page);

				Lwt_condition.broadcast d.c ();
				return ()
 			done >> return () in

		Hashtbl.add domains address.domid d;
		Hashtbl.add threads port background_thread;
		Some d
	| None ->
		error "Failed to map grant reference: cannot connect to domid %d" address.domid;
		None
 
let rec read t buf ofs len =
	debug "read size=%d ofs=%d len=%d" (String.length buf) ofs len;
	if t.closing then begin
		debug "read failing: Ring_shutdown";
		fail Ring_shutdown
	end else
		let n = Xenstore_ring.Ring.Back.unsafe_read t.ring buf ofs len in
		if n = 0
		then begin
			debug "read of 0, blocking";
			lwt () = Lwt_condition.wait t.c in
			debug "reader woken up";
			read t buf ofs len
		end else begin
			debug "read %d" n;
			Eventchn.notify eventchn t.port;
			return n
		end

let rec write t buf ofs len =
	debug "write size=%d ofs=%d len=%d" (String.length buf) ofs len;
	if t.closing then begin
		debug "write failing: Ring_shutdown";
		fail Ring_shutdown
	end else
		let n = Xenstore_ring.Ring.Back.unsafe_write t.ring buf ofs len in
		if n > 0 then Eventchn.notify eventchn t.port;
		if n < len then begin
			debug "write %d < %d blocking" n len;
			lwt () = Lwt_condition.wait t.c in
			debug "writer woken up";
			write t buf (ofs + n) (len - n)
		end else return ()

let destroy t =
	Eventchn.unbind eventchn t.port;
	if Hashtbl.mem grant_handles t.address.domid then begin
		let h = Hashtbl.find grant_handles t.address.domid in
		begin
			try
				Gnttab.unmap_exn interface h
			with _ ->
				error "Failed to unmap grant for domid: %d" t.address.domid;
		end;
		Hashtbl.remove grant_handles t.address.domid
	end;
	if Hashtbl.mem threads t.port then begin
		let th = Hashtbl.find threads t.port in
		Lwt.cancel th;
		Hashtbl.remove threads t.port
	end;
	Hashtbl.remove domains t.address.domid;
	return ()

let address_of t =
	return (Xs_protocol.Domain t.address.domid)

type server = address Lwt_stream.t

let listen () =
	return stream

let rec accept_forever stream process =
	lwt address = Lwt_stream.next stream in
	begin match create_domain address with
		| Some d ->
			let (_: unit Lwt.t) = process d in
			debug "Connection created"
		 | None ->
			error "Failed to create connection"
	end;
	accept_forever stream process

let namespace_of t =
	let module Interface = struct
		include Namespace.Unsupported

	let read _ (perms: Perms.t) (path: Store.Path.t) =
		Perms.has perms Perms.CONFIGURE;
		match Store.Path.to_string_list path with
		| [] -> ""
		| [ "mfn" ] -> Nativeint.to_string t.address.mfn
		| [ "local-port" ] -> string_of_int (Eventchn.to_int t.port)
		| [ "remote-port" ] -> string_of_int t.address.remote_port
		| [ "closing" ] -> string_of_bool t.closing
		| [ "wakeup" ]
		| [ "request" ]
		| [ "response" ] -> ""
(*
		| [ "request"; "cons" ] -> string_of_int (Xenstore.((get_ring_state t.page).request.cons))
		| [ "request"; "prod" ] -> string_of_int (Xenstore.((get_ring_state t.page).request.prod))
		| [ "request"; "data" ] -> string_of_int (Xenstore.((get_ring_state t.page).request.data))
		| [ "response"; "cons" ] -> string_of_int (Xenstore.((get_ring_state t.page).response.cons))
		| [ "response"; "prod" ] -> string_of_int (Xenstore.((get_ring_state t.page).response.prod))
		| [ "response"; "data" ] -> string_of_int (Xenstore.((get_ring_state t.page).response.data))
*)
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
		| [] -> [ "mfn"; "local-port"; "remote-port"; "closing"; "wakeup"; "request"; "response" ]
		| [ "request" ]
		| [ "response" ] -> [ "cons"; "prod"; "data" ]
		| _ -> []

	end in
	Some (module Interface: Namespace.IO)

