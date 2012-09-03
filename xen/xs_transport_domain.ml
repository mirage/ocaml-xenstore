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

open Introduce

let debug fmt = Logging.debug "xs_transport_domain" fmt
let warn  fmt = Logging.warn  "xs_transport_domain" fmt
let error fmt = Logging.error "xs_transport_domain" fmt

type t = {
	address: address;
	ring: Ring.Xenstore.t;
    port: int;
	c: unit Lwt_condition.t;
	mutable closing: bool;
}

(* Thrown when an attempt is made to read or write to a closed ring *)
exception Ring_shutdown

let domains : (int, t) Hashtbl.t = Hashtbl.create 128
let threads : (int, unit Lwt.t) Hashtbl.t = Hashtbl.create 128

let grant_handles : (int, Gnttab.h) Hashtbl.t = Hashtbl.create 128

(* Handle the DOM_EXC VIRQ *)
let rec virq_thread port =
	lwt () = Activations.wait port in
	(* Check to see if any of our domains have shutdown *)
    (* It would be more efficient to call getdomaininfolist but only getdomaininfo
	   is permitted, and only then with an XSM policy. *)
	let open Domctl.Xen_domctl_getdomaininfo in
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

	virq_thread port

let (_: 'a Lwt.t) =
	let port = Evtchn.Virq.(bind Dom_exc) in
	debug "Bound DOM_EXC VIRQ to port %d" port;
	virq_thread port

let create_domain address =
	let page = Io_page.get () in
	match Gnttab.map_grant ~domid:address.domid ~perm:Gnttab.RW Gnttab.xenstore page with
	| Some h ->
		Hashtbl.replace grant_handles address.domid h;
		let port = Evtchn.bind_interdomain address.domid address.remote_port in
		let d = {
			address = address;
			ring = Ring.Xenstore.of_buf page;
			port = port;
			c = Lwt_condition.create ();
			closing = false;
		} in
		let (background_thread: unit Lwt.t) =
			while_lwt true do
				lwt () = Activations.wait port in
				debug "Waking domid %d" d.address.domid;
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
	if t.closing
	then fail Ring_shutdown
	else
		let n = Ring.Xenstore.unsafe_read t.ring buf (* ofs *) len in
		if n = 0
		then begin
			lwt () = Lwt_condition.wait t.c in
			read t buf ofs len
		end else begin
			Evtchn.notify t.port;
			return n
		end

let rec write t buf ofs len =
	if t.closing
	then fail Ring_shutdown
	else
		let n = Ring.Xenstore.unsafe_write t.ring buf (* ofs *) len in
		if n > 0 then Evtchn.notify t.port;
		if n < len then begin
			lwt () = Lwt_condition.wait t.c in
			write t buf (ofs + n) (len - n)
		end else return ()

let destroy t =
	Evtchn.unbind t.port;
	if Hashtbl.mem grant_handles t.address.domid then begin
		let h = Hashtbl.find grant_handles t.address.domid in
		if not(Gnttab.unmap_grant h)
		then error "Failed to unmap grant for domid: %d" t.address.domid;
		Hashtbl.remove grant_handles t.address.domid
	end;
	if Hashtbl.mem threads t.address.domid then begin
		let th = Hashtbl.find threads t.address.domid in
		Lwt.cancel th;
		Hashtbl.remove threads t.address.domid
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
			()
		 | None ->
			()
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
		| [ "local-port" ] -> string_of_int t.port
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

