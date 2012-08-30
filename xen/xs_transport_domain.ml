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

type t = {
	address: address;
    page: Cstruct.buf;
    port: int;
	c: unit Lwt_condition.t;
	mutable closing: bool;
}

(* Thrown when an attempt is made to read or write to a closed ring *)
exception Ring_shutdown

let domains : (int, t) Hashtbl.t = Hashtbl.create 128
let by_port : (int, t) Hashtbl.t = Hashtbl.create 128

(* Handle the DOM_EXC VIRQ *)
let rec virq_thread port =
	lwt () = Activations.wait port in
	(* Check to see if any of our domains have shutdown *)
	begin match Sysctl.domain_infolist () with
	| None ->
		debug "xc_domain_getinfolist failed"
	| Some dis ->
		let open Domctl.Xen_domctl_getdomaininfo in
		debug "valid domain ids: [%s]" (String.concat ", " (List.fold_left (fun acc di -> string_of_int di.domid :: acc) [] dis));
		List.iter (fun di ->
			if di.dying || di.shutdown
			then debug "domid %d: %s%s%s" di.domid
				(if di.dying then "dying" else "")
				(if di.dying && di.shutdown then " and " else "")
				(if di.shutdown then "shutdown" else "")
		) dis;
		let dis_by_domid = Hashtbl.create 128 in
		List.iter (fun di -> Hashtbl.add dis_by_domid di.domid di) dis;
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
(*
		if release_domain
		then Connection.fire (Xs_packet.Op.Write, Store.Name.releaseDomain);                                               
*)
	end;
	virq_thread port

let (_: 'a Lwt.t) =
	let port = Evtchn.Virq.(bind Dom_exc) in
	debug "Bound DOM_EXC VIRQ to port %d" port;
	virq_thread port

let create_domain address =
	let page = Io_page.get () in
	match address.page with
	| Mfn _ ->
		error "Cannot use xc_map_foreign_range in a unpriviledged domain: cannot connect to domid %d" address.domid;
		None
	| Grant g ->
		begin match Gnttab.map_grant address.domid ~perm:Gnttab.RW g page with
		| Some h ->
			Hashtbl.replace grant_handles domid h;
			let port = Evtchn.bind_interdomain address.domid address.remote_port in
			let d = {
				address = address;
				page = page;
				port = port;
				c = Lwt_condition.create ();
				closing = false;
			} in
			Hashtbl.add domains address.domid d;
			Hashtbl.add by_port port d;
			Some d
		| None ->
			error "Failed to map grant reference: cannot connect to domid %d" address.domid;
			None
		end

let rec read t buf ofs len =
	if t.closing
	then fail Ring_shutdown
	else
		let n = Xenstore.unsafe_read t.page buf ofs len in
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
		let n = Xenstore.unsafe_write t.page buf ofs len in
		if n > 0 then Evtchn.notify t.port;
		if n < len then begin
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
		| [] -> [ "mfn"; "local-port"; "remote-port"; "closing"; "wakeup"; "request"; "response" ]
		| [ "request" ]
		| [ "response" ] -> [ "cons"; "prod"; "data" ]
		| _ -> []

	end in
	Some (module Interface: Namespace.IO)

