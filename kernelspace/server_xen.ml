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
open Xs_protocol
open Xenstore_server

let debug fmt = Logging.debug "server_xen" fmt
let warn  fmt = Logging.warn  "server_xen" fmt
let error fmt = Logging.error "server_xen" fmt

module DomainServer = Xs_server.Server(Xs_transport_domain)

let rec logging_thread logger =
	lwt lines = Logging.get logger in
	lwt () = Lwt_list.iter_s
			(fun x ->
				lwt () = OS.Console.log_s x in
				return ()
			) lines in
	logging_thread logger

let introduce_dom0 () =
	(* the cmd_line should have --event %d set by init-xenstore-domain.c *)
	let cmd_line = (OS.Start_info.((get ()).cmd_line)) in
	let bits = Junk.String.split ' ' cmd_line in
	let rec loop = function
		| "--event" :: e :: _ ->
			Some (int_of_string e)
		| [] ->
			None
		| _ :: rest ->
			loop rest in
	match loop bits with
	| None ->
		error "Failed to find --event <port> on the commandline: %s" cmd_line;
		()
	| Some port ->
		Introduce.(introduce { domid = 0; mfn = 0n; remote_port = port });
		debug "Introduced domain 0 with port = %d" port

let main () =
	debug "Mirage xenstored starting";
	let (_: 'a) = logging_thread Logging.logger in
	let (_: 'a) = logging_thread Logging.access_logger in

	let (a: unit Lwt.t) = DomainServer.serve_forever () in
	debug "Started server on xen inter-domain transport";

	introduce_dom0 ();
(*
	debug "getdomaininfo 0";
	begin match OS.Domctl.getdomaininfo 0 with
	| None ->
		debug "xc_domain_getinfo failed"
	| Some di ->
		let open OS.Domctl.Xen_domctl_getdomaininfo in
		debug "domain %d: dying = %b; shutdown = %b" di.domid di.dying di.shutdown
	end;
*)
	while_lwt true do
		debug "tick";
		OS.Time.sleep 5.
	done

let _ = OS.Main.run (main ())
