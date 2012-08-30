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

let debug fmt = Logging.debug "server_unix" fmt

module UnixServer = Xs_server.Server(Xs_transport_unix)
module DomainServer = Xs_server.Server(Xs_transport_xen)

let syslog = Lwt_log.syslog ~facility:`Local3 ()

let rec logging_thread logger =
	lwt lines = Logging.get logger in
	lwt () = Lwt_list.iter_s
			(fun x ->
				lwt () = Lwt_log.log ~logger:syslog ~level:Lwt_log.Notice x in
				return ()
			) lines in
	logging_thread logger

let main () =
	debug "Unix xenstored starting";
	let (_: 'a) = logging_thread Logging.logger in
	let (_: 'a) = logging_thread Logging.access_logger in

	Arg.parse
		[ "-path", Arg.Set_string Xs_transport_unix.xenstored_socket, Printf.sprintf "Unix domain socket to listen on (default %s)" !Xs_transport_unix.xenstored_socket ]
		(fun _ -> ())
		"User-space xenstore service";
	let (a: unit Lwt.t) = UnixServer.serve_forever () in
	debug "Started server on unix domain socket";
	let (b: unit Lwt.t) = DomainServer.serve_forever () in
	debug "Started server on xen inter-domain transport";
	Introduce.(introduce { domid = 0; mfn = 0n; remote_port = 0 });
	debug "Introduced domain 0";
	lwt () = a in
	lwt () = b in
	return ()

let _ =
	Lwt_daemon.daemonize ();
	Lwt_main.run (main ())
