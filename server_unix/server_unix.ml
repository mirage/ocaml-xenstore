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
open Xs_packet

let debug = Logging.debug "server_unix"

let string_of_date () =
	let time = Unix.gettimeofday () in
	let tm = Unix.gmtime time in
	let msec = time -. (floor time) in
	Printf.sprintf "%d%.2d%.2dT%.2d:%.2d:%.2d.%.3dZ"
		(1900 + tm.Unix.tm_year) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
		tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
		(int_of_float (1000.0 *. msec))

module UnixServer = Xs_server.Server(Xs_transport_unix)
module DomainServer = Xs_server.Server(Xs_transport_xen)

let rec logging_thread logger =
	lwt lines = Logging.get logger in
	lwt () = Lwt_list.iter_s (Lwt_io.write_line Lwt_io.stdout) lines in
	logging_thread logger

let main () =
	Logging.string_of_date := string_of_date;
	debug "Unix xenstored starting";
	let (_: 'a) = logging_thread Logging.logger in
	let (_: 'a) = logging_thread Logging.access_logger in

	Arg.parse
		[ "-path", Arg.Set_string Xs_transport_unix.xenstored_socket, Printf.sprintf "Unix domain socket to listen on (default %s)" !Xs_transport_unix.xenstored_socket ]
		(fun _ -> ())
		"User-space xenstore service";
	let (a: unit Lwt.t) = UnixServer.serve_forever () in
	let (b: unit Lwt.t) = DomainServer.serve_forever () in
	lwt () = a in
	lwt () = b in
	return ()

let _ =
	Lwt_main.run (main ())
