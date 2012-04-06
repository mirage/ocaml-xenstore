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

module Server = Xs_server.Server(Xs_transport_unix)

let main () =
  Arg.parse
    [ "-path", Arg.Set_string Xs_transport_unix.xenstored_socket, Printf.sprintf "Unix domain socket to listen on (default %s)" !Xs_transport_unix.xenstored_socket ]
    (fun _ -> ())
    "User-space xenstore service";
  Server.serve_forever ()

let _ =
  Lwt_main.run (main ())
