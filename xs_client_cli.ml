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
module Client = Xs_client.Client(Xs_transport_unix)
open Client

let test () =
  lwt client = make () in
  with_xst client
    (fun xs ->
      lwt all = directory xs "/" in
      List.iter print_endline all;
      lwt x = read xs "/squeezed/pid" in
      print_endline x;
      return ()
    )
  >>
  wait client
    (fun xs ->
      try_lwt
         lwt _ = read xs "/foobar" in
         lwt _ = read xs "/waz" in
         return ()
      with (Enoent _) -> fail Eagain
    )


let _ = Lwt_main.run (test ())
