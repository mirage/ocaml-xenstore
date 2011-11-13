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

module type TRANSPORT = sig
  type t
  val create: unit -> t Lwt.t
  val destroy: t -> unit Lwt.t
  val read: t -> string -> int -> int -> int Lwt.t
  val write: t -> string -> int -> int -> int Lwt.t
end

module Unix_domain_socket = struct
  let xenstored_socket = "/var/run/xenstored/socket"
  type t = Lwt_unix.file_descr
  let create () =
    let sockaddr = Lwt_unix.ADDR_UNIX(xenstored_socket) in
    let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
    lwt () = Lwt_unix.connect fd sockaddr in
    return fd
  let destroy fd = Lwt_unix.close fd
  let read = Lwt_unix.read
  let write = Lwt_unix.write
end

type watch_queue = {
  events: string Queue.t;
  c: unit Lwt_condition.t;
  m: Lwt_mutex.t;
}

module Client = functor(T: TRANSPORT) -> struct

  (* Represents a single acive connection to a server *)
  type client = {
    transport: T.t;
    mutable incoming_pkt: Parser.parse; (* incrementally parses the next packet *)
    outgoing_mutex: Lwt_mutex.t;        (* held to serialise outgoing packets *)
    rid_to_wakeup: (int32, t Lwt.u) Hashtbl.t;
    mutable dispatcher_thread: unit Lwt.t;
    mutable dispatcher_shutting_down: bool;
    watchevents: (Token.t, watch_queue) Hashtbl.t;
  }

  exception Unknown_xenstore_operation of int32
  exception Response_parser_failed
  exception Malformed_watch_event
  exception Unexpected_rid of int32
  exception Dispatcher_failed

  (* [recv_one client] returns a single Packet, or fails *)
  let rec recv_one t =
    let open Parser in match Parser.state t.incoming_pkt with
    | Packet pkt ->
      t.incoming_pkt <- start ();
      return pkt
    | Need_more_data x ->
      let buf = String.make x '\000' in
      lwt n = T.read t.transport buf 0 x in
      let fragment = String.sub buf 0 n in
      t.incoming_pkt <- input t.incoming_pkt fragment;
      recv_one t
    | Unknown_operation x -> raise_lwt (Unknown_xenstore_operation x)
    | Parser_failed -> raise_lwt Response_parser_failed

  (* [send_one client pkt] sends [pkt] and returns (), or fails *)
  let send_one t request =
    let req = to_string request in
    lwt n = Lwt_mutex.with_lock t.outgoing_mutex
        (fun () -> T.write t.transport req 0 (String.length req)) in
    return ()

  let rec dispatcher t =
    try_lwt
      lwt pkt = recv_one t in
      begin match get_ty pkt with
        | Op.Watchevent  ->
          lwt () = begin match Unmarshal.list pkt with
            | Some [path; token] ->
              let token = Token.of_string token in
              (* We may get old watches: silently drop these *)
              if Hashtbl.mem t.watchevents token then begin
                let wq = Hashtbl.find t.watchevents token in
	        lwt () = Lwt_mutex.with_lock wq.m
	          (fun () ->
	            Queue.push path wq.events;
	            Lwt_condition.signal wq.c ();
	            return ()
	          ) in
	        dispatcher t
              end else dispatcher t
	    | _ ->
              raise_lwt Malformed_watch_event
          end in
          dispatcher t
        | _ ->
          let rid = get_rid pkt in
          if not(Hashtbl.mem t.rid_to_wakeup rid)
	  then raise_lwt (Unexpected_rid rid)
	  else begin
            Lwt.wakeup (Hashtbl.find t.rid_to_wakeup rid) pkt;
            dispatcher t
	  end
      end
   with e ->
     t.dispatcher_shutting_down <- true; (* no more hashtable entries after this *)
     (* all blocking threads are failed with our exception *)
     Hashtbl.iter (fun _ u -> Lwt.wakeup_later_exn u e) t.rid_to_wakeup;
     raise_lwt e

  let make () =
    lwt transport = T.create () in
    let t = {
      transport = transport;
      incoming_pkt = Parser.start ();
      outgoing_mutex = Lwt_mutex.create ();
      rid_to_wakeup = Hashtbl.create 10;
      dispatcher_thread = return ();
      dispatcher_shutting_down = false;
      watchevents = Hashtbl.create 10;
    } in
    t.dispatcher_thread <- dispatcher t;
    return t

  let rpc (tid, client) request unmarshal =
    let request = match request tid with Some x -> x | None -> failwith "bad request" in
    let rid = get_rid request in
    let t, u = wait () in
    if client.dispatcher_shutting_down
    then raise_lwt Dispatcher_failed
    else begin
      Hashtbl.add client.rid_to_wakeup rid u;
      lwt () = send_one client request in
      lwt res = t in
      Hashtbl.remove client.rid_to_wakeup rid;
      try_lwt
        return (response "" request res unmarshal)
 end

  let directory (tid, client) path = rpc (tid, client) (Request.directory path) Unmarshal.list
  let read (tid, client) path = rpc (tid, client) (Request.read path) Unmarshal.string

  let with_xs client f = f (0l, client)

  let rec with_xst client f =
    lwt tid = rpc (0l, client) (fun _ -> Request.transaction_start ()) Unmarshal.int32 in
    lwt result = f (tid, client) in
    try_lwt
      lwt res' = rpc (tid, client) (Request.transaction_end true) Unmarshal.string in
      if res' = "OK" then return result else raise_lwt (Error (Printf.sprintf "Unexpected transaction result: %s" res'))
    with Eagain ->
      with_xst client f
end

module Test = Client(Unix_domain_socket)
open Test

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

let _ = Lwt_main.run (test ())


