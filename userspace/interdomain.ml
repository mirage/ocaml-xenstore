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
open Xenstored

type 'a t = 'a Lwt.t
let return x = return x
let ( >>= ) m f = m >>= f

open Introduce

let debug fmt = Logging.debug "xs_transport_xen" fmt
let error fmt = Logging.error "xs_transport_xen" fmt

type channel = {
  address: address;
  page: Io_page.t;
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
  let page_opt = Domains.map_fd fd 4096 in
  Unix.close fd;
  page_opt

let virq_thread () =
  let eventchn = Eventchn.init () in
  let virq_port = Eventchn.bind_dom_exc_virq eventchn in
  debug "Bound virq_port = %d" (Eventchn.to_int virq_port);
  let rec loop from =
    (* Check to see if any of our domains have shutdown *)
    let dis = Domains.list () in
(*						debug "valid domain ids: [%s]" (String.concat ", " (List.fold_left (fun acc di -> string_of_int di.Xenstore.domid :: acc) [] dis)); *)
    List.iter (fun di ->
      if di.Domains.dying || di.Domains.shutdown
      then debug "domid %d: %s%s%s" di.Domains.domid
        (if di.Domains.dying then "dying" else "")
        (if di.Domains.dying && di.Domains.shutdown then " and " else "")
        (if di.Domains.shutdown then "shutdown" else "")
      ) dis;
      let dis_by_domid = Hashtbl.create 128 in
      List.iter (fun di -> Hashtbl.add dis_by_domid di.Domains.domid di) dis;
        (* Connections to domains which are missing or 'dying' should be closed *)
        let to_close = Hashtbl.fold (fun domid _ acc ->
          if not(Hashtbl.mem dis_by_domid domid) || (Hashtbl.find dis_by_domid domid).Domains.dying
          then domid :: acc else acc) domains [] in
        (* If any domain is missing, shutdown or dying then we should send @releaseDomain *)
        let release_domain = Hashtbl.fold (fun domid _ acc ->
          acc || (not(Hashtbl.mem dis_by_domid domid) ||
        (let di = Hashtbl.find dis_by_domid domid in
         di.Domains.shutdown || di.Domains.dying))
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
      then Connection.fire (Xs_protocol.Op.Write, Store.Name.releaseDomain);
    lwt after = Unix_activations.after virq_port from in
    loop after in
  loop Unix_activations.program_start

let service_domain d =
  let rec loop from =
    Lwt_condition.broadcast d.c ();
    lwt after = Unix_activations.after (Eventchn.of_int d.port) from in
    if d.shutdown
    then return ()
    else loop after in
  loop Unix_activations.program_start

let create_dom0 () =
  lwt remote_port = read_port () in
  let eventchn = Eventchn.init () in
  let page = map_page () in
  let port = Eventchn.(bind_interdomain eventchn 0 remote_port) in
  Eventchn.notify eventchn port;
  let port = Eventchn.to_int port in
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
  let (_: unit Lwt.t) = service_domain d in
  Hashtbl.add domains 0 d;
  Hashtbl.add by_port port d;
  return (Some d)

let create_domU address =
  let page = Domains.map_foreign address.domid address.mfn in
  let eventchn = Eventchn.init () in
  let port = Eventchn.(to_int (bind_interdomain eventchn address.domid address.remote_port)) in
  let d = {
    address = address;
    page = page;
    ring = Cstruct.of_bigarray page;
    port = port;
    c = Lwt_condition.create ();
    shutdown = false;
  } in
  let (_: unit Lwt.t) = service_domain d in
  Hashtbl.add domains address.domid d;
  Hashtbl.add by_port port d;
  return (Some d)

let rec read t buf ofs len =
  if t.shutdown
  then fail Ring_shutdown
  else
    let n = Xenstore_ring.Ring.Back.unsafe_read t.ring buf ofs len in
    if n = 0 then begin
      lwt () = Lwt_condition.wait t.c in
      read t buf ofs len
    end else begin
      let eventchn = Eventchn.init () in
      Eventchn.(notify eventchn (of_int t.port));
      return n
    end

let rec write t buf ofs len =
  if t.shutdown
  then fail Ring_shutdown
  else
    let n = Xenstore_ring.Ring.Back.unsafe_write t.ring buf ofs len in
    let eventchn = Eventchn.init () in
    if n > 0 then Eventchn.(notify eventchn (of_int t.port));
    if n < len then begin
      lwt () = Lwt_condition.wait t.c in
      write t buf (ofs + n) (len - n)
    end else return ()

let destroy t =
  let eventchn = Eventchn.init () in
  Eventchn.(unbind eventchn (of_int t.port));
  Domains.unmap_foreign t.page;
  Hashtbl.remove domains t.address.domid;
  Hashtbl.remove by_port t.port;
  return ()

let address_of t =
  return (Uri.make ~scheme:"domain" ~path:(string_of_int t.address.domid) ())

let domain_of t = t.address.domid

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

module Introspect = struct
  let read t path =
      let pairs = Xenstore_ring.Ring.to_debug_map t.ring in
      match path with
      | [] -> Some ""
      | [ "mfn" ] -> Some (Nativeint.to_string t.address.mfn)
      | [ "local-port" ] -> Some (string_of_int t.port)
      | [ "remote-port" ] -> Some (string_of_int t.address.remote_port)
      | [ "shutdown" ] -> Some (string_of_bool t.shutdown)
      | [ "wakeup" ]
      | [ "request" ]
      | [ "response" ] -> Some ""
      | [ x ] when List.mem_assoc x pairs -> Some (List.assoc x pairs)
      | _ -> None

  let write t path v = match path with
    | [ "wakeup" ] -> Lwt_condition.broadcast t.c (); true
    | _ -> false

  let list t = function
    | [] -> [ "mfn"; "local-port"; "remote-port"; "shutdown"; "wakeup"; "request"; "response" ]
    | [ "request" ]
    | [ "response" ] -> [ "cons"; "prod"; "data" ]
    | _ -> []
end
