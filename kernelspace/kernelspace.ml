(*
 * Copyright (C) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Sexplib.Std
open Lwt
open Xenstore
open S

type 'a t = 'a Lwt.t
let return x = return x
let ( >>= ) m f = m >>= f

let debug fmt = Logging.debug "kernelspace" fmt
let error fmt = Logging.error "kernelspace" fmt

let fail_on_error = function
| `Ok x -> return x
| `Error x -> fail (Failure x)

type address = {
  domid: int;
  mfn: nativeint;
  remote_port: int;
} with sexp
(** A remote domain address *)

let max_packet_size = Protocol.xenstore_payload_max + Protocol.Header.sizeof

type conn = {
  address: address;
  ring: Cstruct.t;
  port: Eventchn.t;
  mutable shutdown: bool;
}

(* Thrown when an attempt is made to read or write to a closed ring *)
exception Ring_shutdown

module Reader(A: ACTIVATIONS with type channel = Eventchn.t) = struct
  type t = conn

  type offset = int64

  let next t =
    let rec loop from =
      if t.shutdown
      then fail Ring_shutdown
      else
        let seq, available = Xenstore_ring.Ring.Back.read_prepare t.ring in
        let available_bytes = Cstruct.len available in
        if available_bytes = 0 then begin
          A.after t.port from >>= fun from ->
          loop from
        end else return (Int64.of_int32 seq, available) in
    loop A.program_start

  let ack t seq =
    Xenstore_ring.Ring.Back.read_commit t.ring (Int64.to_int32 seq);
    Eventchn.(notify (init ()) t.port);
    return ()

  let read t buf =
    let rec loop buf =
      if Cstruct.len buf = 0
      then return ()
      else next t >>= fun (seq, available) ->
        let available_bytes = Cstruct.len available in
        let consumable = min (Cstruct.len buf) available_bytes in
        Cstruct.blit available 0 buf 0 consumable;
        ack t Int64.(add seq (of_int consumable)) >>= fun () ->
        loop (Cstruct.shift buf consumable) in
    loop buf
end

module Writer(A: ACTIVATIONS with type channel = Eventchn.t) = struct
  type t = conn

  type offset = int64

  let next t =
    let rec loop from =
      if t.shutdown
      then fail Ring_shutdown
      else
        let seq, available = Xenstore_ring.Ring.Back.write_prepare t.ring in
        let available_bytes = Cstruct.len available in
        if available_bytes = 0 then begin
          A.after t.port from >>= fun from ->
          loop from
        end else return (Int64.of_int32 seq, available) in
    loop A.program_start

  let ack t seq =
    Xenstore_ring.Ring.Back.write_commit t.ring (Int64.to_int32 seq);
    Eventchn.(notify (init ()) t.port);
    return ()

  let write t buf =
    let rec loop buf =
      if Cstruct.len buf = 0
      then return ()
      else next t >>= fun (seq, available) ->
        let available_bytes = Cstruct.len available in
        let consumable = min (Cstruct.len buf) available_bytes in
        Cstruct.blit buf 0 available 0 consumable;
        ack t Int64.(add seq (of_int consumable)) >>= fun () ->
        loop (Cstruct.shift buf consumable) in
    loop buf
end

let domains : (int, conn) Hashtbl.t = Hashtbl.create 128

let (stream: address Lwt_stream.t), introduce_fn = Lwt_stream.create ()

module Make
  (A: ACTIVATIONS with type channel = Eventchn.t)
  (DS: DOMAIN_STATE)
  (FPM: FOREIGN_PAGE_MAPPER) = struct

  type 'a t = 'a Lwt.t
  let ( >>= ) = Lwt.( >>= )
  let return = Lwt.return

  type connection = conn

  module Reader = Reader(A)
  module Writer = Writer(A)

  let read = Reader.read
  let write = Writer.write

  module BufferedReader = BufferedReader.Make(Reader)
  module BufferedWriter = BufferedWriter.Make(Writer)

  let address_of t =
    return (Uri.make
      ~scheme:"domain"
      ~path:(Printf.sprintf "%d/%nu/%d" t.address.domid t.address.mfn t.address.remote_port)
      ()
    )

  let domain_of t = t.address.domid

  type server = address Lwt_stream.t

  let listen () =
    return stream

  let from_address address =
    (* this function should be idempotent *)
    if Hashtbl.mem domains address.domid
    then return (Hashtbl.find domains address.domid)
    else begin
      let ring = FPM.map address.domid address.mfn in
      let eventchn = Eventchn.init () in
      let port = Eventchn.(bind_interdomain eventchn address.domid address.remote_port) in

      let d = {
        address; ring; port;
        shutdown = false;
      } in
      Hashtbl.add domains address.domid d;
      return d
    end

  let rec accept_forever stream process =
    Lwt_stream.next stream >>= fun address ->
    from_address address >>= fun d ->
    let (_: unit Lwt.t) = process d in
    accept_forever stream process

  let create () =
    failwith "It's not possible to directly 'create' an interdomain ring."

  let destroy t =
    let eventchn = Eventchn.init () in
    Eventchn.(unbind eventchn t.port);
    FPM.unmap t.ring;
    Hashtbl.remove domains t.address.domid;
    return ()

  module Introspect = struct
    type t = connection

    let read t path =
        let pairs = Xenstore_ring.Ring.to_debug_map t.ring in
        match path with
        | [] -> Some ""
        | [ "mfn" ] -> Some (Nativeint.to_string t.address.mfn)
        | [ "local-port" ] -> Some (string_of_int (Eventchn.to_int t.port))
        | [ "remote-port" ] -> Some (string_of_int t.address.remote_port)
        | [ "shutdown" ] -> Some (string_of_bool t.shutdown)
        | [ "request" ]
        | [ "response" ] -> Some ""
        | [ x ] when List.mem_assoc x pairs -> Some (List.assoc x pairs)
        | _ -> None

    let write t path v = match path with
      | _ -> false

    let ls t = function
      | [] -> [ "mfn"; "local-port"; "remote-port"; "shutdown"; "request"; "response" ]
      | [ "request" ]
      | [ "response" ] -> [ "cons"; "prod"; "data" ]
      | _ -> []
  end
end
