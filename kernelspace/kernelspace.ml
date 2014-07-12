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

module Address = struct
  type t = {
    domid: int;
    mfn: nativeint;
    remote_port: int;
  } with sexp
  (** A remote domain address *)

  let uri_of address =
    Uri.make
      ~scheme:"domain"
      ~path:(Printf.sprintf "%d/%nu/%d" address.domid address.mfn address.remote_port)
      ()
end

let max_packet_size = Protocol.xenstore_payload_max + Protocol.Header.sizeof

module Connection = struct
  type t = {
    address: Address.t;
    ring: Cstruct.t;
    port: Eventchn.t;
    mutable shutdown: bool;
  }

  let domains : (int, t) Hashtbl.t = Hashtbl.create 128

  let destroy t =
    let eventchn = Eventchn.init () in
    Eventchn.(unbind eventchn t.port);
    Hashtbl.remove domains t.address.Address.domid;
    return ()
end

(* Thrown when an attempt is made to read or write to a closed ring *)
exception Ring_shutdown

module RingReader(A: ACTIVATIONS with type channel = Eventchn.t) = struct
  type t = Connection.t
  open Connection

  type offset = int64
  type item = Cstruct.t

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

module RingWriter(A: ACTIVATIONS with type channel = Eventchn.t) = struct
  type t = Connection.t
  open Connection

  type offset = int64
  type item = Cstruct.t

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


let (stream: Address.t Lwt_stream.t), introduce_fn = Lwt_stream.create ()

module Make
  (A: ACTIVATIONS with type channel = Eventchn.t)
  (DS: DOMAIN_STATE)
  (FPM: FOREIGN_PAGE_MAPPER) = struct

  type 'a t = 'a Lwt.t
  let ( >>= ) = Lwt.( >>= )
  let return = Lwt.return

  module Window = struct
    include A
    type item = Cstruct.t
  end

  module Reader = RingReader(Window)
  module Writer = RingWriter(Window)

  module BufferedReader = BufferedReader.Make(Reader)
  module BufferedWriter = BufferedWriter.Make(Writer)

  module PacketWriter = PacketWriter.Make(BufferedWriter)

  type connection = {
    conn: Connection.t;
    reader: BufferedReader.t;
    writer: BufferedWriter.t;
  }

  module Request = struct
    module PacketReader = PacketReader.Make(Protocol.Request)(BufferedReader)

    module Reader = struct
      type t = connection
      type offset = int64
      type item = [ `Ok of (Protocol.Header.t * Protocol.Request.t) | `Error of string ]
      let next t = PacketReader.next t.reader
      let ack t = PacketReader.ack t.reader
    end
    module Writer = struct
      type t = connection
      type offset = int64
      let write t = PacketWriter.write t.writer
      let ack t = PacketWriter.ack t.writer
    end
  end

  module Response = struct
    module PacketReader = PacketReader.Make(Protocol.Response)(BufferedReader)

    module Reader = struct
      type t = connection
      type offset = int64
      type item = [ `Ok of (Protocol.Header.t * Protocol.Response.t) | `Error of string ]
      let next t = PacketReader.next t.reader
      let ack t = PacketReader.ack t.reader
    end
    module Writer = struct
      type t = connection
      type offset = int64
      let write t = PacketWriter.write t.writer
      let ack t = PacketWriter.ack t.writer
    end
  end

  let uri_of t = return (Address.uri_of t.conn.Connection.address)

  let domain_of t = t.conn.Connection.address.Address.domid

  type server = Address.t Lwt_stream.t

  let listen () =
    return stream

  let from_address address =
    (* this function should be idempotent *)
    if Hashtbl.mem Connection.domains address.Address.domid
    then return (Hashtbl.find Connection.domains address.Address.domid)
    else begin
      let ring = FPM.map address.Address.domid address.Address.mfn in
      let eventchn = Eventchn.init () in
      let port = Eventchn.(bind_interdomain eventchn address.Address.domid address.Address.remote_port) in

      let d = {
        Connection.address; ring; port;
        shutdown = false;
      } in
      Hashtbl.add Connection.domains address.Address.domid d;
      return d
    end

  let rec accept_forever stream process =
    Lwt_stream.next stream >>= fun address ->
    from_address address >>= fun conn ->
    let reader_buffer = Cstruct.create max_packet_size in
    let writer_buffer = Cstruct.create max_packet_size in
    let reader = BufferedReader.create conn reader_buffer in
    let writer = BufferedWriter.create conn writer_buffer in
    let t = { conn; reader; writer } in
    let (_: unit Lwt.t) = process t  in
    accept_forever stream process

  let create () =
    failwith "It's not possible to directly 'create' an interdomain ring."

  let destroy t =
    FPM.unmap t.conn.Connection.ring;
    Connection.destroy t.conn

  module Introspect = struct
    type t = connection
    open Connection

    let read t path =
        let t = t.conn in
        let pairs = Xenstore_ring.Ring.to_debug_map t.ring in
        match path with
        | [] -> Some ""
        | [ "mfn" ] -> Some (Nativeint.to_string t.address.Address.mfn)
        | [ "local-port" ] -> Some (string_of_int (Eventchn.to_int t.port))
        | [ "remote-port" ] -> Some (string_of_int t.address.Address.remote_port)
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
