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

include IO_lwt

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
  type stream = Connection.t
  open Connection
  include IO_lwt

  type position = int64 with sexp
  type item = Cstruct.t

  let read stream =
    let rec loop from =
      if stream.shutdown
      then fail Ring_shutdown
      else
        let seq, available = Xenstore_ring.Ring.Back.Reader.read stream.ring in
        let available_bytes = Cstruct.len available in
        if available_bytes = 0 then begin
          A.after stream.port from >>= fun from ->
          loop from
        end else return (Int64.of_int32 seq, `Ok available) in
    loop A.program_start

  let advance stream seq =
    Xenstore_ring.Ring.Back.Reader.advance stream.ring (Int64.to_int32 seq);
    Eventchn.(notify (init ()) stream.port);
    return ()
end

module RingWriter(A: ACTIVATIONS with type channel = Eventchn.t) = struct
  type stream = Connection.t
  open Connection
  include IO_lwt

  type position = int64 with sexp
  type item = Cstruct.t

  let read stream =
    let rec loop from =
      if stream.shutdown
      then fail Ring_shutdown
      else
        let seq, available = Xenstore_ring.Ring.Back.Writer.write stream.ring in
        let available_bytes = Cstruct.len available in
        if available_bytes = 0 then begin
          A.after stream.port from >>= fun from ->
          loop from
        end else return (Int64.of_int32 seq, `Ok available) in
    loop A.program_start

  let advance stream seq =
    Xenstore_ring.Ring.Back.Writer.advance stream.ring (Int64.to_int32 seq);
    Eventchn.(notify (init ()) stream.port);
    return ()
end


let (stream: Address.t Lwt_stream.t), introduce_fn = Lwt_stream.create ()

module Make
  (A: ACTIVATIONS with type channel = Eventchn.t)
  (DS: DOMAIN_STATE)
  (FPM: FOREIGN_PAGE_MAPPER) = struct

  include IO_lwt

  module Window = struct
    include A
    type item = Cstruct.t
  end

  module Reader = RingReader(Window)
  module WriteBuffer = RingWriter(Window)

  module BufferedReader = BufferedReader.Make(Reader)
  module WriteBufferStream = WriteBufferStream.Make(WriteBuffer)

  type connection = {
    conn: Connection.t;
    reader: BufferedReader.stream;
    writeBuffers: WriteBufferStream.stream;
  }

  module Request = struct
    type item = Protocol.Header.t * Protocol.Request.t

    module PacketReader = PacketReader.Make(Protocol.Request)(BufferedReader)
    module PacketWriter = PacketWriter.Make(Protocol.Request)(WriteBufferStream)

    module Reader = struct
      include IO_lwt
      type stream = connection
      type position = int64 with sexp
      type item = Protocol.Header.t * Protocol.Request.t
      let read stream = PacketReader.read stream.reader
      let advance stream = PacketReader.advance stream.reader
    end
    module Writer = struct
      include IO_lwt
      type stream = connection
      type position = int64 with sexp
      type item = Protocol.Header.t * Protocol.Request.t
      let write stream = PacketWriter.write stream.writeBuffers
      let advance stream = PacketWriter.advance stream.writeBuffers
    end
  end

  module Response = struct
    type item = Protocol.Header.t * Protocol.Response.t

    module PacketReader = PacketReader.Make(Protocol.Response)(BufferedReader)
    module PacketWriter = PacketWriter.Make(Protocol.Response)(WriteBufferStream)

    module Reader = struct
      include IO_lwt
      type stream = connection
      type position = int64 with sexp
      type item = Protocol.Header.t * Protocol.Response.t
      let read stream = PacketReader.read stream.reader
      let advance stream = PacketReader.advance stream.reader
    end
    module Writer = struct
      include IO_lwt
      type stream = connection
      type position = int64 with sexp
      type item = Protocol.Header.t * Protocol.Response.t
      let write stream = PacketWriter.write stream.writeBuffers
      let advance stream = PacketWriter.advance stream.writeBuffers
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
    let writeBuffers = WriteBufferStream.create conn writer_buffer in
    let t = { conn; reader; writeBuffers } in
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
