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

(* We store the next whole packet to transmit in this persistent buffer.
   The contents are only valid iff valid <> 0 *)
cstruct buffer {
  uint16_t length; (* total number of payload bytes in buffer. *)
  uint64_t offset; (* offset in the output stream to write first byte *)
  uint8_t buffer[4112];
} as little_endian
let _ = assert(4112 = Protocol.xenstore_payload_max + Protocol.Header.sizeof)

type conn = {
  address: address;
  ring: Cstruct.t;
  input: Cstruct.t;
  output: Cstruct.t;
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

      PBuffer.create sizeof_buffer >>= fun poutput ->
      let output = PBuffer.get_cstruct poutput in
      set_buffer_offset output 0L;
      set_buffer_length output 0;
      (* This output buffer is safe to flush *)
      PBuffer.create sizeof_buffer >>= fun pinput ->
      let input = PBuffer.get_cstruct pinput in
      set_buffer_offset input 0L;
      set_buffer_length input 0;

      let d = {
        address; ring; port; input; output;
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

  type offset = int64 with sexp

  (* Flush any pending output to the channel. This function can suffer a crash and
     restart at any point. On exit, the output buffer is invalid and the whole
     packet has been transmitted. *)
  let rec flush t next_write_ofs =
    let offset = get_buffer_offset t.output in
    if next_write_ofs <> offset then begin
      (* packet is from the future *)
      return ()
    end else begin
      let length = get_buffer_length t.output in
      Writer.next t >>= fun (offset', space') ->
      let space = Cstruct.sub (get_buffer_buffer t.output) 0 length in
      (* write as much of (offset, space) into (offset', space') as possible *)
      (* 1. skip any already-written data, check we haven't lost any *)
      ( if offset < offset'
        then
          let to_skip = Int64.sub offset' offset in
          return (offset', Cstruct.shift space (Int64.to_int to_skip))
        else
          if offset > offset'
          then fail (Failure (Printf.sprintf "Some portion of the output stream has been skipped. Our data starts at %Ld, the stream starts at %Ld" offset offset'))
          else return (offset, space)
      ) >>= fun (offset, space) ->
      (* 2. write as much as there is space for *)
      let to_write = min (Cstruct.len space) (Cstruct.len space') in
      Cstruct.blit space 0 space' 0 to_write;
      let next_offset = Int64.(add offset' (of_int to_write)) in
      Writer.ack t next_offset >>= fun () ->
      let remaining = to_write - (Cstruct.len space) in
      if remaining = 0
      then return ()
      else flush t next_write_ofs
    end

  (* Enqueue an output packet. This assumes that the output buffer is empty. *)
  let enqueue t hdr response =
    let reply_buf = get_buffer_buffer t.output in
    let payload_buf = Cstruct.shift reply_buf Protocol.Header.sizeof in

    let next = Protocol.Response.marshal response payload_buf in
    let length = next.Cstruct.off - payload_buf.Cstruct.off in
    let hdr = Protocol.Header.({ hdr with len = length }) in
    ignore (Protocol.Header.marshal hdr reply_buf);
    Writer.next t >>= fun (offset, _) ->
    set_buffer_length t.output (length + Protocol.Header.sizeof);
    set_buffer_offset t.output offset;
    return offset

  (* [fill ()] fills up input with the next request. This function can crash and
     be restarted at any point. On exit, a whole packet is available for unmarshalling
     and the next packet is still on the ring. *)
  let rec fill t =
    (* compute the maximum number of bytes we definitely need *)
    let length = get_buffer_length t.input in
    let offset = get_buffer_offset t.input in
    let buffer = get_buffer_buffer t.input in
    ( if length < Protocol.Header.sizeof
      then return (Protocol.Header.sizeof - length)
      else begin
        (* if we have the header then we know how long the payload is *)
        fail_on_error (Protocol.Header.unmarshal buffer) >>= fun hdr ->
        return (Protocol.Header.sizeof + hdr.Protocol.Header.len - length)
      end ) >>= fun bytes_needed ->
    if bytes_needed = 0
    then return () (* packet ready for reading, stream positioned at next packet *)
    else begin
      let offset = Int64.(add offset (of_int length)) in
      let space = Cstruct.sub buffer length bytes_needed in
      Reader.next t >>= fun (offset', space') ->
      (* 1. skip any already-read data, check we haven't lost any *)
      ( if offset < offset'
        then fail (Failure (Printf.sprintf "Some portion of the input stream has been skipped. We need data from %Ld, the stream starts at %Ld" offset offset'))
        else
          if offset > offset'
          then
            let to_skip = Int64.sub offset offset' in
            return (offset, Cstruct.shift space' (Int64.to_int to_skip))
          else return (offset, space') ) >>= fun (offset', space') ->
      (* 2. read as much as there is space for *)
      let to_copy = min (Cstruct.len space) (Cstruct.len space') in
      Cstruct.blit space' 0 space 0 to_copy;
      set_buffer_length t.input (length + to_copy);

      let next_offset = Int64.(add offset' (of_int to_copy)) in
      Reader.ack t next_offset >>= fun () ->
      fill t
    end

  let rec recv t read_ofs =
    if get_buffer_offset t.input <> read_ofs then begin
      (* drop previously buffered packet *)
      set_buffer_length t.input 0;
      set_buffer_offset t.input read_ofs;
    end;
    fill t >>= fun () ->

    let buffer = get_buffer_buffer t.input in
    fail_on_error (Protocol.Header.unmarshal buffer) >>= fun hdr ->
    let payload = Cstruct.sub buffer Protocol.Header.sizeof hdr.Protocol.Header.len in
    (* return the read_ofs value a future caller would need to get the next packet *)
    let read_ofs' = Int64.(add read_ofs (of_int (get_buffer_length t.input))) in
    match Protocol.Request.unmarshal hdr payload with
     | `Ok r ->
       return (read_ofs', `Ok (hdr, r))
     | `Error e ->
       return (read_ofs', `Error e)

  let get_read_offset t =
    Reader.next t >>= fun (next_read_ofs, _) ->
    return next_read_ofs

  let get_write_offset t =
    Writer.next t >>= fun (next_write_ofs, _) ->
    return next_write_ofs

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
