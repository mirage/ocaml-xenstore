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
open Lwt

let max_packet_size = Protocol.xenstore_payload_max + Protocol.Header.sizeof

module Make(Marshal: S.MARSHALABLE)(Writer: S.STREAM
  with type offset = int64
  and type item = Cstruct.t) = struct

  type t = Writer.t
  type offset = Writer.offset
  type item = Marshal.t

  let write t offset hdr item =
    let rec loop () =
      Writer.next t >>= fun (_, space) ->
      if Cstruct.len space >= max_packet_size
      then return ()
      else loop () in
    loop () >>= fun () ->
    Writer.next t >>= fun (offset, space) ->
    let payload_buf = Cstruct.shift space Protocol.Header.sizeof in
    let next = Marshal.marshal item payload_buf in
    let length = next.Cstruct.off - payload_buf.Cstruct.off in
    let hdr = Protocol.Header.({ hdr with len = length }) in
    ignore (Protocol.Header.marshal hdr space);
    return (Int64.(add offset (of_int (Protocol.Header.sizeof + length))))

  let ack t ofs =
    Writer.ack t ofs
end
