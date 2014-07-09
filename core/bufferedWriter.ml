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

module Make(Writer: S.WINDOW with type offset = int64) = struct
  cstruct hdr {
    uint64_t producer;
    uint64_t consumer;
  } as little_endian

  type t = {
    t: Writer.t;
    output: Cstruct.t;
  }

  let attach t output = { t; output }

  let create t output =
    set_hdr_producer output 0L;
    set_hdr_consumer output 0L;
    attach t output

  (* Return the next contiguous buffer fragment available for writing *)
  let next t =
    let len = Cstruct.len t.output in
    (* total space available is len - (producer - consumer_ but we only want to
       return contiguous space *)
    let producer = get_hdr_producer t.output in
    let consumer = get_hdr_consumer t.output in
    let used = Int64.(to_int (sub producer consumer)) in
    let available = len - used in
    let producer_wrapped = Int64.(to_int (rem producer (of_int len))) in
    let to_buffer_end = len - producer_wrapped in
    let contiguous = min available to_buffer_end in
    return (producer, Cstruct.sub t.output producer_wrapped contiguous)

  (* Ensure all data [up_to] have been transmitted *)
  let rec ack t up_to =
    let len = Cstruct.len t.output in
    let consumer = get_hdr_consumer t.output in
    Writer.next t.t >>= fun (offset, space) ->
    ( if offset < consumer
      then fail (Failure (Printf.sprintf "Some portion of the output stream has been dropped. Our data starts at %Ld, the stream starts at %Ld" consumer offset))
      else return () ) >>= fun () ->
    (* If we crashed on a previous run we might not have bumped the consumer
       pointer -- do it now *)
    let consumer = offset in
    set_hdr_consumer t.output offset;

    if consumer = up_to
    then return ()
    else begin
      (* total data we should write is up_to - consumer but we need to
         subdivide this into contiguous chunks *)
      let used = Int64.(to_int (sub up_to consumer)) in
      let consumer_wrapped = Int64.(to_int (rem consumer (of_int len))) in
      let to_buffer_end = len - consumer_wrapped in
      let contiguous = min used to_buffer_end in
      let n = min (Cstruct.len space) contiguous in
      let to_write = Cstruct.sub t.output consumer_wrapped n in
      Cstruct.blit to_write 0 space 0 n;
      let consumer = Int64.(add consumer (of_int n)) in
      Writer.ack t.t consumer >>= fun () ->
      ack t up_to
    end
end
