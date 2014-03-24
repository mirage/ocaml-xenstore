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

module type STREAM_READER = sig
  type s
  (** a stream of data *)

  val next: s -> (int32 * Cstruct.t) Lwt.t
  (** [next s] returns [ofs, chunk] where [chunk] is the data
      starting at offset [ofs]. *)

  val ack: s -> int32 -> unit Lwt.t
  (** [ack s ofs] acknowledges that data before [ofs] has
      been processed. *)
end

module type S = sig
  type s
  (** a stream of data *)

  type t
  (** A persistent binary stream of data *)

  val create: string list -> t Lwt.t
  (** [create name]: loads the buffer at [name] *)

  val read: t -> Cstruct.t -> int32 -> Cstruct.t option Lwt.t
  (** [read t buffer ofs]: returns a chunk of data starting
      at offset [ofs] from the buffer, or [None] if the
      data has been acknowledged. *)

  val ack: t -> int32 -> unit Lwt.t
  (** [ack t ofs]: acknowledges that data before [ofs] has
      been completely processed, and hence may be forgotten.
      Any attempt to [read] this offset will result in a
      [None] *)
end
(** A persistent dynamically-sized stream of data *)

open Sexplib.Std
open Xenstore

let debug fmt = Logging.debug "pBinStream" fmt
let info  fmt = Logging.info  "pBinStream" fmt
let error fmt = Logging.debug "pBinStream" fmt

open Lwt

module Make(S: STREAM_READER) = struct
  module M = PMap.Make(Int32)(struct type t = string with sexp end)

  type t = {
    s: S.s;
    mutable root: M.t;
  }

  let create name s =
    M.create name >>= fun root ->
    return { s; root }

  let read_one t =
    S.next t.s >>= fun (offset, buffer) ->
    let string = Cstruct.to_string buffer in
    M.add offset string t.root >>= fun () ->
    S.ack t.s offset >>= fun () ->
    return (offset, string)

  let read t buffer ofs =
    M.fold (fun acc k v -> (k, v) :: acc) [] t.root >>= fun all ->
    ( if all = []
      then read_one t >>= fun x -> return [ x ]
      else return (List.sort (fun (a, _) (b, _) -> compare a b) all ) ) >>= fun all ->
    (* if ofs is lower than the lowest key in the map, then
       we've forgotten the data, so return None *)
    if ofs < fst (List.hd all)
    then return None
    else begin
      (* if ofs + len(buffer) is not in the map, keep
         reading elements *)
      let rec fill all_rev =
        let last =
          let ofs, buffer = List.hd all_rev in
          Int32.(add ofs (of_int (String.length buffer))) in
        if last < Int32.(add ofs (of_int (Cstruct.len buffer))) then begin
          read_one t >>= fun x ->
          fill (x :: all_rev)
        end else return all_rev in
      fill (List.rev all) >>= fun all_rev ->
      (* blit into the buffer *)
      let apply (ofs', string') =
        let open Int32 in
        let len' = of_int (String.length string') in
        (* start is the offset in [buffer'] of the first byte in [ofs, buffer] *)
        let start = sub ofs ofs' in
        if start < 0l then begin
          let target = Cstruct.shift buffer (to_int (sub 0l start)) in
          let avail = min (Cstruct.len target) (String.length string') in
          if avail > 0
          then Cstruct.blit_from_string string' 0 target 0 avail
        end else begin
          let avail = min (to_int (sub len' start)) (Cstruct.len buffer) in
          if avail > 0
          then Cstruct.blit_from_string string' (to_int start) buffer 0 avail
        end in
      List.iter apply all_rev;
      return (Some buffer)
    end

  let ack t ofs =
    (* if an element offset + len(buffer) < ofs then
       delete it from the map *)
    M.fold (fun acc k v -> (k, v) :: acc) [] t.root >>= fun all ->
    Lwt_list.iter_s
      (fun (offset, buffer) ->
        if Int32.(add offset (of_int (String.length buffer))) < ofs
        then M.remove offset t.root
        else return ()
      ) all
end
