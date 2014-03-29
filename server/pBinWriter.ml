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

module type S = sig
  type s
  (** a writable stream of data *)

  type t
  (** A persistent binary writable stream of data *)

  val create: string list -> s -> t Lwt.t
  (** [create name]: loads the stream at [name] *)

  val destroy: t -> unit Lwt.t
  (** [destroy t]: permanently deletes the persistent stream *)

  val write: t -> Cstruct.t -> int64 -> unit Lwt.t
  (** [write t buffer ofs]: inserts a chunk of data at offset [ofs]
      into the output stream. For simplicity we assume the chunks
      are all non-overlapping. *)

  val sync: t -> unit Lwt.t
  (** [sync t]: ensures that as much queued data as possible is
      written. *)
end
(** A persistent dynamically-sized writable stream of data *)

open Sexplib.Std
open Xenstore

let debug fmt = Logging.debug "pBinWriter" fmt
let info  fmt = Logging.info  "pBinWriter" fmt
let error fmt = Logging.debug "pBinWriter" fmt

(* Copy any fragment from [ofs', string'] into [ofs, buffer] *)
let blit_fragment (ofs, buffer) (ofs', string') =
  let open Int64 in
  let len' = of_int (String.length string') in
  (* start is the offset in [buffer'] of the first byte in [ofs, buffer] *)
  let start = sub ofs ofs' in
  if start < 0L then begin
    let target = Cstruct.shift buffer (to_int (sub 0L start)) in
    let avail = min (Cstruct.len target) (String.length string') in
    if avail > 0
    then Cstruct.blit_from_string string' 0 target 0 avail
  end else begin
    let avail = min (to_int (sub len' start)) (Cstruct.len buffer) in
    if avail > 0
    then Cstruct.blit_from_string string' (to_int start) buffer 0 avail
  end

open Lwt

module Make(C: S.SHARED_MEMORY_CHANNEL) = (struct
  module M = PMap.Make(Int64)(struct type t = string with sexp end)

  type s = C.t

  type t = {
    c: C.t;
    mutable root: M.t;
  }

  let create name c =
    M.create name >>= fun root ->
    return { c; root }

  let destroy t = M.clear t.root

  let write t buf ofs =
    M.add ofs (Cstruct.to_string buf) t.root

  let sync t =
    C.next t.c >>= fun (offset, buffer) ->
    M.fold (fun acc k v -> (k, v) :: acc) [] t.root >>= fun all ->
    List.iter (blit_fragment (offset, buffer)) all;
    (* acknowledge the data we successfully wrote *)
    ( match List.sort (fun (a, _) (b, _) -> compare b a) all with
      | [] -> return ()
      | (ofs, string) :: _ ->
        let max_data_avail = Int64.(add ofs (of_int (String.length string))) in
        let written = min (Int64.of_int (Cstruct.len buffer)) (Int64.sub max_data_avail offset) in
        C.ack t.c written ) >>= fun () ->
    (* clear uneeded data *)
    Lwt_list.iter_s
      (fun (ofs, buf) ->
        if Int64.(add ofs (of_int (String.length buf))) <= Int64.(add offset (of_int (Cstruct.len buffer)))
        then M.remove ofs t.root
        else return ()
      ) all
end: S with type s = C.t)
