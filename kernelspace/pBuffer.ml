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

let debug fmt = Logging.debug "kernelspace/pBuffer" fmt
let error fmt = Logging.error "kernelspace/pBuffer" fmt

type handle = int64

type t = {
  handle: handle;
  buffer: Cstruct.t;
}

let table : (int64, t) Hashtbl.t = Hashtbl.create 10

let fresh_handle =
  let next = ref 0L in
  fun () ->
    let this = !next in
    next := Int64.succ !next;
    this

let create size =
  let handle = fresh_handle () in
  let buffer = Cstruct.create size in
  let t = { handle; buffer } in
  Hashtbl.replace table handle t;
  return t

let destroy t =
  Hashtbl.remove table t.handle;
  return ()

let get_cstruct t = t.buffer
let handle t = t.handle

let lookup handle =
  if Hashtbl.mem table handle
  then return (Some (Hashtbl.find table handle))
  else return None
