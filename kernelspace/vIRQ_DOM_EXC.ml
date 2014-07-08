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

let debug fmt = Logging.debug "kernelspace/VIRQ_DOM_EXC" fmt
let error fmt = Logging.error "kernelspace/VIRQ_DOM_EXC" fmt

module Make(A: ACTIVATIONS with type channel = Eventchn.t) = struct

  type state = A.event

  let initial = A.program_start

  (* Bind the VIRQ at most once *)
  let get_virq_port =
    let cache = ref None in
    fun () -> match !cache with
    | Some p -> p
    | None ->
      begin
        try
          let eventchn = Eventchn.init () in
          let virq_port = Eventchn.bind_dom_exc_virq eventchn in
          debug "Bound VIRQ_DOM_EXC on port %d" virq_port;
          cache := Some virq_port;
          virq_port
        with e ->
          error "Failed to bind the DOM_EXC VIRQ. Are you running in dom0 as root?";
          raise e
      end

  let next state =
    let virq_port = get_virq_port () in
    A.after virq_port from >>= fun from ->
    return ((), from)
end
