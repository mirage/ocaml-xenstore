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

let debug fmt = Logging.debug "kernelspace/closer" fmt
let error fmt = Logging.error "kernelspace/closer" fmt

module Make(A: ACTIVATIONS with type channel = Eventchn.t)(DS: DOMAIN_STATE) = struct
  let thread () =
    let eventchn = Eventchn.init () in
    let virq_port = Eventchn.bind_dom_exc_virq eventchn in
    debug "Bound virq_port = %d" (Eventchn.to_int virq_port);
    let rec loop from =
      (* Check to see if any of our domains have shutdown *)
      let dis = DS.list () in
      List.iter (fun di ->
        if di.DS.dying || di.DS.shutdown
        then debug "domid %d: %s%s%s" di.DS.domid
          (if di.DS.dying then "dying" else "")
          (if di.DS.dying && di.DS.shutdown then " and " else "")
          (if di.DS.shutdown then "shutdown" else "")
        ) dis;
      let dis_by_domid = Hashtbl.create 128 in
      List.iter (fun di -> Hashtbl.add dis_by_domid di.DS.domid di) dis;
      (* Connections to domains which are missing or 'dying' should be closed *)
      let to_close = Hashtbl.fold (fun domid _ acc ->
        if not(Hashtbl.mem dis_by_domid domid) || (Hashtbl.find dis_by_domid domid).DS.dying
        then domid :: acc else acc) domains [] in
      (* If any domain is missing, shutdown or dying then we should send @releaseDomain *)
      let release_domain = Hashtbl.fold (fun domid _ acc ->
        acc
        || (not(Hashtbl.mem dis_by_domid domid)
        || (let di = Hashtbl.find dis_by_domid domid in di.DS.shutdown || di.DS.dying))
      ) domains false in
      (* Set the connections to "closing", wake up any readers/writers *)
      List.iter
        (fun domid ->
          debug "closing connection to domid: %d" domid;
          let t = Hashtbl.find domains domid in
          t.shutdown <- true;
          (* XXX wakeup threads stuck in activations somehow *)
        ) to_close;
        (* XXX
      if release_domain
      then Connection.fire (Protocol.Op.Write, Protocol.Name.(Predefined ReleaseDomain));
      *)
    lwt after = A.after virq_port from in
    loop after in
  loop A.program_start
end
