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

(** A multiplexing xenstore protocol client over a byte-level transport *)

open Lwt
open Xs_packet

module type TRANSPORT = sig
  type t
  val create: unit -> t Lwt.t
  val destroy: t -> unit Lwt.t
  val read: t -> string -> int -> int -> int Lwt.t
  val write: t -> string -> int -> int -> int Lwt.t
end

let ( |> ) a b = b a
let ( ++ ) f g x = f (g x)

type watch_queue = {
  q: string Queue.t;
  c: unit Lwt_condition.t;
  m: Lwt_mutex.t;
}

let empty_watch_queue () = {
  q = Queue.create ();
  c = Lwt_condition.create ();
  m = Lwt_mutex.create ();
}

module Client = functor(T: TRANSPORT) -> struct

  (* Represents a single acive connection to a server *)
  type client = {
    transport: T.t;
    mutable incoming_pkt: Parser.parse; (* incrementally parses the next packet *)
    outgoing_mutex: Lwt_mutex.t;        (* held to serialise outgoing packets *)
    rid_to_wakeup: (int32, t Lwt.u) Hashtbl.t;
    mutable dispatcher_thread: unit Lwt.t;
    mutable dispatcher_shutting_down: bool;
    watchevents: (Token.t, watch_queue) Hashtbl.t;
  }

  exception Unknown_xenstore_operation of int32
  exception Response_parser_failed
  exception Malformed_watch_event
  exception Unexpected_rid of int32
  exception Dispatcher_failed

  (* [recv_one client] returns a single Packet, or fails *)
  let rec recv_one t =
    let open Parser in match Parser.state t.incoming_pkt with
    | Packet pkt ->
      t.incoming_pkt <- start ();
      return pkt
    | Need_more_data x ->
      let buf = String.make x '\000' in
      lwt n = T.read t.transport buf 0 x in
      let fragment = String.sub buf 0 n in
      t.incoming_pkt <- input t.incoming_pkt fragment;
      recv_one t
    | Unknown_operation x -> raise_lwt (Unknown_xenstore_operation x)
    | Parser_failed -> raise_lwt Response_parser_failed

  (* [send_one client pkt] sends [pkt] and returns (), or fails *)
  let send_one t request =
    let req = to_string request in
    lwt n = Lwt_mutex.with_lock t.outgoing_mutex
        (fun () -> T.write t.transport req 0 (String.length req)) in
    return ()

  let rec dispatcher t =
    try_lwt
      lwt pkt = recv_one t in
      begin match get_ty pkt with
        | Op.Watchevent  ->
          lwt () = begin match Unmarshal.list pkt with
            | Some [path; token] ->
              let token = Token.of_string token in
              (* We may get old watches: silently drop these *)
              if Hashtbl.mem t.watchevents token then begin
                let wq = Hashtbl.find t.watchevents token in
	        lwt () = Lwt_mutex.with_lock wq.m
	          (fun () ->
	            Queue.push path wq.q;
	            Lwt_condition.signal wq.c ();
	            return ()
	          ) in
	        dispatcher t
              end else dispatcher t
	    | _ ->
              raise_lwt Malformed_watch_event
          end in
          dispatcher t
        | _ ->
          let rid = get_rid pkt in
          if not(Hashtbl.mem t.rid_to_wakeup rid)
	  then raise_lwt (Unexpected_rid rid)
	  else begin
            Lwt.wakeup (Hashtbl.find t.rid_to_wakeup rid) pkt;
            dispatcher t
	  end
      end
   with e ->
     t.dispatcher_shutting_down <- true; (* no more hashtable entries after this *)
     (* all blocking threads are failed with our exception *)
     Hashtbl.iter (fun _ u -> Lwt.wakeup_later_exn u e) t.rid_to_wakeup;
     raise_lwt e

  let make () =
    lwt transport = T.create () in
    let t = {
      transport = transport;
      incoming_pkt = Parser.start ();
      outgoing_mutex = Lwt_mutex.create ();
      rid_to_wakeup = Hashtbl.create 10;
      dispatcher_thread = return ();
      dispatcher_shutting_down = false;
      watchevents = Hashtbl.create 10;
    } in
    t.dispatcher_thread <- dispatcher t;
    return t

  type handle = {
    tid: int32;
    client: client;
    mutable paths_to_watch: string list option;
  }

  let no_transaction client = { tid = 0l; client = client; paths_to_watch = None }
  let transaction client tid = { tid = tid; client = client; paths_to_watch = None }
  let watching_paths client = { tid = 0l; client = client; paths_to_watch = Some [] }

  let add_path h path = match h.paths_to_watch with
    | None -> h
    | Some ps -> h.paths_to_watch <- Some (path :: ps); h
  let get_paths h = match h.paths_to_watch with
    | None -> []
    | Some xs -> xs


  let rpc hint h request unmarshal =
    let request = match request h.tid with Some x -> x | None -> failwith "bad request" in
    let rid = get_rid request in
    let t, u = wait () in
    if h.client.dispatcher_shutting_down
    then raise_lwt Dispatcher_failed
    else begin
      Hashtbl.add h.client.rid_to_wakeup rid u;
      lwt () = send_one h.client request in
      lwt res = t in
      Hashtbl.remove h.client.rid_to_wakeup rid;
      try_lwt
        return (response hint request res unmarshal)
 end

  let directory h path = rpc "directory" (add_path h path) (Request.directory path) Unmarshal.list
  let read h path = rpc "read" (add_path h path) (Request.read path) Unmarshal.string
  let write h path data = rpc "write" (add_path h path) (Request.write path data) Unmarshal.ok
  let watch h path token = rpc "watch" h (fun _ -> Request.watch path token) Unmarshal.ok
  let unwatch h path token = rpc "unwatch" h (fun _ -> Request.unwatch path token) Unmarshal.ok

  let with_xs client f = f (no_transaction client)

  let wait client f =
    let token = Token.of_user_string "xs_client.wait" in
    let watch_queue = empty_watch_queue () in
    Hashtbl.add client.watchevents token watch_queue;

    let rec loop current_paths =
      let h = watching_paths client in
      try_lwt
	lwt result = f h in
        return result
      with Eagain ->
        (* aa - bb = aa symmetric difference bb *)
	let ( - ) aa bb = List.filter (fun a -> not(List.mem a bb)) aa in
	let ( + ) aa bb = aa @ bb in
        (* Paths which weren't read don't need to be watched: *)
        let old_paths = current_paths - (get_paths h) in
        lwt () = Lwt_list.iter_s (fun p -> unwatch h p token) old_paths in
        (* Paths which were read do need to be watched: *)
        let new_paths = get_paths h - current_paths in
        lwt () = Lwt_list.iter_s (fun p -> watch h p token) new_paths in
        (* If we're watching the correct set of paths already then just block *)
        lwt () =
	    if old_paths = [] && (new_paths = [])
	    then
	      (* block until some events arrive in our queue *)
	      Lwt_mutex.with_lock watch_queue.m
		(fun () ->
		  while_lwt Queue.is_empty watch_queue.q do
		    Lwt_condition.wait ~mutex:watch_queue.m watch_queue.c
		  done >>
	          (* We don't actually need the values *)
		  return (Queue.clear watch_queue.q)
	      )
	    else return () in
        loop ((current_paths - old_paths) + new_paths) in
    try_lwt
      loop []
    finally
      return (Hashtbl.remove client.watchevents token)


  let rec with_xst client f =
    lwt tid = rpc "transaction_start" (no_transaction client) (fun _ -> Request.transaction_start ()) Unmarshal.int32 in
    let h = transaction client tid in
    lwt result = f h in
    try_lwt
      lwt res' = rpc "transaction_end" h (Request.transaction_end true) Unmarshal.string in
      if res' = "OK" then return result else raise_lwt (Error (Printf.sprintf "Unexpected transaction result: %s" res'))
    with Eagain ->
      with_xst client f
end

