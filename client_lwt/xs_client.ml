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
open Xs_protocol

module type IO = sig
  type 'a t = 'a Lwt.t
  val return: 'a -> 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t

  type channel
  val create: unit -> channel t
  val destroy: channel -> unit t
  val read: channel -> string -> int -> int -> int t
  val write: channel -> string -> int -> int -> unit t
end

let ( |> ) a b = b a
let ( ++ ) f g x = f (g x)

module StringSet = Xs_handle.StringSet

module Watcher = struct

  (** Someone who is watching paths is represented by one of these: *)
  type t = {
    mutable paths: StringSet.t; (* we never care about events or ordering, only paths *)
    mutable cancelling: bool; (* we need to stop watching and clean up *)
    c: unit Lwt_condition.t;
    m: Lwt_mutex.t;
  }

  let make () = {
    paths = StringSet.empty;
    cancelling = false;
    c = Lwt_condition.create ();
    m = Lwt_mutex.create ();
  }

  (** Register that a watched path has been changed *)
  let put (x: t) path =
    Lwt_mutex.with_lock x.m
      (fun () ->
	x.paths <- StringSet.add path x.paths;
	Lwt_condition.signal x.c ();
	return ();
      )

  (** Return a set of modified paths, or an empty set if we're cancelling *)
  let get (x: t) =
    Lwt_mutex.with_lock x.m
      (fun () ->
        while_lwt x.paths = StringSet.empty && not x.cancelling do
          Lwt_condition.wait ~mutex:x.m x.c
        done >>
        let results = x.paths in
        x.paths <- StringSet.empty;
	return results
      )

  (** Called to shutdown the watcher and trigger an orderly cleanup *)
  let cancel (x: t) =
    let (_: unit Lwt.t) =
      Lwt_mutex.with_lock x.m
	(fun () ->
	  x.cancelling <- true;
	  Lwt_condition.signal x.c ();
	  return ()
	) in
    ()
end

exception Malformed_watch_event
exception Unexpected_rid of int32
exception Dispatcher_failed

module Client = functor(IO: IO with type 'a t = 'a Lwt.t) -> struct
  module PS = PacketStream(IO)

  (* Represents a single acive connection to a server *)
  type client = {
    transport: IO.channel;
    ps: PS.stream;
    rid_to_wakeup: (int32, Xs_protocol.t Lwt.u) Hashtbl.t;
    mutable dispatcher_thread: unit Lwt.t;
    mutable dispatcher_shutting_down: bool;
    watchevents: (Token.t, Watcher.t) Hashtbl.t;

    mutable suspended : bool;
    suspended_m : Lwt_mutex.t;
    suspended_c : unit Lwt_condition.t;
  }

  let recv_one t = match_lwt (PS.recv t.ps) with
    | Ok x -> return x
    | Exception e -> raise_lwt e
  let send_one t = PS.send t.ps

  let handle_exn t e =
    Printf.fprintf stderr "Caught: %s\n%!" (Printexc.to_string e);
    lwt () = begin 
      match e with
      | Xs_protocol.Response_parser_failed x ->
      (* Lwt_io.hexdump Lwt_io.stderr x *)
         return ()
      | _ -> return () end in
    t.dispatcher_shutting_down <- true; (* no more hashtable entries after this *)
    (* all blocking threads are failed with our exception *)
    lwt () = Lwt_mutex.with_lock t.suspended_m (fun () ->
      Printf.fprintf stderr "Propagating exception to %d threads\n%!" (Hashtbl.length t.rid_to_wakeup);
      Hashtbl.iter (fun _ u -> Lwt.wakeup_later_exn u e) t.rid_to_wakeup;
      return ()) in
    raise_lwt e

  let rec dispatcher t =
    lwt pkt = try_lwt recv_one t with e -> handle_exn t e in
    match get_ty pkt with
      | Op.Watchevent  ->
        lwt () = begin match Unmarshal.list pkt with
          | Some [path; token] ->
            let token = Token.of_string token in
            (* We may get old watches: silently drop these *)
            if Hashtbl.mem t.watchevents token
            then Watcher.put (Hashtbl.find t.watchevents token) path >> dispatcher t
            else dispatcher t
          | _ ->
            handle_exn t Malformed_watch_event
          end in
        dispatcher t
      | _ ->
        let rid = get_rid pkt in
        lwt thread = Lwt_mutex.with_lock t.suspended_m (fun () -> 
          if Hashtbl.mem t.rid_to_wakeup rid
          then return (Some (Hashtbl.find t.rid_to_wakeup rid))
          else return None) in
        match thread with
          | None -> handle_exn t (Unexpected_rid rid)
          | Some thread -> 
            begin
              Lwt.wakeup_later thread pkt;
              dispatcher t
            end


  let make () =
    lwt transport = IO.create () in
    let t = {
      transport = transport;
      ps = PS.make transport;
      rid_to_wakeup = Hashtbl.create 10;
      dispatcher_thread = return ();
      dispatcher_shutting_down = false;
      watchevents = Hashtbl.create 10;
      suspended = false;
      suspended_m = Lwt_mutex.create ();
      suspended_c = Lwt_condition.create ();
    } in
    t.dispatcher_thread <- dispatcher t;
    return t

  let suspend t =
    lwt () = Lwt_mutex.with_lock t.suspended_m
      (fun () -> 
        t.suspended <- true;
        while_lwt (Hashtbl.length t.rid_to_wakeup > 0) do
          Lwt_condition.wait ~mutex:t.suspended_m t.suspended_c
        done) in
      Hashtbl.iter (fun _ watcher -> Watcher.cancel watcher) t.watchevents;
      Lwt.cancel t.dispatcher_thread;
      return ()

  let resume t =
    lwt () = Lwt_mutex.with_lock t.suspended_m (fun () -> 
      t.suspended <- false;
      t.dispatcher_shutting_down <- false;
      Lwt_condition.broadcast t.suspended_c (); 
      return ()) in
    t.dispatcher_thread <- dispatcher t;
    return ()

  type handle = client Xs_handle.t

  let make_rid =
	  let counter = ref 0l in
	  fun () ->
		  let result = !counter in
		  counter := Int32.succ !counter;
		  result

  let rpc hint h payload unmarshal =
    let open Xs_handle in
    let rid = make_rid () in
    let request = Request.print payload (get_tid h) rid in
    let t, u = wait () in
    let c = get_client h in
    if c.dispatcher_shutting_down
    then raise_lwt Dispatcher_failed
    else begin
      lwt () = Lwt_mutex.with_lock c.suspended_m (fun () ->
        lwt () = while_lwt c.suspended do
          Lwt_condition.wait ~mutex:c.suspended_m c.suspended_c
        done in
        Hashtbl.add c.rid_to_wakeup rid u;   
        lwt () = send_one c request in
        return ()) in
      lwt res = t in
      lwt () = Lwt_mutex.with_lock c.suspended_m 
        (fun () -> 
          Hashtbl.remove c.rid_to_wakeup rid;
          Lwt_condition.broadcast c.suspended_c (); 
          return ()) in
      try_lwt
        return (response hint request res unmarshal)
 end

  let directory h path = rpc "directory" (Xs_handle.accessed_path h path) Request.(PathOp(path, Directory)) Unmarshal.list
  let read h path = rpc "read" (Xs_handle.accessed_path h path) Request.(PathOp(path, Read)) Unmarshal.string
  let write h path data = rpc "write" (Xs_handle.accessed_path h path) Request.(PathOp(path, Write data)) Unmarshal.ok
  let rm h path = rpc "rm" (Xs_handle.accessed_path h path) Request.(PathOp(path, Rm)) Unmarshal.ok
  let mkdir h path = rpc "mkdir" (Xs_handle.accessed_path h path) Request.(PathOp(path, Mkdir)) Unmarshal.ok
  let setperms h path acl = rpc "setperms" (Xs_handle.accessed_path h path) Request.(PathOp(path, Setperms acl)) Unmarshal.ok
  let debug h cmd_args = rpc "debug" h (Request.Debug cmd_args) Unmarshal.list
  let restrict h domid = rpc "restrict" h (Request.Restrict domid) Unmarshal.ok
  let getdomainpath h domid = rpc "getdomainpath" h (Request.Getdomainpath domid) Unmarshal.string
  let watch h path token = rpc "watch" (Xs_handle.watch h path) (Request.Watch(path, Token.to_string token)) Unmarshal.ok
  let unwatch h path token = rpc "unwatch" (Xs_handle.watch h path) (Request.Unwatch(path, Token.to_string token)) Unmarshal.ok
  let introduce h domid store_mfn store_port = rpc "introduce" h (Request.Introduce(domid, store_mfn, store_port)) Unmarshal.ok
  let set_target h stubdom_domid domid = rpc "set_target" h (Request.Set_target(stubdom_domid, domid)) Unmarshal.ok
  let with_xs client f = f (Xs_handle.no_transaction client)

  let counter = ref 0l

  let wait client f =
    let open StringSet in
    counter := Int32.succ !counter;
    let token = Token.of_string (Printf.sprintf "%ld:xs_client.wait" !counter) in
    (* When we register the 'watcher', the dispatcher thread will signal us when
       watches arrive. *)
    let watcher = Watcher.make () in
    Hashtbl.add client.watchevents token watcher;

    (* We signal the caller via this cancellable task: *)
    let result, wakener = Lwt.task () in
    on_cancel result
      (fun () ->
        (* Trigger an orderly cleanup in the background: *)
	Watcher.cancel watcher
      );
    let h = Xs_handle.watching client in
    (* Adjust the paths we're watching (if necessary) and block (if possible) *)
    let adjust_paths () =
      let current_paths = Xs_handle.get_watched_paths h in
      (* Paths which weren't read don't need to be watched: *)
      let old_paths = diff current_paths (Xs_handle.get_accessed_paths h) in
      lwt () = Lwt_list.iter_s (fun p -> unwatch h p token) (elements old_paths) in
      (* Paths which were read do need to be watched: *)
      let new_paths = diff (Xs_handle.get_accessed_paths h) current_paths in
      lwt () = Lwt_list.iter_s (fun p -> watch h p token) (elements new_paths) in
      (* If we're watching the correct set of paths already then just block *)
      if old_paths = empty && (new_paths = empty)
      then begin
        lwt results = Watcher.get watcher in
        (* an empty results set means we've been cancelled: trigger cleanup *)
        if results = empty
        then fail (Failure "goodnight")
        else return ()
      end else return () in
    (* Main client loop: *)
    let rec loop () =
      lwt finished =
        try_lwt
          lwt result = f h in
          wakeup wakener result;
          return true
        with Eagain ->
          return false in
      if finished
      then return ()
      else adjust_paths () >> loop ()
    in
    let (_: unit Lwt.t) =
      try_lwt
        loop ()
      finally
        let current_paths = Xs_handle.get_watched_paths h in
        lwt () = Lwt_list.iter_s (fun p -> unwatch h p token) (elements current_paths) in
        Hashtbl.remove client.watchevents token;
        return () in
    result

  let rec with_xst client f =
    lwt tid = rpc "transaction_start" (Xs_handle.no_transaction client) Request.Transaction_start Unmarshal.int32 in
    let h = Xs_handle.transaction client tid in
    lwt result = f h in
    try_lwt
      lwt res' = rpc "transaction_end" h (Request.Transaction_end true) Unmarshal.string in
      if res' = "OK" then return result else raise_lwt (Error (Printf.sprintf "Unexpected transaction result: %s" res'))
    with Eagain ->
      with_xst client f
end

