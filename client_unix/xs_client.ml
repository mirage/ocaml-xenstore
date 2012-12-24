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

open Thread
open Xs_protocol

let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e
let with_mutex m f =
  Mutex.lock m;
  finally f (fun () -> Mutex.unlock m)

module type IO = sig
  type 'a t = 'a
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

module StringSet = Set.Make(struct type t = string let compare = compare end)

module Watcher = struct

  (** Someone who is watching paths is represented by one of these: *)
  type t = {
    mutable paths: StringSet.t; (* we never care about events or ordering, only paths *)
    mutable cancelling: bool; (* we need to stop watching and clean up *)
    c: Condition.t;
    m: Mutex.t;
  }

  let make () = {
    paths = StringSet.empty;
    cancelling = false;
    c = Condition.create ();
    m = Mutex.create ();
  }

  (** Register that a watched path has been changed *)
  let put (x: t) path =
    with_mutex x.m
      (fun () ->
        x.paths <- StringSet.add path x.paths;
        Condition.signal x.c
      )

  (** Return a set of modified paths, or an empty set if we're cancelling *)
  let get (x: t) =
    with_mutex x.m
      (fun () ->
        while x.paths = StringSet.empty && not x.cancelling do
          Condition.wait x.c x.m
        done;
        let results = x.paths in
        x.paths <- StringSet.empty;
        results
      )

  (** Called to shutdown the watcher and trigger an orderly cleanup *)
  let cancel (x: t) =
    with_mutex x.m
      (fun () ->
        x.cancelling <- true;
        Condition.signal x.c
      )
end

exception Malformed_watch_event
exception Unexpected_rid of int32
exception Dispatcher_failed

exception Cancelled

module Task = struct
  type 'a u = {
    mutable thing: 'a option;
    mutable cancelling: bool;
    mutable on_cancel: unit -> unit;
    m: Mutex.t;
    c: Condition.t
  }
  let make () = {
    thing = None;
    cancelling = false;
    on_cancel = (fun () -> ());
    m = Mutex.create ();
    c = Condition.create ();
  }
  let wakeup u thing = with_mutex u.m
    (fun () ->
      u.thing <- Some thing;
      Condition.signal u.c
    )
  let on_cancel u on_cancel = u.on_cancel <- on_cancel
  let cancel u = with_mutex u.m
    (fun () ->
      u.cancelling <- true;
      Condition.signal u.c
    );
    u.on_cancel ()
  let wait u = with_mutex u.m  (fun () ->
    let rec loop () =
      if u.cancelling then raise Cancelled
      else match u.thing with
      | None -> Condition.wait u.c u.m; loop ()
      | Some thing -> thing in
    loop ()
  )
end

module Client = functor(IO: IO with type 'a t = 'a) -> struct
  module PS = PacketStream(IO)

  (* Represents a single acive connection to a server *)
  type client = {
    transport: IO.channel;
    ps: PS.stream;
    rid_to_wakeup: (int32, Xs_protocol.t Task.u) Hashtbl.t;
    mutable dispatcher_thread: Thread.t option;
    mutable dispatcher_shutting_down: bool;
    watchevents: (Token.t, Watcher.t) Hashtbl.t;

    mutable suspended : bool;
    suspended_m : Mutex.t;
    suspended_c : Condition.t;
  }

  let recv_one t = match (PS.recv t.ps) with
    | Ok x -> x
    | Exception e -> raise e
  let send_one t = PS.send t.ps

  let handle_exn t e =
    Printf.fprintf stderr "Caught: %s\n%!" (Printexc.to_string e);
    begin match e with
      | Xs_protocol.Response_parser_failed x ->
      (* Lwt_io.hexdump Lwt_io.stderr x *)
         ()
      | _ -> ()
    end;
    t.dispatcher_shutting_down <- true;
    raise e

  let rec dispatcher t =
    let pkt = try recv_one t with e -> handle_exn t e in
    match get_ty pkt with
      | Op.Watchevent  ->
        begin match Unmarshal.list pkt with
          | Some [path; token] ->
            let token = Token.of_string token in
            (* We may get old watches: silently drop these *)
            if Hashtbl.mem t.watchevents token
            then (Watcher.put (Hashtbl.find t.watchevents token) path; dispatcher t)
            else dispatcher t
          | _ ->
            handle_exn t Malformed_watch_event
        end;
        dispatcher t
      | _ ->
        let rid = get_rid pkt in
        if Hashtbl.mem t.rid_to_wakeup rid then begin
          let u = Hashtbl.find t.rid_to_wakeup rid in
          Task.wakeup u pkt
        end else begin
          Printf.fprintf stderr "Unexpected rid: %ld\n%!" rid
        end


  let make () =
    let transport = IO.create () in
    let t = {
      transport = transport;
      ps = PS.make transport;
      rid_to_wakeup = Hashtbl.create 10;
      dispatcher_thread = None;
      dispatcher_shutting_down = false;
      watchevents = Hashtbl.create 10;
      suspended = false;
      suspended_m = Mutex.create ();
      suspended_c = Condition.create ();
    } in
    t.dispatcher_thread <- Some (Thread.create dispatcher t);
    t

    (** A 'handle' is a sub-connection used for a particular purpose.
        The handle is a convenient place to store sub-connection state *)
  type handle = {
    client: client;
    tid: int32; (** transaction id in use (0 means no transaction) *)
    mutable accessed_paths: StringSet.t option; (** paths read or written to *)
    mutable watched_paths: StringSet.t; (** paths being watched *)
  }

  module Handle = struct
    let make client = {
      client = client;
      tid = 0l;                       (* no transaction *)
      accessed_paths = None;          (* not recording accesses *)
      watched_paths = StringSet.empty (* no paths watched *)
    }

    (** Handle used for 'immediate' non-transactional read/writes *)
    let no_transaction client = make client

    (** Handle used for transactional read/writes *)
    let transaction client tid = { (make client) with tid = tid }

    (** Handle used to store watch-related information *)
    let watching client = { (make client) with accessed_paths = Some StringSet.empty }

    (** Get the list of recorded path accesses *)
    let accessed_path h path = match h.accessed_paths with
      | None -> h
      | Some ps -> h.accessed_paths <- Some (StringSet.add path ps); h

    (** Get the list of paths we have accessed *)
    let get_accessed_paths h = match h.accessed_paths with
      | None -> StringSet.empty
      | Some xs -> xs

    (** Declare that we are watching a path *)
    let watch h path = h.watched_paths <- StringSet.add path h.watched_paths; h

    (** Declare that we are nolonger watching a path *)
    let unwatch h path = h.watched_paths <- StringSet.remove path h.watched_paths; h

    (** Get the list of paths we're currently watching *)
    let get_watched_paths h = h.watched_paths
  end

  let rpc hint h payload unmarshal =
    let open Handle in
	let request = Request.print payload h.tid in
    let rid = get_rid request in
    let t = Task.make () in
    if h.client.dispatcher_shutting_down
    then raise Dispatcher_failed
    else begin
        Hashtbl.add h.client.rid_to_wakeup rid t;
        send_one h.client request;
        let res = Task.wait t in
        Hashtbl.remove h.client.rid_to_wakeup rid;
        response hint request res unmarshal
    end

  let directory h path = rpc "directory" (Handle.accessed_path h path) Request.(PathOp(path, Directory)) Unmarshal.list
  let read h path = rpc "read" (Handle.accessed_path h path) Request.(PathOp(path, Read)) Unmarshal.string
  let write h path data = rpc "write" (Handle.accessed_path h path) Request.(PathOp(path, Write data)) Unmarshal.ok
  let rm h path = rpc "rm" (Handle.accessed_path h path) Request.(PathOp(path, Rm)) Unmarshal.ok
  let mkdir h path = rpc "mkdir" (Handle.accessed_path h path) Request.(PathOp(path, Mkdir)) Unmarshal.ok
  let setperms h path acl = rpc "setperms" (Handle.accessed_path h path) Request.(PathOp(path, Setperms acl)) Unmarshal.ok
  let debug h cmd_args = rpc "debug" h (Request.Debug cmd_args) Unmarshal.list
  let restrict h domid = rpc "restrict" h (Request.Restrict domid) Unmarshal.ok
  let getdomainpath h domid = rpc "getdomainpath" h (Request.Getdomainpath domid) Unmarshal.string
  let watch h path token = rpc "watch" (Handle.watch h path) (Request.Watch(path, Token.to_string token)) Unmarshal.ok
  let unwatch h path token = rpc "unwatch" (Handle.watch h path) (Request.Unwatch(path, Token.to_string token)) Unmarshal.ok

  let with_xs client f = f (Handle.no_transaction client)

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
    let t = Task.make () in
    Task.on_cancel t
      (fun () ->
        (* Trigger an orderly cleanup in the background: *)
        Watcher.cancel watcher
      );
    let h = Handle.watching client in
    (* Adjust the paths we're watching (if necessary) and block (if possible) *)
    let adjust_paths () =
      let current_paths = Handle.get_watched_paths h in
      (* Paths which weren't read don't need to be watched: *)
      let old_paths = diff current_paths (Handle.get_accessed_paths h) in
      List.iter (fun p -> unwatch h p token) (elements old_paths);
      (* Paths which were read do need to be watched: *)
      let new_paths = diff (Handle.get_accessed_paths h) current_paths in
      List.iter (fun p -> watch h p token) (elements new_paths);
      (* If we're watching the correct set of paths already then just block *)
      if old_paths = empty && (new_paths = empty)
      then begin
        let results = Watcher.get watcher in
        (* an empty results set means we've been cancelled: trigger cleanup *)
        if results = empty
        then raise (Failure "goodnight")
      end in
    (* Main client loop: *)
    let rec loop () =
      let finished =
        try
          let result = f h in
          Task.wakeup t result;
          true
        with Eagain ->
          false in
      if finished
      then ()
      else adjust_paths (); loop ()
    in
    finally loop
      (fun () ->
        let current_paths = Handle.get_watched_paths h in
        List.iter (fun p -> unwatch h p token) (elements current_paths);
        Hashtbl.remove client.watchevents token;
      );
    Task.wait t

  let rec with_xst client f =
    let tid = rpc "transaction_start" (Handle.no_transaction client) Request.Transaction_start Unmarshal.int32 in
    let h = Handle.transaction client tid in
    let result = f h in
    try
      let res' = rpc "transaction_end" h (Request.Transaction_end true) Unmarshal.string in
      if res' = "OK" then result else raise (Error (Printf.sprintf "Unexpected transaction result: %s" res'))
    with Eagain ->
      with_xst client f

end
