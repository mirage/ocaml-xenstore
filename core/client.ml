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
open Protocol

let ( |> ) a b = b a
let ( ++ ) f g x = f (g x)

module StringSet = Handle.StringSet

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

let fail_on_error = function
| `Ok x -> return x
| `Error x -> fail (Failure x)

module Make = functor(IO: S.TRANSPORT) -> struct

  type 'a t = 'a IO.t
  let ( >>= ) = IO.( >>= )
  let return = IO.return

  (* Represents a single acive connection to a server *)
  type client = {
    mutable transport: IO.channel;
    rid_to_wakeup: (int32, Response.t Lwt.u) Hashtbl.t;
    mutable dispatcher_thread: unit Lwt.t;
    mutable dispatcher_shutting_down: bool;
    watchevents: (Token.t, Watcher.t) Hashtbl.t;

    mutable suspended : bool;
    suspended_m : Lwt_mutex.t;
    suspended_c : unit Lwt_condition.t;
    send_header : Cstruct.t; (* protected by suspended_m *)
    send_payload : Cstruct.t;
  }

  let client_cache = ref None
  (* The whole application must only use one xenstore client, which will
     multiplex all requests onto the same ring. *)

  let client_cache_m = Lwt_mutex.create ()
  (* Multiple threads will call 'make' in parallel. We must ensure only
     one client is created. *)

  let handle_exn t e =
    Printf.fprintf stderr "Caught: %s\n%!" (Printexc.to_string e);
    t.dispatcher_shutting_down <- true; (* no more hashtable entries after this *)
    (* all blocking threads are failed with our exception *)
    lwt () = Lwt_mutex.with_lock t.suspended_m (fun () ->
      Printf.fprintf stderr "Propagating exception to %d threads\n%!" (Hashtbl.length t.rid_to_wakeup);
      Hashtbl.iter (fun _ u -> Lwt.wakeup_later_exn u e) t.rid_to_wakeup;
      return ()) in
    raise_lwt e

  let rec dispatcher t =
    let buf = Cstruct.create Protocol.xenstore_payload_max in
    IO.read t.transport (Cstruct.sub buf 0 Header.sizeof) >>= fun () ->
    fail_on_error (Header.unmarshal buf) >>= fun hdr ->
    let payload = Cstruct.sub buf 0 hdr.Header.len in
    IO.read t.transport payload >>= fun () ->

    fail_on_error (Response.unmarshal hdr payload) >>= fun r ->
    match r with
    | Response.Watchevent(path, token) ->
        lwt () =
          let token = Token.unmarshal token in
          (* We may get old watches: silently drop these *)
          if Hashtbl.mem t.watchevents token
          then Watcher.put (Hashtbl.find t.watchevents token) (Name.to_string path) >> dispatcher t
          else dispatcher t in
        dispatcher t
    | r ->
        let rid = hdr.Header.rid in
        lwt thread = Lwt_mutex.with_lock t.suspended_m (fun () -> 
          if Hashtbl.mem t.rid_to_wakeup rid
          then return (Some (Hashtbl.find t.rid_to_wakeup rid))
          else return None) in
        match thread with
          | None -> handle_exn t (Unexpected_rid rid)
          | Some thread -> 
            begin
              Lwt.wakeup_later thread r;
              dispatcher t
            end


  let make_unsafe () =
    lwt transport = IO.create () in
    let t = {
      transport = transport;
      rid_to_wakeup = Hashtbl.create 10;
      dispatcher_thread = return ();
      dispatcher_shutting_down = false;
      watchevents = Hashtbl.create 10;
      suspended = false;
      suspended_m = Lwt_mutex.create ();
      suspended_c = Lwt_condition.create ();
      send_header = Cstruct.create Header.sizeof;
      send_payload = Cstruct.create Protocol.xenstore_payload_max;
    } in
    t.dispatcher_thread <- dispatcher t;
    return t

  let make () =
    Lwt_mutex.with_lock client_cache_m
      (fun () -> match !client_cache with
         | Some c -> return c
         | None ->
           lwt c = make_unsafe () in
           client_cache := Some c;
           return c
      )

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

  let resume_unsafe t =
    lwt () = Lwt_mutex.with_lock t.suspended_m (fun () -> 
      t.suspended <- false;
      t.dispatcher_shutting_down <- false;
      Lwt_condition.broadcast t.suspended_c (); 
      return ()) in
    t.dispatcher_thread <- dispatcher t;
    return ()

  let resume t = match !client_cache with
    | None -> Lwt.return ()
    | Some c ->
      IO.create () >>= fun transport ->
      c.transport <- transport;
      resume_unsafe t

  type handle = client Handle.t

  let make_rid =
	  let counter = ref 0l in
	  fun () ->
		  let result = !counter in
		  counter := Int32.succ !counter;
		  result

  let rpc h payload f =
    let open Handle in
    let rid = make_rid () in
    let tid = get_tid h in
    let ty = Request.get_ty payload in
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
        let next = Request.marshal payload c.send_payload in
        let len = next.Cstruct.off in
        let payload = Cstruct.sub c.send_payload 0 len in
        let hdr = { Header.rid; tid; ty; len } in
        ignore(Header.marshal hdr c.send_header);
        IO.write c.transport c.send_header >>= fun () ->
        IO.write c.transport payload >>= fun () ->
        return ()) in
      lwt res = t in
      lwt () = Lwt_mutex.with_lock c.suspended_m 
        (fun () -> 
          Hashtbl.remove c.rid_to_wakeup rid;
          Lwt_condition.broadcast c.suspended_c (); 
          return ()) in
      f res
 end

  let error hint = function
  | Response.Error "ENOENT" -> raise (Enoent hint)
  | Response.Error "EAGAIN" -> raise Eagain
  | Response.Error "EINVAL" -> raise Invalid
  | Response.Error x        -> raise (Error x)
  | x              -> raise (Error (Printf.sprintf "%s: unexpected response: %s" hint (Sexplib.Sexp.to_string_hum (Response.sexp_of_t x))))

  let directory h path = rpc (Handle.accessed_path h path) Request.(PathOp(path, Directory))
    (function Response.Directory ls -> return ls
    | x -> error "directory" x)
  let read h path = rpc (Handle.accessed_path h path) Request.(PathOp(path, Read))
    (function Response.Read x -> return x
    | x -> error "read" x)
  let write h path data = rpc (Handle.accessed_path h path) Request.(PathOp(path, Write data))
    (function Response.Write -> return ()
    | x -> error "write" x)
  let rm h path = rpc (Handle.accessed_path h path) Request.(PathOp(path, Rm))
    (function Response.Rm -> return ()
    | x -> error "rm" x)
  let mkdir h path = rpc (Handle.accessed_path h path) Request.(PathOp(path, Mkdir))
    (function Response.Mkdir -> return ()
    | x -> error "mkdir" x)
  let setperms h path acl = rpc (Handle.accessed_path h path) Request.(PathOp(path, Setperms acl))
    (function Response.Setperms -> return ()
    | x -> error "setperms" x)
  let debug h cmd_args = rpc h (Request.Debug cmd_args)
    (function Response.Debug debug -> return debug
    | x -> error "debug" x)
  let restrict h domid = rpc h (Request.Restrict domid)
    (function Response.Restrict -> return ()
    | x -> error "restrict" x)
  let getdomainpath h domid = rpc h (Request.Getdomainpath domid)
    (function Response.Getdomainpath x -> return x
    | x -> error "getdomainpath" x)
  let watch h path token = rpc (Handle.watch h path) (Request.Watch(path, Token.marshal token))
    (function Response.Watch -> return ()
    | x -> error "watch" x)
  let unwatch h path token = rpc (Handle.watch h path) (Request.Unwatch(path, Token.marshal token))
    (function Response.Unwatch -> return ()
    | x -> error "unwatch" x)
  let introduce h domid store_mfn store_port = rpc h (Request.Introduce(domid, store_mfn, store_port))
    (function Response.Introduce -> return ()
    | x -> error "introduce" x)
  let set_target h stubdom_domid domid = rpc h (Request.Set_target(stubdom_domid, domid))
    (function Response.Set_target -> return ()
    | x -> error "set_target" x)
  let immediate client f = f (Handle.no_transaction client)

  let counter = ref 0l

  let wait client f =
    let open StringSet in
    counter := Int32.succ !counter;
    let token = Token.unmarshal (Printf.sprintf "%ld:xs_client.wait" !counter) in
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
    let h = Handle.watching client in
    (* Adjust the paths we're watching (if necessary) and block (if possible) *)
    let adjust_paths () =
      let current_paths = Handle.get_watched_paths h in
      (* Paths which weren't read don't need to be watched: *)
      let old_paths = diff current_paths (Handle.get_accessed_paths h) in
      lwt () = Lwt_list.iter_s (fun p -> unwatch h p token) (elements old_paths) in
      (* Paths which were read do need to be watched: *)
      let new_paths = diff (Handle.get_accessed_paths h) current_paths in
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
        let current_paths = Handle.get_watched_paths h in
        lwt () = Lwt_list.iter_s (fun p -> unwatch h p token) (elements current_paths) in
        Hashtbl.remove client.watchevents token;
        return () in
    result

  let rec transaction client f =
    lwt tid = rpc (Handle.no_transaction client) Request.Transaction_start
      (function Response.Transaction_start tid -> return tid
       | x -> error "transaction_start" x) in
    let h = Handle.transaction client tid in
    lwt result = f h in
    try_lwt
      rpc h (Request.Transaction_end true)
        (function Response.Transaction_end -> return result
         | x -> error "transaction_end" x)
    with Eagain ->
      transaction client f
end

