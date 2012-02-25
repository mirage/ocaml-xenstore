(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 * Copyright (C) 2010 Anil Madhavapeddy <anil@recoil.org>
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

open Lwt

exception Partial_not_empty
exception Unexpected_packet of string

(** Thrown when a path looks invalid e.g. if it contains "//" *)
exception Invalid_path of string

let unexpected_packet expected received =
	let s = Printf.sprintf "expecting %s received %s"
	                       (Xs_packet.Op.to_string expected)
	                       (Xs_packet.Op.to_string received) in
	fail (Unexpected_packet s)

type watch_queue = {
    events: (string * string) Queue.t;
    c: unit Lwt_condition.t;
    m: Lwt_mutex.t;
}

type con = {
	xb: Xb.t;
    watchevents: (Xs_packet.token, watch_queue) Hashtbl.t;
}


let pkt_send con =
    match Xb.has_old_output con.xb with
    | true -> fail Partial_not_empty
    | false ->
        let rec loop_output () = 
            lwt w = Xb.output con.xb in
            if w then return () else loop_output ()
        in
        loop_output ()

(* receive one packet - can sleep *)
let pkt_recv_one con =
        let rec loop_input () =
            lwt w = Xb.input con.xb in 
            if w then return () else loop_input ()
        in 
        lwt () = loop_input () in
	return (Xb.get_in_packet con.xb)

let find_watch_event_queue con token =
    if not(Hashtbl.mem con.watchevents token) 
	then Hashtbl.replace con.watchevents token {
        events = Queue.create ();
        c = Lwt_condition.create ();
        m = Lwt_mutex.create ()
    };
    Hashtbl.find con.watchevents token

let remove_watch_event_queue con token =
    if Hashtbl.mem con.watchevents token
    then Hashtbl.remove con.watchevents token

let queue_watchevent con pkt =
    let data = Xs_packet.get_data pkt in
	let ls = split_string ~limit:2 '\000' data in
	if List.length ls != 2 then
                print_endline "XXX queue_watchevent: arg mismatch";
(* XXX		raise (Xs_packet.DataError "arguments number mismatch"); *)
	let event = List.nth ls 0
	and event_data = List.nth ls 1 in
    let token = Xs_packet.parse_token event_data in
    let w = find_watch_event_queue con token in
	Queue.push (event, Xs_packet.user_string_of_token token) w.events;
	Lwt_condition.signal w.c ()

let has_watchevents con token =
    let w = find_watch_event_queue con token in
	Queue.length w.events > 0
let get_watchevent con token =
    let w = find_watch_event_queue con token in
    Queue.pop w.events
let read_watchevent con token =
    let w = find_watch_event_queue con token in
    Lwt_mutex.with_lock w.m
        (fun () ->
            lwt () = Lwt_condition.wait ~mutex:w.m w.c in
            return (Queue.pop w.events)
        )

let rid_to_wakeup = Hashtbl.create 10

let rec dispatcher con =
    lwt pkt = pkt_recv_one con in
	match Xs_packet.get_ty pkt with
	| Xs_packet.Op.Watchevent  ->
		queue_watchevent con pkt;
		dispatcher con
    | _ ->
        let rid = Xs_packet.get_rid pkt in
        if not(Hashtbl.mem rid_to_wakeup rid)
        then Printf.printf "Unknown rid:%d in ty:%s\n%!" rid (Xs_packet.Op.to_string (Xs_packet.get_ty pkt))
        else Lwt.wakeup (Hashtbl.find rid_to_wakeup rid) pkt;
	    dispatcher con

let create () = 
	let xb = Xb.init () in
	let con = { xb = xb; watchevents = Hashtbl.create 10 } in
	ignore(dispatcher con);
	con
    
let rpc request con =
    let th, wakeup = Lwt.wait () in
    let rid = Xs_packet.get_rid request in
	Hashtbl.replace rid_to_wakeup rid wakeup;
    Xb.queue con.xb request;
	lwt () = pkt_send con in
    lwt pkt = th in
    Hashtbl.remove rid_to_wakeup rid;

    let request_ty = Xs_packet.get_ty request in
	match Xs_packet.get_ty pkt with
	| Xs_packet.Op.Error       -> (
		match Xs_packet.get_data pkt with
		| "ENOENT" -> fail Xb.Noent
		| "EAGAIN" -> fail Xb.Eagain
		| "EINVAL" -> fail Xb.Invalid
		| s        -> fail (Xs_packet.Error s))
	| resp_ty when resp_ty = request_ty -> return (rid, Xs_packet.get_data pkt)
	| resp_ty -> unexpected_packet request_ty resp_ty

let ack s =
        Lwt.bind s (fun (_, s) ->
	  if s = "OK" then return () else fail (Xs_packet.DataError s)
        ) 

let data s =
     Lwt.bind s (fun (_, s) -> return s)

let debug command con =
	data (rpc (Xs_packet.debug command) con)

let directory tid path con =
	lwt _, data = rpc (Xs_packet.directory tid path) con in
	return (split_string '\000' data)

let read tid path con =
	data (rpc (Xs_packet.read tid path) con)

let readv tid dir vec con =
	Lwt_list.map_s (fun path -> read tid path con)
		(if dir <> "" then
			(List.map (fun v -> dir ^ "/" ^ v) vec) else vec)

let getperms tid path con =
	lwt _, perms = rpc (Xs_packet.getperms tid path) con in
        return (perms_of_string perms)

let watch path token con =
	lwt rid, _ = rpc (Xs_packet.watch path token) con in
    return ()

let unwatch path token con =
    lwt () = ack (rpc (Xs_packet.unwatch path token) con) in
    (* No more events should be arriving in the queue, so it should be
       ok to delete it without worrying about it coming back *)
    remove_watch_event_queue con token;
    return ()

let transaction_start con =
	lwt _, data = rpc (Xs_packet.transaction_start ()) con in
	try_lwt
		return (int_of_string data)
	with
		_ -> fail (Xs_packet.DataError (Printf.sprintf "int expected; got '%s'" data))

let transaction_end tid commit con =
	try_lwt
		ack (rpc (Xs_packet.transaction_end tid commit) con) >>
                return true
	with | Xb.Eagain ->
                return false

let introduce domid mfn port con =
	ack (rpc (Xs_packet.introduce domid mfn port) con)

let release domid con =
	ack (rpc (Xs_packet.release domid) con)

let resume domid con =
	ack (rpc (Xs_packet.resume domid) con)

let getdomainpath domid con =
	data (rpc (Xs_packet.getdomainpath domid) con)

let write tid path value con =
	ack (rpc (Xs_packet.write tid path value) con)

let writev tid dir vec con =
	Lwt_list.iter_s (fun (entry, value) ->
		let path = (if dir <> "" then dir ^ "/" ^ entry else entry) in
		write tid path value con) vec

let mkdir tid path con =
	ack (rpc (Xs_packet.mkdir tid path) con)

let rm tid path con =
	try_lwt
		ack (rpc (Xs_packet.rm tid path) con)
	with
		Xb.Noent -> return ()

let setperms tid path perms con =
	ack (rpc (Xs_packet.setperms tid path (string_of_perms perms)) con)

let setpermsv tid dir vec perms con =
	Lwt_list.iter_s (fun entry ->
		let path = (if dir <> "" then dir ^ "/" ^ entry else entry) in
		setperms tid path perms con) vec
