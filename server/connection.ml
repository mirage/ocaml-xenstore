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
open Sexplib.Std
open Lwt
open Xenstore

let debug fmt = Logging.debug "connection" fmt
let info  fmt = Logging.info  "connection" fmt
let error fmt = Logging.debug "connection" fmt

module Watch = struct
  type t = Protocol.Name.t * string with sexp
end
module Watch_events = PQueue.Make(Watch)
module Watch_registrations = PSet.Make(Watch)

module PInt32 = PRef.Make(struct type t = int32 with sexp end)
module PPerms = PRef.Make(Perms)

type w = {
  con: t;
  watch: Watch.t;
  mutable count: int;
}

and t = {
	address: Uri.t;
	domid: int;
	idx: int; (* unique counter *)
	transactions: (int32, Transaction.t) Hashtbl.t;
        next_tid: PInt32.t;
	ws: (Protocol.Name.t, w list) Hashtbl.t;
	mutable nb_watches: int;
	mutable nb_dropped_watches: int;
	mutable stat_nb_ops: int;
        perm: PPerms.t;
        watch_registrations: Watch_registrations.t;
	watch_events: Watch_events.t;
        m: Lwt_mutex.t;
	cvar: unit Lwt_condition.t;
	domainpath: Protocol.Name.t;
}

let index t = t.idx
let domainpath t = t.domainpath
let domid t = t.domid
let address t = t.address
let watch_events t = t.watch_events
let perm t = t.perm

let incr_nb_ops t = t.stat_nb_ops <- t.stat_nb_ops + 1

let pop_watch_events_nowait_nolock t =
  Watch_events.fold (fun acc x -> x :: acc) [] (watch_events t) >>= fun q ->
  Watch_events.clear (watch_events t) >>= fun () ->
  return (List.rev q)

let pop_watch_events t =
  Lwt_mutex.with_lock t.m
    (fun () ->
      let rec wait () =
        Watch_events.length (watch_events t) >>= fun l ->
        if l = 0 then begin
          Lwt_condition.wait ~mutex:t.m t.cvar >>= fun () ->
          wait ()
        end else return () in
      wait () >>= fun () ->
      pop_watch_events_nowait_nolock t
    )

let pop_watch_events_nowait t =
  Lwt_mutex.with_lock t.m
    (fun () ->
      pop_watch_events_nowait_nolock t
    )

let by_address : (Uri.t, t) Hashtbl.t = Hashtbl.create 128
let by_index   : (int,   t) Hashtbl.t = Hashtbl.create 128

let watches : (string, w list) Trie.t ref = ref (Trie.create ())

let w_create ~con ~name ~token = { 
  con = con;
  watch = name, token;
  count = 0;
}

let counter = ref 0

let path_of_address thing address idx = [ "tool"; "xenstored"; thing; (match Uri.scheme address with Some x -> x | None -> "unknown"); string_of_int idx ]

let key_of_name x =
  let open Protocol.Name in match x with
  | Predefined IntroduceDomain -> [ "@introduceDomain" ]
  | Predefined ReleaseDomain   -> [ "@releaseDomain" ]
  | Absolute p -> "" :: (List.map Protocol.Path.Element.to_string (Protocol.Path.to_list p))
  | Relative p -> "" :: (List.map Protocol.Path.Element.to_string (Protocol.Path.to_list p))

let fire_one limits name watch =
  let name = match name with
  | None ->
    (* If no specific path was modified then we fire the generic watch *)
    fst watch.watch
  | Some name ->
    (* If the watch was registered as a relative path, then we make
       all the watch events relative too *)
    if Protocol.Name.is_relative (fst watch.watch)
    then Protocol.Name.(relative name watch.con.domainpath)
    else name in
  let token = snd watch.watch in
  let open Xenstore.Protocol in
  watch.count <- watch.count + 1;
  begin match limits with
  | Some limits ->
    Watch_events.length watch.con.watch_events >>= fun w ->
    if w >= limits.Limits.number_of_queued_watch_events then begin
      error "domain %u reached watch event quota (%d >= %d): dropping watch %s:%s" watch.con.domid w limits.Limits.number_of_queued_watch_events (Protocol.Name.to_string name) token;
      watch.con.nb_dropped_watches <- watch.con.nb_dropped_watches + 1;
      return ()
    end else begin
      Watch_events.add (name, token) watch.con.watch_events >>= fun () ->
      Lwt_condition.signal watch.con.cvar ();
      return ()
    end
  | None ->
      Watch_events.add (name, token) watch.con.watch_events >>= fun () ->
      Lwt_condition.signal watch.con.cvar ();
      return ()
  end

let watch con limits (name, token) =
  let l =
    if Hashtbl.mem con.ws name
    then Hashtbl.find con.ws name
    else [] in

  ( if List.exists (fun w -> snd w.watch = token) l then begin
      info "watch %s:%s already exists" (Protocol.Name.to_string name) token;
      (* registering a watch needs to be idempotent to allow crashes *)
      return ()
    end else return () ) >>= fun () ->

  ( match limits with
    | Some limits ->
      if con.nb_watches >= limits.Limits.number_of_registered_watches then begin
        error "Failed to add watch for domain %u, already reached quota (%d >= %d)" con.domid con.nb_watches limits.Limits.number_of_registered_watches;
        fail Limits.Limit_reached;
      end else return ()
    | None -> return () ) >>= fun () ->

  let watch = w_create ~con ~token ~name in
  fire_one limits None watch >>= fun () ->
  
  Hashtbl.replace con.ws name (watch :: l);
  con.nb_watches <- con.nb_watches + 1;

  watches :=
    (let key = key_of_name (Protocol.Name.(resolve name con.domainpath)) in
     let ws = if Trie.mem !watches key then Trie.find !watches key else [] in
     Trie.set !watches key (watch :: ws));

  Watch_registrations.add (name, token) con.watch_registrations

let unwatch con (name, token) =
  match (
    try
      let ws = Hashtbl.find con.ws name in
      let w = List.find (fun w -> snd w.watch = token) ws in
      Some (ws, w)
    with Not_found ->
      None
  ) with
  | None ->
    info "unwatch: watch %s not currently registered" (Protocol.Name.to_string name);
    (* unwatch should be idempotent *)
    return ()
  | Some (ws, w) ->
    let filtered = List.filter (fun e -> e != w) ws in
    if List.length filtered > 0
    then Hashtbl.replace con.ws name filtered
    else Hashtbl.remove con.ws name;

    con.nb_watches <- con.nb_watches - 1;

    watches :=
      (let key = key_of_name (Protocol.Name.(resolve name con.domainpath)) in
       let ws = List.filter (fun x -> x != w) (Trie.find !watches key) in
       if ws = [] then Trie.unset !watches key else Trie.set !watches key ws);

    Watch_registrations.remove (name, token) con.watch_registrations

let fire limits (op, name) =
	let key = key_of_name name in
        let ws = Trie.fold_path (fun acc _ w -> match w with None -> acc | Some ws -> acc @ ws) !watches [] key in
        Lwt_list.iter_s (fire_one limits (Some name)) ws >>= fun () ->
	if op = Protocol.Op.Rm
	then
          let ws = Trie.fold (fun acc _ w -> match w with None -> acc | Some ws -> acc @ ws) (Trie.sub !watches key) [] in
          Lwt_list.iter_s (fire_one limits None) ws
        else
          return ()

let register_transaction limits con store =
  ( match limits with
    | Some limits ->
      if Hashtbl.length con.transactions >= limits.Limits.number_of_active_transactions then begin
        error "domain %u has reached the open transaction limit (%d >= %d)" con.domid (Hashtbl.length con.transactions) limits.Limits.number_of_active_transactions;
        fail Limits.Limit_reached;
      end else return ()
    | None -> return ()
  ) >>= fun () ->
  PInt32.get con.next_tid >>= fun id ->
  PInt32.set (Int32.succ id) con.next_tid >>= fun () ->
  let ntrans = Transaction.make id store in
  Hashtbl.add con.transactions id ntrans;
  return id

let unregister_transaction con tid =
	Hashtbl.remove con.transactions tid

let get_transaction con tid =
	try
		Hashtbl.find con.transactions tid
	with Not_found as e ->
		error "Failed to find transaction %lu on %s" tid (Uri.to_string con.address);
		raise e

let mark_symbols con =
	Hashtbl.iter (fun _ t -> Store.mark_symbols (Transaction.get_store t)) con.transactions

let destroy address =
  if not(Hashtbl.mem by_address address) then begin
    error "Failed to remove connection for: %s" (Uri.to_string address);
    return ()
  end else begin
    let c = Hashtbl.find by_address address in
    info "Destroying connection to %s" (Uri.to_string c.address);
    watches := Trie.map
      (fun watches ->
        match List.filter (fun w -> w.con != c) watches with
        | [] -> None
        | ws -> Some ws
      ) !watches;
    Hashtbl.remove by_address address;
    Hashtbl.remove by_index c.idx;
    Watch_events.clear c.watch_events >>= fun () ->
    Watch_registrations.clear c.watch_registrations >>= fun () ->
    PInt32.destroy c.next_tid >>= fun () ->
    PPerms.destroy c.perm
  end

let create (address, domid) =
  if Hashtbl.mem by_address address then begin
    info "Connection.create: found existing connection for %s" (Uri.to_string address);
    return (Hashtbl.find by_address address)
  end else begin
    let idx = !counter in
    incr counter;
    Watch_events.create (path_of_address "events" address idx) >>= fun watch_events ->
    Watch_registrations.create (path_of_address "registrations" address idx) >>= fun watch_registrations ->
    PInt32.create (path_of_address "next-transaction-id" address idx) 1l >>= fun next_tid ->
    PPerms.create (path_of_address "permissions" address idx) (Perms.of_domain domid) >>= fun perm ->
    let con = {
      address; domid; idx; next_tid; perm;
      transactions = Hashtbl.create 5;
      ws = Hashtbl.create 8;
      nb_watches = 0;
      nb_dropped_watches = 0;
      stat_nb_ops = 0;
      watch_events; watch_registrations;
      m = Lwt_mutex.create ();
      cvar = Lwt_condition.create ();
      domainpath = Store.getdomainpath domid;
    } in
    info "New connection from %s" (Uri.to_string con.address);
    Hashtbl.replace by_address address con;
    Hashtbl.replace by_index con.idx con;

    (* Recreate the in-memory tables of watch registrations *)
    Watch_registrations.fold (fun acc w -> w :: acc) [] watch_registrations >>= fun watches ->
    Lwt_list.iter_s (watch con None) watches >>= fun () ->

    return con
  end

module Introspect = struct
	include Tree.Unsupported

	let read_connection t perms path c = function
		| [] ->
			""
		| "address" :: [] ->
			Uri.to_string c.address
		| "current-transactions" :: [] ->
			string_of_int (Hashtbl.length c.transactions)
		| "total-operations" :: [] ->
			string_of_int c.stat_nb_ops
		| "total-dropped-watches" :: [] ->
			string_of_int c.nb_dropped_watches
		| _ -> raise (Node.Doesnt_exist path)

	let read t (perms: Perms.t) (path: Protocol.Path.t) =
		Perms.has perms Perms.CONFIGURE;
		match Protocol.Path.to_string_list path with
		| [] -> ""
		| scheme :: [] -> ""
		| scheme :: idx :: rest ->
			let idx = int_of_string idx in
			if not(Hashtbl.mem by_index idx) then raise (Node.Doesnt_exist path);
			let c = Hashtbl.find by_index idx in
			read_connection t perms path c rest

	let exists t perms path = try ignore(read t perms path); true with Node.Doesnt_exist _ -> false

	let rec between start finish = if start > finish then [] else start :: (between (start + 1) finish)

	let list_connection t perms c = function
		| [] ->
			[ "address"; "current-transactions"; "total-operations"; "total-dropped-watches" ]
		| _ -> []

	let ls t perms path =
		Perms.has perms Perms.CONFIGURE;
		match Protocol.Path.to_string_list path with
		| [] -> [ "unix"; "domain" ]
		| [ scheme ] ->
			Hashtbl.fold (fun x c acc -> match Uri.scheme x with
                        | Some scheme' when scheme = scheme' -> string_of_int c.idx :: acc
			| _ -> acc) by_address []
		| scheme :: idx :: rest ->
			let idx = try int_of_string idx with _ -> raise (Node.Doesnt_exist path) in
			if not(Hashtbl.mem by_index idx) then raise (Node.Doesnt_exist path);
			let c = Hashtbl.find by_index idx in
			list_connection t perms c rest
end
let _ = Mount.mount (Protocol.Path.of_string "/tool/xenstored/connection") (module Introspect: Tree.S)
