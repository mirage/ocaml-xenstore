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

exception End_of_file

module Watch_event = struct
  type t = string * string with sexp
end
module Watch_events = PQueue.Make(Watch_event)

type watch = {
	con: t;
	token: string;
	name: Protocol.Name.t;
	mutable count: int;
}

and t = {
	address: Uri.t;
	domid: int;
	domstr: string;
	idx: int; (* unique counter *)
	transactions: (int32, Transaction.t) Hashtbl.t;
	mutable next_tid: int32;
	watches: (Protocol.Name.t, watch list) Hashtbl.t;
	mutable nb_watches: int;
	mutable nb_dropped_watches: int;
	mutable stat_nb_ops: int;
	mutable perm: Perms.t;
	watch_events: Watch_events.t;
	cvar: unit Lwt_condition.t;
	domainpath: Protocol.Name.t;
}

let by_address : (Uri.t, t) Hashtbl.t = Hashtbl.create 128
let by_index   : (int,   t) Hashtbl.t = Hashtbl.create 128

let watches : (string, watch list) Trie.t ref = ref (Trie.create ())

let list_of_watches () =
	Trie.fold (fun acc path v_opt ->
		match v_opt with
		| None -> Printf.sprintf "%s <- None" path :: acc
		| Some vs -> Printf.sprintf "%s <- %s" path (String.concat ", " (List.map (fun v -> v.con.domstr) vs)) :: acc
	) !watches []

let watch_create ~con ~name ~token = { 
	con = con; 
	token = token; 
	name = name;
	count = 0;
}

let get_con w = w.con
 
let number_of_transactions con =
	Hashtbl.length con.transactions

let anon_id_next = ref 1

let destroy address =
	try
		let c = Hashtbl.find by_address address in
		Logging.end_connection ~tid:Transaction.none ~con:c.domstr;
		watches := Trie.map
			(fun watches ->
				match List.filter (fun w -> w.con != c) watches with
				| [] -> None
				| ws -> Some ws
			) !watches;
		Hashtbl.remove by_address address;
		Hashtbl.remove by_index c.idx;
	with Not_found ->
		error "Failed to remove connection for: %s" (Uri.to_string address)

let counter = ref 0

let path_of_address address idx = [ "tool"; "xenstored"; "connection"; match Uri.scheme address with Some x -> x | None -> "unknown"; string_of_int idx ]

let create (address, dom) =
	if Hashtbl.mem by_address address then begin
		info "Connection.create: found existing connection for %s: closing" (Uri.to_string address);
		destroy address
	end;
	let idx = !counter in
	incr counter;
	Watch_events.create (path_of_address address idx) >>= fun watch_events ->
	let con = 
	{
		address = address;
		domid = dom;
		idx;
		domstr = Uri.to_string address;
		transactions = Hashtbl.create 5;
		next_tid = 1l;
		watches = Hashtbl.create 8;
		nb_watches = 0;
		nb_dropped_watches = 0;
		stat_nb_ops = 0;
		perm = Perms.of_domain dom;
		watch_events;
		cvar = Lwt_condition.create ();
		domainpath = Store.getdomainpath dom;
	}
	in
	Logging.new_connection ~tid:Transaction.none ~con:con.domstr;
	Hashtbl.replace by_address address con;
	Hashtbl.replace by_index con.idx con;
	return con

let restrict con domid =
	con.perm <- Perms.restrict con.perm domid

let get_watches (con: t) name =
	if Hashtbl.mem con.watches name
	then Hashtbl.find con.watches name
	else []

let key_of_name x =
  let open Protocol.Name in match x with
  | Predefined IntroduceDomain -> [ "@introduceDomain" ]
  | Predefined ReleaseDomain   -> [ "@releaseDomain" ]
  | Absolute p -> "" :: (List.map Protocol.Path.Element.to_string (Protocol.Path.to_list p))
  | Relative p -> "" :: (List.map Protocol.Path.Element.to_string (Protocol.Path.to_list p))

let add_watch con limits name token =
  begin match limits with
  | Some limits ->
    if con.nb_watches >= limits.Limits.number_of_registered_watches then begin
      error "Failed to add watch for domain %u, already reached quota (%d >= %d)" con.domid con.nb_watches limits.Limits.number_of_registered_watches;
      raise Limits.Limit_reached;
    end
  | None -> ()
  end;

	let l = get_watches con name in
	if List.exists (fun w -> w.token = token) l
	then raise (Store.Already_exists (Printf.sprintf "%s:%s" (Protocol.Name.to_string name) token));
	let watch = watch_create ~con ~token ~name in
	Hashtbl.replace con.watches name (watch :: l);
	con.nb_watches <- con.nb_watches + 1;

	watches :=
		(let key = key_of_name (Protocol.Name.(resolve name con.domainpath)) in
		let ws =
            if Trie.mem !watches key
            then Trie.find !watches key
            else []
        in
        Trie.set !watches key (watch :: ws));
	watch

let del_watch con name token =
	let ws = Hashtbl.find con.watches name in
	let w = List.find (fun w -> w.token = token) ws in
	let filtered = List.filter (fun e -> e != w) ws in
	if List.length filtered > 0 then
		Hashtbl.replace con.watches name filtered
	else
		Hashtbl.remove con.watches name;
	con.nb_watches <- con.nb_watches - 1;

	watches :=
		(let key = key_of_name (Protocol.Name.(resolve name con.domainpath)) in
		let ws = List.filter (fun x -> x != w) (Trie.find !watches key) in
        if ws = [] then
                Trie.unset !watches key
        else
                Trie.set !watches key ws)


let fire_one limits name watch =
	let name = match name with
		| None ->
			(* If no specific path was modified then we fire the generic watch *)
			watch.name
		| Some name ->
			(* If the watch was registered as a relative path, then we make
			   all the watch events relative too *)
			if Protocol.Name.is_relative watch.name
			then Protocol.Name.(relative name watch.con.domainpath)
			else name in
	let name = Protocol.Name.to_string name in
	let open Xenstore.Protocol in
	Logging.response ~tid:0l ~con:watch.con.domstr (Response.Watchevent(name, watch.token));
	watch.count <- watch.count + 1;
        begin match limits with
        | Some limits ->
                Watch_events.length watch.con.watch_events >>= fun w ->
	        if w >= limits.Limits.number_of_queued_watch_events then begin
		        error "domain %u reached watch event quota (%d >= %d): dropping watch %s:%s" watch.con.domid w limits.Limits.number_of_queued_watch_events name watch.token;
                        watch.con.nb_dropped_watches <- watch.con.nb_dropped_watches + 1;
                        return ()
        	end else begin
	        	Watch_events.add (name, watch.token) watch.con.watch_events >>= fun () ->
                        Lwt_condition.signal watch.con.cvar ();
                        return ()
	        end
        | None -> return ()
        end

let fire limits (op, name) =
	let key = key_of_name name in
        let ws = Trie.fold_path (fun acc _ w -> match w with None -> acc | Some ws -> acc @ ws) !watches [] key in
        Lwt_list.iter_s (fire_one limits (Some name)) ws >>= fun () ->
        (*
	Trie.iter_path
		(fun _ w -> match w with
		| None -> ()
		| Some ws -> List.iter (fire_one (Some name)) ws
		) !watches key;
	*)
	if op = Protocol.Op.Rm
	then
          let ws = Trie.fold (fun acc _ w -> match w with None -> acc | Some ws -> acc @ ws) (Trie.sub !watches key) [] in
          Lwt_list.iter_s (fire_one limits None) ws
        else
          return ()
          (*
        Trie.iter
		(fun _ w -> match w with
		| None -> ()
		| Some ws -> List.iter (fire_one None) ws
                ) (Trie.sub !watches key)
        *)
let find_next_tid con =
	let ret = con.next_tid in con.next_tid <- Int32.add con.next_tid 1l; ret

let register_transaction limits con store =
  begin match limits with
  | Some limits ->
    if Hashtbl.length con.transactions >= limits.Limits.number_of_active_transactions then begin
      error "domain %u has reached the open transaction limit (%d >= %d)" con.domid (Hashtbl.length con.transactions) limits.Limits.number_of_active_transactions;
      raise Limits.Limit_reached;
    end
  | None -> ()
  end;

	let id = find_next_tid con in
	let ntrans = Transaction.make id store in
	Hashtbl.add con.transactions id ntrans;
	Logging.start_transaction ~tid:id ~con:con.domstr;
	id

let unregister_transaction con tid =
	Hashtbl.remove con.transactions tid

let get_transaction con tid =
	try
		Hashtbl.find con.transactions tid
	with Not_found as e ->
		error "Failed to find transaction %lu on %s" tid con.domstr;
		raise e

let mark_symbols con =
	Hashtbl.iter (fun _ t -> Store.mark_symbols (Transaction.get_store t)) con.transactions

let stats con =
	Hashtbl.length con.watches, con.stat_nb_ops

let debug con =
	let list_watches con =
		let ll = Hashtbl.fold 
			(fun _ watches acc -> List.map (fun watch -> watch.name, watch.token) watches :: acc)
			con.watches [] in
		List.concat ll in

	let watches = List.map (fun (name, token) -> Printf.sprintf "watch %s: %s %s\n" con.domstr (Protocol.Name.to_string name) token) (list_watches con) in
	String.concat "" watches

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
		| "watch" :: [] ->
			""
		| "watch" :: n :: [] ->
			let n = int_of_string n in
			let all = Hashtbl.fold (fun _ w acc -> w @ acc) c.watches [] in
			if n > (List.length all) then raise (Node.Doesnt_exist path);
			""
		| "watch" :: n :: "name" :: [] ->
			let n = int_of_string n in
			let all = Hashtbl.fold (fun _ w acc -> w @ acc) c.watches [] in
			if n > (List.length all) then raise (Node.Doesnt_exist path);
			Protocol.Name.to_string (List.nth all n).name
		| "watch" :: n :: "token" :: [] ->
			let n = int_of_string n in
			let all = Hashtbl.fold (fun _ w acc -> w @ acc) c.watches [] in
			if n > (List.length all) then raise (Node.Doesnt_exist path);
			(List.nth all n).token
		| "watch" :: n :: "total-events" :: [] ->
			let n = int_of_string n in
			let all = Hashtbl.fold (fun _ w acc -> w @ acc) c.watches [] in
			if n > (List.length all) then raise (Node.Doesnt_exist path);
			string_of_int (List.nth all n).count
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
		| _ -> raise (Node.Doesnt_exist path)

	let exists t perms path = try ignore(read t perms path); true with Node.Doesnt_exist _ -> false

	let rec between start finish = if start > finish then [] else start :: (between (start + 1) finish)

	let list_connection t perms c = function
		| [] ->
			[ "address"; "current-transactions"; "total-operations"; "watch"; "total-dropped-watches" ]
		| [ "watch" ] ->
			let all = Hashtbl.fold (fun _ w acc -> w @ acc) c.watches [] in
			List.map string_of_int (between 0 (List.length all - 1))
		| [ "watch"; n ] -> [ "name"; "token"; "total-events" ]
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
			let idx = int_of_string idx in
			if not(Hashtbl.mem by_index idx) then raise (Node.Doesnt_exist path);
			let c = Hashtbl.find by_index idx in
			list_connection t perms c rest
		| _ -> []
end
let _ = Mount.mount (Protocol.Path.of_string "/tool/xenstored/connection") (module Introspect: Tree.S)
