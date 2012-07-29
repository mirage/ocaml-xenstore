(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Thomas Gazagnaire <thomas.gazagnaire@citrix.com>
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
open Printf

type logger = {
	stream: string Lwt_stream.t;
	push: string -> unit;
	elements: int ref;
	max_elements: int;
	dropped_elements: int ref;
}

let create max_elements =
	let elements = ref (ref 0) in
	let dropped_elements = ref (ref 0) in
	let stream, stream_push = Lwt_stream.create () in
	let push line =
		if !(!elements) > max_elements then begin
			incr !dropped_elements
		end else begin
			stream_push (Some line);
			incr !elements
		end in
	{
		stream = stream;
		push = push;
		elements = !elements;
		max_elements = max_elements;
		dropped_elements = !dropped_elements;
	}

let get (logger: logger) =
	let return_lines all =
		logger.elements := !(logger.elements) - (List.length all);
		let dropped = !(logger.dropped_elements) in
		logger.dropped_elements := 0;
		return (if dropped <> 0
			then Printf.sprintf "<-- dropped %d log lines" dropped :: all
			else all) in

	(* Grab as many elements as we can without blocking *)
	let all = Lwt_stream.get_available logger.stream in
	if all <> []
	then return_lines all
	else begin
		(* Block for at least one line *)
		lwt all = Lwt_stream.nget 1 logger.stream in
		return_lines all
	end

(* General system logging *)
let logger = create 512

(* Operation logging *)
let access_logger = create 512

let string_of_date = ref (fun () -> "unknown")

type level = Debug | Info | Warn | Error | Null

let log_level = ref Warn

let int_of_level = function
	| Debug -> 0 | Info -> 1 | Warn -> 2
	| Error -> 3 | Null -> max_int

let string_of_level = function
	| Debug -> "debug" | Info -> "info" | Warn -> "warn"
	| Error -> "error" | Null -> "null"

let log level key (fmt: (_,_,_,_) format4) =
	let date = !string_of_date() in
	let level = string_of_level level in
	Printf.ksprintf logger.push ("[%s|%5s|%s] " ^^ fmt) date level key

let debug key = log Debug key
let info key = log Info key
let warn key = log Warn key
let error key = log Error key

(* Access logger *)

type access_type =
	| Coalesce
	| Conflict
	| Commit
	| Newconn
	| Endconn
	| XbOp of Xs_packet.Op.t

let string_of_tid ~con tid =
	if tid = 0l
	then sprintf "%-12s" con
	else sprintf "%-12s" (sprintf "%s.%li" con tid)

let string_of_access_type = function
	| Coalesce                -> "coalesce "
	| Conflict                -> "conflict "
	| Commit                  -> "commit   "
	| Newconn                 -> "newconn  "
	| Endconn                 -> "endconn  "

	| XbOp op ->
	  let open Xs_packet.Op in match op with
	| Debug             -> "debug    "

	| Directory         -> "directory"
	| Read              -> "read     "
	| Getperms          -> "getperms "

	| Watch             -> "watch    "
	| Unwatch           -> "unwatch  "

	| Transaction_start -> "t start  "
	| Transaction_end   -> "t end    "

	| Introduce         -> "introduce"
	| Release           -> "release  "
	| Getdomainpath     -> "getdomain"
	| Isintroduced      -> "is introduced"
	| Resume            -> "resume   "
 
	| Write             -> "write    "
	| Mkdir             -> "mkdir    "
	| Rm                -> "rm       "
	| Setperms          -> "setperms "
(*	| Restrict          -> "restrict "*)
	| Set_target        -> "settarget"

	| Error             -> "error    "
	| Watchevent        -> "w event  "
(*	| Invalid           -> "invalid  " *)
	(*
	| x                       -> to_string x
	*)

let sanitize_data data =
	let data = String.copy data in
	for i = 0 to String.length data - 1
	do
		if data.[i] = '\000' then
			data.[i] <- ' '
	done;
	String.escaped data

let access_log_read_ops = ref false
let access_log_transaction_ops = ref false
let access_log_special_ops = ref false

let access_logging ~con ~tid ?(data="") access_type =
	let date = !string_of_date() in
	let tid = string_of_tid ~con tid in
	let access_type = string_of_access_type access_type in
	let data = sanitize_data data in
	Printf.ksprintf logger.push "[%s] %s %s %s" date tid access_type data

let new_connection = access_logging Newconn
let end_connection = access_logging Endconn
let read_coalesce ~tid ~con data =
	if !access_log_read_ops
	then access_logging Coalesce ~tid ~con ~data:("read "^data)
let write_coalesce data = access_logging Coalesce ~data:("write "^data)
let conflict = access_logging Conflict
let commit = access_logging Commit

let xb_op ~tid ~con ~ty data =
	let print =
	  let open Xs_packet.Op in match ty with
		| Read | Directory | Getperms -> !access_log_read_ops
		| Transaction_start | Transaction_end ->
			false (* transactions are managed below *)
		| Introduce | Release | Getdomainpath | Isintroduced | Resume ->
			!access_log_special_ops
		| _ -> true in
	if print then access_logging ~tid ~con ~data (XbOp ty)

let start_transaction ~tid ~con = 
	if !access_log_transaction_ops && tid <> 0l
	then access_logging ~tid ~con (XbOp Xs_packet.Op.Transaction_start)

let end_transaction ~tid ~con = 
	if !access_log_transaction_ops && tid <> 0l
	then access_logging ~tid ~con (XbOp Xs_packet.Op.Transaction_end)

let startswith prefix x =
        let x_l = String.length x and prefix_l = String.length prefix in
        prefix_l <= x_l && String.sub x 0 prefix_l  = prefix

let xb_answer ~tid ~con ~ty data =
	let print =
	  let open Xs_packet.Op in match ty with
		| Error when startswith "ENOENT " data -> !access_log_read_ops
		| Error -> true
		| Watchevent -> true
		| _ -> false
	in
	if print then access_logging ~tid ~con ~data (XbOp ty)
