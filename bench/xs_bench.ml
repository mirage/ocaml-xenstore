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

open Lwt
open Xs_packet
module Client = Xs_client.Client(Xs_transport_unix)
open Client

let ( |> ) a b = b a

let vm_create domid client =
	(* create /local/domain/<domid> *)
	(* create 3 VBDs, 1 VIF (workaround transaction problem?) *)
	lwt dom_path = with_xs client (fun xs -> getdomainpath xs domid) in
	let uuid = Printf.sprintf "uuid-%d" domid in
	let name = "name" in
	let vm_path = "/vm/" ^ uuid in
	let vss_path = "/vss/" ^ uuid in
	let xsdata = [
		"xsdata", "xsdata"
	] in
	let platformdata = [
		"platformdata", "platformdata"
	] in
	let bios_strings = [
		"bios_strings", "bios_strings"
	] in
	let roperm = Xs_packet.ACL.({owner = 0; other = NONE; acl = [ domid, READ ]}) in
	let rwperm = Xs_packet.ACL.({owner = domid; other = NONE; acl = []}) in
	lwt () =
		with_xst client
			(fun xs ->
				(* Clear any existing rubbish in xenstored *)
				lwt () = rm xs dom_path in
				lwt () = mkdir xs dom_path in
				lwt () = setperms xs dom_path roperm in

				(* The /vm path needs to be shared over a localhost migrate *)
				lwt vm_exists = try_lwt lwt _ = read xs vm_path in return true with _ -> return false in
				lwt () = if not vm_exists then begin
					lwt () = mkdir xs vm_path in
					lwt () = setperms xs vm_path roperm in
					lwt () = write xs (vm_path ^ "uuid") uuid in
					lwt () = write xs (vm_path ^ "name") name in
					return ()
				end else return () in
				lwt () = write xs (Printf.sprintf "%s/domains/%d" vm_path domid) dom_path in

				lwt () = rm xs vss_path in
				lwt () = mkdir xs vss_path in
				lwt () = setperms xs vss_path rwperm in

				lwt () = write xs (dom_path ^ "/vm") vm_path in
				lwt () = write xs (dom_path ^ "/vss") vss_path in
				lwt () = write xs (dom_path ^ "/name") name in

				(* create cpu and memory directory with read only perms *)
				lwt () = Lwt_list.iter_s (fun dir ->
					let ent = Printf.sprintf "%s/%s" dom_path dir in
					lwt () = mkdir xs ent in
					setperms xs ent roperm
				) [ "cpu"; "memory" ] in
				(* create read/write nodes for the guest to use *)
				lwt () = Lwt_list.iter_s (fun dir ->
					let ent = Printf.sprintf "%s/%s" dom_path dir in
					lwt () = mkdir xs ent in
					setperms xs ent rwperm
				) [ "device"; "error"; "drivers"; "control"; "attr"; "data"; "messages"; "vm-data" ] in
				return ()
		) in
	lwt () = with_xs client
		(fun xs ->

			lwt () = Lwt_list.iter_s (fun (x, y) -> write xs (dom_path ^ x) y) xsdata in

			lwt () = Lwt_list.iter_s (fun (x, y) -> write xs (dom_path ^ "/platform/" ^ x) y) platformdata in
			lwt () = Lwt_list.iter_s (fun (x, y) -> write xs (dom_path ^ "/bios-strings/" ^ x) y) bios_strings in
	
			(* If a toolstack sees a domain which it should own in this state then the
			   domain is not completely setup and should be shutdown. *)
			lwt () = write xs (dom_path ^ "/action-request") "poweroff" in

			lwt () = write xs (dom_path ^ "/control/platform-feature-multiprocessor-suspend") "1" in

			(* CA-30811: let the linux guest agent easily determine if this is a fresh domain even if
			   the domid hasn't changed (consider cross-host migrate) *)
			lwt () = write xs (dom_path ^ "/unique-domain-id") uuid in
			return ()
		) in
	return ()


let vm_shutdown domid xs =
	(* destroy devices *)
	(* destroy /local/domain/<domid> *)
	()

let usage () =
  let bin x = Sys.argv.(0) ^ x in
  let lines = [
    bin " : a xenstore benchmark tool";
    "";
    "Usage:";
	bin " [-path /var/run/xenstored/socket]";
  ] in
  List.iter (fun x -> Printf.fprintf stderr "%s\n" x) lines

let main () =
  let verbose = ref false in
  let args = Sys.argv |> Array.to_list |> List.tl in
  (* Look for "-h" or "-v" arguments *)
  if List.mem "-h" args then begin
    usage ();
    return ();
  end else begin
    verbose := List.mem "-v" args;
    let args = List.filter (fun x -> x <> "-v") args in
    (* Extract any -path X argument *)
	let extract args key =
		let result = ref None in
		let args =
			List.fold_left (fun (acc, foundit) x ->
				if foundit then (result := Some x; (acc, false))
				else if x = key then (acc, true)
				else (x :: acc, false)
			) ([], false) args |> fst |> List.rev in
		!result, args in
	let path, args = extract args "-path" in
	begin match path with
	| Some path -> Xs_transport_unix.xenstored_socket := path
	| None -> ()
	end;

	return ()
 end

let _ =
  Lwt_main.run (main ())
