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
open Xs_protocol
module Client = Xs_client_lwt.Client(Xs_transport_lwt_unix_client)
open Client

let ( |> ) a b = b a

(* Used for expressing a xenstore 'wait' condition and also in
   the special case of a set of writes (And(Eq, And(Eq, ...))) *)
type expr =
  | Val of string
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Eq of expr * expr

let rec pretty_print () =
  let open Format in function
    | Val x -> sprintf "\"%s\"" x
    | Not x -> sprintf "Not(@[%a@])" pretty_print x
    | And (x, y) -> sprintf "And(@[%a,@ %a@])" pretty_print x pretty_print y
    | Or (x, y) -> sprintf "Or(@[%a,@ %a@])" pretty_print x pretty_print y
    | Eq (x, y) -> sprintf "Eq(@[%a,@ %a@])" pretty_print x pretty_print y

exception Invalid_expression

(* "type-check" the expr. It should be "(k=v) and (kn=vn)*" *)
let rec to_conjunction = function
  | And(x, y) -> (to_conjunction x) @ (to_conjunction y)
  | Eq(Val k, Val v) -> [ k, v ]
  | _ -> raise Invalid_expression

let parse_expr s =
  let open Genlex in
  let keywords = ["("; ")"; "not"; "="; "and"; "or"] in
  (* Collapse streams of Idents together (eg /a/b/c) *)
  let flatten s =
    let to_list s =
      let result = ref [] in
      Stream.iter (fun x -> result := x :: !result) s;
      List.rev !result in
    let ident is = if is = [] then [] else [Ident (String.concat "" (List.rev is))] in
    let is, tokens = List.fold_left
      (fun (is, tokens) x -> match is, x with
	| is, Ident i -> (i :: is), tokens
	| is, x -> [], (x :: (ident is) @ tokens))
      ([], []) (to_list s) in
    ident is @ tokens
  |> List.rev |> Stream.of_list in
  let rec parse_atom = parser
    | [< 'Int n >] -> Val (string_of_int n)
    | [< 'Ident n >] -> Val n
    | [< 'Float n >] -> Val (string_of_float n)
    | [< 'String n >] -> Val n
    | [< 'Kwd "not"; e=parse_expr >] -> Not(e)
    | [< 'Kwd "("; e=parse_expr; 'Kwd ")" >] -> e
  and parse_expr = parser
    | [< e1=parse_atom; stream >] ->
      (parser
        | [< 'Kwd "and"; e2=parse_expr >] -> And(e1, e2)
        | [< 'Kwd "or"; e2=parse_expr >] -> Or(e1, e2)
        | [< 'Kwd "="; e2=parse_expr >] -> Eq(e1, e2)
            | [< >] -> e1) stream in
  s |> Stream.of_string |> make_lexer keywords |> flatten |> parse_expr

(* Return true if [expr] holds. Used in the xenstore 'wait' operation *)
let rec eval_expression expr xs = match expr with
  | Val path ->
    begin try_lwt
      lwt k = read xs path in
      return true
    with Enoent _ ->
      return false
    end
  | Not a ->
    lwt a' = eval_expression a xs in
    return (not(a'))
  | And (a, b) ->
    lwt a' = eval_expression a xs and b' = eval_expression b xs in
    return (a' && b')
  | Or (a, b) ->
    lwt a' = eval_expression a xs and b' = eval_expression b xs in
    return (a' || b')
  | Eq (Val path, Val v) ->
    begin try_lwt
      lwt v' = read xs path in
      return (v = v')
    with Enoent _ ->
      return false
    end
  | _ -> fail Invalid_expression

let usage () =
  let bin x = Sys.argv.(0) ^ x in
  let lines = [
    bin " : a xenstore protocol client";
    "";
    "Usage:";
	bin " [-path /var/run/xenstored/socket] [-restrict domid] <subcommand> [args]";
	"";
	"Where <subcommand> can be one of:";
	"";
    bin " read <key>";
    "   -- read the value stored at <key>, or fail if it doesn't exist";
    bin " write <key=val> [and keyN=valN]*";
    "   -- write the key value pair(s)";
    bin " directory <key>";
    "   -- list the direct children of <key>";
    bin " wait <expr>";
    "   -- block until the <expr> is true";
	bin " debug <cmd> [arg]";
	"   -- execute the given debug command";
    "";
    "Example expressions:";
    "";
    bin " wait /foo";
    "   -- block until the key \"/foo\" exists";
    bin " wait not(/foo)";
    "   -- block until the key \"/foo\" is deleted";
    bin " wait /foo or /bar";
    "   -- block until either key \"/foo\" or \"/bar\" are created";
    bin " wait /foo and (/bar = hello)";
    "   -- block until either key \"/foo\" is created or key \"/bar\" has value \"hello\"";
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
	| Some path -> Xs_transport.xenstored_socket := path
	| None -> ()
	end;
	let restrict_domid, args = extract args "-restrict" in
	let do_restrict xs = match restrict_domid with
		| Some domid -> restrict xs (int_of_string domid)
		| None -> return () in
	match args with
	| [ "read"; key ] ->
		lwt client = make () in
			immediate client
				(fun xs ->
					lwt () = do_restrict xs in
					lwt v = read xs key in
					Lwt_io.write Lwt_io.stdout v
				) >> return ()
    | [ "directory"; key ] ->
		lwt client = make () in
		immediate client
			(fun xs ->
				lwt () = do_restrict xs in
				lwt ls = directory xs key in
				Lwt_list.iter_s (fun x -> Lwt_io.write Lwt_io.stdout (x ^ "\n")) ls
		) >> return ()
    | "write" :: expr ->
		begin lwt items = try_lwt
			let expr = String.concat " " expr |> parse_expr in
			if !verbose then Printf.printf "Parsed: %s\n%!" (pretty_print () expr);
			expr |> to_conjunction |> return
		with Invalid_expression as e ->
			Lwt_io.write Lwt_io.stderr "Invalid expression; expected <key=val> [and key=val]*\n" >> raise_lwt e in
			lwt client = make () in
			immediate client
			(fun xs ->
				lwt () = do_restrict xs in
				Lwt_list.iter_s (fun (k, v) -> write xs k v) items
			) >> return ()
		end
	| "debug" :: cmd_args ->
		lwt client = make () in
		immediate client
			(fun xs ->
				lwt () = do_restrict xs in
				lwt results = debug xs cmd_args in
				Lwt_list.iter_s (fun x -> Lwt_io.write Lwt_io.stdout (x ^ "\n")) results
			) >> return ()
    | "wait" :: expr ->
		begin try_lwt
			let expr = String.concat " " expr |> parse_expr in
			if !verbose then Printf.printf "Parsed: %s\n%!" (pretty_print () expr);
			lwt client = make () in
			let result =
				wait client
					(fun xs ->
						lwt () = do_restrict xs in
						lwt result = eval_expression expr xs in
						if not result then fail Eagain else return ()
					) in
			Lwt_timeout.create 5 (fun () -> cancel result) |> Lwt_timeout.start;
			result
 		with Invalid_expression as e ->
			Lwt_io.write Lwt_io.stderr "Invalid expression\n" >> raise_lwt e
 		end
    | _ ->
		usage ();
		return ()
 end

let _ =
  Lwt_main.run (main ())
