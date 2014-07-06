(*
 * Copyright (C) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let project_url = "http://github.com/mirage/ocaml-xenstore"

module Client = Xenstore.Client.Make(Userspace)

let debug fmt = Xenstore.Logging.debug "xs" fmt
let error fmt = Xenstore.Logging.error "xs" fmt

open Lwt

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
      Client.read path xs >>= fun k ->
      return true
    with Xenstore.Protocol.Enoent _ ->
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
      Client.read path xs >>= fun v' ->
      return (v = v')
    with Xenstore.Protocol.Enoent _ ->
      return false
    end
  | _ -> fail Invalid_expression

open Cmdliner

module Common = struct
        type t = {
        debug: bool;
        verbose: bool;
        restrict: int option;
        }

        let make debug verbose restrict =
                { debug; verbose; restrict }

end

let _common_options = "COMMON OPTIONS"

(* Options common to all commands *)
let common_options_t =
  let docs = _common_options in
  let debug =
    let doc = "Give additional debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc) in
  let verb =
    let doc = "Give verbose output." in
    let verbose = true, Arg.info ["v"; "verbose"] ~docs ~doc in
    Arg.(last & vflag_all [false] [verbose]) in
  let restrict =
    let doc = "Act on behalf of this domain." in
    Arg.(value & opt (some int) None & info ["restrict"] ~docs ~doc) in
  Term.(pure Common.make $ debug $ verb $ restrict)


(* Help sections common to all commands *)
let help = [
 `S _common_options;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]


(*
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
*)

let maybe_print_logs common =
  ( match common.Common.debug with
    | false -> return ()
    | true ->
      Xenstore.Logging.(get logger) >>= fun lines ->
      Lwt_list.iter_s
        (fun x ->
           Lwt_io.write Lwt_io.stderr x >>= fun () ->
           Lwt_io.write Lwt_io.stderr "\n"
        ) lines )

let read common path =
  debug "Program started.";
  let t =
  ( match common.Common.restrict with
    | None -> return ()
    | Some domid -> Client.(immediate (restrict domid) )) >>= fun () ->
  Client.(immediate (read path)) >>= fun v ->
  Lwt_io.write Lwt_io.stdout v >>= fun () ->
  maybe_print_logs common in
  try
    Lwt_main.run t;
    `Ok ()
  with Userspace.Could_not_find_xenstore ->
    let lines = [
      "Failed to connect to xenstore.";
      "First check whether you are running on a Xen system. Try asking";
      "  'virt-what' or looking in /sys/hypervisor/type on Linux.";
      "Second check whether you are running with sufficient privileges.";
      "  Access to xenstore normally requires root privileges.";
      "Third check whether xenstored is running and start it if not.";
    ] in
    Lwt_main.run (maybe_print_logs common);
    `Error(false, String.concat "\n" lines)

let read_cmd =
  let doc = "read the value at a particular path" in
  let man = [
    `S "DESCRIPTION";
    `P "Read the value at a particular path.";
  ] @ help in
  let key =
    let doc = "The path to read" in
    Arg.(value & pos 0 string "" & info [] ~docv:"PATH" ~doc) in
  Term.(ret(pure read $ common_options_t $ key)),
  Term.info "read" ~sdocs:_common_options ~doc ~man

let default_cmd =
  let doc = "manipulate XenStore" in
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "xs" ~version:"1.0.0" ~sdocs:_common_options ~doc ~man

let cmds = [ read_cmd ]

let _ =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
