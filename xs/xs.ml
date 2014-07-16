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

exception Exit of int

(* Implementations of 'simple' commands: *)

let read path () =
  Client.(immediate (read path)) >>= function
  | None ->
    Lwt_io.write Lwt_io.stderr "Path does not exist.\n" >>= fun () ->
    fail (Exit 1)
  | Some v ->
    Lwt_io.write Lwt_io.stdout v

let write path value () =
  Client.(immediate (write path value))

let directory path () =
  Client.(immediate (directory path)) >>= fun ls ->
  Lwt_list.iter_s (fun x -> Lwt_io.write Lwt_io.stdout (x ^ "\n")) ls

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
  try
    Some (s |> Stream.of_string |> make_lexer keywords |> flatten |> parse_expr)
  with e ->
    debug "Caught %s while parsing [%s]" (Printexc.to_string e) s;
    None

(* Return true if [expr] holds. Used in the xenstore 'wait' operation *)
let rec eval_expression expr xs = match expr with
  | Val path ->
    Client.read path xs >>= fun k ->
    return (k <> None)
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
    Client.read path xs >>= fun v' ->
    return (Some v = v')
  | _ -> fail Invalid_expression

let wait expr () =
  match parse_expr expr with
  | None -> fail Invalid_expression
  | Some expr ->
    debug "I parsed the expression as: %s" (pretty_print () expr);
    let t = Client.(wait (fun xs ->
        eval_expression expr xs >>= function
        | false -> return `Retry
        | true -> return (`Ok ())
      )) in
    Lwt_timeout.create 5 (fun () -> cancel t) |> Lwt_timeout.start;
    t >>= function
    | `Ok () -> return ()
    | `Error _ -> assert false (* see eval_expression invocation above *)

module Common = struct
  type t = {
    debug: bool;
    restrict: int option;
  }

  let make debug restrict =
    { debug; restrict }
end

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

let command f common =
  debug "Program started.";
  let t =
    ( match common.Common.restrict with
      | None -> return ()
      | Some domid -> Client.(immediate (restrict domid) )) >>= fun () ->
    f () >>= fun () ->
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
     | Invalid_expression ->
       `Error(true, "My expression parser couldn't understand your expression")
  | Exit x -> exit x

(* Command-line interface *)

open Cmdliner

let _common_options = "COMMON OPTIONS"

(* Options common to all commands *)
let common_options_t =
  let docs = _common_options in
  let debug =
    let doc = "Give additional debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc) in
  let restrict =
    let doc = "Act on behalf of this domain." in
    Arg.(value & opt (some int) None & info ["restrict"] ~docs ~doc) in
  Term.(pure Common.make $ debug $ restrict)

(* Help sections common to all commands *)
let help = [
  `S _common_options;
  `P "These options are common to all commands.";
  `S "MORE HELP";
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
  `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

let read_cmd =
  let doc = "read the value at a particular path" in
  let man = [
    `S "DESCRIPTION";
    `P "Read the value at a particular path.";
  ] @ help in
  let key =
    let doc = "The path to read" in
    Arg.(value & pos 0 string "" & info [] ~docv:"PATH" ~doc) in
  Term.(ret( (pure command) $ (pure read $ key) $ common_options_t )),
  Term.info "read" ~sdocs:_common_options ~doc ~man

let write_cmd =
  let doc = "write the value at a particular path" in
  let man = [
    `S "DESCRIPTION";
    `P "Write the value at a particular path.";
  ] @ help in
  let key =
    let doc = "The path to write" in
    Arg.(value & pos 0 string "" & info [] ~docv:"PATH" ~doc) in
  let value =
    let doc = "The value to write" in
    Arg.(value & pos 1 string "" & info [] ~docv:"VALUE" ~doc) in
  Term.(ret( (pure command) $ (pure write $ key $ value) $ common_options_t )),
  Term.info "write" ~sdocs:_common_options ~doc ~man

let directory_cmd =
  let doc = "list the child keys of a particular path" in
  let man = [
    `S "DESCRIPTION";
    `P "List the child keys of a particular path.";
  ] @ help in
  let key =
    let doc = "The path to list" in
    Arg.(value & pos 0 string "" & info [] ~docv:"PATH" ~doc) in
  Term.(ret( (pure command) $ (pure directory $ key) $ common_options_t )),
  Term.info "directory" ~sdocs:_common_options ~doc ~man

let wait_cmd =
  let doc = "wait for a condition to become true" in
  let man = [
    `S "DESCRIPTION";
    `P "Wait for the condition described by the expression to become true";
    `P "The implementation uses the Xenstore 'watch' mechanism for efficiency.";
    `S "EXAMPLES";
    `P "";
    `P "$(mname) wait '/foo'";
    `P "   -- block until the key \"/foo\" exists";
    `P "$(mname) wait 'not(/foo)'";
    `P "   -- block until the key \"/foo\" is deleted";
    `P "$(mname) wait '/foo or /bar'";
    `P "   -- block until either key \"/foo\" or \"/bar\" are created";
    `P "$(mname) wait '/foo and (/bar = hello)'";
    `P "   -- block until either key \"/foo\" is created or key \"/bar\" has value \"hello\"";
  ] @ help in
  let expr =
    let doc = "The expression which we want to become true" in
    Arg.(value & pos 0 string "" & info [] ~docv:"EXPR" ~doc) in
  Term.(ret( (pure command) $ (pure wait $ expr) $ common_options_t )),
  Term.info "wait" ~sdocs:_common_options ~doc ~man

let default_cmd =
  let doc = "manipulate XenStore" in
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "xs" ~version:"1.0.0" ~sdocs:_common_options ~doc ~man

let cmds = [ read_cmd; write_cmd; directory_cmd; wait_cmd ]

let _ =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
