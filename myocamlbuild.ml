(* OASIS_START *)
(* DO NOT EDIT (digest: 7963f704e8b35bda7f05a5c4b450ec07) *)
module OASISGettext = struct
# 21 "/home/avsm/.opam/3.12.1+mirage-xen/build/oasis.0.3.0/src/oasis/OASISGettext.ml"

  let ns_ str =
    str

  let s_ str =
    str

  let f_ (str : ('a, 'b, 'c, 'd) format4) =
    str

  let fn_ fmt1 fmt2 n =
    if n = 1 then
      fmt1^^""
    else
      fmt2^^""

  let init =
    []

end

module OASISExpr = struct
# 21 "/home/avsm/.opam/3.12.1+mirage-xen/build/oasis.0.3.0/src/oasis/OASISExpr.ml"



  open OASISGettext

  type test = string 

  type flag = string 

  type t =
    | EBool of bool
    | ENot of t
    | EAnd of t * t
    | EOr of t * t
    | EFlag of flag
    | ETest of test * string
    

  type 'a choices = (t * 'a) list 

  let eval var_get t =
    let rec eval' =
      function
        | EBool b ->
            b

        | ENot e ->
            not (eval' e)

        | EAnd (e1, e2) ->
            (eval' e1) && (eval' e2)

        | EOr (e1, e2) ->
            (eval' e1) || (eval' e2)

        | EFlag nm ->
            let v =
              var_get nm
            in
              assert(v = "true" || v = "false");
              (v = "true")

        | ETest (nm, vl) ->
            let v =
              var_get nm
            in
              (v = vl)
    in
      eval' t

  let choose ?printer ?name var_get lst =
    let rec choose_aux =
      function
        | (cond, vl) :: tl ->
            if eval var_get cond then
              vl
            else
              choose_aux tl
        | [] ->
            let str_lst =
              if lst = [] then
                s_ "<empty>"
              else
                String.concat
                  (s_ ", ")
                  (List.map
                     (fun (cond, vl) ->
                        match printer with
                          | Some p -> p vl
                          | None -> s_ "<no printer>")
                     lst)
            in
              match name with
                | Some nm ->
                    failwith
                      (Printf.sprintf
                         (f_ "No result for the choice list '%s': %s")
                         nm str_lst)
                | None ->
                    failwith
                      (Printf.sprintf
                         (f_ "No result for a choice list: %s")
                         str_lst)
    in
      choose_aux (List.rev lst)

end


# 117 "myocamlbuild.ml"
module BaseEnvLight = struct
# 21 "/home/avsm/.opam/3.12.1+mirage-xen/build/oasis.0.3.0/src/base/BaseEnvLight.ml"

  module MapString = Map.Make(String)

  type t = string MapString.t

  let default_filename =
    Filename.concat
      (Sys.getcwd ())
      "setup.data"

  let load ?(allow_empty=false) ?(filename=default_filename) () =
    if Sys.file_exists filename then
      begin
        let chn =
          open_in_bin filename
        in
        let st =
          Stream.of_channel chn
        in
        let line =
          ref 1
        in
        let st_line =
          Stream.from
            (fun _ ->
               try
                 match Stream.next st with
                   | '\n' -> incr line; Some '\n'
                   | c -> Some c
               with Stream.Failure -> None)
        in
        let lexer =
          Genlex.make_lexer ["="] st_line
        in
        let rec read_file mp =
          match Stream.npeek 3 lexer with
            | [Genlex.Ident nm; Genlex.Kwd "="; Genlex.String value] ->
                Stream.junk lexer;
                Stream.junk lexer;
                Stream.junk lexer;
                read_file (MapString.add nm value mp)
            | [] ->
                mp
            | _ ->
                failwith
                  (Printf.sprintf
                     "Malformed data file '%s' line %d"
                     filename !line)
        in
        let mp =
          read_file MapString.empty
        in
          close_in chn;
          mp
      end
    else if allow_empty then
      begin
        MapString.empty
      end
    else
      begin
        failwith
          (Printf.sprintf
             "Unable to load environment, the file '%s' doesn't exist."
             filename)
      end

  let var_get name env =
    let rec var_expand str =
      let buff =
        Buffer.create ((String.length str) * 2)
      in
        Buffer.add_substitute
          buff
          (fun var ->
             try
               var_expand (MapString.find var env)
             with Not_found ->
               failwith
                 (Printf.sprintf
                    "No variable %s defined when trying to expand %S."
                    var
                    str))
          str;
        Buffer.contents buff
    in
      var_expand (MapString.find name env)

  let var_choose lst env =
    OASISExpr.choose
      (fun nm -> var_get nm env)
      lst
end


# 215 "myocamlbuild.ml"
module MyOCamlbuildFindlib = struct
# 21 "/home/avsm/.opam/3.12.1+mirage-xen/build/oasis.0.3.0/src/plugins/ocamlbuild/MyOCamlbuildFindlib.ml"

  (** OCamlbuild extension, copied from 
    * http://brion.inria.fr/gallium/index.php/Using_ocamlfind_with_ocamlbuild
    * by N. Pouillard and others
    *
    * Updated on 2009/02/28
    *
    * Modified by Sylvain Le Gall 
    *)
  open Ocamlbuild_plugin

  (* these functions are not really officially exported *)
  let run_and_read = 
    Ocamlbuild_pack.My_unix.run_and_read

  let blank_sep_strings = 
    Ocamlbuild_pack.Lexers.blank_sep_strings

  let split s ch =
    let x = 
      ref [] 
    in
    let rec go s =
      let pos = 
        String.index s ch 
      in
        x := (String.before s pos)::!x;
        go (String.after s (pos + 1))
    in
      try
        go s
      with Not_found -> !x

  let split_nl s = split s '\n'

  let before_space s =
    try
      String.before s (String.index s ' ')
    with Not_found -> s

  (* this lists all supported packages *)
  let find_packages () =
    List.map before_space (split_nl & run_and_read "ocamlfind list")

  (* this is supposed to list available syntaxes, but I don't know how to do it. *)
  let find_syntaxes () = ["camlp4o"; "camlp4r"]

  (* ocamlfind command *)
  let ocamlfind x = S[A"ocamlfind"; x]

  let dispatch =
    function
      | Before_options ->
          (* by using Before_options one let command line options have an higher priority *)
          (* on the contrary using After_options will guarantee to have the higher priority *)
          (* override default commands by ocamlfind ones *)
          Options.ocamlc     := ocamlfind & A"ocamlc";
          Options.ocamlopt   := ocamlfind & A"ocamlopt";
          Options.ocamldep   := ocamlfind & A"ocamldep";
          Options.ocamldoc   := ocamlfind & A"ocamldoc";
          Options.ocamlmktop := ocamlfind & A"ocamlmktop"
                                  
      | After_rules ->
          
          (* When one link an OCaml library/binary/package, one should use -linkpkg *)
          flag ["ocaml"; "link"; "program"] & A"-linkpkg";
          
          (* For each ocamlfind package one inject the -package option when
           * compiling, computing dependencies, generating documentation and
           * linking. *)
          List.iter 
            begin fun pkg ->
              flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
            end 
            (find_packages ());

          (* Like -package but for extensions syntax. Morover -syntax is useless
           * when linking. *)
          List.iter begin fun syntax ->
          flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "infer_interface"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          end (find_syntaxes ());

          (* The default "thread" tag is not compatible with ocamlfind.
           * Indeed, the default rules add the "threads.cma" or "threads.cmxa"
           * options when using this tag. When using the "-linkpkg" option with
           * ocamlfind, this module will then be added twice on the command line.
           *                        
           * To solve this, one approach is to add the "-thread" option when using
           * the "threads" package using the previous plugin.
           *)
          flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "doc"] (S[A "-I"; A "+threads"]);
          flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"])

      | _ -> 
          ()

end

module MyOCamlbuildBase = struct
# 21 "/home/avsm/.opam/3.12.1+mirage-xen/build/oasis.0.3.0/src/plugins/ocamlbuild/MyOCamlbuildBase.ml"

  (** Base functions for writing myocamlbuild.ml
      @author Sylvain Le Gall
    *)



  open Ocamlbuild_plugin
  module OC = Ocamlbuild_pack.Ocaml_compiler

  type dir = string 
  type file = string 
  type name = string 
  type tag = string 

# 56 "/home/avsm/.opam/3.12.1+mirage-xen/build/oasis.0.3.0/src/plugins/ocamlbuild/MyOCamlbuildBase.ml"

  type t =
      {
        lib_ocaml: (name * dir list) list;
        lib_c:     (name * dir * file list) list; 
        flags:     (tag list * (spec OASISExpr.choices)) list;
        (* Replace the 'dir: include' from _tags by a precise interdepends in
         * directory.
         *)
        includes:  (dir * dir list) list; 
      } 

  let env_filename =
    Pathname.basename 
      BaseEnvLight.default_filename

  let dispatch_combine lst =
    fun e ->
      List.iter 
        (fun dispatch -> dispatch e)
        lst 

  let tag_libstubs nm =
    "use_lib"^nm^"_stubs"

  let nm_libstubs nm =
    nm^"_stubs"

  let dispatch t e = 
    let env = 
      BaseEnvLight.load 
        ~filename:env_filename 
        ~allow_empty:true
        ()
    in
      match e with 
        | Before_options ->
            let no_trailing_dot s =
              if String.length s >= 1 && s.[0] = '.' then
                String.sub s 1 ((String.length s) - 1)
              else
                s
            in
              List.iter
                (fun (opt, var) ->
                   try 
                     opt := no_trailing_dot (BaseEnvLight.var_get var env)
                   with Not_found ->
                     Printf.eprintf "W: Cannot get variable %s" var)
                [
                  Options.ext_obj, "ext_obj";
                  Options.ext_lib, "ext_lib";
                  Options.ext_dll, "ext_dll";
                ]

        | After_rules -> 
            (* Declare OCaml libraries *)
            List.iter 
              (function
                 | nm, [] ->
                     ocaml_lib nm
                 | nm, dir :: tl ->
                     ocaml_lib ~dir:dir (dir^"/"^nm);
                     List.iter 
                       (fun dir -> 
                          List.iter
                            (fun str ->
                               flag ["ocaml"; "use_"^nm; str] (S[A"-I"; P dir]))
                            ["compile"; "infer_interface"; "doc"])
                       tl)
              t.lib_ocaml;

            (* Declare directories dependencies, replace "include" in _tags. *)
            List.iter 
              (fun (dir, include_dirs) ->
                 Pathname.define_context dir include_dirs)
              t.includes;

            (* Declare C libraries *)
            List.iter
              (fun (lib, dir, headers) ->
                   (* Handle C part of library *)
                   flag ["link"; "library"; "ocaml"; "byte"; tag_libstubs lib]
                     (S[A"-dllib"; A("-l"^(nm_libstubs lib)); A"-cclib";
                        A("-l"^(nm_libstubs lib))]);

                   flag ["link"; "library"; "ocaml"; "native"; tag_libstubs lib]
                     (S[A"-cclib"; A("-l"^(nm_libstubs lib))]);
                        
                   flag ["link"; "program"; "ocaml"; "byte"; tag_libstubs lib]
                     (S[A"-dllib"; A("dll"^(nm_libstubs lib))]);

                   (* When ocaml link something that use the C library, then one
                      need that file to be up to date.
                    *)
                   dep ["link"; "ocaml"; "program"; tag_libstubs lib]
                     [dir/"lib"^(nm_libstubs lib)^"."^(!Options.ext_lib)];

                   dep  ["compile"; "ocaml"; "program"; tag_libstubs lib]
                     [dir/"lib"^(nm_libstubs lib)^"."^(!Options.ext_lib)];

                   (* TODO: be more specific about what depends on headers *)
                   (* Depends on .h files *)
                   dep ["compile"; "c"] 
                     headers;

                   (* Setup search path for lib *)
                   flag ["link"; "ocaml"; "use_"^lib] 
                     (S[A"-I"; P(dir)]);
              )
              t.lib_c;

              (* Add flags *)
              List.iter
              (fun (tags, cond_specs) ->
                 let spec = 
                   BaseEnvLight.var_choose cond_specs env
                 in
                   flag tags & spec)
              t.flags
        | _ -> 
            ()

  let dispatch_default t =
    dispatch_combine 
      [
        dispatch t;
        MyOCamlbuildFindlib.dispatch;
      ]

end


# 476 "myocamlbuild.ml"
open Ocamlbuild_plugin;;
let package_default =
  {
     MyOCamlbuildBase.lib_ocaml =
       [
          ("xenstore", ["core"]);
          ("xenstore_client", ["client"]);
          ("xenstore_server", ["server"]);
          ("xenstore_unix", ["unix"])
       ];
     lib_c = [("xenstore_unix", "unix", [])];
     flags =
       [
          (["oasis_library_xenstore_unix_ccopt"; "compile"],
            [
               (OASISExpr.EBool true,
                 S
                   [
                      A "-ccopt";
                      A "-I.";
                      A "-ccopt";
                      A "-I/home/djs/.opam/system/lib";
                      A "-ccopt";
                      A "-I/home/djs/.opam/system/lib/lwt";
                      A "-ccopt";
                      A "-I/usr/lib/ocaml/lwt"
                   ])
            ]);
          (["oasis_library_xenstore_unix_cclib"; "link"],
            [(OASISExpr.EBool true, S [A "-cclib"; A "-lxenctrl"])]);
          (["oasis_library_xenstore_unix_cclib"; "ocamlmklib"; "c"],
            [(OASISExpr.EBool true, S [A "-lxenctrl"])])
       ];
     includes =
       [
          ("xen", ["core"; "server"]);
          ("unix", ["core"; "server"]);
          ("server_unix", ["core"; "server"; "unix"]);
          ("server_test", ["core"; "server"]);
          ("server", ["core"]);
          ("lib_test", ["core"]);
          ("client", ["core"]);
          ("cli", ["client"; "core"; "unix"]);
          ("bench", ["client"; "core"; "unix"])
       ];
     }
  ;;

let dispatch_default = MyOCamlbuildBase.dispatch_default package_default;;

# 528 "myocamlbuild.ml"
(* OASIS_STOP *)

(* Utility functions (e.g. to execute a command and return lines read) *)
module Util = struct
  let split s ch =
    let x = ref [] in
    let rec go s =
      let pos = String.index s ch in
      x := (String.before s pos)::!x;
      go (String.after s (pos + 1))
    in
    try
      go s
    with Not_found -> !x

    let split_nl s = split s '\n'
    let run_and_read x = List.hd (split_nl (Ocamlbuild_pack.My_unix.run_and_read x))
end

(* XXX this wont work with a custom ocamlc not on the path *)
let ocaml_libdir = Util.run_and_read "ocamlc -where"

open Ocamlbuild_plugin
open Command
open Ocamlbuild_pack.Ocaml_compiler
open Ocamlbuild_pack.Ocaml_utils
open Ocamlbuild_pack.Tools
open Printf

(* Rules to directly invoke GCC rather than go through OCaml. *)
module CC = struct

  let cc = getenv "CC" ~default:"cc"
  let ar = getenv "AR" ~default:"ar"
  let cflags = getenv "CFLAGS" ~default:""

  let cc_call tags dep prod env builder =
    let dep = env dep and prod = env prod in
    let tags = tags_of_pathname dep++"cc"++"compile"++tags in 
    let flags = [A"-c"; Sh cflags] in
    let inc = A (Printf.sprintf "-I%s/%s" Pathname.pwd (Filename.dirname dep)) in
    Cmd (S (A cc :: inc :: flags @ [T tags; A"-o"; Px prod; P dep]))

  let cc_archive clib a path env builder =
    let clib = env clib and a = env a and path = env path in
    let objs = List.map (fun x -> path / x) (string_list_of_file clib) in
    let objs = List.map (fun x -> (Filename.chop_extension x)^".o") objs in
    let objs = List.map Outcome.good (builder (List.map (fun x -> [x]) objs)) in
    Cmd(S[A ar; A"rc"; Px a; T(tags_of_pathname a++"c"++"archive"); atomize objs])

  (** Copied from ocaml/ocamlbuild/ocaml_specific.ml and modified to add
      the output_obj tag *)
  let native_output_obj x =
    link_gen "cmx" "cmxa" !Options.ext_lib [!Options.ext_obj; "cmi"]
       ocamlopt_link_prog
      (fun tags -> tags++"ocaml"++"link"++"native"++"output_obj") x

  let bytecode_output_obj x =
    link_gen "cmo" "cma" !Options.ext_lib [!Options.ext_obj; "cmi"]
       ocamlc_link_prog
      (fun tags -> tags++"ocaml"++"link"++"byte"++"output_obj") x

  let rules () = 
    rule "cc: %.c -> %.o" ~prod:"%.o" ~dep:"%.c" (cc_call "c" "%.c" "%.o");
    rule "cc: %.S -> %.o" ~prod:"%.o" ~dep:"%.S" (cc_call "asm" "%.S" "%.o");
    rule "archive: cclib .o -> .a archive"
      ~prod:"%(path:<**/>)lib%(libname:<*>).a"
      ~dep:"%(path)lib%(libname).cclib"
      (cc_archive "%(path)lib%(libname).cclib" "%(path)lib%(libname).a" "%(path)");
    (* Rule to link a module and output a standalone native object file *)
    rule "ocaml: cmx* & o* -> .m.o"
      ~prod:"%.m.o"
      ~deps:["%.cmx"; "%.o"]
      (native_output_obj "%.cmx" "%.m.o");
    (* Rule to link a module and output a standalone bytecode C file *)
    rule "ocaml: cmo* & o* -> .mb.c"
      ~prod:"%.mb.c"
      ~deps:["%.cmo"; "%.o"]
      (bytecode_output_obj "%.cmo" "%.mb.c")

  let flags () =
     flag ["cc";"depend"; "c"] & S [A("-I"^ocaml_libdir)];
     flag ["cc";"compile"; "c"] & S [A("-I"^ocaml_libdir)];
     flag ["cc";"compile"; "asm"] & S [A"-D__ASSEMBLY__"]
end

module Xen = struct
  (** Link to a standalone Xen microkernel *)
  let cc_xen_link bc tags arg out env =
    (* XXX check ocamlfind path here *)
    let xenlib = Util.run_and_read "ocamlfind query mirage" in
    let jmp_obj = Px (xenlib / "longjmp.o") in
    let head_obj = Px (xenlib / "x86_64.o") in
    let ocamllib = match bc with |true -> "ocamlbc" |false -> "ocaml" in
    let ld = getenv ~default:"ld" "LD" in
    let ldlibs = List.map (fun x -> Px (xenlib / ("lib" ^ x ^ ".a")))
      [ocamllib; "xen"; "xencaml"; "diet"; "m"] in
    Cmd (S ( A ld :: [ T(tags++"link"++"xen");
      A"-d"; A"-nostdlib"; A"-m"; A"elf_x86_64"; A"-T";
      Px (xenlib / "mirage-x86_64.lds");  head_obj; P arg ]
      @ ldlibs @ [jmp_obj; A"-o"; Px out]))

  let cc_xen_bc_link tags arg out env = cc_xen_link true tags arg out env
  let cc_xen_nc_link tags arg out env = cc_xen_link false tags arg out env

  (* Rewrite sections for Xen LDS layout *)
  let xen_objcopy dst src env builder =
    let dst = env dst in
    let src = env src in
    let cmd = ["objcopy";"--rename-section";".bss=.mlbss";"--rename-section";
      ".data=.mldata";"--rename-section";".rodata=.mlrodata";
      "--rename-section";".text=.mltext"] in
    let cmds = List.map (fun x -> A x) cmd in
    Cmd (S (cmds @ [Px src; Px dst]))

  (** Generic CC linking rule that wraps both Xen and C *) 
  let cc_link_c_implem ?tag fn c o env build =
    let c = env c and o = env o in
    fn (tags_of_pathname c++"implem"+++tag) c o env

  let rules () =
    (* Rule to rename module sections to ml* equivalents for the static vmem layout *)
    rule "ocaml: .m.o -> .mx.o"
      ~prod:"%.mx.o"
      ~dep:"%.m.o"
      (xen_objcopy "%.mx.o" "%.m.o");

     (* Xen link rule *)
    rule ("final link: %.mx.o -> %.xen")
      ~prod:"%(file).native"
      ~dep:"%(file).mx.o"
      (cc_link_c_implem cc_xen_nc_link "%(file).mx.o" "%(file).native")

end

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
         | Before_rules ->
             Xen.rules (); CC.rules ();
         | After_rules ->
             flag [ "ocaml"; "link"; "native" ] & S[A"-linkpkg"];
             flag [ "ocaml"; "compile"; "lwt_debug" ] & S[A"-ppopt"; A"-lwt-debug"];
             flag [ "ocaml"; "ocamldep"; "lwt_debug" ] & S[A"-ppopt"; A"-lwt-debug"];
         | _ -> ()
    )

