(lang dune 2.0)

(name xenstore)

(generate_opam_files true)

(source
 (github mirage/ocaml-xenstore))

(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")

(authors
 "Vincent Hanquez"
 "Thomas Gazagnaire"
 "Dave Scott"
 "Anil Madhavapeddy"
 "Vincent Bernardoff")

(maintainers "Hannes Mehnert <hannes@mehnert.org>")

(package
 (name xenstore)
 (synopsis "Xenstore protocol in pure OCam")
 (description
  "This repo contains:\n\n  1. a xenstore client library, a merge of the Mirage and XCP ones\n\n  2. a xenstore server library\n\n  3. a xenstore server instance which runs under Unix with libxc\n\n  4. a xenstore server instance which runs on mirage.\n\n\n  The client and the server libraries have sets of unit-tests.\n")
 (depends
  (ocaml
   (>= 4.13.0))
  (ounit2
   (>= 2.2.2))
  (lwt
   (>= 4.5.0))))
