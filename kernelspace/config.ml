open Mirage

let () =
  add_to_ocamlfind_libraries ["xenstore"; "xenstored"; "shared-memory-ring.xenstore"];
  register "console" [
    foreign "Handler.Main" (console @-> job) $ default_console
  ]
