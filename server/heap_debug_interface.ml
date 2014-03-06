open Xenstore

module Introspect = struct
  include Tree.Unsupported

  let debug fmt = Logging.debug "memory_interface" fmt

  let ( |> ) a b = b a

  let read t (perms: Perms.t) (path: Protocol.Path.t) =
    Perms.has perms Perms.CONFIGURE;
    match Protocol.Path.to_string_list path with
    | [] -> ""
    | "heap_words" :: [] -> string_of_int (Gc.stat ()).Gc.heap_words
    | "live_words" :: [] -> string_of_int (Gc.stat ()).Gc.live_words
    | "free_words" :: [] -> string_of_int (Gc.stat ()).Gc.free_words
    | "symbols"    :: [] -> string_of_int (Symbol.stats ())
    | _ -> raise (Node.Doesnt_exist path)

  let exists t perms path = try ignore(read t perms path); true with Node.Doesnt_exist _ -> false

  let ls t perms path =
    Perms.has perms Perms.CONFIGURE;
    match Protocol.Path.to_string_list path with
    | [] -> [ "heap_words"; "live_words"; "free_words"; "symbols" ]
    | _ -> []
end

let _ = Mount.mount (Protocol.Path.of_string "/tool/xenstored/memory") (module Introspect: Tree.S)

