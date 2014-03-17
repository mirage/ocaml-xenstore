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
open Xenstore

module Introspect = struct
  include Tree.Unsupported

  let read t (perms: Perms.t) (path: Protocol.Path.t) =
    Perms.has perms Perms.CONFIGURE;
    match Protocol.Path.to_string_list path with
    | [] -> ""
    | domid :: [] ->
      begin
        try
          string_of_int (Hashtbl.find t.Transaction.store.Store.created (int_of_string domid))
        with Not_found ->
          raise (Node.Doesnt_exist path)
      end
    | _ -> raise (Node.Doesnt_exist path)

  let exists t perms path = try ignore(read t perms path); true with Node.Doesnt_exist _ -> false

  let ls t perms path =
    Perms.has perms Perms.CONFIGURE;
    match Protocol.Path.to_string_list path with
    | [ ] -> Hashtbl.fold (fun d _ acc -> string_of_int d :: acc) t.Transaction.store.Store.created []
    | _ -> []
end

let _ = Mount.mount (Protocol.Path.of_string "/tool/xenstored/entries") (module Introspect: Tree.S)
