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

let debug fmt = Logging.debug "pqueue" fmt
let info  fmt = Logging.info  "pqueue" fmt
let error fmt = Logging.debug "pqueue" fmt

open Lwt

module Make(T: S.SEXPABLE) = struct
  type t = T.t Queue.t

  let create name = return (Queue.create ())

  let length t = Queue.length t

  let add item t = Queue.add item t; return ()

  let clear t = Queue.clear t; return ()

  let fold f initial t = Queue.fold f initial t
end
