open Xenstore

module type IO = sig
	val exists: Transaction.t -> Perms.t -> Protocol.Path.t -> bool
	val mkdir: Transaction.t -> int -> Perms.t -> Protocol.Path.t -> unit
	val read: Transaction.t -> Perms.t -> Protocol.Path.t -> string
	val write: Transaction.t -> int -> Perms.t -> Protocol.Path.t -> string -> unit
	val ls: Transaction.t -> Perms.t -> Protocol.Path.t -> string list
	val rm: Transaction.t -> Perms.t -> Protocol.Path.t -> unit
	val getperms: Transaction.t -> Perms.t -> Protocol.Path.t -> Protocol.ACL.t
	val setperms: Transaction.t -> Perms.t -> Protocol.Path.t -> Protocol.ACL.t -> unit
end

exception Unsupported

module Unsupported = struct
	let exists _ _ _ = raise Unsupported
	let mkdir _ _ _ _ = raise Unsupported
	let read _ _ _ = raise Unsupported
	let write _ _ _ _ _ = raise Unsupported
	let ls _ _ _ = raise Unsupported
	let rm _ _ _ = raise Unsupported
	let getperms _ _ _ = raise Unsupported
	let setperms _ _ _ _ = raise Unsupported
end
