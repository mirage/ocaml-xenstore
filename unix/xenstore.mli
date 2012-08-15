
val map_foreign: int -> nativeint -> Cstruct.buf Lwt.t

val unsafe_read: Cstruct.buf -> string -> int -> int -> int
val unsafe_write: Cstruct.buf -> string -> int -> int -> int
