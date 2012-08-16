
val map_foreign: int -> nativeint -> Cstruct.buf Lwt.t

val map_fd: Unix.file_descr -> int -> Cstruct.buf

val unsafe_read: Cstruct.buf -> string -> int -> int -> int
val unsafe_write: Cstruct.buf -> string -> int -> int -> int
