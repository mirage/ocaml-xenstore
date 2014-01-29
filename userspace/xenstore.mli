type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val map_foreign: int -> nativeint -> buf Lwt.t
val unmap_foreign: buf -> unit

val map_fd: Unix.file_descr -> int -> buf option

type info = {
	domid: int;
	dying: bool;
	shutdown: bool;
}

val domain_infolist: unit -> info list option Lwt.t
