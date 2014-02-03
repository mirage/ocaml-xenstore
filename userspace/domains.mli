
val map_foreign: int -> nativeint -> Io_page.t
val unmap_foreign: Io_page.t -> unit

val map_fd: Unix.file_descr -> int -> Io_page.t

type info = {
	domid: int;
	dying: bool;
	shutdown: bool;
}

val domain_infolist: unit -> info list
