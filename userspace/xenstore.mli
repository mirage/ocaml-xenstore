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

type xc_evtchn
val xc_evtchn_open: unit -> xc_evtchn option

val xc_evtchn_close: destroy: xc_evtchn -> unit

val xc_evtchn_fd: xc_evtchn -> Unix.file_descr option

val xc_evtchn_notify: xc_evtchn -> int -> unit

val xc_evtchn_bind_interdomain: xc_evtchn -> int -> int -> int option

val xc_evtchn_bind_virq_dom_exc: xc_evtchn -> int option

val xc_evtchn_unbind: xc_evtchn -> int -> unit

val xc_evtchn_pending: xc_evtchn -> int option

val xc_evtchn_unmask: xc_evtchn -> int -> unit
