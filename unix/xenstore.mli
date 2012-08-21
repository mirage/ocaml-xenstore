
val map_foreign: int -> nativeint -> Cstruct.buf Lwt.t
val unmap_foreign: Cstruct.buf -> unit

val map_fd: Unix.file_descr -> int -> Cstruct.buf option

val unsafe_read: Cstruct.buf -> string -> int -> int -> int
val unsafe_write: Cstruct.buf -> string -> int -> int -> int

type channel_state = {
	cons: int;
	prod: int;
	data: int;
}
type ring_state = {
	request: channel_state;
	response: channel_state;
}

val get_ring_state: Cstruct.buf -> ring_state

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
