
type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external map_foreign_job: int -> nativeint -> buf Lwt_unix.job = "lwt_map_foreign_job"

let map_foreign domid mfn = Lwt_unix.run_job (map_foreign_job domid mfn)

external map_fd: Unix.file_descr -> int -> buf option = "ml_map_fd"

external unmap_foreign: buf -> unit = "ml_unmap"

external sizeof_xc_domaininfo_t: unit -> int = "ml_sizeof_xc_domaininfo_t"

external alloc_page_aligned: int -> buf option = "ml_alloc_page_aligned"

external free_page_aligned: buf -> unit = "ml_free_page_aligned"

external domain_infolist_job: int -> int -> Cstruct.t -> int Lwt_unix.job = "lwt_domain_infolist_job"

type info = {
	domid: int;
	dying: bool;
	shutdown: bool;
}

external xc_domaininfo_t_parse: Cstruct.t -> info = "ml_domain_infolist_parse"

open Lwt

let batch_size = 512 (* number of domains to query in one hypercall *)

let xc_domain_getinfolist lowest_domid =
	let sizeof = sizeof_xc_domaininfo_t () in
	let buf = alloc_page_aligned (batch_size * sizeof) in
	match buf with
		| None -> return None
		| Some buf ->
			try_lwt
				let buf = Cstruct.of_bigarray buf in
				lwt number_found = Lwt_unix.run_job (domain_infolist_job lowest_domid batch_size buf) in
				let rec parse buf n acc =
					if n = number_found
					then acc
					else parse (Cstruct.shift buf sizeof) (n + 1) (xc_domaininfo_t_parse buf :: acc) in
				return (Some(parse buf 0 []))
			finally
				return (free_page_aligned buf)

let domain_infolist () =
	let rec loop from =
		lwt first = xc_domain_getinfolist from in
		match first with
			| None -> return None
			| Some first ->
				(* If we returned less than a batch then there are no more. *)
				if List.length first < batch_size
				then return (Some first)
				else match first with
					| [] -> return (Some [])
					| x :: xs ->
						(* Don't assume the last entry has the highest domid *)
						let largest_domid = List.fold_left (fun domid di -> max domid di.domid) x.domid xs in
						lwt rest = loop (largest_domid + 1) in
						match rest with
							| None -> return None
							| Some rest -> return (Some (first @ rest)) in
	loop 0

type xc_evtchn
external xc_evtchn_open: unit -> xc_evtchn option = "stub_xc_evtchn_open"

external xc_evtchn_close: destroy: xc_evtchn -> unit = "stub_xc_evtchn_close"

external xc_evtchn_fd: xc_evtchn -> Unix.file_descr option = "stub_xc_evtchn_fd"

external xc_evtchn_notify: xc_evtchn -> int -> unit = "stub_xc_evtchn_notify"

external xc_evtchn_bind_interdomain: xc_evtchn -> int -> int -> int option = "stub_xc_evtchn_bind_interdomain"

external xc_evtchn_bind_virq_dom_exc: xc_evtchn -> int option = "stub_xc_evtchn_bind_virq_dom_exc"

external xc_evtchn_unbind: xc_evtchn -> int -> unit = "stub_xc_evtchn_unbind"

external xc_evtchn_pending: xc_evtchn -> int option = "stub_xc_evtchn_pending"

external xc_evtchn_unmask: xc_evtchn -> int -> unit = "stub_xc_evtchn_unmask"
