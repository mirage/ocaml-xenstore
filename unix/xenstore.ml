
external map_foreign_job: int -> nativeint -> Cstruct.buf Lwt_unix.job = "lwt_map_foreign_job"

let map_foreign domid mfn = Lwt_unix.run_job (map_foreign_job domid mfn)

external map_fd: Unix.file_descr -> int -> Cstruct.buf = "ml_map_fd"

external unmap_foreign: Cstruct.buf -> unit = "ml_unmap"

external unsafe_read: Cstruct.buf -> string -> int -> int -> int = "ml_interface_read"
external unsafe_write: Cstruct.buf -> string -> int -> int -> int = "ml_interface_write"

external sizeof_xc_domaininfo_t: unit -> int = "ml_sizeof_xc_domaininfo_t"

external alloc_page_aligned: int -> Cstruct.buf = "ml_alloc_page_aligned"

external free_page_aligned: Cstruct.buf -> unit = "ml_free_page_aligned"

external domain_infolist_job: int -> int -> Cstruct.buf -> int Lwt_unix.job = "lwt_domain_infolist_job"

type info = {
	domid: int;
	dying: bool;
	shutdown: bool;
}

external xc_domaininfo_t_parse: Cstruct.buf -> info = "ml_domain_infolist_parse"

open Lwt

let batch_size = 512 (* number of domains to query in one hypercall *)

let xc_domain_getinfolist lowest_domid =
	let sizeof = sizeof_xc_domaininfo_t () in
	let buf = alloc_page_aligned (batch_size * sizeof) in
	lwt number_found = Lwt_unix.run_job (domain_infolist_job lowest_domid batch_size buf) in
	let rec parse buf n acc =
		if n = number_found
		then acc
		else parse (Cstruct.shift buf sizeof) (n + 1) (xc_domaininfo_t_parse buf :: acc) in
	return (parse buf 0 [])

let domain_infolist () =
	let rec loop from =
		lwt first = xc_domain_getinfolist from in
		(* If we returned less than a batch then there are no more. *)
		if List.length first < batch_size
		then return first
		else match first with
			| [] -> return []
			| x :: xs ->
				(* Don't assume the last entry has the highest domid *)
				let largest_domid = List.fold_left (fun domid di -> max domid di.domid) x.domid xs in
				lwt rest = loop (largest_domid + 1) in
				return (first @ rest) in
	loop 0

