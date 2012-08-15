
external map_foreign_job: int -> nativeint -> Cstruct.buf Lwt_unix.job = "lwt_map_foreign_job"

let map_foreign domid mfn = Lwt_unix.run_job (map_foreign_job domid mfn)

external unsafe_read: Cstruct.buf -> string -> int -> int -> int = "ml_interface_read"
external unsafe_write: Cstruct.buf -> string -> int -> int -> int = "ml_interface_write"
