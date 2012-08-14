
external map_foreign_job: int -> nativeint -> Cstruct.buf job = "lwt_map_foreign_job"

let map_foreign domid mfn = Lwt_unix.run_job (map_foreign_job domid mfn)
