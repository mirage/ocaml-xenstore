open Lwt

let debug fmt = Xenstore_server.Logging.debug "xenstored" fmt
let info  fmt = Xenstore_server.Logging.info  "xenstored" fmt
let error fmt = Xenstore_server.Logging.error "xenstored" fmt

let requests_and_responses = [
  "\002\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000",
  "\024\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000EINVAL\000";

  "\013\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000",
  "\024\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000EINVAL\000";

  "\012\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000",
  "\012\000\000\000\000\000\000\000\000\000\000\000\024\000\000\000/local/domain/0\000";
]

let socket =
  let tmp = Filename.temp_file "xenstore-test" (string_of_int (Unix.getpid ())) in
  Unix.unlink tmp;
  tmp

let _ =
  Xs_server_lwt_unix.xenstored_socket := socket

module Server = Xenstore_server.Xs_server.Server(Xs_server_lwt_unix)

let rec logging_thread logger =
  lwt lines = Xenstore_server.Logging.get logger in
  lwt () = Lwt_list.iter_s
    (fun x ->
      Lwt_io.write_line Lwt_io.stdout x
    ) lines in
  logging_thread logger

let server_thread =
  let (_: 'a) = logging_thread Xenstore_server.Logging.logger in
  let (_: 'a) = logging_thread Xenstore_server.Logging.access_logger in
  info "Starting test";
  Server.serve_forever ()

let test (request, response) () =
  return ()

open OUnit

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test xenstore server code";

  let suite = "xenstore" >::: (List.map (fun x ->
    "unknown" >:: (fun () -> Lwt_main.run (test x ()))
  ) requests_and_responses) in
  run_test_tt ~verbose:!verbose suite


