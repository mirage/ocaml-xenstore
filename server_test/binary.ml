open Lwt

let debug fmt = Xenstore_server.Logging.debug "xenstored" fmt
let info  fmt = Xenstore_server.Logging.info  "xenstored" fmt
let error fmt = Xenstore_server.Logging.error "xenstored" fmt

let socket =
  let tmp = Filename.temp_file "xenstore-test" (string_of_int (Unix.getpid ())) in
  Unix.unlink tmp;
  tmp

let _ =
  Xs_transport.xenstored_socket := socket

module Server = Xenstore_server.Xs_server.Server(Xs_server_lwt_unix)

let debug = ref false

let rec logging_thread logger =
  lwt lines = Xenstore_server.Logging.get logger in
  lwt () = Lwt_list.iter_s
    (fun x ->
      if !debug
      then Lwt_io.write_line Lwt_io.stdout x
      else return ()
    ) lines in
  logging_thread logger

let server_thread =
  let (_: 'a) = logging_thread Xenstore_server.Logging.logger in
  let (_: 'a) = logging_thread Xenstore_server.Logging.access_logger in
  info "Starting test";
  Server.serve_forever ()

open OUnit

let test (request, response) () =
  let open Xs_transport_lwt_unix_client in
  lwt c = create () in
  lwt () = write c request 0 (String.length request) in
  let buffer = String.make (String.length response) '\000' in
  lwt _ = read c buffer 0 (String.length response) in
  assert_equal ~printer:String.escaped response buffer;
  return ()

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
    "-connect", Arg.Set_string Xs_transport.xenstored_socket, "Connect to a specified xenstored (otherwise use an internal server)";
    "-debug",   Arg.Set debug, "Print debug logging";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test xenstore server code";

  let suite = "xenstore" >::: (List.map (fun (x', x, y', y) ->
    Printf.sprintf "%s -> %s" (String.escaped x') (String.escaped y')
    >:: (fun () -> Lwt_main.run (test (x, y) ()))
  ) Messages.all) in
  run_test_tt ~verbose:!verbose suite


