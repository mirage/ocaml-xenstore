open Lwt
open Xenstore
open Xenstored

let debug fmt = Logging.debug "xenstored" fmt
let info  fmt = Logging.info  "xenstored" fmt
let error fmt = Logging.error "xenstored" fmt

let socket =
  let tmp = Filename.temp_file "xenstore-test" (string_of_int (Unix.getpid ())) in
  Unix.unlink tmp;
  tmp

let _ =
  Sockets.xenstored_socket := socket

module Server = Server.Make(Sockets)

let debug = ref false

let rec logging_thread logger =
  lwt lines = Logging.get logger in
  lwt () = Lwt_list.iter_s
    (fun x ->
      if !debug
      then Lwt_io.write_line Lwt_io.stdout x
      else return ()
    ) lines in
  logging_thread logger

let server_thread =
  let (_: 'a) = logging_thread Logging.logger in
  let (_: 'a) = logging_thread Logging.access_logger in
  info "Starting test";
  Server.serve_forever S.NoPersistence

open OUnit

let fail_on_error = function
| `Ok x -> return x
| `Error x -> fail (Failure x)

let test (request, response) () =
  let open Sockets in
  lwt c = create () in
  let request' = Cstruct.create (String.length request) in
  Cstruct.blit_from_string request 0 request' 0 (String.length request);
  lwt () = write c request' in

  let response' = Cstruct.create 1024 in
  let header = Cstruct.sub response' 0 Protocol.Header.sizeof in
  lwt () = read c header in
  let payload = Cstruct.shift response' Protocol.Header.sizeof in
  fail_on_error (Protocol.Header.unmarshal header) >>= fun hdr ->
  let payload' = Cstruct.sub payload 0 (min hdr.Protocol.Header.len (Cstruct.len payload)) in
  lwt () = read c payload' in
  let response' = Cstruct.sub response' 0 (payload'.Cstruct.off + payload'.Cstruct.len) in
  assert_equal ~printer:String.escaped response (Cstruct.to_string response');
  return ()

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
    "-connect", Arg.Set_string Sockets.xenstored_socket, "Connect to a specified xenstored (otherwise use an internal server)";
    "-debug",   Arg.Set debug, "Print debug logging";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test xenstore server code";

  let suite = "xenstore" >::: (List.map (fun (x', x, y', y) ->
    Printf.sprintf "%s -> %s" (String.escaped x') (String.escaped y')
    >:: (fun () -> Lwt_main.run (test (x, y) ()))
  ) Messages.all) in
  run_test_tt ~verbose:!verbose suite


