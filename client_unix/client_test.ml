(* A place to put client library tests *)

module Client = Xs_client_unix.Client(Xs_transport_unix_client)

let test_watch_callbacks () =
  let client = Client.make () in
  let finished = ref false in

  let watch_callback _ =
    let domid = 
      Client.with_xs client (fun xs ->
	Client.read xs "domid") in
    finished := true;
    Printf.printf "Read domid: %s\n" domid
  in

  Client.set_watch_callback client watch_callback;

  Client.with_xs client (fun xs ->
    Client.watch xs "/tmp" "");

  Thread.delay 5.0;
  
  if not !finished then
    failwith "Test failed"

let _ = 
  test_watch_callbacks ()

    
    
    
    


