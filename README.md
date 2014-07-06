XenStore protocol implementation for Mirage
===========================================

Layout of this project:
  core/        : protocol parser/printer, Lwt-based client
  core_test/   : unit tests for the protocol parser/printer
  unix/        : userspace 'transport' over sockets and xenbus mmap
  mirage/      : kernelspace 'transport' for Mirage
  xs/          : example userspace CLI

This code is all available under the standard Mirage license (ISC).

How to use the client
=====================

For Unix userspace, add the ocamlfind package 'xenstore.userspace'
and create a 'Client':
```
#use "topfind";;
#require "lwt.syntax";;
#require "xenstore.userspace";;

module Client = Xenstore.Client.Make(Userspace)
lwt client = Client.make ()
```
To perform a single non-transactional read or wrote:
```
lwt my_domid = Client.immediate client (Client.read "domid");;
```
To perform a transactional update:
```
Client.transaction client (
  let open Client.Transaction in
  Client.write "a" "b" >>= fun () ->
  Client.write "c" "d" >>= fun () ->
  return ()
)
```
To wait for a condition to be met:
```
Client.wait client (
  let open Client.Wait in
  Client.read "hotplug-status" >>= fun status ->
  Client.read "hotplug-error" >>= fun error ->
  match status, error with
  | "", "" -> retry
  | status, _ -> return (`Ok status)
  | _, error -> return (`Error error)
)

Open issues
-----------
Can we hide the client instances? The library could keep track of the (single) client
and provide 'suspend' 'resume' functions. Otherwise the application has to thread
a client through everywhere.

