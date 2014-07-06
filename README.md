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

open Lwt
module Client = Xenstore.Client.Make(Userspace)
```
To perform a single non-transactional read or wrote:
```
lwt my_domid = Client.(immediate (read "domid"));;
```
To perform a transactional update:
```
Client.(transaction (
  let open M in
  write "a" "b" >>= fun () ->
  write "c" "d" >>= fun () ->
  return ()
))
```
To wait for a condition to be met:
```
Client.(wait (
  let open M in
  read "hotplug-status" >>= fun status ->
  read "hotplug-error" >>= fun error ->
  match status, error with
  | "", "" -> return `Retry
  | status, _ when status <> "" -> return (`Ok status)
  | _, error -> return (`Error error)
))
