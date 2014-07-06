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
```
#use "topfind";;
#require "lwt.syntax";;
#require "xenstore.userspace";;

module Client = Xenstore.Client.Make(Userspace);;
lwt client = Client.make ();;
Client.immediate client (fun h -> Client.read h "domid");;
```

