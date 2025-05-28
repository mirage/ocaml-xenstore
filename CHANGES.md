## v2.4.0 (2025-05-28)

* minor performance fixes (#53 by freddy77)
* use dune 2.0 (#54 by lindig)
* require OCaml 4.13 (#56 by last-genius)
* Add support `XS_DIRECTORY_PART` to the client (#55 by last-genius)
* Add x-maintenance-intent to opam  metadata (#57 by hannesm)

## v2.3.0 (2024-05-06)

* remove cstruct and ppx_cstruct dependency (#52 @palainp)
* require OCaml 4.08 (for Bytes.set_int32_le) (#52 @palainp)

## v2.2.0 (2022-07-26)

* Fix crash if watch quota is exceeded (#47 @talex5)
* Switch to ounit2 (#49 @Alessandro-Barbieri)
* Add license to opam metadata (#48 @psafont)

## 2.1.1 (2019-11-24)

* Do not open Pervasives unnecessarily. Avoids a warning on
  4.08 in dev builds on Dune (#44 @talex5)
* Update opam metadata to remove the `build`-only dep on Dune
  (#45 @craigfe)

## 2.1.0 (2019-02-03)

* Upgrade opam metadata to 2.0 format (@avsm)
* Port build from jbuilder to Dune (@avsm)
* Test on OCaml 4.07 (@avsm)
* Remove topkg metadata in favour of dune-release (@avsm)

## 2.0.1 (2018-08-01)

* Improved efficiency of unmarshalling code
* Use modern `ppx_cstruct` dependency for build.

## 2.0.0 (2017-12-06):
* Fix build with OCaml 4.04 (and `-safe-string`)
* Remove unnecessary dependency on `ocamlfind` and make `ounit` a test
  dependency
* Ensure the CI runs the unit tests

## 1.4.0 (2017-06-08):
* Add terminating replacements for transaction function
* Switch to jbuilder
* Release with topkg
* Use Docker in travis

## 1.3.0 (2016-03-14):
* Add EEXIST exception to the interface
* Allow Ocaml xenstore clients to receive oversized replies from xenstored
* Return the task for a wait asynchronously
* Increase maximum incoming watch limit from 1024 to 65536
* Don't leak watch strings in clients
* Add opam file
* Ensure errors from wait functions aren't lost
* Fix a non-tail call in the dispatcher
* Switch to ppx from camlp4

## 1.2.5 (2013-10-04):
* Add Travis continuous integration scripts
* fix a spurious EQUOTA failure when processing transactions

## 1.2.4 (2013-09-11):
* fix watches

## 1.2.3 (2013-08-27):
* export a signature, which can be satisfied by both Unix userspace
  and xen kernelspace clients

## 1.2.2 (2013-08-06):
* Generate documentation with make doc.
* Documentation improvements/uniformization.
* Xs_handle rewritten in a more functional style.
* Function names now are equal to mirage ones.
* Renamed Xs_client to Xs_client_lwt.

## 1.2.1 (2013-03-15):
* Drop packets which are too large

## 1.2.0 (2013-02-08):
* Use the latest cstruct >=0.6.0 API.

## 1.1.0 (2012-12-15):
* Add suspend/resume support to the Xenstore client

## 1.0.0 (2012-10-05):
* Initial public release.
