This repo contains:
  1. a xenstore client library, a merge of the Mirage and XCP ones
  2. a xenstore server library
  3. a xenstore server instance which runs under Unix with libxc
  4. a xenstore server instance which runs on mirage.

The client and the server libraries have sets of unit-tests.

Notes on persistence modes
==========================

The server supports 2 persistence modes:
  1. none (default): nothing is remembered across server invocations
  2. crash resilient: the process may be killed and restarted at any
     time without loss of service
     
In crash resilient mode, all critical data is stored in an irminsule[1]
git database. This data includes:
  * all key/value pairs and metadata
  * all interdomain ring domid/mfn/event-channel (so connections can
    be re-established)
  * all unsent watch events (clients will see each event at least once)
  * all current watch registrations
  * any partially-read packets from rings
  * the highest used transaction id per connection
  * all untransmitted reply packets
  * all log settings
  * all quota information

The server will ensure that the side-effects of an operation will be
persisted before any reply packet is transmitted.

When the server restarts, the only visible artifacts should be:
  1. possible duplicate watch events (we assume these are idempotent)
  2. all outstanding transactions will be artificially aborted (client
     is expected to restart the transaction anyway)

The irminsule database should be persisted to storage which is cleared
on host restart. Ideally it should be stored to RAM (eg tmpfs) and not
a physical disk (slow and unnecessarily durable).

