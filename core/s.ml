(*
 * Copyright (C) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Sexplib

module type STRINGABLE = sig
  type t
  val to_string: t -> string
  val of_string: string -> t
end

module type SEXPABLE = sig
  type t
  val sexp_of_t: t -> Sexp.t
  val t_of_sexp: Sexp.t -> t
end

module type INTROSPECTABLE = sig
  type t
  val ls: t -> string list -> string list
  val read: t -> string list -> string option
  val write: t -> string list -> string -> bool
end

module type MONAD = sig
  type 'a t
  val return: 'a -> 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
end

module type IO = MONAD
  with type 'a t = 'a Lwt.t

module type STREAM = sig
  include IO

  type state

  val initial: state

  type data

  val next: state -> (data * state) t
end

module type WINDOW = sig
  (** A view of the underlying stream *)

  type t

  type offset

  type item

  val next: t -> (offset * item) Lwt.t
  (** [next t] returns the next item from the stream, together with the offset
      which should be passed to [ack] *)

  val ack: t -> offset -> unit Lwt.t
  (** [ack buf offset] declares that we have processed all data up to [offset]
      and therefore any buffers may be recycled. *)
end

module type PACKET_WRITER = sig
  type t

  type offset

  val write: t -> offset -> Protocol.Header.t -> (Cstruct.t -> Cstruct.t) -> offset Lwt.t
  (** [write t offset hdr marshal] writes a packet to the output at [offset],
      and returns the next [offset] value, suitable for [ack] *)

  val ack: t -> offset -> unit Lwt.t
  (** [ack offset] ensures all data written before [offset] is flushed *)
end

module type CONNECTION = sig
  include IO

  type connection

  val create: unit -> connection t

  val destroy: connection -> unit t

  val uri_of: connection -> Uri.t t

  val domain_of: connection -> int

  module Request : sig
    module Reader : WINDOW
      with type offset = int64
      and type item = [ `Ok of (Protocol.Header.t * Cstruct.t) | `Error of string ]
    module Writer : PACKET_WRITER
  end
  module Response : sig
    module Reader : WINDOW
      with type offset = int64
      and type item = [ `Ok of (Protocol.Header.t * Cstruct.t) | `Error of string ]
    module Writer : PACKET_WRITER
  end

  val read: connection -> Cstruct.t -> unit t

  val write: connection -> Cstruct.t -> unit t
end

module type SERVER = sig
  include IO
  include CONNECTION
    with type 'a t := 'a t

  type server

  val listen: unit -> server t

  val accept_forever: server -> (connection -> unit t) -> 'a t
end

module type TRANSPORT = sig
  include IO

  include SERVER
    with type 'a t := 'a t

  module Introspect : INTROSPECTABLE with type t = connection
end

module type CLIENT = sig
  include IO

  val suspend : unit -> unit t
  val resume : unit -> unit t

  type ctx

  module M: MONAD with type 'a t = ctx -> 'a t

  val directory     : string -> ctx -> string list t
  val read          : string -> ctx -> string t
  val write         : string -> string -> ctx -> unit t
  val rm            : string -> ctx -> unit t
  val mkdir         : string -> ctx -> unit t
  val setperms      : string -> Protocol.ACL.t -> ctx -> unit t
  val debug         : string list -> ctx -> string list t
  val restrict      : int -> ctx -> unit t
  val getdomainpath : int -> ctx -> string t
  val watch         : string -> Protocol.Token.t -> ctx -> unit t
  val unwatch       : string -> Protocol.Token.t -> ctx -> unit t
  val introduce     : int -> nativeint -> int -> ctx -> unit t
  val set_target    : int -> int -> ctx -> unit t

  val immediate : (ctx -> 'a t) -> 'a t
  (** [immediate op] executes [op] in a regular, non-transactional context *)

  val transaction: (ctx -> 'a t) -> 'a t
  (** [transaction op] executes [op] as a transaction *)

  val wait: (ctx -> [ `Ok of 'a | `Error of 'b | `Retry ] t) -> [ `Ok of 'a | `Error of 'b ] t
end

module type ACTIVATIONS = sig
  include IO

  type channel
  (** An entity which receives events, which we can wait for *)

  type event
  (** An individual event notification *)

  val program_start: event
  (** represents an event which 'fired' when the program started *)

  val after: channel -> event -> event t
  (** [next channel event] blocks until the system receives an event
      newer than [event] on channel [channel]. If an event is received
      while we aren't looking then this will be remembered and the
      next call to [after] will immediately unblock. If the system
      is suspended and then resumed, all event channel bindings are invalidated
      and this function will fail with Generation.Invalid *)
end

module type DOMAIN_STATE = sig
  type t = {
    domid: int;     (** unique id for a given domain *)
    dying: bool;    (** the domain is being cleaned up *)
    shutdown: bool; (** the domain has stopped running *)
  }
  (** The state of a domain *)

  val list: unit -> t list
  (** [list ()] returns a list of known domains *)
end

module type FOREIGN_PAGE_MAPPER = sig
  val map: int -> nativeint -> Cstruct.t
  (** [map domid mfn] maps a foreign page *)

  val unmap: Cstruct.t -> unit
  (** [unmap page] unmaps a previously-mapped foreign page *)
end
