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

module Make(Writer: S.WINDOW with type offset = int64) : sig
  (** Create a buffered WINDOW intended for Writing on top of an unbuffered
      one *)

  include S.WINDOW with type offset = int64

  val attach: Writer.t -> Cstruct.t -> t
  (** [attach writer buffer] return a buffered writer layered on top of
      [writer]. Data written here will be buffered, and only flushed to the
      underlying writer when [ack] is called.

      This call does not initialise [buffer]. *)

  val create: Writer.t -> Cstruct.t -> t
  (** [create writer buffer] return a buffered writer layered on top of
      [writer]. Initialises the [buffer]. *)

end
