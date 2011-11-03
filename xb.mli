exception End_of_file
exception Eagain
exception Noent
exception Invalid
type backend
type t = {
  backend : backend;
  pkt_in : Xs_packet.t Queue.t;
  pkt_out : Xs_packet.t Queue.t;
  mutable partial_in : Xs_packet.Partial.buf;
  mutable partial_out : string;
}
val queue : t -> Xs_packet.t -> unit
val read : t -> string -> int -> int Lwt.t
val write : t -> string -> int -> int Lwt.t
val output : t -> bool Lwt.t
val input : t -> bool Lwt.t
val init : unit -> t
val output_len : t -> int
val has_new_output : t -> bool
val has_old_output : t -> bool
val has_output : t -> bool
val peek_output : t -> Xs_packet.t
val input_len : t -> int
val has_in_packet : t -> bool
val get_in_packet : t -> Xs_packet.t
