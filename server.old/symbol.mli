type t = int
type 'a record = { data : 'a; mutable garbage : bool; }
val int_string_tbl : (int, string record) Hashtbl.t
val string_int_tbl : (string, int) Hashtbl.t
val created_counter : int ref
val used_counter : int ref
val count : int ref
val fresh : unit -> int
val new_record : 'a -> 'a record
val of_string : string -> int
val to_string : int -> string
val mark_all_as_unused : unit -> unit
val mark_as_used : int -> unit
val garbage : unit -> unit
val stats : unit -> int
val created : unit -> int
val used : unit -> int
