open Xenstore
open Logging

include Namespace.Unsupported

let ( |> ) a b = b a

let general_params = [
	"conflict", disable_conflict;
	"commit", disable_conflict;
	"newconn", disable_newconn;
	"endconn", disable_endconn;
	"transaction", disable_transaction;
]

let op_of_string path op =
        try Protocol.Op.t_of_sexp (Sexplib.Sexp.of_string op) with _ -> raise (Node.Doesnt_exist path)

let read t (perms: Perms.t) (path: Protocol.Path.t) =
	Perms.has perms Perms.CONFIGURE;
	match Protocol.Path.to_string_list path with
		| [] -> ""
		| "request" :: [] -> ""
		| "reply-ok" :: [] -> ""
		| "reply-err" :: [] -> ""
		| "request" :: x :: [] -> if List.mem (op_of_string path x) !disable_request then "1" else raise (Node.Doesnt_exist path)
		| "reply-ok" :: x :: [] -> if List.mem (op_of_string path x) !disable_reply_ok then "1" else raise (Node.Doesnt_exist path)
		| "reply-err" :: x :: [] -> if List.mem x !disable_reply_err then "1" else raise (Node.Doesnt_exist path)
		| x :: [] ->
			if List.mem_assoc x general_params
			then if !(List.assoc x general_params) then "1" else raise (Node.Doesnt_exist path)
			else raise (Node.Doesnt_exist path)
		| _ -> raise (Node.Doesnt_exist path)

let exists t perms path = try ignore(read t perms path); true with Node.Doesnt_exist _ -> false

let write t creator perms path value =
	Perms.has perms Perms.CONFIGURE;
	let f list value key =
                match value with
        	| "1" -> if not(List.mem key !list) then list := key :: !list
	        | _ -> raise (Invalid_argument value)  in
	match Protocol.Path.to_string_list path with
		| "request" :: x :: [] -> f disable_request value (op_of_string path x)
		| "reply-ok" :: x :: [] -> f disable_reply_ok value (op_of_string path x)
		| "reply-err" :: x :: [] -> f disable_reply_err value x
		| x :: [] ->
			begin
				if List.mem_assoc x general_params then
					(List.assoc x general_params) := match value with
						| "1" -> true
						| _ -> raise (Invalid_argument value)
			end
		| _ -> raise (Node.Doesnt_exist path)

let ls t perms path =
	Perms.has perms Perms.CONFIGURE;
        let string_of_op op = Sexplib.Sexp.to_string (Protocol.Op.sexp_of_t op) in
	match Protocol.Path.to_string_list path with
		| [] -> [ "request"; "reply-ok"; "reply-err" ] @ (List.map fst (List.filter (fun (_, b) -> !b) general_params))
		| "request" :: [] -> List.map string_of_op !disable_request
		| "reply-ok" :: [] -> List.map string_of_op !disable_reply_ok
		| "reply-err" :: [] -> !disable_reply_err
		| _ -> []

let rm t perms path =
	Perms.has perms Perms.CONFIGURE;
	let f list key = list := List.filter (fun x -> x <> key) !list in
	match Protocol.Path.to_string_list path with
		| "request" :: x :: [] -> f disable_request (op_of_string path x)
		| "reply-ok" :: x :: [] -> f disable_reply_ok (op_of_string path x)
		| "reply-err" :: x :: [] -> f disable_reply_err x
		| x :: [] ->
			if List.mem_assoc x general_params
			then (List.assoc x general_params) := false
			else raise (Node.Doesnt_exist path)
		| _ -> raise (Node.Doesnt_exist path)

