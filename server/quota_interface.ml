include Namespace.Unsupported

let ( |> ) a b = b a

let read t (perms: Perms.t) (path: Store.Path.t) =
	match Store.Path.to_string_list path with
	| "default" :: [] -> ""
	| "domain" :: [] -> ""
	| "maxent" :: [] -> ""
	| "default" :: "maxent" :: [] ->
		string_of_int (!Quota.maxent)
	| "default" :: "maxsize" :: [] ->
		string_of_int (!Quota.maxsize)
	| "domain" :: domid :: [] ->
		let q = t.Transaction.store.Store.quota in
		let domid = int_of_string domid in
		let n = Quota.get q domid in
		string_of_int n
	| "maxent" :: domid :: [] ->
		begin match Quota.get_maxent_override (int_of_string domid) with
		| Some x -> string_of_int x
		| None -> raise Store.Path.Doesnt_exist
		end
	| _ -> raise Store.Path.Doesnt_exist

let exists t perms path = try ignore(read t perms path); true with Store.Path.Doesnt_exist -> false

let write t creator perms path value =
	match Store.Path.to_string_list path with
		| "default" :: "maxent" :: [] ->
			Quota.maxent := int_of_string value
		| "default" :: "maxsize" :: [] ->
			Quota.maxsize := int_of_string value
		| "maxent" :: domid :: [] ->
			Quota.set_maxent_override (int_of_string domid) (Some (int_of_string value))
		| _ -> raise Store.Path.Doesnt_exist

let list t perms path =
	match Store.Path.to_string_list path with
	| [] -> [ "default"; "domain"; "maxent" ]
	| [ "default" ] -> [ "maxent"; "maxsize" ]
	| [ "domain" ] ->
		let q = t.Transaction.store.Store.quota in
		Quota.list q |> List.map fst |> List.map string_of_int
	| [ "maxent" ] ->
		Quota.list_maxent_overrides () |> List.map fst |> List.map string_of_int
	| _ -> []


let rm t perms path =
	match Store.Path.to_string_list path with
	| "maxent" :: domid :: [] ->
		Quota.set_maxent_override (int_of_string domid) None
	| _ -> raise Perms.Permission_denied
