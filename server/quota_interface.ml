include Namespace.Unsupported

let ( |> ) a b = b a

let read t (perms: Perms.t) (path: Store.Path.t) =
	match Store.Path.to_string_list path with
	| "default" :: [] -> ""
	| "domain" :: [] -> ""
	| "maxent" :: [] -> ""
	| "maxwatch" :: [] -> ""
	| "maxtransaction" :: [] -> ""
	| "default" :: "maxent" :: [] ->
		string_of_int (!Quota.maxent)
	| "default" :: "maxsize" :: [] ->
		string_of_int (!Quota.maxsize)
	| "default" :: "maxwatch" :: [] ->
		string_of_int (!Quota.maxwatch)
	| "default" :: "maxtransaction" :: [] ->
		string_of_int (!Quota.maxtransaction)
	| "domain" :: domid :: [] ->
		let q = t.Transaction.store.Store.quota in
		let domid = int_of_string domid in
		let n = Quota.get q domid in
		string_of_int n
	| "maxent" :: domid :: [] ->
		begin match Quota.get_override Quota.maxent_overrides (int_of_string domid) with
		| Some x -> string_of_int x
		| None -> raise Store.Path.Doesnt_exist
		end
	| "maxwatch" :: domid :: [] ->
		begin match Quota.get_override Quota.maxwatch_overrides (int_of_string domid) with
		| Some x -> string_of_int x
		| None -> raise Store.Path.Doesnt_exist
		end
	| "maxtransaction" :: domid :: [] ->
		begin match Quota.get_override Quota.maxtransaction_overrides (int_of_string domid) with
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
		| "default" :: "maxwatch" :: [] ->
			Quota.maxwatch := int_of_string value
		| "default" :: "maxtransaction" :: [] ->
			Quota.maxtransaction := int_of_string value
		| "maxent" :: domid :: [] ->
			Quota.set_override Quota.maxent_overrides (int_of_string domid) (Some (int_of_string value))
		| "maxwatch" :: domid :: [] ->
			Quota.set_override Quota.maxwatch_overrides (int_of_string domid) (Some (int_of_string value))
		| "maxtransaction" :: domid :: [] ->
			Quota.set_override Quota.maxtransaction_overrides (int_of_string domid) (Some (int_of_string value))
		| _ -> raise Store.Path.Doesnt_exist

let list t perms path =
	match Store.Path.to_string_list path with
	| [] -> [ "default"; "domain"; "maxent"; "maxwatch"; "maxtransaction" ]
	| [ "default" ] -> [ "maxent"; "maxsize"; "maxwatch"; "maxtransaction" ]
	| [ "domain" ] ->
		let q = t.Transaction.store.Store.quota in
		Quota.list q |> List.map fst |> List.map string_of_int
	| [ "maxent" ] ->
		Quota.list_overrides Quota.maxent_overrides |> List.map fst |> List.map string_of_int
	| [ "maxwatch" ] ->
		Quota.list_overrides Quota.maxwatch_overrides |> List.map fst |> List.map string_of_int
	| [ "maxtransaction" ] ->
		Quota.list_overrides Quota.maxtransaction_overrides |> List.map fst |> List.map string_of_int
	| _ -> []


let rm t perms path =
	match Store.Path.to_string_list path with
	| "maxent" :: domid :: [] ->
		Quota.set_override Quota.maxent_overrides (int_of_string domid) None
	| "maxwatch" :: domid :: [] ->
		Quota.set_override Quota.maxwatch_overrides (int_of_string domid) None
	| "maxtransaction" :: domid :: [] ->
		Quota.set_override Quota.maxtransaction_overrides (int_of_string domid) None
	| _ -> raise Perms.Permission_denied
