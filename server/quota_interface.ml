include Namespace.Unsupported

let ( |> ) a b = b a

let read t (perms: Perms.t) (path: Store.Path.t) =
	match Store.Path.to_string_list path with
		| "default" :: "maxent" :: [] ->
			string_of_int (!Quota.maxent)
		| "default" :: "maxsize" :: [] ->
			string_of_int (!Quota.maxsize)
		| "domain" :: domid :: [] ->
			let q = t.Transaction.store.Store.quota in
			let domid = int_of_string domid in
			let n = Quota.get q domid in
(*			if n = 0 then raise Store.Path.Doesnt_exist; *)
			string_of_int n
		| _ -> raise Store.Path.Doesnt_exist

let exists t perms path =
	match Store.Path.to_string_list path with
		| "default" :: [] -> true
		| "domain" :: [] -> true
		| "default" :: "maxent" :: [] -> true
		| "default" :: "maxsize" :: [] -> true
		| "domain" :: domid :: [] ->
			let q = t.Transaction.store.Store.quota in
			let domid = int_of_string domid in
			Quota.get q domid <> 0
		| _ -> false

let write t creator perms path value =
	match Store.Path.to_string_list path with
		| "default" :: "maxent" :: [] ->
			Quota.maxent := int_of_string value
		| "default" :: "maxsize" :: [] ->
			Quota.maxsize := int_of_string value
		| _ -> raise Store.Path.Doesnt_exist

let list t perms path =
	match Store.Path.to_string_list path with
	| [] -> [ "default"; "domain" ]
	| [ "default" ] -> [ "maxent"; "maxsize" ]
	| [ "domain" ] ->
		let q = t.Transaction.store.Store.quota in
		Quota.list q |> List.map fst |> List.map string_of_int
	| _ -> raise Store.Path.Doesnt_exist

