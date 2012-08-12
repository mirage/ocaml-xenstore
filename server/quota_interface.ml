include Namespace.Unsupported

let read t (perms: Perms.t) (path: Store.Path.t) =
	match Store.Path.to_string_list path with
		| "default" :: "maxent" :: [] ->
			string_of_int (!Quota.maxent)
		| "default" :: "maxsize" :: [] ->
			string_of_int (!Quota.maxsize)
		| "domain" :: domid :: [] ->
			begin
				try
					let q = t.Transaction.store.Store.quota in
					let domid = int_of_string domid in
					string_of_int (Hashtbl.find q.Quota.cur domid)
				with _ -> raise Store.Path.Doesnt_exist
			end
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
			Hashtbl.mem q.Quota.cur domid
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
		let domids = Hashtbl.fold (fun domid _ acc -> domid :: acc) q.Quota.cur [] in
		List.map string_of_int domids
	| _ -> raise Store.Path.Doesnt_exist

