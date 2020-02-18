(** Type for lists of integer indexes *)
module IndexList = struct

	(** Type of a list of indexes *)
	type t = Index.Int.t list

	(** Checks that all indexes of the list have the same length *)
	let check_len : t -> bool
		= fun il ->
		try let len = Index.Int.len (List.hd il) in
			List.for_all (fun i -> Index.Int.len i = len) il
		with Failure _ -> true

	let to_string : t -> string
		= fun il ->
		Misc.list_to_string Index.Int.to_string il " ; "

	(** Equality test between two lists of *)
	let equal : t -> t -> bool
		= fun il1 il2 ->
		Misc.list_eq2 (fun i j -> Index.Int.compare i j = 0) il1 il2

	(** [get_min dim il] returns an index [ind] such that [ind(i)] = min_j il(j)(i). *)
	let get_min : int -> t -> Index.Int.t
		= fun dim il ->
		Misc.init_list dim (fun i ->
			Misc.min Stdlib.compare (List.map (fun j -> Index.Int.get j i) il)
		)
		|> Index.Int.mk

	let get_next : int -> t -> Index.Int.t
		= fun dim il ->
		let il_length = List.length il in
		let n_pred_min = Stdlib.max 2 (il_length/2) in
		let rec get_next_rec : Index.Int.t * t * int -> Index.Int.t * t * int
			= fun (ind, il, n_pred) ->
			if List.length il <= 1
			then (ind, il, n_pred)
			else
			 	let l = Misc.init_list dim (fun i ->
					let ind' = Index.Int.incr ind i in (* Index incremented at cell i *)
					let il' = List.filter (Index.Int.le ind') il in
					let n_pred = List.length il' in
					let value = Index.Int.value ind' in
					(ind', il', n_pred, value)
				) in
				(* Computing the maximum n_pred that was found *)
				let (_,_,max_n_pred,_) = Misc.max (fun (_,_,n_pred1,_) (_,_,n_pred2,_) ->
					Stdlib.compare n_pred1 n_pred2
				) l in
				(* Filters indexes with this maximum n_pred *)
				let (ind',il',n_pred',_) = List.filter (fun (_,_,n_pred,_) ->
					n_pred = max_n_pred
				) l
				(* Take maximum value *)
				|> Misc.max (fun (_,_,_,v1) (_,_,_,v2) ->
					Stdlib.compare v1 v2
				) in
				if n_pred' < n_pred_min
				then (ind, il, n_pred)
				else get_next_rec (ind',il',n_pred')
		in
		let ind = get_min dim il in
		let (ind_res,_,_) = get_next_rec (ind, il, il_length) in
		ind_res

    let components : Index.Int.t -> t
		= fun ind ->
		let dim = Index.Int.len ind in
        let init = Index.Int.init dim in
		Misc.fold_left_i (fun i res v ->
                if v <> 0
                then let id = Index.Int.set init i 1 in
                (Misc.init_list v (fun _ -> id)) @ res
                else res
            ) [] ind

	let components_one_nneg : Index.Int.t -> t
		= fun ind ->
		let dim = Index.Int.len ind in
        let init = Index.Int.init dim in
		Misc.fold_left_i (fun i acc v ->
                if v <> 0
                then let id = Index.Int.set init i v in id :: acc
                else acc
            ) [] ind

	(** computes the list of indexes that are strictly smaller than a given one {i w.r.t} element-wise comparison *)
	let get_preds : Index.Int.t -> t
		= let rec get_preds_rec : Index.Int.t -> int -> t
			= fun ind col ->
			ind ::
			(List.concat
				(List.mapi
					(fun i _ -> if Index.Int.get ind i = 0
						then []
						else get_preds_rec (Index.Int.decr ind i) i)
					(Misc.sublist (Index.Int.data ind) 0 (col+1))
				)
			)
		in
		fun ind ->
		get_preds_rec ind (Index.Int.len ind)
		|> fun l -> Misc.pop Index.Int.equal l ind
		|> List.filter (fun i -> not (Index.Int.is_null i))

	(** [le dim max] computes the list of indexes of dimension dim whose sum of elements is <= max *)
	let (le : int -> int -> t)
		= let rec le : Index.Int.t -> int -> int -> int -> t
			= fun ind col valeur val_max->
			ind ::
			(List.concat
				(List.mapi
					(fun i _ -> if valeur = val_max
						then []
						else le (Index.Int.incr ind i) i (valeur+1) val_max)
					(Misc.sublist (Index.Int.data ind) 0 (col+1))
				)
			)
		in
		fun dim val_max ->
		le (Index.Int.init dim) dim 0 val_max
		|> List.filter (fun i -> not (Index.Int.is_null i))

end


module MapI = struct
	include Map.Make(Index.Int)

	(** [addMap f keys map] adds to [map] bindings from [k] to f[k] for all [k] in [keys]. *)
	let addMap : (key -> 'a) -> key list -> 'a t -> 'a t
		= fun f keys map ->
		List.fold_left
			(fun map key -> add key (f key) map)
			map
			keys

	(** [addMapCond cond f keys map] adds to [map] bindings from [k] to f[k] for all [k] in [keys] that satisfy [cond]. *)
	let addMapCond : (key -> bool) -> (key -> 'a) -> key list -> 'a t -> 'a t
		= fun cond f keys map ->
		List.fold_left
			(fun map key -> if cond key
				then add key (f key) map
				else map)
			map
			keys

	let addMapCond' : (key -> 'a t -> bool) -> (key -> 'a) -> key list -> 'a t -> 'a t
		= fun cond f keys map ->
		List.fold_left
			(fun map key -> if cond key map
				then add key (f key) map
				else map)
			map
			keys

	(** Adds elements of a list into a map.
		@param 	cond 	condition for elements to be added
		@param	f		function applied on elements before they are added *)
	let addMapCond2 : (key -> 'b -> bool) -> (key -> 'b -> 'a) -> key list -> 'b list -> 'a t -> 'a t
		= fun cond f keys elems map ->
		List.fold_left2
			(fun map key elem -> if cond key elem
				then add key (f key elem) map
				else map)
			map
			keys
			elems

	let addMapCond2' : (key -> 'a t -> bool) -> (key -> 'b -> 'a) -> key list -> 'b list -> 'a t -> 'a t
		= fun cond f keys elems map ->
		List.fold_left2
			(fun map key elem -> if cond key map
				then add key (f key elem) map
				else map)
			map
			keys
			elems

	let rec addComponentsOf : 'a t -> Index.Int.t -> IndexList.t * 'a t
		= fun map ind ->
		if Index.Int.is_unitary ind
		then ([], map)
		else if Index.Int.one_coeff_nn ind
		then let decomposition = IndexList.components ind in
			let map' = add ind decomposition map in
			(decomposition, map')
		else let decomposition = IndexList.components_one_nneg ind in
			let map' = add ind decomposition map in
			let map' = List.fold_left (fun map ind' ->
				addComponentsOf map ind' |> snd
			) map' decomposition in
			(decomposition, map')

end

module Map = struct

	type t = IndexList.t MapI.t

	let to_string : t -> string
		= let to_string2 (i,il) =
			Printf.sprintf "%s -> %s"
			(Index.Int.to_string i) (IndexList.to_string il)
		in fun m ->
		Misc.list_to_string to_string2 (MapI.bindings m) "\n"

	let get_next : Index.Int.t -> t -> Index.Int.t
		= fun ind map ->
		let l = MapI.fold (fun ind' _ acc ->
		 	let i = Index.Int.sub ind ind' in
			if Index.Int.is_nonnegative i
			then (i, Index.Int.value i) :: acc
			else acc
		) map []
		in
		if List.length l = 0
		then Stdlib.raise Not_found
		else Misc.max (fun (_,v1) (_,v2) -> if v1 > v2 then -1 else 1) l (* on cherche le min des valeurs*)
		|> Stdlib.fst

	let rec compute_from_map : Index.Int.t -> t -> IndexList.t * t
		= fun i map ->
		try
			(MapI.find i map, map)
		with Not_found ->
			if Index.Int.is_unitary i
			then ([], map)
			else
			try
				let ind = get_next i map in
				let i' = Index.Int.sub i ind in
				let (_,map') = (compute_from_map ind map) in
				let il' = ind :: (if Index.Int.is_null i' then [] else [i']) in
				(il', MapI.add i il' map')
			with Not_found ->
				let (decomposition, map') = MapI.addComponentsOf map i in
				let map' = List.fold_left (fun map ind ->
					let (_,map') = compute_from_map ind map in map'
				) map' decomposition in
				(decomposition, map')

	(* Hypothesis : il <> [] *)
	let rec compute_rec : int -> IndexList.t -> t
		= fun dim il ->
		if List.length il = 0
		then MapI.empty
		else if List.length il = 1
		then MapI.addComponentsOf MapI.empty (List.hd il) |> snd
		else
			let next_ind = IndexList.get_next dim il in
			if Index.Int.is_null next_ind
			then MapI.addMapCond
				(fun i -> not (Index.Int.is_unitary i))
				IndexList.components il MapI.empty
			else begin
				(* rem : indexes for which next_ind is a predecessor
				   keep : others *)
				let (rem,keep) = List.partition (Index.Int.le next_ind) il in
				let subs = List.map (fun j -> Index.Int.sub j next_ind) rem in (* différences de rem avec next_ind*)
				let keep' = List.filter (fun ind ->
					not (Index.Int.is_null ind || Index.Int.is_unitary ind)
				) (next_ind :: subs @ keep) in
				let map = (compute_rec dim keep') in
				MapI.addMapCond2
					(fun i sub -> not (Index.Int.is_unitary i || Index.Int.is_null sub))
					(fun _ sub -> [next_ind ; sub])
					rem subs map
			end

	let compute : IndexList.t -> t
		= fun il ->
		if List.length il = 0
		then Stdlib.invalid_arg "IndexBuild.Map.compute : empty input list"
		else
            let dim = Index.Int.len (List.hd il) in
            Misc.rem_dupl Index.Int.equal il
            |> compute_rec dim

	let compute_list_from_map : IndexList.t -> t -> t
		= fun il map ->
		let map' = List.stable_sort Index.Int.compare il
		|> List.fold_left (fun map ind ->
			let (_,map') = compute_from_map ind map in map'
		) map in
		map'
end
