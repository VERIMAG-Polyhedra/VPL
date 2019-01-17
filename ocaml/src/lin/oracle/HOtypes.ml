module CP = CstrPoly
module Poly = CP.Poly

module Debug = Hi.Debug

module MapP = Map.Make(Poly)

module MapPolyHi = struct

	type t = Hi.t list MapP.t

	let to_string : t -> string
		= let to_string2 : Poly.t * Hi.t list -> string
			= fun (p,hilist) ->
			Printf.sprintf "%s -> %s"
				(Poly.to_string p)
				(Misc.list_to_string Hi.to_string hilist " ; ")
        in
		fun map ->
		String.concat "\n" (List.map (fun x -> to_string2 x) (MapP.bindings map))

	let memMonom : Poly.MonomialBasis.t -> t -> bool
		= fun m map ->
		MapP.exists (fun p _ ->
            List.exists (fun (m',_) ->
                Poly.MonomialBasis.compare m m' = 0
            ) p
        ) map

	let memMonomSigned : Poly.Monomial.t -> t -> bool
		= let same_sign : Q.t -> Q.t -> bool
			= fun a b ->
			(Q.geq a Q.zero && Q.geq b Q.zero)
            || (Q.leq a Q.zero && Q.leq b Q.zero)
        in
		fun (mb,c) map ->
		MapP.exists (fun p _ ->
            List.exists (fun (mb',c') ->
                same_sign c c'
                && Poly.MonomialBasis.compare mb mb' = 0
            ) p
		) map

	let merge : t -> t -> t
		= fun map1 map2 ->
		MapP.merge (fun _ a_opt b_opt ->
				match (a_opt,b_opt) with
				| (Some l1, Some l2) -> Some (Misc.rem_dupl Hi.eq (l1 @ l2))
				| (Some l1, None) -> Some l1
				| (None, Some l2) -> Some l2
				| (None,None) -> None)
			map1 map2

end

module MapI = IndexBuild.MapI

module MapIndexP
	= struct

	type t = Poly.t MapI.t

	let of_polyhedron : Poly.t list -> t
		= fun cl ->
        let id = Index.Int.init (List.length cl) in
        Misc.fold_left_i (fun i map p ->
            let id' = Index.Int.set id i 1 in
            MapI.add id' p map
        ) MapI.empty cl

	let to_string : t -> string
		= let to_string2 : Index.Int.t * Poly.t -> string
			= fun (i,p) ->
			Printf.sprintf "%s -> %s" (Index.Int.to_string i) (Poly.to_string p) in
		fun map ->
		String.concat "\n" (List.map (fun x -> to_string2 x) (MapI.bindings map))

	let poly_to_deg_max : Poly.t -> Var.t list -> Index.Int.t
		= fun p vl ->
		List.map (fun var ->
            Poly.get_max_exponent var p
        ) vl
		|> Index.Int.mk

	(** special case of {!function get} when the index has only one non null coefficient. *)
	let get_one_coeff_nn : Index.Int.t -> t -> IndexBuild.Map.t -> (Poly.t * t * IndexBuild.Map.t)
		= fun id mapIP mapI ->
		let i = Index.Int.first_positive id in
		let p = MapI.find (Index.Int.unitary i (Index.Int.len id)) mapIP in
		let p' = Poly.pow p (Index.Int.get id i) in
		(p', MapI.add id p' mapIP, mapI)

	let rec get : Index.Int.t -> t -> IndexBuild.Map.t -> (Poly.t * t * IndexBuild.Map.t)
		= fun id mapIP mapI ->
		try (MapI.find id mapIP, mapIP, mapI)
		with Not_found ->
			if Index.Int.one_coeff_nn id
			then get_one_coeff_nn id mapIP mapI
			else
				let (il,mapI') = try (IndexBuild.MapI.find id mapI, mapI)
				with Not_found ->
					IndexBuild.Map.compute_from_map id mapI
				in
				let (p',mapIP',mapI') = List.fold_left
					(fun (p,mapIP,mapI) i ->
						let (p',mapIP',mapI') = get i mapIP mapI in
						(Poly.mul p p', MapI.add i p' mapIP', mapI')
					)
					(Poly.u, mapIP, mapI')
					il
				in
				(p', MapI.add id p' mapIP', mapI')
end

module MapV = Map.Make(struct type t = Var.t let compare = Var.cmp end)

module LPMaps = struct

	type t = Upper | Lower
	type mapDetBound = (bool option * bool option) MapV.t
	type mapBound = (Hi.boundIndex option * Hi.boundIndex option)  MapV.t
	type bounds = {mapDB : mapDetBound ; mapB : mapBound}

	let (init : Poly.t list -> Var.t list -> bounds)
		= let (updateMapDB_left : mapDetBound -> Var.t -> mapDetBound)
			= fun mapDB v ->
				let res = try let (_,value) = MapV.find v mapDB in value
				with Not_found -> None in
				MapV.add v (Some true, res) mapDB
			in

			let (updateMapB_left : mapBound -> Var.t -> int -> int -> mapBound)
			= fun mapB v i len ->
				let res = try let (_,value) = MapV.find v mapB in value
				with Not_found -> None in
				let id = Index.Rat.unitary i len in
				MapV.add v (Some id, res) mapB
			in

			let (updateMapDB_right : mapDetBound -> Var.t -> mapDetBound)
			= fun mapDB v ->
				let res = try let (value,_) = MapV.find v mapDB in value
				with Not_found -> None in
				MapV.add v (res, Some true) mapDB
			in

			let (updateMapB_right : mapBound -> Var.t -> int -> int -> mapBound)
			= fun mapB v i len ->
				let res = try let (value,_) = MapV.find v mapB in value
				with Not_found -> None in
				let id = Index.Rat.unitary i len in
				MapV.add v (res, Some id) mapB
		in
		fun polyhedron vars ->
		Debug.log DebugTypes.Detail
			(lazy("LP.initMapDB, poly = " ^ (Misc.list_to_string Poly.to_string polyhedron " ; ")));
		let n = List.length polyhedron in
		let module MB = Poly.MonomialBasis in
		let (mapDB,mapB) =
		List.fold_left
			(fun (mapDB, mapB) i ->
				match List.nth polyhedron i
					|> fun p -> (List.map Poly.Monomial.data (Poly.data p))
				with
				| [(m,c)] when Q.leq Q.zero c && (MB.to_list_expanded m |> List.length = 1) ->
					let v = List.hd (MB.to_list_expanded m) in
					(updateMapDB_left mapDB v, updateMapB_left mapB v i n)
				| (m0,_) :: [(m,c)] when Q.leq Q.zero c && (MB.to_list_expanded m |> List.length = 1)
					&& MB.equal m0 MB.null ->
					let v = List.hd (MB.to_list_expanded m) in
					(updateMapDB_left mapDB v, updateMapB_left mapB v i n)
				| [(m,c)] when Q.lt c Q.zero && (MB.to_list_expanded m |> List.length = 1) ->
					let v = List.hd (MB.to_list_expanded m) in
					(updateMapDB_right mapDB v, updateMapB_right mapB v i n)
				| (m0,_) :: [(m,c)] when Q.lt c Q.zero && (MB.to_list_expanded m |> List.length = 1)
					&& MB.equal m0 MB.null ->
					let v = List.hd (MB.to_list_expanded m) in
					(updateMapDB_right mapDB v, updateMapB_right mapB v i n)
				| _ -> (mapDB, mapB))
			(MapV.empty, MapV.empty)
			(Misc.range 0 n)
		|> fun (mapDB,mapB) -> List.fold_left
			(fun (mapDB',mapB') v -> let mapDB' = if MapV.mem v mapDB' then mapDB' else MapV.add v (None,None) mapDB' in
				let mapB' =  if MapV.mem v mapB' then mapB' else MapV.add v (None,None) mapB' in
				(mapDB',mapB'))
			(mapDB,mapB)
			vars
		in
		{mapDB = mapDB ; mapB = mapB}

	let (hasSup : Var.t -> mapDetBound -> Q.t)
		= fun v mapDB ->
		match MapV.find v mapDB with
		| (_,Some false) -> Q.zero
		| (_,_) -> Q.one

	let (hasInf : Var.t -> mapDetBound -> Q.t)
		= fun v mapDB ->
		match MapV.find v mapDB with
		| (Some false,_) -> Q.zero
		| (_,_) -> Q.one

	let (detSup : Var.t -> mapDetBound -> Q.t)
		= fun v mapDB ->
		match MapV.find v mapDB with
		| (_,Some true) -> Q.one
		| (_,_) -> Q.zero

	let (detInf : Var.t -> mapDetBound -> Q.t)
		= fun v mapDB ->
		match MapV.find v mapDB with
		| (Some true,_) -> Q.one
		| (_,_) -> Q.zero

	let (mapDB_to_string : mapDetBound -> string)
		= fun mapDB ->
		Misc.list_to_string (fun (v,(i,s)) -> Printf.sprintf "%s -> (%s,%s)"
			(Var.to_string v)
			(match i with Some b -> string_of_bool b | None -> "None")
			(match s with Some b -> string_of_bool b | None -> "None"))
			(MapV.bindings mapDB) " ; "

	let (mapB_to_string : mapBound -> string)
		= fun mapB ->
		Misc.list_to_string (fun (v,(i,s)) -> Printf.sprintf "%s -> (%s,%s)"
			(Var.to_string v)
			(match i with Some b -> Index.Rat.to_string b | None -> "None")
			(match s with Some b -> Index.Rat.to_string b | None -> "None"))
			(MapV.bindings mapB) " ; "

	let (updateMapDB : mapDetBound -> Var.t -> bool -> t -> mapDetBound)
		= fun mapDB v value bound ->
		try let (b1,b2) = MapV.find v mapDB in
			MapV.add v (if bound = Upper then (b1, Some value) else (Some value, b2)) mapDB
		with Not_found -> MapV.add v (if bound = Upper then (None, Some value) else (Some value, None)) mapDB

	let (updateMapB : mapBound -> Var.t -> Hi.boundIndex -> t -> mapBound)
		= fun mapB v bI bound ->
		try let (b1,b2) = MapV.find v mapB in
			MapV.add v (if bound = Upper then (b1, Some bI) else (Some bI, b2)) mapB
		with Not_found -> MapV.add v (if bound = Upper then (None, Some bI) else (Some bI, None)) mapB
end

module Pneuma
	= struct

	type t = {
	p : Poly.t;
	vl : Var.t list;
	mapP : MapPolyHi.t;
	mapIP : MapIndexP.t;
	mapI : IndexBuild.Map.t;
	ph : Poly.t list;
	sx : Splx.t;
	lp : LPMaps.bounds}

	let (to_string : t -> string)
		= fun pn ->
		Printf.sprintf "\n\tPolynomial : %s\n\tVariables : %s\n\tMap Poly -> Index list :\n%s \n\tMap Index -> Poly :\n%s\n\tMap Index -> Index list : \n%s"
		(Poly.to_string pn.p)
		(Poly.MonomialBasis.to_string (Poly.MonomialBasis.mk_expanded pn.vl))
		(MapPolyHi.to_string pn.mapP |> Misc.add_tab 2)
		(MapIndexP.to_string pn.mapIP |> Misc.add_tab 2)
		(IndexBuild.Map.to_string pn.mapI |> Misc.add_tab 2)

	(* l'oracle traite les polynômes sous la forme p >= 0 *)
	let neg_poly : CstrPoly.t -> Poly.t list
		= fun cp ->
		let p = cp.CstrPoly.p in
		match cp.CstrPoly.typ with
		| Cstr_type.Le -> [Poly.neg p]
		| Cstr_type.Lt -> [Poly.neg p]
		| Cstr_type.Eq -> p :: [Poly.neg p]

	(* On initialise uniquement avec les inégalités du polyèdre.
	Les polynômes à linéariser ont été réécris pour ne plus parler des variables définies par des égalités. *)
	let (init : Poly.t -> 'c HPol.t -> t)
		= fun p ph ->
		let cl = List.fold_left
			(fun res cp -> res @ (neg_poly cp))
			[] ph#get_noneq_poly in
		let inequalities = ph#get_ineqs() in
		let sx = Splx.mk
			(Var.next (Var.max ph#get_vars))
			(List.mapi (fun i cstr -> (i,cstr)) inequalities)
		in
		let p' = Poly.neg p in
		match sx with
		| Splx.IsOk sx ->
			{p=p' ;
			 vl=ph#get_vars ;
			 mapIP = MapIndexP.of_polyhedron cl ;
			 mapP = MapP.empty ;
			 mapI = IndexBuild.MapI.empty ;
			 ph = cl ;
			 sx = sx ;
			 lp = LPMaps.init cl ph#get_vars}
		| Splx.IsUnsat _ -> Pervasives.failwith "HOtypes.Pneuma.init : polyhedron empty"

	let (n_cstrs : t -> int)
		= fun pn ->
		List.length pn.ph

	let (computeVarIndex : Index.Int.t -> Var.t list -> Poly.t)
		= fun id vl ->
		let varlist = List.fold_left2
			(fun r i v -> r @ (List.map (fun _ -> v) (Misc.range 0 i)))
			[]
			(Index.Int.data id) vl
		in
		(Poly.mk_expanded_list [varlist, Q.of_int 1])

	let (computeBoundIndex : Index.Rat.t -> Poly.t list -> Poly.t)
		= fun id polyhedron ->
		List.fold_left2
			(fun p ci q-> Poly.add p (Poly.mul (Poly.cste q) ci))
			Poly.z polyhedron (Index.Rat.data id)

	let (computeBoundIndexList : Index.Rat.t list -> Poly.t list -> Poly.t)
		= fun il polyhedron ->
		Poly.prod (List.map (fun i -> computeBoundIndex i polyhedron) il)

	let (hi_to_poly : Hi.t -> t -> Poly.t * MapIndexP.t * IndexBuild.Map.t)
		= fun hi pn ->
		match hi with
		| Hi.Ci i -> MapIndexP.get i pn.mapIP pn.mapI
		| Hi.VarCi (j,i) -> let (pi,mapIP',mapI') = MapIndexP.get i pn.mapIP pn.mapI in
			(Poly.mul pi (computeVarIndex j pn.vl), mapIP', mapI')
		| Hi.VarBounds (j,b) -> (Poly.mul (computeBoundIndexList b pn.ph) (computeVarIndex j pn.vl), pn.mapIP, pn.mapI)
end
