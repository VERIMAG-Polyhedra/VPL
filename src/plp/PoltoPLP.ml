module Cons = IneqSet.Cons
module Cert = Cons.Cert
module Cs = IneqSet.Cs
module Vec = IneqSet.Vec
module V = Cs.Vec.V

module Debug = Join.Debug

module type Type = sig

	val join : 'c1 Cert.t -> 'c2 Cert.t -> V.t option -> 'c1 Cons.t list -> 'c2 Cons.t list -> 'c1 Cons.t list * 'c2 Cons.t list
end

module MinFloat = Min.RatVec_Splx2(Vec)
module Join = struct
	include Join.Build(MinFloat)
	
	(*
	(* À priori inutile *)
	(** [computePoints x0 cstrs x'] takes the result of a minimization and the inner point on which it was done.
	It attaches to each constraint a new point within the polyhedron. *)
	let computePoints : Vec.t -> (Cs.t * Vec.t) list -> Vec.t -> (Cs.t * Vec.t) list
		= fun x0 cstrs x' ->
		let conv = MinFloat.vecInput_Vec in
		let convCs = MinFloat.float_of_cstr in
		List.map
			(fun (cstr,x) ->
			let (direction,value) = 
				let direction = MinFloat.TwoPoints (conv x, conv x0) in
				let value = MinFloat.Sort.value direction (convCs cstr) in
				(direction,value)
				(*if Vec.Coeff.well_formed_nonnull value
				then (direction,value)
				else begin
					let normal = MinFloat.Sort.normal cstr in 
					let direction = MinFloat.Normal(x0,normal) in
					let value = MinFloat.Sort.value direction cstr in
					(direction,value)
				end*)
			in
			(* intersection between [x,x0) and cstr: *)
			let x_inter = MinFloat.Sort.getPoint' (direction, value) in 
			let x_output = Vec.divc (Vec.add x_inter x') (Scalar.Rat.mk1 2) in
			(cstr, x_output) 
			)
			cstrs
	*)

	module BuildSx = struct
	
		let build_paramCoeff : Cs.Vec.V.t list -> Cs.t -> PLP.PSplx.ParamCoeff.t
			= fun params cstr ->
			let vec = List.map 
				(fun param ->
					 Cs.get_v cstr |> fun vec -> Cs.Vec.get vec param) 
				params
			in
			PLP.PSplx.ParamCoeff.mk vec (Cs.get_c cstr |> Cs.Coeff.neg)
	
		let objective : Cs.Vec.V.t list -> Cs.t list -> PLP.PSplx.Objective.t
			= fun params cstrs ->
			let null_paramCoeff = PLP.PSplx.ParamCoeff.mk (List.map (fun _ -> Scalar.Rat.z) params) Scalar.Rat.z in
			let coeffs = List.map (fun cstr -> build_paramCoeff params (Cs.mulc Cs.Coeff.negU cstr)) cstrs in
			PLP.PSplx.Objective.mk coeffs null_paramCoeff
		
		let build_norm_from_point : Vec.t -> Cs.t list -> Tableau.Vector.t
			= fun init_point cstrs ->
			List.map
				(fun cstr -> 
					Scalar.Rat.sub
						(Cs.Vec.dot_product 
							init_point 
							(Cs.get_v cstr))
						(Cs.get_c cstr)
				)
				cstrs
			@ [Scalar.Rat.negU] (* Normalization constant *)
	
		let build_params : PLP.Naming.t -> Cs.t list -> Vec.V.t list * PLP.Naming.t
			= fun names cstrs ->
			let params = Cs.getVars cstrs
				|> Vec.V.Set.elements
			in
			(params, PLP.Naming.mkParam params names)
			
		let build_vars : Cs.t list -> Vec.V.t list * PLP.Naming.t
			= fun cstrs ->
			let vars = List.mapi (fun i _ -> Vec.V.fromInt (i+1)) cstrs in
			(vars, PLP.Naming.mkVar vars PLP.Naming.empty)
		
		let build : Vec.t -> Cs.t list -> PLP.PSplx.t
			= fun init_point cstrs ->
			let (_, names) = build_vars cstrs in
			let (params, names) = build_params names cstrs in
			let obj = objective params cstrs 
			and mat = [build_norm_from_point init_point cstrs] in
			PLP.({PSplx.obj = obj ; PSplx.mat = mat ; PSplx.basis = [] ; PSplx.names = names})
	end

	type 'c regionsT = {
		mapping : (PLP.Region.t * 'c Cons.t) list;
		interior_point : Vec.t ;
	}
	
	let regions_to_string' : (PLP.Region.t * 'c Cons.t) list -> string
		= fun regs ->
		Printf.sprintf "\tRegions : \n%s"
			(Misc.list_to_string 
				(fun (reg,cons) -> Printf.sprintf "%s\n%s\n"
					(PLP.Region.to_string reg)
					(Cons.to_string Vec.V.to_string cons))
				regs "\n")
	
	let regions_to_string : 'c regionsT -> string
		= fun regs ->
		regions_to_string' regs.mapping

	let to_plp : 'c Cert.t -> Vec.t -> ('c Cons.t * Vec.t) list -> 'c regionsT
		= let build_map : 'c Cons.t list -> 'c PLP.mapVar_t
			= fun conss ->
			Misc.fold_left_i (fun i map cons -> PLP.MapV.add i cons map)
				PLP.MapV.empty
				conss
		in
		fun factory x0 conss_points ->
		(*Misc.list_to_string 
			(fun (c,v) -> Printf.sprintf "%s -> %s"
				(Cs.to_string Cs.Vec.V.to_string c)
				(Vec.to_string Vec.V.to_string v))
			cstrs " ; "
			|> print_endline;*)
		let (conss,points) = List.split conss_points in
		let config = PLP.std_config (*{PLP.std_config with
			PLP.points = (List.map (fun v -> PLP.ExplorationPoint.Point v) points);
			(*PLP.add_region = PLP.standard_test;*)
		} *)in
		let mapVar = build_map conss in
		let get_cert = PLP.get_cert_default factory mapVar in
		let sx = BuildSx.build x0 (List.map Cons.get_c conss) in
		match PLP.run config sx get_cert with
		| None -> Pervasives.failwith "PoltoPLP.to_plp"
		| Some regs -> {mapping = regs ; interior_point = x0}

	let minimize_and_plp : 'c Cert.t -> Vec.t -> 'c Cons.t list -> 'c regionsT
		= fun factory init_point conss ->
		(* TODO: ces étapes devraient être évitée, la minimization devrait pouvoir gérer des Cons.t directement*)
		MinFloat.minimize init_point (List.map Cons.get_c conss)
		|> List.map (fun (cstr,point) -> 
			let cons = List.find (fun cons -> Cs.equalSyn (Cons.get_c cons) cstr) conss in
			(cons,point)) 
		|> to_plp factory init_point

	module ReNormalize = struct
		
		(** Computes the denominantor of lambda from the new point (homogenized) and the polyhedron face. *)
		let compute_denominator : Vec.t -> Vec.t -> Vec.Coeff.t
			= fun polyhedron_face new_point ->
			Vec.dot_product polyhedron_face new_point
			|> Vec.Coeff.neg
		
		let compute_numerator : Vec.t -> Vec.t -> Vec.Coeff.t
			= fun vec_to_normalize new_point ->
			Vec.dot_product vec_to_normalize new_point
		
		let homogenize_point : Vec.t -> Vec.V.t -> Vec.t 
			= fun point additional_var ->
			Vec.set point additional_var Vec.Coeff.u
		
		let homogenize_cstr : Vec.V.t -> Cs.t -> Vec.t 
			= fun additional_var cstr ->
			[cstr.Cs.c , additional_var]
				|> Vec.mk 
				|> Vec.sub cstr.Cs.v
		
		let remove_additional_var : Vec.V.t -> Vec.t -> Vec.t
			= fun additional_var point ->
			Vec.set point additional_var Vec.Coeff.z
			(*
			Vec.get point additional_var
			|> Vec.divr point
			|> fun point -> Vec.set point additional_var Vec.Coeff.z
			*)
			(* TODO faut il diviser tous les coefficients par le coefficient de la variable additionnelle? *)
			
		let renormalize_vec : Vec.Coeff.t -> Vec.V.t -> Vec.t -> Vec.t -> Vec.t -> Vec.t
			= fun denominator additional_var new_point polyhedron_face vec ->
			let lambda = Vec.Coeff.div (compute_numerator vec new_point) denominator in
			Vec.add vec (Vec.mulc lambda polyhedron_face)
			
		let renormalize_boundary : Vec.Coeff.t -> Vec.V.t -> Vec.t -> Vec.t -> PLP.Boundary.t -> PLP.Boundary.t
			= fun denominator additional_var new_point polyhedron_face (cstr,point_other_side) ->
			let renormalize_vec = renormalize_vec denominator additional_var new_point polyhedron_face in
			let vec' = homogenize_cstr additional_var cstr
				|> renormalize_vec
			and point_other_side' = homogenize_point point_other_side additional_var 
				|> renormalize_vec
				|> remove_additional_var additional_var
			in
			let cstr' = Cs.mk2 (Cs.get_typ cstr) 
				(Vec.set vec' additional_var Vec.Coeff.z) 
				(Vec.get vec' additional_var |> Vec.Coeff.neg) in
			(cstr',point_other_side')
		
		let renormalize_region : Vec.V.t -> Vec.t -> (PLP.Region.t * 'c Cons.t) -> (PLP.Region.t * 'c Cons.t)
			= fun additional_var new_point (reg,cons) ->
			let polyhedron_face = Cons.get_c cons
				|> homogenize_cstr additional_var in
			let new_point = homogenize_point new_point additional_var in
			let denominator = compute_denominator polyhedron_face new_point in
			let renormalize_boundary = renormalize_boundary denominator additional_var new_point polyhedron_face in
			let boundaries = List.map
				(fun (boundary,i) -> renormalize_boundary boundary, i)
				reg.PLP.Region.r 
			in
			let point' = homogenize_point reg.PLP.Region.point additional_var 
				|> renormalize_vec denominator additional_var new_point polyhedron_face 
				|> remove_additional_var additional_var
			in
			({reg with PLP.Region.r = boundaries ; PLP.Region.point = point'},
			 cons)
		
		let renormalize : 'c regionsT -> Vec.t -> 'c regionsT
			= fun regs new_point ->
			let additional_var = List.map (fun (_,cons) -> Cons.get_c cons) regs.mapping
				|> Cs.getVars
				|> Cs.Vec.V.horizon
			in
			let mapping' = List.map (renormalize_region additional_var new_point) regs.mapping
			in
			{mapping = mapping' ; interior_point = new_point}
		
	end
	
	
	(** [keep factory next cons conss] returns either a {!val:Cons.t} that proves that [conss] is included in [cons], or None. *)
	let keep : 'a Cert.t -> Cs.Vec.V.t -> 'b Cons.t -> 'a Cons.t list -> 'a Cons.t option
		= fun factory next cons conss ->
		match IneqSet.incl factory next [] conss [cons] with
		| IneqSet.NoIncl -> None
		| IneqSet.Incl [cert] -> Some (Cons.get_c cons, cert)
		| _ -> Pervasives.failwith "PoltoPLP:discard_constraints:keep"
		
	(* int list -> list of discarded regions indices *)
	let discard_constraints : 'c1 Cert.t -> 'c2 Cert.t -> 'c1 regionsT -> 'c2 regionsT 
		-> ('c1 regionsT * 'c2 regionsT) * ('c1 Cons.t list * 'c2 Cons.t list) * PLP.ExplorationPoint.t list * int list
		= fun factory1 factory2 regs1 regs2 ->
		let conss1 = List.split regs1.mapping |> Pervasives.snd
		and conss2 = List.split regs2.mapping |> Pervasives.snd
		in
		let next = List.map Cons.get_c conss1 |> Cs.getVars
			|> Cs.Vec.V.Set.union (List.map Cons.get_c conss2 |> Cs.getVars) 
			|> Cs.Vec.V.horizon 
		in
		
		let (to_keep1, (to_keep_conss1_1,to_keep_conss1_2), to_throw1) = List.fold_left
			(fun (to_keep1, (to_keep_conss1_1,to_keep_conss1_2), to_throw1) (reg,cons) ->
				match keep factory2 next cons conss2 with
				| None -> (to_keep1, (to_keep_conss1_1,to_keep_conss1_2), reg :: to_throw1)
				| Some cons2 -> ((reg,cons) :: to_keep1, (cons::to_keep_conss1_1, cons2::to_keep_conss1_2), to_throw1))
			([],([],[]),[])
			regs1.mapping
		and (to_keep2, (to_keep_conss2_1,to_keep_conss2_2), to_throw2) = List.fold_left
			(fun (to_keep2, (to_keep_conss2_1,to_keep_conss2_2), to_throw2) (reg,cons) ->
				match keep factory1 next cons conss1 with
				| None -> (to_keep2, (to_keep_conss2_1,to_keep_conss2_2), reg :: to_throw2)
				| Some cons1 -> ((reg,cons) :: to_keep2, (cons1::to_keep_conss2_1, cons::to_keep_conss2_2), to_throw2))
			([],([],[]),[])
			regs2.mapping
		in
		let (explorationPoints, id_list) = List.map 
			(fun reg -> (PLP.ExplorationPoint.Point reg.PLP.Region.point, reg.PLP.Region.id))
			(to_throw1 @ to_throw2)
			|> List.split
		in
		(({regs1 with mapping = to_keep1}, {regs2 with mapping = to_keep2}),
		 (to_keep_conss1_1 @ to_keep_conss2_1, to_keep_conss1_2 @ to_keep_conss2_2),
		 explorationPoints,
		 id_list)
		
	(*
	let explorationPointsFromCstrs : (Cs.t * Vec.t) list -> Cs.t list -> PLP.ExplorationPoint.t list
		= fun l cstrs ->
		List.map (fun cs -> PLP.ExplorationPoint.Point (List.assoc cs l)) cstrs
	*)
	
	let get_join_cert : 'c1 Cert.t -> 'c2 Cert.t ->  'c1 regionsT  -> 'c2 regionsT  
		-> (('c1,'c2) certT) PLP.mapVar_t -> (PLP.Region.t * (('c1,'c2) certT) Cons.t) list 
		-> 'c1 Cons.t list * 'c2 Cons.t list
		= let get_cert_p1 : 'c1 Cert.t -> (int * Q.t) list -> (('c1,'c2) certT) PLP.mapVar_t -> 'c1
			= fun factory1 basisValue map ->
			List.fold_left 
				(fun r_cert (col,q) -> try
					match PLP.MapV.find col map with
					| (c, C1 cert) -> factory1.Cert.add r_cert (factory1.Cert.mul q cert)
					| (_,_) -> Pervasives.failwith "Join.get_join_cert.get_cert_p1"
					with Not_found -> r_cert)
				factory1.Cert.top
				basisValue
		in
		let get_cert_p2 : 'c2 Cert.t -> (int * Q.t) list -> (('c1,'c2) certT) PLP.mapVar_t -> 'c2
			= fun factory2 basisValue map ->
			List.fold_left 
				(fun r_cert (col,q) -> try
					match PLP.MapV.find col map with
					| (c, C2 cert) -> factory2.Cert.add r_cert (factory2.Cert.mul q cert)
					| (_,_) -> Pervasives.failwith "Join.get_join_cert.get_cert_p1"
					with Not_found -> r_cert)
				factory2.Cert.top
				basisValue
		in
		let is_strict : (int * Q.t) list -> (('c1,'c2) certT) PLP.mapVar_t -> bool
			= fun basisValue map ->
			List.exists
				(fun (col,_) -> try
					let (c,_) = PLP.MapV.find col map in
					Cs.get_typ c = Cstr.Lt
					with Not_found -> false)
				basisValue
		in
		fun factory1 factory2 p1 p2 map sols ->
		let p1 = p1.mapping
		and p2 = p2.mapping in
		(* Colonnes correspondant au premier polyèdre*)
		let p1_col_min = 0 and p1_col_max = List.length p1
		(* Colonnes correspondant au second polyèdre*)
		and p2_col_min = (List.length p1) + 1 and p2_col_max = (List.length p1) + 1 + (List.length p2)
		in
		List.map
			(fun (reg, cons) ->
					let cstr = Cons.get_c cons in
					match reg.PLP.Region.sx with
					| None -> Pervasives.failwith "Join.get_join_cert"
					| Some sx ->
						let basisValue = PSplx.getCurVal sx in 
						let arg1 = List.filter (fun (i,q) -> p1_col_min <= i && i <= p1_col_max && Scalar.Rat.cmpz q < 0) basisValue
						and arg2 = List.filter (fun (i,q) -> p2_col_min <= i && i <= p2_col_max && Scalar.Rat.cmpz q < 0) basisValue
						in
						if is_strict arg1 map && is_strict arg2 map
						then let cstr' = {cstr with Cs.typ = Cstr.Lt} in
							((cstr', get_cert_p1 factory1 arg1 map),
						 	 (cstr', get_cert_p2 factory2 arg2 map))
						else (* TODO: dans ce cas, il faut élargir les deux certificats*)
							((cstr, get_cert_p1 factory1 arg1 map |> factory1.Cert.to_le),
						 	 (cstr, get_cert_p2 factory2 arg2 map |> factory2.Cert.to_le)))
			sols
		|> List.split
			
	let make_certs : 'c1 Cert.t -> 'c2 Cert.t -> 'c1 regionsT -> 'c2 regionsT -> 'c1 regionsT -> 'c2 regionsT
		-> (('c1,'c2) certT) PLP.mapVar_t -> (PLP.Region.t * (('c1,'c2) certT) Cons.t) list option 
		-> 'c1 Cons.t list * 'c2 Cons.t list
		= fun factory1 factory2 regs1 regs2 p1 p2 map -> 
		function 
		| None -> [],[]
		| Some sols ->
			let cstr_regs1 = List.map (fun (_,cons) -> Cons.get_c cons) regs1.mapping
			and cstr_regs2 = List.map (fun (_,cons) -> Cons.get_c cons) regs2.mapping
			in
			let sols' = List.filter
				(fun (_,cons) -> let cstr = Cons.get_c cons in
					not (List.exists (Cs.equalSyn cstr) cstr_regs1)
					&& not(List.exists (Cs.equalSyn cstr) cstr_regs2))
				sols
			in
			get_join_cert factory1 factory2 p1 p2 map sols'
	
	let init_regions : 'c1 regionsT -> 'c2 regionsT -> PLP.Region.t list
		= fun regs1 regs2 ->
		Debug.log DebugTypes.Normal
			(lazy (Printf.sprintf "Regions of initialization : \n%s\n%s"
			(regions_to_string regs1)
			(regions_to_string regs2)));
		let regs1' = List.split regs1.mapping |> Pervasives.fst
		and regs2' = List.split regs2.mapping |> Pervasives.fst
		in
		regs1' @ regs2'
	
	let update_regions : 'c1 regionsT -> 'c2 regionsT -> ('c1 regionsT * 'c2 regionsT)
		= fun regs1 regs2 ->
		let shift = List.length regs1.mapping -1 in
		let id_map = List.mapi (fun i (reg,_) -> (reg.PLP.Region.id, i + shift)) regs2.mapping in
		let regs2' = {regs2 with
			mapping = List.map
				(fun (reg,cons) -> let reg' = 
					{reg with 
					PLP.Region.id = List.assoc reg.PLP.Region.id id_map;
					PLP.Region.r = List.map
						(fun (b,id_opt) -> match id_opt with 
						| None -> (b,None)
						| Some id -> (b, Some (List.assoc id id_map)))
						reg.PLP.Region.r;
					}
					in
					(reg', cons))
				regs2.mapping
		} in
		(regs1, regs2')
	
	let exploration_points_from_regions : int list -> PLP.Region.t list -> PLP.ExplorationPoint.t list
		= fun id_list regs ->
		List.fold_left
			(fun res reg ->
				List.fold_left
					(fun res ((cstr,point),id_opt) ->
						match id_opt with
						| Some id -> if List.mem id id_list
							then res
							else PLP.ExplorationPoint.Direction (reg.PLP.Region.id, (cstr,point)) :: res
						| None -> 
							PLP.ExplorationPoint.Direction (reg.PLP.Region.id, (cstr,point)) :: res)
					res reg.PLP.Region.r)
			[] regs
	
	let update : int list -> 'c regionsT -> 'c regionsT 
		= fun id_list regs ->
		{regs with
		 mapping = List.map
			(fun (reg,cons) -> let reg' = 
				{reg with 
				PLP.Region.r = List.map
					(fun (b,id_opt) -> match id_opt with 
					| None -> (b,None)
					| Some id -> 
						if List.mem id id_list 
						then (b,None)
						else (b, id_opt))
					reg.PLP.Region.r;
				}
				in
				(reg', cons))
			regs.mapping
		}
		
	let update_frontiers : int list -> 'c1 regionsT -> 'c2 regionsT -> ('c1 regionsT * 'c2 regionsT)
		= fun id_list regs1 regs2 ->
		(update id_list regs1, update id_list regs2)
	
	(** Both polyhedra are assumed to be normalized on the same point *)
	let join' : 'c1 Cert.t -> 'c2 Cert.t -> Vec.V.t option -> 'c1 regionsT -> 'c2 regionsT -> 'c1 Cons.t list * 'c2 Cons.t list
		= fun factory1 factory2 epsilon_opt p1 p2 ->
		let conss1 = List.split p1.mapping |> Pervasives.snd
		and conss2 = List.split p2.mapping |> Pervasives.snd
		in
		let (p1',p2') = update_regions p1 p2 in
		let init_point = p1'.interior_point in	
		let (sx,map) = build factory1 factory2 epsilon_opt init_point conss1 conss2 in
		let ((regs1, regs2), (certs1, certs2), explorationPoints, id_list) = discard_constraints factory1 factory2 p1' p2'
		in
		(*
		let cstrs_points1 = minimizeAndComputePoint init_point (List.map Cons.get_c conss1) 
		and cstrs_points2 = minimizeAndComputePoint init_point (List.map Cons.get_c conss2) 
		in
		let explorationPoints = explorationPointsFromCstrs cstrs_points1 cstrs_to_throw1 
			@ explorationPointsFromCstrs cstrs_points2 cstrs_to_throw2 
		in
		*)
		let (regs1_updated,regs2_updated) = update_frontiers id_list regs1 regs2 in
		let init_regions = init_regions regs1_updated regs2_updated in
		let explorationPoints' = exploration_points_from_regions id_list init_regions in
		let config = {PLP.std_config with
			 PLP.points = explorationPoints @ explorationPoints';
			 PLP.regions = init_regions; 
			(*PLP.add_region = PLP.standard_test;*)
		} in
		let new_regs = PLP.run config sx (get_no_cert factory1 factory2) in
		let regs_filtered = filter_trivial new_regs |> rem_dupl in
		begin
			match regs_filtered with
			| None -> ()
			| Some regs_filtered ->
				Printf.sprintf "regs1 = \n%s\n\nregs2 = \n%s\n\nnew_regs = \n%s\n"
				(regions_to_string regs1)
				(regions_to_string regs2)
				(regions_to_string' regs_filtered)
				|> print_endline
		end;
		let (conss1', conss2') = make_certs factory1 factory2 regs1 regs2 p1 p2 map regs_filtered in
		(conss1' @ certs1,  conss2' @ certs2)
		
	(** Returns the convex hull of the given inequalities (no equality should be given).
		Computes the region partitioning of both polyhedra. *)
	let join : 'c1 Cert.t -> 'c2 Cert.t -> V.t option -> 'c1 Cons.t list -> 'c2 Cons.t list -> 'c1 Cons.t list * 'c2 Cons.t list
		= fun factory1 factory2 epsilon_opt p1 p2 ->
		Debug.log DebugTypes.Title 
			(lazy "Convex hull by Region Partitioning");
		Debug.log DebugTypes.MInput
			(lazy (Printf.sprintf "First polyhedron : %s\nSecond Polyhedron : %s"
				(Misc.list_to_string (Cons.to_string Cs.Vec.V.to_string) p1 "\n")
				(Misc.list_to_string (Cons.to_string Cs.Vec.V.to_string) p2 "\n")));
		let ineqs1 = List.map (fun (cstr,_) -> cstr) p1 
		and ineqs2 = List.map (fun (cstr,_) -> cstr) p2 in
		let init_point1 = match Opt.getAsg_raw ineqs1 with
			| Some x -> Vector.Symbolic.Positive.toRat x
			| None -> Pervasives.failwith "join: empty polyhedron p1"
		and init_point2 = match Opt.getAsg_raw ineqs2 with
			| Some x -> Vector.Symbolic.Positive.toRat x
			| None -> Pervasives.failwith "join: empty polyhedron p2"
		in
		let regs1 = minimize_and_plp factory1 init_point1 p1
		and regs2 = minimize_and_plp factory2 init_point2 p2 
		in
		let regs2 = ReNormalize.renormalize regs2 init_point1 in
		let (conss1, conss2) = join' factory1 factory2 epsilon_opt regs1 regs2 in
		Debug.log DebugTypes.MOutput
			(lazy (Printf.sprintf "Polyhedron1 : %s\nPolyhedron2 : %s"
				(Misc.list_to_string (Cons.to_string_ext factory1 Cs.Vec.V.to_string) conss1 "\n")
				(Misc.list_to_string (Cons.to_string_ext factory2 Cs.Vec.V.to_string) conss2 "\n")));
		(conss1, conss2)
end
