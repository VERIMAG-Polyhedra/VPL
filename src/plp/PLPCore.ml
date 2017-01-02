module Cs = Cstr.Rat.Positive
module EqSet = EqSet.EqSet(Cs)
module Cons = EqSet.Cons

module Debug = DebugTypes.Debug(struct let name = "PLPCore" end)

module Stat = struct
	let nb_regions : int ref = ref 0
	
	let incr_nb_regions : unit -> unit
		= fun () ->
		nb_regions := !nb_regions + 1
	
	(** (nb_redundancies, nb_true_bounds) *)
	let ratio_redundancy : (int * int) ref = ref (0,0) 
	
	let incr_redundancy : int -> int -> unit
		= fun i j ->
		ratio_redundancy := 
			((Pervasives.fst !ratio_redundancy) + i,
			 (Pervasives.snd !ratio_redundancy) + j)
			 
	(** (nb_subdivisions, nb_regions) *)
	let ratio_subdivisions : (int * int) ref = ref (0,0)
	
	let incr_subdivisions : int -> int -> unit
		= fun i j ->
		ratio_subdivisions := 
			((Pervasives.fst !ratio_subdivisions) + i,
			 (Pervasives.snd !ratio_subdivisions) + j)
	
	let nb_corrections : int ref = ref 0
	
	let incr_nb_corrections : unit -> unit
		= fun () ->
		nb_corrections := !nb_corrections + 1
	
	let reset : unit -> unit
		= fun () ->
		begin
		nb_regions := 0;
		ratio_redundancy := (0,0);
		nb_corrections := 0;
		ratio_subdivisions := (0,0);
		end
	
	let to_string : unit -> string
		= fun () ->
		Printf.sprintf ("PLP stats : \n\tnb_regions = %i
			\n\tnumber of redundancies (in region representations) = %i
			\n\tnumber of true boundaries = %i
			\n\t -> redundancy ratio = %f
			\n\tnumber of corrections = %i
			\n\ratio of subdivisions = %f")
			!nb_regions
			(Pervasives.fst !ratio_redundancy)
			(Pervasives.snd !ratio_redundancy)
			((Pervasives.fst !ratio_redundancy |> float_of_int) /. (Pervasives.snd !ratio_redundancy |> float_of_int))
			!nb_corrections
			((Pervasives.fst !ratio_subdivisions |> float_of_int) /. (Pervasives.snd !ratio_subdivisions |> float_of_int))
end
	
module PLP(Minimization : Min.Type) = struct
	module Vec = Minimization.Vec
	module PSplx_ = PSplx.PSplx(Cs)
	module PSplx = PSplx_.PSplx(Vec)
	
	module Explore = PSplx.Explore
	
	module Objective = PSplx.Objective
	module ParamCoeff = PSplx.ParamCoeff
	module Pivot = PSplx.Pivot
	module Naming = PSplx.Naming
	
	module V = Vec.V
	module Coeff = Vec.Coeff

	module MapV = Map.Make(struct type t = int let compare = Pervasives.compare end)
	
	(** This map is used for the certificate generation.
	Is associates to each simplex column an element of {!type:Cons.t}. *)
	type 'c mapVar_t = ('c Cons.t) MapV.t
	
	type region_t = Cone | NCone
	
	module Boundary = struct
		type t = Cs.t * Vec.t
		
		let equal : t -> t -> bool
			= fun (c1,_) (c2,_) ->
			Cs.equalSyn c1 c2
		
		let get_cstr : t -> Cs.t
			= fun (c,_) ->
			c
			
		let get_cstrs : t list -> Cs.t list
			= fun bs ->
			List.map get_cstr bs
			
		let to_string : t -> string
			= fun (c,p) -> 
			"Boundary : " ^ (Cs.to_string V.to_string c) 
			^ " - point_other_side : " ^ (Vec.to_string V.to_string p)
	end

	let (strict_comp : Cs.t -> Cs.t)
		= fun cstr ->
		let cstr_comp = Cs.compl cstr in
		(match Cs.get_typ cstr_comp with
			| Cstr.Le -> {cstr_comp with Cs.typ = Cstr.Lt}
			| _ -> cstr_comp)
	
	let (cstr_eq : Cs.t -> Cs.t)
		= fun cstr ->
		{cstr with Cs.typ = Cstr.Eq}
			
	let is_trivial : Cs.t -> bool
			= fun c ->
			(Cs.Vec.equal (Cs.get_v c) Cs.Vec.nil)
			
	module Region = struct
	
		type t = {
			id : int;
			r : (Boundary.t * int option) list;
			point : Vec.t; (* Un point dans la région *)
			sx : PSplx.t option (* Tableau de simplexe dont l'objectif a donné cette région *)
		}
	
		let mk : int -> Boundary.t list -> Vec.t -> PSplx.t -> t
			= fun id bounds point sx ->
			{id = id;
			 r = List.map (fun b -> (b,None)) bounds;
			 point = point;
			 sx = Some sx 
			}
		
		let get_cstrs : t -> Cs.t list
			= fun reg ->
			List.map (fun (b,_) -> Boundary.get_cstr b) reg.r
		
		let to_string : t -> string
			= fun r ->
			let cstrs = get_cstrs r in
			let vars = Cs.getVars cstrs |> V.Set.elements in
			(Printf.sprintf "Region %i : \n\tBoundaries : %s\n\t Point : %s\n\n\tPlot : %s"
				r.id
				(Misc.list_to_string 
					(fun (b,i) -> Printf.sprintf "%s -> %s" 
						(Boundary.to_string b)
						(match i with None -> "unknown" | Some i -> string_of_int i))
					r.r "\n\t\t")
				(Vec.to_string V.to_string r.point)
				("P = Polyhedron(ieqs = " ^ (Misc.list_to_string 
					(fun c -> Cs.plot vars c)
					cstrs ",") ^ ")"))
			^
			(if Debug.is_enabled DebugTypes.Detail
			then Printf.sprintf "\n\tTableau : \n%s" (match r.sx with None -> "None" | Some sx -> PSplx.to_string sx)
			else "")
		
		let extract : PSplx.t -> Cs.t list
		  	= fun sx ->
		  	Objective.foldi
			 	(fun i c l ->
			  		(ParamCoeff.to_cstr
					(Naming.to_vpl sx.PSplx.names)
					ParamCoeff.GT0 c)
			  		:: l)
			 	sx.PSplx.obj
			 	[]
			(* XXX: ça serait bien d'avoir une forme canonique des contraintes pour pourvoir remplacer equal par equalSyn*)
		  	|> Misc.rem_dupl (fun c c' -> Cs.equal c c') 
		  	|> List.filter (fun c -> is_trivial c |> not) 
		
		(** Returns a point in the regions' interior. The chosen point is a 1 from each boundary if possible. *)
		let getPointInside : region_t -> Cs.Vec.V.t -> Cs.t list -> Vec.t option
			= let getPointInside_cone : Cs.Vec.V.t -> Cs.t list -> Vec.t option
				= fun horizon cstrs -> 
				match Splx.getPointInside_cone horizon cstrs with
				| None -> None
				| Some pl -> Some (Vec.M.map Vec.ofSymbolic pl)
			and getPointInside_not_cone : Cs.Vec.V.t -> Cs.t list -> Vec.t option
				= fun horizon cstrs -> 
				let cstrs' = List.mapi
					(fun i cstr -> i, cstr)
					cstrs
				in
				match Opt.getAsg horizon cstrs' with
				| None -> None
				| Some pl -> Some (Vec.M.map Vec.ofSymbolic pl)
			in
			function
			| Cone -> getPointInside_cone
			| NCone -> getPointInside_not_cone
			
		(** Returns true if the given point lies in the given region. *)
		let (contains : t -> Vec.t -> bool)
			= fun reg p ->
			List.for_all 
				(fun ((c,_),_) -> Cs.satisfy (Vec.toRat p) c)
				reg.r
				
		(** Returns true if the given point lies in the given region. *)			
		let (contains' : Cs.t list -> Vec.t -> bool)
			= fun cstrs p ->
			List.for_all 
				(fun c -> Cs.satisfy (Vec.toRat p) c)
				cstrs
		
		(** Returns true if the given point lies in the given relaxed ({i i.e.} non-strict) region. *)
		let (contains_large : t -> Vec.t -> bool)
			= fun reg p ->
			List.for_all 
				(fun ((c,_),_) -> Cs.satisfy (Vec.toRat p) {c with Cs.typ = Cstr.Le})
				reg.r
				
		(** Returns true if the given point lies in the given relaxed ({i i.e.} non-strict) region. *)			
		let (contains'_large : Cs.t list -> Vec.t -> bool)
			= fun cstrs p ->
			List.for_all 
				(fun c -> Cs.satisfy (Vec.toRat p) {c with Cs.typ = Cstr.Le})
				cstrs
				
		let (equalSyn : t -> t -> bool)
			= fun reg1 reg2 ->
			List.for_all2 Cs.equalSyn (get_cstrs reg1) (get_cstrs reg2)
		
		let (equal : t -> t -> bool)
			= fun reg1 reg2 ->
			Misc.list_eq (fun c cstrs -> List.exists (fun c' -> Cs.equal c c') cstrs)
				(get_cstrs reg1) (get_cstrs reg2)
	end
	
	type mapRegs_t = Region.t MapV.t
	
	(*let next_id : int ref = ref 0*)
	
	module ExplorationPoint = struct
	
		type t = 
			| Direction of int * Boundary.t (** (id of origin region, boundary*)
			| Point of Vec.t
		
		let to_string : t -> string
			= function
			| Direction (id,(c,x)) -> 
				Printf.sprintf "(%s, %s, %s)"
				(string_of_int id)
				(Cs.to_string Cs.Vec.V.to_string c)
				(Vec.to_string Vec.V.to_string x)
			| Point v ->
				Vec.to_string Vec.V.to_string v
				
		let equal : t -> t -> bool
			= fun p1 p2 ->
			match (p1,p2) with
			| Direction(id1, (cstr1, pointToExplore1)), Direction (id2, (cstr2, pointToExplore2)) -> 
					id1 = id2
					&& Cs.equalSyn cstr1 cstr2
					&& Vec.equal pointToExplore1 pointToExplore2
			| Point v1, Point v2 -> Vec.equal v1 v2
			| _,_ -> false
	end
	
	type t = {
		regs : mapRegs_t;
		todo: ExplorationPoint.t list 
	}	
	
	let empty : t
		= {regs = MapV.empty ; todo = []}
	
	let to_string : t -> string
		= fun plp ->
		let regs = MapV.bindings plp.regs 
			|> List.split
			|> Pervasives.snd
		in
		Printf.sprintf "Regions = %s\nTodo list : \n%s"
			(Misc.list_to_string Region.to_string regs "\n")
			(Misc.list_to_string ExplorationPoint.to_string plp.todo " ; ")
	
	(** Module that checks results on the fly. 
		It is based on {!module:Debug.Check}, thus these verifications cost almost nothing if checking is disabled. *)		
	module Check = struct
	
		let satisfy : Vec.t -> Cs.t -> bool
				= fun v c ->
				let v' = Vec.toRat v in
				Cs.satisfy v' c
		
		let check_boundary : Boundary.t -> unit
			= fun (cstr,point) ->
			Debug.Check.check 
				(lazy (not (satisfy point cstr)))
				(lazy(Printf.sprintf "Point %s satisfies its boundary %s"
					(Vec.to_string Vec.V.to_string point)
					(Cs.to_string Cs.Vec.V.to_string cstr)))
				
		let check_region : Region.t -> unit
			= fun r ->
			begin
				Debug.Check.check (lazy(Region.contains r r.Region.point)) 
					(lazy(Printf.sprintf "point not in region %s"
					(Region.to_string r)));
				List.iter (fun (b,_) -> check_boundary b) r.Region.r
			end
						
		let check_result : t -> unit
			= fun plp ->
			MapV.iter (fun _ -> check_region) plp.regs
	end
	
	(** Module that chooses the next point to explore by raytracing. *)
	module RayTracing = struct
		
		module Min = Min.Min2(Vec)(Vec)(Cs)(MinLP.Glpk(Cs))
	
		let conv : Min.conversion
			= {Min.vec_CsVec = (fun x -> Vec.toRat x);
				csVec_Vec  = (fun x -> Vec.ofRat x) ;
				csCoeff_Coeff = (fun x -> Vec.Coeff.ofQ x) ;
				csInput_Cs = (fun x -> x) ;
				vecInput_Vec = (fun x -> x) ;
				vec_VecInput = (fun x -> x)
			}
		
		let eval : (int * Region.t) list -> Min.direction_type -> ((int * Cs.t) * Min.direction) list
			= fun regs dir_type ->
			List.fold_left  
				(fun res (id,reg) ->
					List.fold_left 
					(fun res ((cstr',_),_) ->
						let v = Min.Sort.value dir_type cstr' in
						((id, cstr'), (dir_type, v)) :: res)
					res reg.Region.r)
				[] regs
		
		(*
		(** Removes the initial region [reg] (identified by [id]) and those that are on the same side of [cstr] that [reg]. *)
		let filter_regions : int -> Cs.t -> mapRegs_t -> (int * Region.t) list
			= fun id cstr regMap ->
			MapV.fold
				(fun id' reg res ->
					if id = id' || (List.exists (fun ((cstr',_),_) -> Cs.equal cstr' cstr) reg.Region.r)
					then res
					else (id',reg) :: res )
				regMap
				[]
		*)
		(** Removes the initial region [reg] (identified by [id]) and those that are on the same side of [cstr] that [reg]. 
			TODO: use LP.*)
		let filter_regions : int -> Cs.t -> mapRegs_t -> (int * Region.t) list
			= fun id cstr regMap ->
			MapV.fold
				(fun id' reg res ->
					if id = id' || (List.exists (fun ((cstr',_),_) -> Cs.equal cstr' cstr) reg.Region.r)
					then res
					else (id',reg) :: res )
				regMap
				[]
				
		(** Returns true if [reg1] and [reg2] are adjacent through [cstr1] and [cstr2].
		@param cstr1 must belong to [reg1]
		@param cstr2 must belong to [reg2] *)
		let adjacent_cstr : Region.t -> Region.t -> Cs.t -> Cs.t -> bool
			= fun reg1 reg2 cstr1 cstr2->
			
				let cstrs_reg1 = {cstr1 with Cs.typ = Cstr.Le}
					:: (Misc.popAll Cs.equalSyn (Region.get_cstrs reg1) cstr1)
				and cstrs_reg2 = {cstr2 with Cs.typ = Cstr.Le}
					:: (Misc.popAll Cs.equal (Region.get_cstrs reg2) cstr2)
				in
				let cstrs = cstrs_reg1 @ cstrs_reg2 in
				let horizon = Cs.getVars cstrs 
					|> Vec.V.horizon
				in
				let cstrs = List.mapi (fun i c -> (i,c)) cstrs in
				match Splx.mk horizon cstrs |> Splx.checkFromAdd with
				| Splx.IsUnsat _ -> false
				| Splx.IsOk _ -> true		
		
		(** returns [Some i] with [i] the index of the adjacent region if it exists, [None] otherwise. *)
		let adjacent : int -> Cs.t -> ((int * Cs.t) * Min.direction) list -> mapRegs_t -> int option
			= fun id_init cstr l regMap ->
			let reg = MapV.find id_init regMap in
			try
				let ((id',_),_) = List.find
					(fun ((id', cstr'), _) -> 
						id_init <> id' 
					  && 
					  	(adjacent_cstr reg (MapV.find id' regMap) cstr cstr'))
					l
				in
				Some id'
			with Not_found -> None
				
		let debug_evals 
			= fun l ->
			Debug.exec l DebugTypes.Detail 
				(lazy(Printf.sprintf "Evals before post-processing : \n%s"
						(Misc.list_to_string 
							(fun ((id,c),(_,v)) -> Printf.sprintf "id %i, %s -> %s"
								id 
								(Cs.to_string Cs.Vec.V.to_string c)  
								(Cs.Vec.Coeff.to_float v |> string_of_float))
							l "\n")))
		
		let compute_next_point : Cs.t -> ('a * Min.direction) list list ->  Cs.Vec.Coeff.t option
			= fun cstr evals ->
			let i = Misc.findi (List.exists (fun ((_,cstr'),_) -> Cs.equalSyn cstr cstr')) evals 
			in 
			let eval_i = List.filter 
				(fun ((_,cstr'),_) -> not (Cs.equal cstr cstr')
					&& not (Cs.equal (strict_comp cstr) cstr')) 
				(List.nth evals i)
			in
			match eval_i with
			| [] -> 
				if List.length evals = (i+1) (* il n'y a pas d'élément après cstr dans la liste*)
				then None
				else let (_,(_,v2)) = List.nth evals (i+1) |> List.hd in
				Some v2
			| (_,(_,v2))  :: _ -> Some v2
		
		let eval_to_string : ('a * Min.direction) list list -> string
			= fun l ->
			Misc.list_to_string 
				(fun l -> Misc.list_to_string 
					(fun ((_,c),(_,v)) -> 
						(Cs.to_string Cs.Vec.V.to_string c) ^ ", " ^ (Cs.Vec.Coeff.to_float v|>string_of_float)) l " ; ")
				l "\n"
		
		exception Adjacent of int
		
		let adjust'' : region_t -> (int * Boundary.t) -> mapRegs_t -> (int * Region.t) list -> ExplorationPoint.t
			= fun reg_t (id, (cstr, pointOtherSide)) regMap regions_to_eval ->
			Min.conv := conv;
			let x0 = (MapV.find id regMap).Region.point in
			let dir_type = Min.TwoPoints (x0,pointOtherSide) in
			let v1 = Min.Sort.value dir_type cstr in
			let evals = ((id,cstr), (dir_type, v1)) 
				:: (eval regions_to_eval dir_type)
				|> debug_evals
				|> Min.Sort.sort
				|> Min.Sort.stack
			in
			Debug.log DebugTypes.Detail (lazy(Printf.sprintf "evals = %s"
				(eval_to_string evals)));
			try
				(* XXX: que se passe t-il si i n'est pas 0? *) 
				let i = Misc.findi (List.exists (fun ((_,cstr'),_) -> Cs.equal cstr cstr')) evals in
				let evals = 
					if List.length (List.nth evals i) > 1
					then (* On a trouvé des candidat adjacents, il faut vérifier qu'ils sont bien adjacents *)
						match adjacent id cstr (List.nth evals i) regMap with 
						| None -> begin match reg_t with
							| Cone -> (Misc.sublist evals 0 i) @ [[(id,cstr), (dir_type, v1)]] @ (Misc.sublist evals (i+1) (List.length evals))
							| NCone -> evals
						end
						| Some id_adj -> Pervasives.raise (Adjacent id_adj) (* un des candidats était bien adjacent *)
					else evals
				in
				match compute_next_point cstr evals with
				| None -> ExplorationPoint.Direction(id, (cstr, pointOtherSide))
				| Some v2 -> 
					let v' = Cs.Vec.Coeff.div 
						(Cs.Vec.Coeff.add v1 v2)
						(Cs.Vec.Coeff.of_float 2.)
					in
					Debug.log DebugTypes.Detail 
						(lazy (Printf.sprintf "computing middle of %s and %s : %s"
						(Cs.Vec.Coeff.to_string v1)
						(Cs.Vec.Coeff.to_string v2)
						(Cs.Vec.Coeff.to_string v')));
					let x = Min.Sort.getPoint cstr (dir_type,v') in
						Debug.log DebugTypes.Detail 
						(lazy (Printf.sprintf "The next exploration point will be %s"
							(Vec.to_string Vec.V.to_string x)));
					ExplorationPoint.Direction(id, (cstr, x))
			with Not_found -> Pervasives.failwith "PLP.adjust"
			
		let adjust' : region_t -> (int * Boundary.t) -> mapRegs_t -> ExplorationPoint.t
			= fun reg_t (id, (cstr, pointOtherSide)) regMap ->
			let regions_to_eval = filter_regions id cstr regMap in
			adjust'' reg_t (id, (cstr, pointOtherSide)) regMap regions_to_eval
		
		let adjust : region_t -> (int * Boundary.t) -> mapRegs_t -> ExplorationPoint.t
			= fun reg_t (id, (cstr, pointOtherSide)) regMap ->
			Debug.log DebugTypes.Detail 
				(lazy (Printf.sprintf "Adjusting exploration point (%s, %s, %s)"
					(Vec.to_string Vec.V.to_string((MapV.find id regMap).Region.point))
					(Cs.to_string Cs.Vec.V.to_string cstr)
					(Vec.to_string Vec.V.to_string pointOtherSide))); 
			adjust' reg_t (id, (cstr, pointOtherSide)) regMap
		
		let apply_adjacency : t -> int -> Boundary.t -> int -> t
			= fun plp id_init boundary id_adj ->
			(* Updating the region map*)
			let (cstr,_) = boundary in 
			let cstr_adj =  strict_comp cstr in
			let reg = MapV.find id_init plp.regs
			and reg_adj = MapV.find id_adj plp.regs in
			let reg = {reg with
				Region.r = List.map (fun (b,i) -> 
					if Boundary.equal b boundary
					then (b,Some id_adj)
					else (b,i))
				 	reg.Region.r
			} 
			and reg_adj = {reg_adj with
				Region.r = List.map (fun ((cstr',v),i) -> 
					if Cs.equal cstr' cstr_adj(* XXX: equal?*)
					then ((cstr',v), Some id_init)
					else ((cstr',v),i))
				 	reg_adj.Region.r
			} in
			let map' = MapV.add id_init reg plp.regs
				|> MapV.add id_adj reg_adj in
			(* Updating the exploration point list*)
			let todo' = List.fold_left
				(fun res -> function
					| ExplorationPoint.Point v as x -> x :: res
					| ExplorationPoint.Direction (id,(cstr,b)) as x ->
						if id = id_adj && (Cs.equal cstr cstr_adj)(* XXX: equal?*)
						then res
						else x :: res)
				[] plp.todo in
			{regs = map' ; todo = todo'}
			
			
		(** The PLP state [t] is modified if an adjacency is found. *)
		let rec get_next_point_rec : region_t -> t -> t
			= fun reg_t plp ->
			match plp.todo with
			| [] -> plp
			| (ExplorationPoint.Direction (id,b)) :: tl -> begin
				try
					let p = adjust reg_t (id,b) plp.regs in
					{plp with todo = p :: tl}
				with Adjacent id_adjacent -> 
					apply_adjacency {plp with todo = tl} id b id_adjacent
					|> get_next_point_rec reg_t 
				end
			| (ExplorationPoint.Point vec) :: tl ->
				if MapV.exists (fun _ reg -> Region.contains reg vec) plp.regs
				then (Debug.log DebugTypes.Detail 
						(lazy (Printf.sprintf "Exploration point %s lies within an already discovered region"
						(Vec.to_string Vec.V.to_string vec)));
					get_next_point_rec reg_t {plp with todo = tl})
				else {plp with todo = (ExplorationPoint.Point vec) :: tl}
				
		(** Returns the next point to explore, or [None] if there is none. 
			The returned point is the first element of [plp.todo]. *)
		let get_next_point : region_t -> t -> ExplorationPoint.t option * t 
			= fun reg_t plp ->
			let plp = get_next_point_rec reg_t plp in
			if plp.todo = []
			then (None, plp)
			else (Some (List.hd plp.todo),plp)
	end
	
	let reg_id : int ref = ref 0
	
	(** Returns a fresh id for a new region. *)
	let get_fresh_id : unit -> int
		= fun () ->
		let id = !reg_id in
		reg_id := id + 1;
		id
		
	(** Module that runs one exploration *)
	module Exec = struct
		
		(*
		(** [correct_point names cstrs point] returns a point that lies in [cstrs]'s interior.
			If [point] fulfills this criterion, if is returned.
			If no such point exists, [None] is returned.
		*)
		let correct_point : Naming.t -> Cs.t list -> Vec.t -> Vec.t option
			= fun names cstrs point -> 
			Debug.log DebugTypes.Detail (lazy("Correcting region"));
			if Region.contains' cstrs point
			then Debug.exec (Some point) DebugTypes.Detail (lazy "Point unchanged")
			else match Region.getPointInside (Naming.vpl_max names) cstrs with
				| None -> Debug.exec None DebugTypes.Detail (lazy "Region has empty interior")
				| Some p -> begin
					Debug.log DebugTypes.Detail (lazy(Printf.sprintf "Changing point %s to %s"
						(Vec.to_string V.to_string point)
						(Vec.to_string V.to_string p)));
					Some p
				end
		*)
		
		(* XXX: faut-il remettre la version ci-dessus pour les floats? *)
		
		(** [correct_point names cstrs point] returns a point that lies in [cstrs]'s interior.
			If no such point exists, [None] is returned.
			It uses {!val:Region.getPointInside}.
		*)
		let correct_point : region_t -> Naming.t -> Cs.t list -> Vec.t -> Vec.t option
			= fun reg_t names cstrs point -> 
			Debug.log DebugTypes.Normal (lazy("Correcting region"));
			match Region.getPointInside reg_t (Naming.vpl_max names) cstrs with 
			| None -> Debug.exec None DebugTypes.Normal 
				(lazy (Printf.sprintf "Region has empty interior:\n%s"
					(Cs.list_to_string cstrs)))
			| Some p when Region.contains' cstrs p -> begin
				Debug.log DebugTypes.Normal (lazy(Printf.sprintf "Changing point %s to %s"
					(Vec.to_string V.to_string point)
					(Vec.to_string V.to_string p)));
				Some p
				end
			| Some p -> Pervasives.failwith 
				(Printf.sprintf "Point correction returned %s, which is not in region %s"
					(Vec.to_string Vec.V.to_string p)
					(Cs.list_to_string cstrs))
		
		(** [get_boundaries cstrs point] returns the list of boundaries by minimizing [cstrs].
			@param point must be a point in the interior of [cstrs]. *)
		let (get_boundaries : region_t -> Cs.t list -> Vec.t -> Boundary.t list)
			= fun reg_t cstrs point ->
			Debug.log DebugTypes.Detail (lazy("Looking for true boundaries in new region"));
			let res = match reg_t with 
				| Cone -> Minimization.minimize_cone point cstrs 
				| NCone -> Minimization.minimize point cstrs in
			let len = List.length cstrs
			and len_res = List.length res in
			Stat.incr_redundancy (len - len_res) len_res;
			res
		
		(** [exec strat sx map exp] returns the region resulting from the exploration of [exp].
			If this region has empty interior, it returns [None]. *)
		let exec : region_t -> Objective.pivotStrgyT -> PSplx.t -> Vec.t -> Region.t option
		  	= fun reg_t st sx pointToExplore ->
		  	Debug.log DebugTypes.Normal  
		  		(lazy("Exec on the point " ^ (Vec.to_string V.to_string pointToExplore)));
		 	let sx' = Explore.push st pointToExplore sx in
		 	Debug.log DebugTypes.Normal  
		  		(lazy("Found solution " ^ (PSplx.obj_value sx' |> PSplx.ParamCoeff.to_string)));
		 	let cstrs = Region.extract sx' in
		 	match correct_point reg_t sx'.PSplx.names cstrs pointToExplore with
		 	| None -> None
		 	| Some point ->
		 		let bounds = get_boundaries reg_t cstrs point in
		 		let id = get_fresh_id() in
		 		let reg = Region.mk id bounds point sx' in 
		 		Check.check_region reg;
		 		Some reg
	end
	
	let extract_points : Region.t -> int -> ExplorationPoint.t list
			= fun reg reg_id->
			List.map
				(fun (b,_) -> ExplorationPoint.Direction (reg_id, b))
				reg.Region.r	
	
	(** Initialized simplex tableau *)
	let sx_glob : PSplx.t ref = ref PSplx.empty
	
	let get_sx : Region.t -> PSplx.t
		= fun reg ->
		match reg.Region.sx with
		| Some sx -> sx
		| None -> !sx_glob
	
	type config = {
		add_region : Region.t -> ExplorationPoint.t -> t -> t;
		reg_t : region_t;
		points : ExplorationPoint.t list;
		stgy : Objective.pivotStrgyT;
		regions : Region.t list;
		}
		
	(* TODO : tirer parti des égalités! *)
	module Add_Region = struct
		
		(** [should_explore_again cstr reg] checks wether the point that has been explored should be explored again.
		@param reg is the new discovered region.
		@param cstr is constraint that has been crossed in the previous region.*)
		let should_explore_again : Cs.t -> Region.t -> bool
			= fun cstr reg ->
			let cstr_comp = strict_comp cstr in
			List.exists
				(fun ((cstr', _),_) -> Cs.equal cstr_comp cstr') (* XXX: equalSyn?*)
				reg.Region.r 
			|> not
		
		(* TODO: better equality test? *)
		(** [region_already_known reg plp] checks whether [reg] is already present in [plp]. *)
		let region_already_known : Region.t -> t -> bool
			= fun reg plp ->
			MapV.exists 
				(fun _ reg' -> Region.equal reg reg')
				plp.regs
	
		(** Return a witness if the region has no interior, [None] otherwise. *)
		let has_interior : Region.t -> bool
			= fun reg ->
			let cstrs = Region.get_cstrs reg in
			let nvar = Cs.getVars cstrs |> Cs.Vec.V.horizon in
			let cstrs' = List.mapi (fun i c -> (i,c)) cstrs in
			match Splx.checkFromAdd (Splx.mk nvar cstrs') with
			| Splx.IsOk _ -> true
			| Splx.IsUnsat _ -> false (*match w with
				| [i,_] -> Some (List.assoc i cstrs')
				| _ -> Pervasives.failwith "PLP.has_interior"*)
		
		let remove_point : ExplorationPoint.t -> ExplorationPoint.t list -> ExplorationPoint.t list
			= fun point points ->
			Misc.pop ExplorationPoint.equal points point
		
		let do_not_add : Region.t -> ExplorationPoint.t -> t -> t
			= fun reg explorationPoint plp ->
			Debug.log DebugTypes.Normal (lazy "Region not added");
			{plp with todo = (remove_point explorationPoint plp.todo)}
			
		let add : config -> Region.t -> ExplorationPoint.t -> t -> t
			= fun config reg explorationPoint plp ->
			(* We look for a constraint saturated by the point *)
			if region_already_known reg plp
			then do_not_add reg explorationPoint plp 
			else if has_interior reg
				then config.add_region reg explorationPoint plp
				else do_not_add reg explorationPoint plp 
	end
		
	module InitSx = struct
		(** Leads to a feasible parametric simplex tableau. *)
		let init_sx : Objective.pivotStrgyT -> PSplx.t -> Vec.t -> PSplx.t option
			= fun st sx point ->
			Debug.log DebugTypes.Normal 
				(lazy(Printf.sprintf "PLP initialization with point = %s"
					(Vec.to_string Vec.V.to_string point)));
		  	match Explore.Init.findFeasibleBasis sx point with
		  	| None -> Debug.exec None DebugTypes.Normal (lazy "Problem Unsat")
		  	| Some sx as r -> Debug.exec r DebugTypes.Normal (lazy "Problem Sat")
		
		(** Returns true if the input simplex tableau is feasible, and initializes variable sx with it.*)
		let init : Vec.t -> config -> PSplx.t -> bool
			= fun point config sx ->
			match init_sx config.stgy sx point with
		  	| None -> false
		  	| Some sx' -> (sx_glob := sx'; true)
		  
	end
		
	(** This function is used when an execution (ran on point [p]) led to a region with empty interior.
	It returns the middle between the initial region's point and [p]. *)
	let find_new_point : Vec.t -> Vec.t -> Vec.t
		= fun init_point explored_point ->
		Vec.middle init_point explored_point
	
	let rec exec : config -> t -> t
		= fun config plp ->
		Debug.log DebugTypes.Normal
			(lazy (Printf.sprintf "Current exploration list : \n%s"
				(Misc.list_to_string ExplorationPoint.to_string plp.todo " ; ")));
		match RayTracing.get_next_point config.reg_t plp with
		| (None, plp') -> plp'
		| (Some explorationPoint, plp') -> 
			match explorationPoint with
			| ExplorationPoint.Direction (id, (cstr, pointToExplore)) -> begin
				Debug.log DebugTypes.Detail
					(lazy (Printf.sprintf "Exec on constraint %s and point %s"
						(Cs.to_string Cs.Vec.V.to_string cstr)
						(Vec.to_string Vec.V.to_string pointToExplore)));
				let reg = MapV.find id plp'.regs in 
				match Exec.exec config.reg_t config.stgy (get_sx reg) pointToExplore with
				| None -> 
					let newPointToExplore = find_new_point reg.Region.point pointToExplore in
					let newExplorationPoint = ExplorationPoint.Direction(id, (cstr, newPointToExplore)) in
					(* on remplace l'ancien ExplorationPoint.t de todo par le nouveau *)
					exec config {plp' with todo = List.tl plp'.todo @ [newExplorationPoint]}
				| Some reg -> exec config (Add_Region.add config reg explorationPoint plp') 
				end
			| ExplorationPoint.Point pointToExplore ->
				match Exec.exec config.reg_t config.stgy !sx_glob pointToExplore with
				| None -> exec config {plp' with todo = (Add_Region.remove_point explorationPoint plp'.todo)}
				| Some reg -> exec config (Add_Region.add config reg explorationPoint plp')
	
	(** [extract_sol sx map] returns the output constraint corresponding to the objective value of [sx].
	This constraint is of type {!type:Cons.t}, which certificate is computed thanks to [map]. *)
	let extract_sol : PSplx.t -> (PSplx.t -> 'c) -> 'c Cons.t
		= fun sx get_cert ->
		let pCoeff = sx |> PSplx.get_obj |> Objective.value in
		let cstr = ParamCoeff.to_cstr 
			(fun i -> Naming.to_user sx.PSplx.names Naming.Param i
				|> Pervasives.fst) 
			ParamCoeff.LE0 pCoeff  
		in
		let cert = get_cert sx in
		(cstr, cert) 
		
	let result_to_string : (Region.t * 'c Cons.t) list option -> string
		= fun res ->
		match res with
		| None -> "None"
		| Some res ->
			Printf.sprintf "Regions = %s\n"
				(Misc.list_to_string 
					(fun (reg, (c,_)) -> 
						(Region.to_string reg) 
						^ "\nSolution = " 
						^ (Cs.to_string Cs.Vec.V.to_string c))
					res "\n")
	
	(* Under construction, for testing purpose *)
	(*
	let check_redundancies : (Region.t * 'c Cons.t) list -> unit
		= let is_orthogonal : 'c Cons.t -> Cs.t -> bool
			= fun (cstr,_) cstr' ->
			Cs.Vec.dot_product cstr.Cs.v cstr'.Cs.v 
			|> Cs.Vec.Coeff.isZ
		in
		(*let find_common_constraint : Region.t list -> Cs.t
			= fun *)
		fun res ->
		let reg_list = List.map
			(fun (_, cons) -> 
				let regs = List.filter (fun (_,cons') -> Cons.equal cons cons') res
					|> List.split
					|> Pervasives.fst
				in
				(regs,cons))
			res
			(* only take regions that are subdivided*)
			|> List.filter (fun (regs,_) -> List.length regs > 1)
		in
		List.iter
			(fun (regs,cons) -> Stat.incr_subdivisions (List.length regs) 1)
			reg_list
		;
		(*
		List.iter
			(fun (regs,cons) -> 
				Printf.sprintf "%s -> \n%s\n\n"
					(Cons.to_string Cs.Vec.V.to_string cons)
					(Misc.list_to_string Region.to_string regs "\n")
				|> print_endline)
			reg_list  
		*)
		()
	*)
		
	(** Careful: if a region has no simplex tableau (because it was given in input), it does not appear in the result. *)
	let get_results : t -> (PSplx.t -> 'c) -> (Region.t * 'c Cons.t) list option
		= let extract_sols : (PSplx.t -> 'c) -> Region.t list -> (Region.t * 'c Cons.t) list
			= fun get_cert regs ->
			List.fold_left 
				(fun res reg -> match reg.Region.sx with
					| None -> res
					| Some sx -> (reg, extract_sol sx get_cert) :: res)
				[] regs
		in
		fun plp get_cert ->
		let regs = MapV.bindings plp.regs
			|> List.split
			|> Pervasives.snd
		in 
		Stat.nb_regions := List.length regs;
		let results = extract_sols get_cert regs in
		Debug.log DebugTypes.MOutput
			(lazy(result_to_string (Some results)));
		(*check_redundancies results;*) (* XXX: pour tests seulement! *)
		Some results
	
	(* also initializes the fresh region id *)
	let init_regions : config -> mapRegs_t
		= fun config ->
		reg_id := 0;
		List.fold_left
			(fun map reg -> 
				if !reg_id <= reg.Region.id
				then reg_id := reg.Region.id + 1;
				MapV.add reg.Region.id {reg with Region.sx = None} map)
			MapV.empty config.regions
	
	let choose_init_point : PSplx.t -> config -> Vec.t * config
		= fun sx config ->
		if config.points = []
		then 
			let point = PSplx.getParams sx 
				|> List.map (fun p -> (Vec.Coeff.mk1 (Random.int 100),p))
		  		|> Vec.mk
		  	in
		  	(point, {config with points = [ExplorationPoint.Point point]})
		else 
			let v = match List.hd config.points with
			| ExplorationPoint.Point v -> v
			| ExplorationPoint.Direction(_,(_,v)) -> v
			in
			(v,config)
	
	let init_and_exec : config -> PSplx.t -> (PSplx.t -> 'c) -> t option
		= fun config sx get_cert ->
		let (point,config) = choose_init_point sx config in
		if InitSx.init point config sx
		then 
			let plp = {
			regs = init_regions config;
			todo = config.points
			} in
			Some (exec config plp) 
		else None
	
	let run : config -> PSplx.t -> (PSplx.t -> 'c) -> (Region.t * 'c Cons.t) list option
		= fun config sx get_cert ->
		Stat.reset();
		Random.init 0;
		Debug.log DebugTypes.Title
			(lazy("PLP solver with scalar_type = " ^ (Vec.Coeff.name)));
		Debug.log DebugTypes.MInput
			(lazy (Printf.sprintf "Exploration points provided : %s\n%s"
				(Misc.list_to_string ExplorationPoint.to_string config.points " ; ")
				(PSplx.to_string sx)));
		match init_and_exec config sx get_cert with
		| None -> None
		| Some plp -> get_results plp get_cert
		
end

