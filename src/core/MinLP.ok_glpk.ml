exception Glpk_not_installed

module type Type = sig
	
	module CsUser : Cstr.Type

	type t
	
	module MapC : Map.S with type key = CsUser.t
	
	val name : string
	
	type mayUnsatT
		= IsOk of t | IsUnsat
	
	val to_string : t -> string
		
	val mk : CsUser.t list -> CsUser.Vec.V.t list -> mayUnsatT

	val get_solution : t -> CsUser.Vec.t
			
	val add_cstrs : CsUser.t list -> t -> mayUnsatT
			
	val add_cstr : CsUser.t -> t -> mayUnsatT
end

module Debug = DebugTypes.Debug(struct let name = "MinLP" end)

module Stat = struct
		
	module Glpk = struct
		let n_lp : int ref = ref 0
		
		let size_lp : int ref = ref 0
		
		let reset : unit -> unit
			= fun () ->
			n_lp := 0;
			size_lp := 0
		
		let incr_size : int -> unit
			= fun size ->
			size_lp := !size_lp + size 
		
		let incr : unit -> unit
			= fun () ->
			n_lp := !n_lp + 1
	end
	
	module Splx = struct
		let n_lp : int ref = ref 0
		
		let size_lp : int ref = ref 0
		
		let reset : unit -> unit
			= fun () ->
			n_lp := 0;
			size_lp := 0
		
		let incr_size : int -> unit
			= fun size ->
			size_lp := !size_lp + size 
		
		let incr : unit -> unit
			= fun () ->
			n_lp := !n_lp + 1
	end
end

module Glpk (CsUser : Cstr.Type) = struct
	
	module CsUser = struct
		include CsUser
		let compare = cmp
	end
	
	module V = CsUser.Vec.V
	
	module MapC = Map.Make(CsUser)
	
	let name = "Glpk"
	
	type c_float = Cstr.cmpT * (V.t * float) list * float
	
	type t = {
		lp : Glpk.lp ; 
		vars : V.t list ; 
		constrs : float array array ;
		p_cstrs : (float * float) array ;
		x_cstrs : (float * float) array}
	
	type mayUnsatT
		= IsOk of t | IsUnsat
	
	(* XXX: tout plante si < 0.01 Pourquoi? *)
	let epsilon : float = 0.01 (*Scalar.Float.delta |> Scalar.Float.to_float *)
	
	let userToPriv : V.t list -> CsUser.t -> c_float
		= fun vars c_user ->
		(
			c_user.CsUser.typ
			,
			(* la liste est triée par l'ordre dans vars*)
			(let vec = c_user.CsUser.v in
			List.map
				(fun v -> (v, CsUser.Vec.get vec v |> CsUser.Vec.Coeff.to_float))
			vars)
			,
			(CsUser.Vec.Coeff.to_float c_user.CsUser.c)
		)

	let to_string : t -> string
		= fun lp ->
		Printf.sprintf "LP : \n%s\np_bounds = %s\nx_bounds = %s"
		(Misc.array_to_string (fun a -> Misc.array_to_string string_of_float a " ; ") lp.constrs "\n")
		(Misc.array_to_string (fun (x,y) -> String.concat "" ["(" ; string_of_float x ; "," ; string_of_float y ; ")"]) lp.p_cstrs " ; ")
		(Misc.array_to_string (fun (x,y) -> String.concat "" ["(" ; string_of_float x ; "," ; string_of_float y ; ")"]) lp.x_cstrs " ; ")
			
	let p_bounds : c_float -> float array * (float * float)
		= fun (typ,cstr,cste) -> 
		let ar = List.split cstr 
			|> Pervasives.snd
			|> Array.of_list
		in
		let (bI,bS) = match typ with
			| Cstr.Eq -> (cste,cste)
			| Cstr.Le -> (-.infinity,cste)
			| Cstr.Lt -> (-.infinity,cste-.epsilon)
		in
		(ar,(bI,bS))
		
	let lambda_constraints : int -> (float * float) array
		= fun dim ->
		Array.map
			(fun _ -> (-.infinity,infinity))
			(Array.make_float dim)
	
	let run : t -> mayUnsatT
		= fun lp ->
		Debug.log DebugTypes.Normal
			(lazy("Running LP"));
		Debug.log DebugTypes.Detail
			(lazy(to_string lp));
		try 
			Glpk.simplex lp.lp;
			Debug.log DebugTypes.Normal
				(lazy (Printf.sprintf "-> Problem is Sat"));
			IsOk lp
		with 
		| Glpk.No_primal_feasible_solution -> 
			Debug.log DebugTypes.MOutput
				(lazy (Printf.sprintf "Problem is UnSat"));
			IsUnsat
		
	let mk_pb : CsUser.t list -> V.t list -> t
		= fun cstrs vars ->
		let cstrs = List.map (userToPriv vars) cstrs in
		let direction = Glpk.Maximize in (* osef, on veut seulement tester la satisfaisabilité *) 
		let dim = List.length vars in
		let obj = Array.make_float dim
			|> Array.map (fun x -> 0.) in (* pas d'objectif *)
		let (constrs,pbounds) = List.map
			p_bounds 
			cstrs
			|> List.split
		in
		let constrs = Array.of_list constrs in
		let pbounds = Array.of_list pbounds in
		let xbounds = lambda_constraints dim in
		{lp = Glpk.make_problem direction obj constrs pbounds xbounds;
		vars = vars;
		constrs = constrs;
		p_cstrs = pbounds;
		x_cstrs = xbounds}
	
	let init : CsUser.t list -> V.t list -> t
		= fun cstrs vars ->
		Stat.Glpk.incr ();
		Stat.Glpk.incr_size (List.length cstrs);
		let lp = mk_pb cstrs vars in
		(* Definition of problem parameters *)
		Glpk.use_presolver lp.lp true;
		Glpk.set_class lp.lp Glpk.Linear_prog;
		Glpk.set_message_level lp.lp 0;
		Debug.log DebugTypes.Title
			(lazy(Printf.sprintf "LP using Glpk"));
		Debug.log DebugTypes.MInput 
			(lazy(Printf.sprintf "Constraints : %s\nVariables : %s"
				(CsUser.list_to_string cstrs)
				(Misc.list_to_string V.to_string vars " ; ")));	
		lp
	
	let mk : CsUser.t list -> V.t list -> mayUnsatT
		= fun cstrs vars ->
		run (init cstrs vars)

	let get_solution : t -> CsUser.Vec.t
		= fun lp ->
		let cols = Glpk.get_col_primals lp.lp in
		let vec = CsUser.Vec.mk
			(List.map2
				(fun x v -> (CsUser.Vec.Coeff.of_float x, v))
				(Array.to_list cols)
				lp.vars)
		in
		Debug.log DebugTypes.MOutput 
			(lazy(Printf.sprintf "Solution : %s" 
			(CsUser.Vec.to_string CsUser.Vec.V.to_string vec)));
		vec
			
	let add_cstrs : CsUser.t list -> t -> mayUnsatT
		= fun cstrs lp ->
		Stat.Glpk.incr_size (List.length cstrs);
		if cstrs = []
		then IsOk lp
		else begin
			Debug.log DebugTypes.Normal 
				(lazy (Printf.sprintf "Adding constraints %s"
					(CsUser.list_to_string cstrs)));
			let cstrs = List.map (userToPriv lp.vars) cstrs in
			let (constrs,pbounds) = List.map
				p_bounds
				cstrs
				|> List.split
			in
			let constrs = Array.of_list constrs in
			let pbounds = Array.of_list pbounds in
			let new_constrs = Array.append lp.constrs constrs in
			let n_row_bef = Glpk.get_num_rows lp.lp in
			let new_p_cstrs = Array.append lp.p_cstrs pbounds in
			Glpk.add_rows lp.lp (Array.length pbounds);
			Glpk.load_matrix lp.lp new_constrs;
			Array.iteri
				(fun i (bI,bS) -> 
					Glpk.set_row_bounds lp.lp (n_row_bef + i) Glpk.Upper_bounded_var bI bS)
				pbounds;
			run {lp with constrs = new_constrs ; p_cstrs = new_p_cstrs}
		end
		
	let add_cstr : CsUser.t -> t -> mayUnsatT
		= fun cstr lp ->
		add_cstrs [cstr] lp
	
end

module Splx (CsUser : Cstr.Type) = struct
	
	module CsPriv = Cstr.Rat.Positive
	module CsUser = struct
		include CsUser
		let compare = cmp
	end
	
	module V = CsPriv.Vec.V
	
	module MapC = Map.Make(CsUser)
	
	module Integer = struct
		type t = int
		let compare = Pervasives.compare
	end
	
	module MapI = Map.Make(Integer)
	type mapInt_t = CsUser.t MapI.t
	
	let name = "Splx"
	
	type t = {
		mapInt : mapInt_t;
		vars : V.t list ; 
		sx : Splx.t}
	
	let userToPriv : CsUser.t -> CsPriv.t
		= fun c_user ->
		{	
			CsPriv.typ = c_user.CsUser.typ
			; 
			CsPriv.c = CsUser.Vec.Coeff.toQ c_user.CsUser.c
			;
			CsPriv.v = 
				CsUser.Vec.toList c_user.CsUser.v
				|> List.map
					(fun (v,c) -> 
						(CsUser.Vec.Coeff.toQ c,
						CsUser.Vec.V.toPos v |> Var.Positive.fromPos))
				|> CsPriv.Vec.mk
		}
		
	type mayUnsatT
		= IsOk of t | IsUnsat
	
	let to_string : t -> string
		= fun x -> 
		Splx.pr Splx.V.to_string x.sx
				
	let mk' : CsUser.t list -> V.t list -> mayUnsatT
		= fun cstrs vars ->
		let horizon = V.max vars |> V.next 
		in  
		let (mapI,cstrs,_) = List.fold_left
			(fun (map,l,i) c -> 
				let c' = userToPriv c in
				(MapI.add i c map, 
				 (i,c') :: l, 
				 i+1)
			)
			(MapI.empty, [], 0)
			cstrs
		in
		match	Splx.mk horizon cstrs with
		| Splx.IsOk sx -> IsOk
			{mapInt = mapI;
			 vars = vars;
			 sx = sx}
		| Splx.IsUnsat _ -> IsUnsat
	
	let mk : CsUser.t list -> V.t list -> mayUnsatT
		= fun cstrs vars ->
		Stat.Splx.incr ();
		Stat.Splx.incr_size (List.length cstrs);
		Debug.log DebugTypes.Title
			(lazy(Printf.sprintf "LP using Splx"));
		Debug.log DebugTypes.MInput 
			(lazy(Printf.sprintf "Constraints : %s\nVariables : %s"
				(CsUser.list_to_string cstrs)
				(Misc.list_to_string V.to_string vars " ; ")));
		mk' cstrs vars
		
	let get_solution : t -> CsUser.Vec.t
		= fun lp ->
		Splx.getAsg lp.sx
		|> Rtree.toList
		|> List.map (fun (v,c) -> 
			(CsUser.Vec.ofSymbolic c,
			Var.Positive.toPos v |> CsUser.Vec.V.fromPos))
		|> CsUser.Vec.mk
		
	let add_cstrs : CsUser.t list -> t -> mayUnsatT
		= fun cstrs lp ->
		Stat.Splx.incr_size (List.length cstrs);
		if cstrs = []
		then IsOk lp
		else begin
			Debug.log DebugTypes.Normal 
				(lazy (Printf.sprintf "Adding constraints %s"
				(CsUser.list_to_string cstrs)));
			let (mapI,cstrs,_) = List.fold_left
				(fun (map,l,i) c -> 
					let c' = userToPriv c in
					(MapI.add i c map, 
					 (i,c') :: l, 
					 i+1)
				)
				(lp.mapInt, [], MapI.cardinal lp.mapInt)
				cstrs
			in
			let res = List.fold_left
				(fun res (i,c) -> Splx.addAcc res (i,c))
				(Splx.add lp.sx (List.hd cstrs))
				(List.tl cstrs)
				|> Splx.checkFromAdd
			in 
			match	res with
			| Splx.IsOk sx -> begin
				Debug.log DebugTypes.Normal
				(lazy (Printf.sprintf "-> Problem is Sat"));
				IsOk {lp with mapInt = mapI ; sx = sx}
				end
			| Splx.IsUnsat _ -> begin
				Debug.log DebugTypes.MOutput
				(lazy (Printf.sprintf "Problem is UnSat"));
				IsUnsat
				end
		end
		
	let add_cstr : CsUser.t -> t -> mayUnsatT
		= fun cstr lp ->
		add_cstrs [cstr] lp
	
end

