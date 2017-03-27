open Vpl
module Cs = CstrPoly.Positive.Cs
module Vec = Cs.Vec
module PSplx_ = PSplx.PSplx(Cs)
module PSplx = PSplx_.PSplx(Vec)

module Objective = PSplx.Objective
module ParamCoeff = PSplx.ParamCoeff
module Naming = PSplx.Naming
module Poly = ParamCoeff.Poly

let addSlackAt_ts : T.testT
  = let chk : string * int * PSplx.t * PSplx.t -> T.testT
	   = fun (nm, i, sx, r) st ->
	   let sx' = PSplx.addSlackAt i sx in
	   T.equals nm PSplx.to_string PSplx.equal r sx' st
	 in
	 [
		 "simplest", 0,
		 {
			PSplx.obj = Objective.mk [] (ParamCoeff.mkCst Scalar.Rat.z);
			PSplx.mat =  [[Scalar.Rat.u]];
			PSplx.basis = [];
			PSplx.names = Naming.empty
		 },
		 {
			PSplx.obj = Objective.mkSparse 1 [] (ParamCoeff.mkCst Scalar.Rat.z);
			PSplx.mat = [[Scalar.Rat.u; Scalar.Rat.u]];
			PSplx.basis = [0];
			PSplx.names = Naming.allocAt Naming.Slack (Vec.V.u) 0 Naming.empty
		 };

		 "two_cons", 0,
		 {
			PSplx.obj = Objective.mk [] (ParamCoeff.mkCst Scalar.Rat.z);
			PSplx.mat = [[Scalar.Rat.u]; [Scalar.Rat.mk1 2]];
			PSplx.basis = [];
			PSplx.names = Naming.empty
		 },
		 {
			PSplx.obj = Objective.mkSparse 1 [] (ParamCoeff.mkCst Scalar.Rat.z);
			PSplx.mat = [[Scalar.Rat.u; Scalar.Rat.u]; [Scalar.Rat.z; Scalar.Rat.mk1 2]];
			PSplx.basis = [0];
			PSplx.names = Naming.allocAt Naming.Slack (Vec.V.u) 0 Naming.empty
		 }
	 ]
	 |> List.map chk
	 |> T.suite "addSlack"

let m : int list list -> Tableau.Matrix.t
  = List.map (List.map Q.of_int)

let get_row_pivot_ts : T.testT
  = let chk : string * int * int option * Tableau.Matrix.t -> T.testT
	   = fun (nm, col, er, m) st ->
	   let ar =
			try Some (PSplx.get_row_pivot m col)
			with Failure _ -> None
	   in
	   if ar = er
	   then T.succeed st
	   else
	   	let pr : int option -> string
		  = function
		  | None -> "None"
		  | Some i -> "Some " ^ Pervasives.string_of_int i
		in
		Printf.printf "%s: expected %s but got %s\n" nm (pr er) (pr ar);
		st
	 in
	 let tcs : (string * int * int option * Tableau.Matrix.t) list
	   = [
	   "simpl_found", 0, Some 0,
	   m [[1; 1]];
	   "simpl_none0", 0, None,
	   m [[0; 1]];
	   "simpl_none1", 0, None,
	   m [[-1; 1]];
	   "min_bound0", 0, Some 0,
	   m [[1; 1];
	 [1; 2]];
	   "min_bound1", 0, Some 1,
	   m [[1; 2];
	 [1; 1]];
	   "min_bound2", 0, Some 2,
	   m [[1; 2];
	 [-1; 1];
	 [1; 1]];
	   "degenerate", 0, Some 1,
	   m [[1; 2];
	 [1; 0]]
	 ] in
	 List.map chk tcs |> T.suite "get_row_pivot"

(** Comparison of PSplx with Splx. *)
module Explore = struct
	
	let ineq_to_cstr : Poly.t -> Cs.t
		= fun ineq ->
		CstrPoly.Positive.mk Cstr.Le ineq
		|> CstrPoly.Positive.toCstr
	
	let eq_to_cstr : Poly.t -> Cs.t
		= fun eq ->
		CstrPoly.Positive.mk Cstr.Eq eq 
		|> CstrPoly.Positive.toCstr
	
	let positivity_constraints : Vec.V.t list -> Cs.t list
		= fun vars ->
		List.map (fun v -> Cs.mk Cstr.Le [Vec.Coeff.negU,v] Vec.Coeff.z) vars
		
	(* On évalue les paramètres *)
	let obj_to_vec : Vec.V.t list -> Poly.t -> Vec.t -> Vec.t * Vec.Coeff.t
		= fun params obj point ->
		let eval : Vec.V.t -> Vec.Coeff.t option 
			= fun v -> 
			if not (List.mem v params)
			then None
			else Some (Vec.get point v)
		in
		Poly.eval_partial obj eval
		|> Poly.neg (* XXX: because PSplx is minimizing?*)
		|>	CstrPoly.Positive.mk Cstr.Eq
		|> CstrPoly.Positive.toCstr 
		|> fun c -> (Cs.get_v c, Cs.get_c c)
		
	let to_splx : Vec.V.t list -> Vec.V.t list -> Poly.t list -> Poly.t list -> Poly.t -> Vec.t -> Splx.t Splx.mayUnsatT * Vec.t * Vec.Coeff.t
		= fun vars params ineqs eqs obj point ->
		let cstrs = List.mapi 
			(fun i c -> (i,c)) 
			((List.map ineq_to_cstr ineqs) @ (List.map eq_to_cstr eqs) @ (positivity_constraints vars))
		in
		let (obj,cste) = obj_to_vec params obj point in
		(Splx.mk (Vec.V.next (Vec.V.max vars)) cstrs 
			|> Splx.checkFromAdd,
		obj,
		cste)
	
	let eval : Vec.t -> Vec.V.t -> Vec.Coeff.t 
		= fun point v -> 
		Vec.get point v
		
	let check_sol : string -> Opt.optT -> PSplx.t -> Vec.t -> Vec.Coeff.t -> T.testT
		= fun name opt psx point cste st ->
		let sol = PSplx.obj_value psx 
			|> ParamCoeff.toPoly (Naming.to_vpl psx.PSplx.names)
			|> fun p -> Poly.eval p (eval point) 
		in
		match opt with
		| Opt.Infty -> T.fail name (Printf.sprintf "Splx : infinity while PSplx solution = \n%s"
			(Scalar.Rat.to_string sol)) st 
		| Opt.Finite (sx,res,_) | Opt.Sup (sx,res,_) ->
			let res =  Scalar.Rat.add cste res in (* la constante n'est pas présente dans le résultat de Opt*)
			if Scalar.Rat.equal sol res
			then T.succeed st
			else T.fail name (Printf.sprintf "PSplx returns %s\n%s\n\nWHILE Splx returns %s\n%s"
				(Scalar.Rat.to_string res)
				(PSplx.to_string psx)
				(Scalar.Rat.to_string sol)
				(Splx.pr Vec.V.to_string sx)) st
			
	let check : string * Poly.V.t list * Poly.V.t list * Poly.t list * Poly.t list * Poly.t * Vec.t -> T.testT
		= fun (name,vars,params,ineqs,eqs,obj,point) st ->
		let psx = PSplx.Build.from_poly vars ineqs eqs obj in 
		let res = PSplx.Explore.init_and_push Objective.Bland point psx in
		let (sx, sx_obj, cste) = to_splx vars params ineqs eqs obj point in
		let max = Opt.max' sx sx_obj in
		match res, max with
		| None, Splx.IsUnsat _ | None, Splx.IsOk (Opt.Infty) -> T.succeed st
		| None, Splx.IsOk sx -> T.fail name 
			(Printf.sprintf "PSplx unsat  : \n%s\nwhile Splx sat : \n%s"
				(PSplx.to_string psx)
				(Opt.prOpt sx)) 
			st
		| Some _, Splx.IsUnsat _ -> T.fail name "PSplx sat while Splx unsat" st
		| Some psx, Splx.IsOk opt -> check_sol name opt psx point cste st
	
	let ts : T.testT = 
		let var = Vec.V.fromInt 
		in
		let tcs : (string * Poly.V.t list * Poly.V.t list * Poly.t list * Poly.t list * Poly.t * Vec.t) list
		= [	
			"no_param_triangle",
			[var 1 ; var 2],
			[],
			[Poly.of_string "-5 + x1 + x2";
			 Poly.of_string " 1 + -1*x1"],
			[],
			Poly.of_string "x1+x2",
			Vec.nil
		;
			"one_param_null",
			[var 1 ; var 2],
			[var 3],
			[Poly.of_string "-5 + x1 + x2";
			 Poly.of_string " 1 + -1*x1"],
			[],
			Poly.of_string "x1+x2 * x3",
			Vec.nil
		;
			"one_param_pos",
			[var 1 ; var 2],
			[var 3],
			[Poly.of_string "-5 + x1 + x2";
			 Poly.of_string " 1 + -1*x1"],
			[],
			Poly.of_string "x1+x2 * x3",
			Vec.mk [Vec.Coeff.of_string "17", var 3]
		;
			"one_param_neg",
			[var 1 ; var 2],
			[var 3],
			[Poly.of_string "-5 + x1 + x2";
			 Poly.of_string " 1 + -1*x1"],
			[],
			Poly.of_string "x1+x2 * x3",
			Vec.mk [Vec.Coeff.of_string "-12", var 3]
		;
			"two_params_neg_pos",
			[var 1 ; var 2],
			[var 3 ; var 4],
			[Poly.of_string "-5 + x1 + x2";
			 Poly.of_string " 1 + -1*x1"],
			[],
			Poly.of_string "x1*x4 + x1*x3 + -2*x1 + -1*x2 + -1*x2*x3 + -2*x2*x4",
			Vec.mk [Vec.Coeff.of_string "-12", var 3 ; Vec.Coeff.of_string "3", var 4]
		]
	in
	   List.map check tcs
	   |> T.suite "explore"
	
end
 
module Init
  = struct

  let getReplacementForA : T.testT
	 = let chk : string * int * int * PSplx.t -> T.testT
	= fun (nm, col, r, sx) st ->
	let i = PSplx.Explore.Init.getReplacementForA sx col in
	T.equals nm Pervasives.string_of_int (=) r i st
	   in
	   [
	"selectionBug", 0, 1,
	{
	  PSplx.obj = Objective.mk [
				 ParamCoeff.mkSparse 1 [] Q.zero;
				 ParamCoeff.mkSparse 1 [0, Q.one] Q.zero;
				 ParamCoeff.mkSparse 1 [] Q.zero
			  ] (ParamCoeff.mkSparse 1 [] Q.zero);
	  PSplx.mat = [[Q.zero; Q.minus_one; Q.one; Q.zero]];
	  PSplx.basis = [0];
	  PSplx.names = Naming.empty
	}
	   ]
	   |> List.map chk
	   |> T.suite "getReplacementForA"
	
	let var : int -> Vec.V.t = Vec.V.fromInt 
	
  let buildInitFeasibilityPb_ts : T.testT
	 = let chk : string * PSplx.t -> T.testT
	= fun (nm, sx) st ->
	let sx' = PSplx.Explore.Init.buildInitFeasibilityPb sx in
	if PSplx.Diag.isCanon sx'
	then T.succeed st
	else
	  let e = Printf.sprintf "\n%s: tableau is not in canonical form\n%s"
				 nm (PSplx.to_string sx')
	  in
	  Pervasives.print_endline e;
	  st
	   in
	   let v = List.map Q.of_int in
	   [
	"rearchitecture_bug",
	{
	  PSplx.obj =
		 Objective.mkSparse
		   5 [
		3, ParamCoeff.mkSparse 1 [0, Q.of_int ~-2] Q.one;
		4, ParamCoeff.mkSparse 1 [0, Q.of_int ~-2] Q.one
		   ] (ParamCoeff.mkSparse 1 [0, Q.of_int ~-2] Q.zero);
	  PSplx.mat = [
		 v [1; 0; 0;  1;  2;  2];
		 v [0; 1; 0; -2; -2; -2];
		 v [0; 0; 1;  2;  2;  2]
	  ];
	  PSplx.basis = [0; 1; 2];
	  PSplx.names =
		 Naming.empty
		 |> Naming.mkParam [var 6]
		 |> Naming.mkVar (List.map var [1; 2; 3; 4; 5])
	}
	   ]
	   |> List.map chk
	   |> T.suite "buildInitFeasibilityPb"

	let findFeasibleBasis_ts : T.testT
		= let chkCanon : string * bool * PSplx.t * Vec.t -> T.testT
			= fun (nm, sat, sx, vec) st ->
			if not sat 
			then T.succeed st
			else match PSplx.Explore.Init.findFeasibleBasis sx vec with
				| None -> T.fail nm "" st (* handled in chkFeasible *)
				| Some sx' ->
					if PSplx.Diag.isCanon sx'
		  			then T.succeed st
		 			else T.fail nm "tableau not in canonical form" st
	   in
	   let chkFeasible : string * bool * PSplx.t * Vec.t -> T.testT
			= fun (nm, sat, sx, vec) st ->
			match PSplx.Explore.Init.findFeasibleBasis sx vec with
			| None ->
				if sat then T.fail nm "unsat" st
				else T.succeed st
			| Some sx' ->
				if not sat then T.fail nm "sat" st
				else
		  			if PSplx.isFeasible sx'
		  			then T.succeed st
		  			else T.fail nm "canonical solution is infeasible" st
	   in
	   let tcs : (string * bool * PSplx.t * Vec.t) list
	= [
	"standard_lp_pos", true,
	(let vars = [var 1] in
	 PSplx.Build.from_poly vars
		[Poly.of_string "x1+-3"]
		[]
		(Poly.of_string "x1")),
	Vec.nil
	;
	"standard_lp_neg", false,
	(let vars = [var 1] in
	 PSplx.Build.from_poly vars
		[Poly.of_string "x1+3"]
		[]
		(Poly.of_string "x1")),
	Vec.nil
	;
	"lp_eq_pos", true,
	(let vars = [var 1] in
	 PSplx.Build.from_poly vars []
		[Poly.of_string "x1+-3"]
		(Poly.of_string "x1")),
	Vec.nil
	;
	"lp_eq_neg", false,
	(let vars = [var 1] in
	 PSplx.Build.from_poly vars []
				[Poly.of_string "x1+3"]
				(Poly.of_string "x1")),
	Vec.nil
	;
	"lp_eq_ineq_pos", true, 
	(let vars = [var 1 ; var 2] in
	 PSplx.Build.from_poly vars 
		[Poly.of_string "x2 + -1*x1"]
		[Poly.of_string "x1 + -3"]
		(Poly.of_string "x1 + x2")),
	Vec.nil
	;
	"lp_eq_2_pos", true, 
	(let vars = [var 1 ; var 2] in
	 PSplx.Build.from_poly vars 
		[]
		[Poly.of_string "x1 + -3";
		 Poly.of_string "x2 + -1*x1"]
		(Poly.of_string "x1")),
	Vec.nil
	;
	"lp_eq_2_neg", false, 
	(let vars = [var 1 ; var 2] in
	 PSplx.Build.from_poly vars 
		[]
		[Poly.of_string "x1 + -3";
		 Poly.of_string "x2 + x1"]
		(Poly.of_string "x1")),
	Vec.nil
	;
	"lp_ineqs_pos", true, 
	(let vars = [var 1 ; var 2] in
	 PSplx.Build.from_poly vars 
		[Poly.of_string "x1";
		 Poly.of_string "x2";
		 Poly.of_string "-5 + -1*x1 + -1*x2"]
		[]
		(Poly.of_string "x1 + x2")),
	Vec.nil
	;
	"lp_ineqs_pos2", true, 
	(let vars = [var 1 ; var 2] in
	 PSplx.Build.from_poly vars 
		[Poly.of_string "x1";
		 Poly.of_string "x2";
		 Poly.of_string "-5 + x1 + x2"]
		[]
		(Poly.of_string "x1 + x2")),
	Vec.nil
	;
	(* XXX: ce test devrait passer. Apparemment, mettre la contrainte de positivité des variables pose problème. *)
	"lp_positivity_eq", true, 
	(let vars = [var 1] in
	 PSplx.Build.from_poly vars 
		[Poly.of_string "-1*x1"]
		[Poly.of_string "x1 + -3"]
		(Poly.of_string "x1")),
	Vec.nil
	;
	   ] in
	   List.map chkCanon tcs @ List.map chkFeasible tcs
	   |> T.suite "initialize"

  let ts : T.testT
	 = [
	 getReplacementForA;
	 buildInitFeasibilityPb_ts;
	 findFeasibleBasis_ts
  ] |> T.suite "Init"

end

let ts : T.testT
  = T.suite Vec.V.name [
		  addSlackAt_ts;
		  get_row_pivot_ts;
		  Explore.ts;
		  Init.ts
		]
