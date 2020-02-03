open Vpl
module Cs = Cstr.Rat
module Vec = Cs.Vec
module PSplxDebug = PSplx.Debug
module PSplx = PSplx.Make(Vec)

module Naming = PSplx.Naming
module Poly = ParamCoeff.Poly

let addSlackAt_ts : Test.t
  = fun () ->
  let chk : string * int * PSplx.t * PSplx.t -> (Test.stateT -> Test.stateT)
	   = fun (nm, i, sx, r) st ->
	   let sx' = PSplx.addSlackAt i sx in
	   Test.equals nm PSplx.to_string PSplx.equal r sx' st
	 in
	 [
		 "simplest", 0,
		 { PSplx.empty with
			PSplx.obj = Objective.mk [] (ParamCoeff.mkCst Scalar.Rat.z);
			PSplx.mat =  [[Scalar.Rat.u]];
			PSplx.basis = [];
			PSplx.names = Naming.empty;
		 },
		 { PSplx.empty with
			PSplx.obj = Objective.mkSparse 1 [] (ParamCoeff.mkCst Scalar.Rat.z);
			PSplx.mat = [[Scalar.Rat.u; Scalar.Rat.u]];
			PSplx.basis = [0];
			PSplx.names = Naming.allocAt Naming.Slack (Var.u) 0 Naming.empty;
		 };

		 "two_cons", 0,
		 { PSplx.empty with
			PSplx.obj = Objective.mk [] (ParamCoeff.mkCst Scalar.Rat.z);
			PSplx.mat = [[Scalar.Rat.u]; [Scalar.Rat.of_int 2]];
			PSplx.basis = [];
			PSplx.names = Naming.empty;
		 },
		 { PSplx.empty with
			PSplx.obj = Objective.mkSparse 1 [] (ParamCoeff.mkCst Scalar.Rat.z);
			PSplx.mat = [[Scalar.Rat.u; Scalar.Rat.u]; [Scalar.Rat.z; Scalar.Rat.of_int 2]];
			PSplx.basis = [0];
			PSplx.names = Naming.allocAt Naming.Slack (Var.u) 0 Naming.empty
		 }
	 ]
	 |> List.map chk
	 |> Test.suite "addSlack"

let m : int list list -> Tableau.Matrix.t
  = List.map (List.map Q.of_int)

let q : int -> int -> Q.t
    = Scalar.Rat.mk

let get_row_pivot_ts : Test.t
  = fun () ->
   let chk : string * int * int option * Tableau.Matrix.t -> (Test.stateT -> Test.stateT)
	   = fun (nm, col, er, m) st ->
	   let ar =
            let basis = [] in (* The basis is not used in the standard pivot  *)
			try PSplx.get_row_pivot PSplx_type.Standard basis m col
			with PSplx.Unbounded_problem -> None
	   in
	   if ar = er
	   then Test.succeed st
	   else
	   	let pr : int option -> string
		  = function
		  | None -> "None"
		  | Some i -> "Some " ^ Stdlib.string_of_int i
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
	 List.map chk tcs |> Test.suite "get_row_pivot"


(** Comparison of PSplx with Splx. *)
module Explore = struct

	let ineq_to_cstr : Poly.t -> Cs.t
		= fun ineq ->
		CstrPoly.mk Cstr_type.Le ineq
		|> CstrPoly.toCstr

	let eq_to_cstr : Poly.t -> Cs.t
		= fun eq ->
		CstrPoly.mk Cstr_type.Eq eq
		|> CstrPoly.toCstr

	let positivity_constraints : Var.t list -> Cs.t list
		= fun vars ->
		List.map (fun v -> Cs.mk Cstr_type.Le [Vec.Coeff.negU,v] Vec.Coeff.z) vars

	(* On évalue les paramètres *)
	let obj_to_vec : Var.t list -> Poly.t -> Vec.t -> Vec.t * Vec.Coeff.t
		= fun params obj point ->
		let eval : Var.t -> Vec.Coeff.t option
			= fun v ->
			if not (List.mem v params)
			then None
			else Some (Vec.get point v)
		in
		Poly.eval_partial obj eval
		|> Poly.neg (* XXX: because PSplx is minimizing?*)
		|>	CstrPoly.mk Cstr_type.Eq
		|> CstrPoly.toCstr
		|> fun c -> (Cs.get_v c, Cs.get_c c)

	let to_splx : Var.t list -> Var.t list -> Poly.t list -> Poly.t list -> Poly.t -> Vec.t -> Splx.t Splx.mayUnsatT * Vec.t * Vec.Coeff.t
		= fun vars params ineqs eqs obj point ->
		let cstrs = List.mapi
			(fun i c -> (i,c))
			((List.map ineq_to_cstr ineqs) @ (List.map eq_to_cstr eqs) @ (positivity_constraints vars))
		in
		let (obj,cste) = obj_to_vec params obj point in
		(Splx.mk (Var.next (Var.max vars)) cstrs
			|> Splx.checkFromAdd,
		obj,
		cste)

	let eval : Vec.t -> Var.t -> Vec.Coeff.t
		= fun point v ->
		Vec.get point v

	let check_sol : string -> Opt.optT -> PSplx.t -> Vec.t -> Vec.Coeff.t -> (Test.stateT -> Test.stateT)
		= fun name opt psx point cste st ->
		let sol = PSplx.obj_value psx
			|> ParamCoeff.toPoly (Naming.to_vpl psx.PSplx.names)
			|> fun p -> Poly.eval p (eval point)
		in
		match opt with
		| Opt.Infty -> Test.fail name (Printf.sprintf "Splx : infinity while PSplx solution = \n%s"
			(Scalar.Rat.to_string sol)) st
		| Opt.Finite (sx,res,_) | Opt.Sup (sx,res,_) ->
			let res =  Scalar.Rat.add cste res in (* la constante n'est pas présente dans le résultat de Opt*)
			if Scalar.Rat.equal sol res
			then Test.succeed st
			else Test.fail name (Printf.sprintf "PSplx returns %s\n%s\n\nWHILE Splx returns %s\n%s"
				(Scalar.Rat.to_string res)
				(PSplx.to_string psx)
				(Scalar.Rat.to_string sol)
				(Splx.pr Var.to_string sx)) st

    let check_pivot : string -> PSplx.t -> PSplx.t -> (Test.stateT -> Test.stateT)
        = fun test_name sx_before sx_after st ->
        List.fold_left (fun st i ->
            let pcoeff = Objective.get i sx_before.PSplx.obj
            and c = Tableau.Matrix.getCol i sx_before.PSplx.mat
            in
            let (pcoeff', c') = sx_after.PSplx.pivot (pcoeff, c) in
            let pcoeff_expected = Objective.get i sx_after.PSplx.obj
            and c_expected = Tableau.Matrix.getCol i sx_after.PSplx.mat
            in
            if ParamCoeff.equal pcoeff' pcoeff_expected
                && Tableau.Vector.equal c' c_expected
            then Test.succeed st
            else let error_msg = Printf.sprintf
                ("Reapplying pivots on each column: \nexpected %s\n%s\ngot %s\n%s")
                (ParamCoeff.to_string pcoeff_expected)
                (Tableau.Vector.to_string c_expected)
                (ParamCoeff.to_string pcoeff')
                (Tableau.Vector.to_string c')
                in
                Test.fail test_name error_msg st
        ) st (Misc.range 0 ((PSplx.nCols sx_before) - 1))

	let check : string * Var.t list * Var.t list * Poly.t list * Poly.t list * Poly.t * Vec.t -> (Test.stateT -> Test.stateT)
		= fun (name,vars,params,ineqs,eqs,obj,point) st ->
		let psx = PSplx.Build.from_poly vars ineqs eqs obj in
		let res = PSplx.Explore.init_and_push Objective.Bland PSplx_type.Standard point psx in
		let (sx, sx_obj, cste) = to_splx vars params ineqs eqs obj point in
		let max = Opt.max' sx sx_obj in
        let st' = match res with
            | None -> st
            | Some sx' -> check_pivot name psx sx' st
        in
		match res, max with
		| None, Splx.IsUnsat _ | None, Splx.IsOk (Opt.Infty) -> Test.succeed st'
		| None, Splx.IsOk sx -> Test.fail name
			(Printf.sprintf "PSplx unsat  : \n%s\nwhile Splx sat : \n%s"
				(PSplx.to_string psx)
				(Opt.prOpt sx))
			st'
		| Some _, Splx.IsUnsat _ -> Test.fail name "PSplx sat while Splx unsat" st'
		| Some psx, Splx.IsOk opt -> check_sol name opt psx point cste st'

	let ts : Test.t =
		fun () ->
        let var = Var.fromInt
		in
		let tcs : (string * Var.t list * Var.t list * Poly.t list * Poly.t list * Poly.t * Vec.t) list
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
	   |> Test.suite "explore"

end

module Init
  = struct

  let getReplacementForA : Test.t
	 = fun () ->
     let chk : string * int * int * PSplx.t -> (Test.stateT -> Test.stateT)
	= fun (nm, col, r, sx) st ->
	let i = PSplx.Explore.Init.getReplacementForA sx col in
	Test.equals nm Stdlib.string_of_int (=) r i st
	   in
	   [
	"selectionBug", 0, 1,
	{ PSplx.empty with
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
	   |> Test.suite "getReplacementForA"

	let var : int -> Var.t = Var.fromInt

  let buildInitFeasibilityPb_ts : Test.t
	=  fun () ->
    let chk : string * PSplx.t -> (Test.stateT -> Test.stateT)
	= fun (nm, sx) st ->
	let sx' = PSplx.Explore.Init.buildInitFeasibilityPb sx in
	if PSplx.isCanon sx'
	then Test.succeed st
	else
	  let e = Printf.sprintf "\n%s: tableau is not in canonical form\n%s"
				 nm (PSplx.to_string sx')
	  in
	  Stdlib.print_endline e;
	  st
	   in
	   let v = List.map Q.of_int in
	   [
	"rearchitecture_bug",
	{ PSplx.empty with
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
	   |> Test.suite "buildInitFeasibilityPb"

	let findFeasibleBasis_ts : Test.t
		= fun () ->
        let chkCanon : string * bool * PSplx.t * Vec.t -> (Test.stateT -> Test.stateT)
			= fun (nm, sat, sx, vec) st ->
			if not sat
			then Test.succeed st
			else match PSplx.Explore.Init.findFeasibleBasis Objective.Bland PSplx_type.Standard sx vec with
				| None -> Test.fail nm "" st (* handled in chkFeasible *)
				| Some sx' ->
					if PSplx.isCanon sx'
		  			then Test.succeed st
		 			else Test.fail nm "tableau not in canonical form" st
	   in
	   let chkFeasible : string * bool * PSplx.t * Vec.t -> (Test.stateT -> Test.stateT)
			= fun (nm, sat, sx, vec) st ->
			match PSplx.Explore.Init.findFeasibleBasis Objective.Bland PSplx_type.Standard sx vec with
			| None ->
				if sat then Test.fail nm "unsat" st
				else Test.succeed st
			| Some sx' ->
				if not sat then Test.fail nm "sat" st
				else
		  			if PSplx.isFeasible sx'
		  			then Test.succeed st
		  			else Test.fail nm "canonical solution is infeasible" st
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
	   |> Test.suite "initialize"

  let ts : Test.t
	 = fun () ->
     [
	 getReplacementForA();
	 buildInitFeasibilityPb_ts();
	 findFeasibleBasis_ts()
  ] |> Test.suite "Init"

end

let ts : Test.t
  = fun () ->
  List.map Test.run [
		  addSlackAt_ts;
		  get_row_pivot_ts;
		  Explore.ts;
		  Init.ts
		]
  |> Test.suite "PSplx"
