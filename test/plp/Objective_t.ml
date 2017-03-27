open Vpl

module Test (Cs : Cstr.Rat.Type) = struct
	module Objective = Objective.Objective(Cs)
	module ParamCoeff = Objective.ParamCoeff
	module Poly = ParamCoeff.Poly
	
	let v = List.map Q.of_int

	let elim_ts : T.testT
	  = let chk : string * int * Tableau.Vector.t * Objective.t * Objective.t -> T.testT
		   = fun (nm, col, v, o, eobj) st ->
		   let aobj = Objective.elim o v col in
		   T.equals nm Objective.to_string Objective.equal eobj aobj st
		 in
		 Poly.(
		   [
		"pos", 0, v [1; 1; 0],
		Objective.mk [
			 ParamCoeff.mkSparse 1 [0, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z;
			 ParamCoeff.mkSparse 1 [0, Cs.Vec.Coeff.mk1 2] Cs.Vec.Coeff.z
		  ] (ParamCoeff.mkSparse 1 [] Cs.Vec.Coeff.z),
		Objective.mk [
			 ParamCoeff.mkSparse 1 [] Cs.Vec.Coeff.z;
			 ParamCoeff.mkSparse 1 [0, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z
		  ] (ParamCoeff.mkSparse 1 [] Cs.Vec.Coeff.z)
		    ;		      
		"neg", 0, v [-1; 0],
		Objective.mk [ParamCoeff.mkCst Cs.Vec.Coeff.u] (ParamCoeff.mkCst Cs.Vec.Coeff.z),
		Objective.mk [ParamCoeff.mkCst Cs.Vec.Coeff.z] (ParamCoeff.mkCst Cs.Vec.Coeff.z)
		 ])
		 |> List.map chk
		 |> T.suite "elim"

	let add_col_ts : T.testT
	  = let chk : string * ParamCoeff.t * int * Objective.t * Objective.t -> T.testT
		   = fun (nm, c, i, o, r) st ->
		   let o' = Objective.add_col o c i in
		   T.equals nm Objective.to_string Objective.equal r o' st
		 in
		 Poly.(
		   [
		"first", ParamCoeff.mkCst Cs.Vec.Coeff.u, 0,
		Objective.mk [ParamCoeff.mkCst Cs.Vec.Coeff.z] (ParamCoeff.mkCst Cs.Vec.Coeff.z),
		Objective.mk [
			 ParamCoeff.mkCst Cs.Vec.Coeff.u;
			 ParamCoeff.mkCst Cs.Vec.Coeff.z
		  ] (ParamCoeff.mkCst Cs.Vec.Coeff.z)
		   ;
		"last", ParamCoeff.mkCst Cs.Vec.Coeff.u, 1,
		Objective.mk [ParamCoeff.mkCst Cs.Vec.Coeff.z] (ParamCoeff.mkCst Cs.Vec.Coeff.z),
		Objective.mk [
			 ParamCoeff.mkCst Cs.Vec.Coeff.z;
			 ParamCoeff.mkCst Cs.Vec.Coeff.u
		  ] (ParamCoeff.mkCst Cs.Vec.Coeff.z)
		 ])
		 |> List.map chk
		 |> T.suite "add_col"
	(* XXX: mettre à jour 
	let getPivotCol_ts : T.testT
	  = let to_vpl = List.nth [Cs.Vec.V.fromInt 1; Cs.Vec.V.fromInt 2] in
		 let vpl_max = Cs.Vec.V.fromInt 3 in
		 let chk : string * Objective.t * Context.t * Objective.choiceT -> T.testT
		   = fun (nm, o, cx, ch) st ->
		   try
		let ch' = Objective.getPivotCol to_vpl vpl_max Objective.Bland cx [] o in
		T.equals nm Objective.choice_to_string Objective.choice_equal ch ch' st
		   with
		Invalid_argument s ->
		Printf.printf "%s: %s\n%s\n" nm (Objective.to_string o) s
		 in
		 let cx = fun l ->
		   match Context.mk to_vpl vpl_max l with
		   | None -> Pervasives.failwith "Context.mk"
		   | Some cx -> cx
		 in
		 Poly.(
		   [
		"pos_constant0",
		Objective.mk [ParamCoeff.mkCst Cs.Vec.Coeff.u] (ParamCoeff.mkCst Cs.Vec.Coeff.z),
		Context.empty vpl_max, Objective.OptReached;

		"pos_constant1",
		Objective.mk [ParamCoeff.mkCst Cs.Vec.Coeff.u] (ParamCoeff.mkCst Cs.Vec.Coeff.minus_one),
		Context.empty vpl_max, Objective.OptReached;

		"neg_constant",
		Objective.mk [ParamCoeff.mkCst Cs.Vec.Coeff.minus_one] (ParamCoeff.mkCst Cs.Vec.Coeff.z),
		Context.empty vpl_max, Objective.PivotOn 0;

		"empty_context",
		Objective.mk [ParamCoeff.mkSparse 1 [0, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z] (ParamCoeff.mkSparse 1 [] Cs.Vec.Coeff.z),
		Context.empty vpl_max,
		Objective.BranchOn (0,
					 (cx [Context.LT0, ParamCoeff.mkSparse 1 [0, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z],
					  cx [Context.GT0, ParamCoeff.mkSparse 1 [0, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z]));

		"context",
		Objective.mk [ParamCoeff.mkSparse 1 [0, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z] (ParamCoeff.mkSparse 1 [] Cs.Vec.Coeff.z),
		cx [Context.GT0, ParamCoeff.mkSparse 1 [0, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z], Objective.OptReached;

		"context_choice_const",
		Objective.mk [ParamCoeff.mkSparse 1 [0, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z; ParamCoeff.mkSparse 1 [] Cs.Vec.Coeff.minus_one]
				  (ParamCoeff.mkSparse 1 [] Cs.Vec.Coeff.z),
		cx [Context.GT0, ParamCoeff.mkSparse 1 [0, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z], Objective.PivotOn 1;

		"context_choice_cx",
		Objective.mk [
			 ParamCoeff.mkSparse 2 [0, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z;
			 ParamCoeff.mkSparse 2 [1, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z
		  ] (ParamCoeff.mkSparse 2 [] Cs.Vec.Coeff.z),
		cx [Context.GT0, ParamCoeff.mkSparse 2 [0, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z],
		Objective.BranchOn (1,
					 (cx [Context.GT0, ParamCoeff.mkSparse 2 [0, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z;
					 Context.LT0, ParamCoeff.mkSparse 2 [1, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z],
					  cx [Context.GT0, ParamCoeff.mkSparse 2 [0, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z;
					 Context.GT0, ParamCoeff.mkSparse 2 [1, Cs.Vec.Coeff.u] Cs.Vec.Coeff.z]));
		 ])
		 |> List.map chk
		 |> T.suite "getColPivot"
	*)
	let ts : T.testT
	  = T.suite Cs.Vec.V.name [
			  elim_ts;
			  add_col_ts;
			  (*getPivotCol_ts*)
			]
end

module Positive = Test(Cstr.Rat.Positive)

module Int = Test(Cstr.Rat.Int)

let ts : T.testT
	  = T.suite "Objective" [
			  Positive.ts;
			  Int.ts
			]
