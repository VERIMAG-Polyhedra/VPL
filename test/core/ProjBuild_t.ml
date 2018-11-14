open Vpl

module Cs = Cstr.Rat.Positive
module Var = Var.Positive

module Make_Tests (Min : Min.Type) = struct

	module Proj_ = Proj.Proj(Min)
	module ProjBuild = Proj_.Build

	let var = Var.fromInt
	let x = var 1 and y = var 2 and z = var 3

	let le
	  = fun l b -> Cs.mk Cstr_type.Le (List.map (fun (a, x) -> (Scalar.Rat.of_int a, x)) l) (Scalar.Rat.of_int b)

	module Norm
	  = struct

		 let buildTs : Test.t
		  = fun () ->
          let chk : string * Var.t list * Cs.t list * Tableau.Vector.t -> Test.stateT -> Test.stateT
		  = fun (nm, xl, l, ev) state ->
		  let av = ProjBuild.Norm.build (Var.Set.of_list xl) l in
		  Test.equals nm Tableau.Vector.to_string Tableau.Vector.equal ev av state
		in
		[
		  "unbounded", [y],
		  [le [-1, x; -1, y] ~-1; le [-1, x; 1, y] 1],
		  [Q.of_ints ~-1 2; Q.of_ints ~-5 2; Q.minus_one]
		]
		|> List.map chk
		|> Test.suite "build"

		 let ts : Test.t
		   = fun () -> [
	 	buildTs()
		   ] |> Test.suite "Norm"

	end

	let buildProjConsTs : Test.t
	  = fun () ->
      let eq : Tableau.Vector.t list -> Tableau.Vector.t list -> bool
		   = fun l l' ->
		   if List.length l <> List.length l' then false
		   else List.for_all2 Tableau.Vector.equal l l'
		 in
		 let pr : Tableau.Vector.t list -> string
		   = fun l ->
		   List.map Tableau.Vector.to_string l
		   |> String.concat "\n"
		 in
		 let chk : string * Var.t list * Cs.t list * Tableau.Vector.t list -> Test.stateT -> Test.stateT
		   = fun (nm, xs, l, el) state ->
		   let al = ProjBuild.buildProjCons xs l in
		   Test.equals nm pr eq el al state
		 in
		 [
		   "sas12", [z], [
			    le [1, x; 1, y; 1, z] 2;
			    le [1, x; 1, y; -1, z] 2;
			    le [-3, x; -1, y; 1, z] ~-3;
			    le [-3, x; -1, y; -1, z] ~-3;
			    le [-1, x; 1, y; 2, z] 2;
			    le [-1, x; 1, y; -2, z] 2;
			    le [] 1
			  ],
		   [
		[Q.one; Q.minus_one; Q.one; Q.minus_one; Q.of_int 2; Q.of_int ~-2; Q.zero; Q.zero]
		   ]
		 ]
		 |> List.map chk
		 |> Test.suite "buildProjCons"

	let buildObjTs : Test.t
	  = fun () ->
      let chk : string * Var.t list * Cs.t list * Objective.t -> Test.stateT -> Test.stateT
		   = fun (nm, xs, l, eo) state ->
		   let ao = ProjBuild.buildObj false xs l in
		   Test.equals nm Objective.to_string Objective.equal eo ao state
		 in
		 [
		   "sas12", [x; y], [
			    le [1, x; 1, y; 1, z] 2;
			    le [1, x; 1, y; -1, z] 2;
			    le [-3, x; -1, y; 1, z] ~-3;
			    le [-3, x; -1, y; -1, z] ~-3;
			    le [-1, x; 1, y; 2, z] 2;
			    le [-1, x; 1, y; -2, z] 2;
			    le [] 1
			    ], Objective.mk [
				   ParamCoeff.mkSparse 2 [0, Q.minus_one; 1, Q.minus_one] Q.zero;
				   ParamCoeff.mkSparse 2 [0, Q.minus_one; 1, Q.minus_one] Q.zero;
				   ParamCoeff.mkSparse 2 [0, Q.of_int 3; 1, Q.one] Q.zero;
				   ParamCoeff.mkSparse 2 [0, Q.of_int 3; 1, Q.one] Q.zero;
				   ParamCoeff.mkSparse 2 [0, Q.one; 1, Q.minus_one] Q.zero;
				   ParamCoeff.mkSparse 2 [0, Q.one; 1, Q.minus_one] Q.zero;
				   ParamCoeff.mkSparse 2 [] Q.zero
				 ] (ParamCoeff.mkSparse 2 [] Q.zero)
		 ]
		 |> List.map chk
		 |> Test.suite "buildObj"

	let ts : Test.t
	  = fun () -> [
	  Norm.ts();
	  buildProjConsTs();
	  buildObjTs()
	] |> Test.suite Min.name
end

module Raytracing = struct
	module Glpk = struct
		module Rat = struct
			module Min = Min.Glpk(Vector.Rat.Positive)
			include Make_Tests(Min)
		end

		module Float = struct
			module Min = Min.Glpk(Vector.Float.Positive)
			include Make_Tests(Min)
		end

		module Symbolic = struct
			module Min = Min.Glpk(Vector.Symbolic.Positive)
			include Make_Tests(Min)
		end
	end
end

let ts : Test.t
	= fun () ->
    List.map Test.run (
		if Wrapper.with_glpk then [
			Raytracing.Glpk.Rat.ts;
			Raytracing.Glpk.Float.ts;
			Raytracing.Glpk.Symbolic.ts;
		] else [])
	|> Test.suite "ProjBuild"
