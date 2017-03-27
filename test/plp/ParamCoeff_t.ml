open Vpl

module Test(Cs : Cstr.Rat.Type) = struct

	module ParamCoeff = ParamCoeff.ParamCoeff(Cs)
	
	let x = Cs.Vec.V.fromInt 1
	let y = Cs.Vec.V.fromInt 2
	let z = Cs.Vec.V.fromInt 3
	let t = Cs.Vec.V.fromInt 4
	
	let equalTs : T.testT
	  = let chk : string * bool * ParamCoeff.t * ParamCoeff.t -> T.stateT -> T.stateT
		   = fun (nm, r, c, c') state ->
		   if r = ParamCoeff.equal c c' 
		   then T.succeed state
		   else
			let e = Printf.sprintf "%s: %s is not equal to %s"
							 nm
							 (ParamCoeff.to_string c)
							 (ParamCoeff.to_string c')
			in
			T.fail nm e state
		 in
		 [
		   "zero", true,
		   {ParamCoeff.lin = []; ParamCoeff.cst = Scalar.Rat.z},
		   {ParamCoeff.lin = []; ParamCoeff.cst = Scalar.Rat.z}
		 ;
		   "one", true,
		   {ParamCoeff.lin = []; ParamCoeff.cst = Scalar.Rat.u},
		   {ParamCoeff.lin = []; ParamCoeff.cst = Scalar.Rat.u}
		 ;
		   "unit", true,
		   {ParamCoeff.lin = [Scalar.Rat.u]; ParamCoeff.cst = Scalar.Rat.z},
		   {ParamCoeff.lin = [Scalar.Rat.u]; ParamCoeff.cst = Scalar.Rat.z}
		 ;
		   "zero_one", false,
		   {ParamCoeff.lin = []; ParamCoeff.cst = Scalar.Rat.z},
		   {ParamCoeff.lin = []; ParamCoeff.cst = Scalar.Rat.u}
		 ;
		   "bad_unit", false,
		   {ParamCoeff.lin = [Scalar.Rat.u]; ParamCoeff.cst = Scalar.Rat.z},
		   {ParamCoeff.lin = [Scalar.Rat.z]; ParamCoeff.cst = Scalar.Rat.z}
		 ;
		   "bad_size", false,
		   {ParamCoeff.lin = [Scalar.Rat.u; Scalar.Rat.z]; ParamCoeff.cst = Scalar.Rat.z},
		   {ParamCoeff.lin = [Scalar.Rat.z]; ParamCoeff.cst = Scalar.Rat.z}
		 ]
		 |> List.map chk
		 |> T.suite "equal"

	let mkSparsesucceedTs : T.testT
	  = let chk : string * ParamCoeff.t * ParamCoeff.t -> T.stateT -> T.stateT
		   = fun (nm, c, c') state ->
		   T.equals nm ParamCoeff.to_string ParamCoeff.equal c' c state
		 in
		 [
		   "zero", ParamCoeff.mkCst Scalar.Rat.z,
		   {ParamCoeff.lin = []; ParamCoeff.cst = Scalar.Rat.z}
		 ;
		   "one", ParamCoeff.mkCst Scalar.Rat.u,
		   {ParamCoeff.lin = []; ParamCoeff.cst = Scalar.Rat.u}
		 ;
		   "one_zeroes", ParamCoeff.mkSparse 1 [0, Scalar.Rat.z] Scalar.Rat.z,
		   {ParamCoeff.lin = [Scalar.Rat.z]; ParamCoeff.cst = Scalar.Rat.z}
		 ;
		   "imp_one_zeroes", ParamCoeff.mkSparse 1 [] Scalar.Rat.z,
		   {ParamCoeff.lin = [Scalar.Rat.z]; ParamCoeff.cst = Scalar.Rat.z}
		 ;
		   "unit_one", ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] Scalar.Rat.z,
		   {ParamCoeff.lin = [Scalar.Rat.u]; ParamCoeff.cst = Scalar.Rat.z}
		 ;
		   "hole", ParamCoeff.mkSparse 3 [0, Scalar.Rat.u; 2, Scalar.Rat.u] Scalar.Rat.z,
		   {ParamCoeff.lin = [Scalar.Rat.u; Scalar.Rat.z; Scalar.Rat.u];
		    ParamCoeff.cst = Scalar.Rat.z}
		 ;
		   "end", ParamCoeff.mkSparse 3 [0, Scalar.Rat.u; 1, Scalar.Rat.u] Scalar.Rat.z,
		   {ParamCoeff.lin = [Scalar.Rat.u; Scalar.Rat.u; Scalar.Rat.z];
		    ParamCoeff.cst = Scalar.Rat.z}
		 ;
		   "beginning", ParamCoeff.mkSparse 3 [1, Scalar.Rat.u; 2, Scalar.Rat.u] Scalar.Rat.z,
		   {ParamCoeff.lin = [Scalar.Rat.z; Scalar.Rat.u; Scalar.Rat.u];
		    ParamCoeff.cst = Scalar.Rat.z}
		 ;
		   "order", ParamCoeff.mkSparse 2 [1, Scalar.Rat.u; 0, Scalar.Rat.z] Scalar.Rat.z,
		   {ParamCoeff.lin = [Scalar.Rat.z; Scalar.Rat.u];
		    ParamCoeff.cst = Scalar.Rat.z}
		 ]
		 |> List.map chk
		 |> T.suite "succeed"

	let mkSparseFailureTs : T.testT
	  = let chk : string * int * (int * Scalar.Rat.t) list * Scalar.Rat.t -> T.stateT -> T.stateT
		   = fun (nm, i, l, a) state ->
		   try
		Pervasives.ignore (ParamCoeff.mkSparse i l a);
		let e = Printf.sprintf "%s: expected Invalid_argument" nm in
			T.fail nm e state
		   with Invalid_argument _ -> T.succeed state
		 in
		 [
		   "zero", 0, [0, Scalar.Rat.u], Scalar.Rat.z
		 ;
		   "one", 0, [-1, Scalar.Rat.u], Scalar.Rat.z
		 ;
		   "too_big1", 0, [0, Scalar.Rat.u], Scalar.Rat.z
		 ;
		   "too_big2", 1, [1, Scalar.Rat.u], Scalar.Rat.z
		 ;
		   "duplicate_diff", 1, [0, Scalar.Rat.u; 0, Scalar.Rat.z], Scalar.Rat.z
		 ;
		   "duplicate_same", 1, [0, Scalar.Rat.u; 0, Scalar.Rat.u], Scalar.Rat.z
		 ]
		 |> List.map chk
		 |> T.suite "failure"

	let mkSparseTs : T.testT
	  = [mkSparsesucceedTs; mkSparseFailureTs] |> T.suite "mkSparse"

	let paramCoeffPolyTcs : (string * (Cs.Vec.V.t -> int) *
					(int -> Cs.Vec.V.t) * int *
					  ParamCoeff.t * ParamCoeff.Poly.t) list
	  = [
	  "zero", (fun i -> (Cs.Vec.V.toInt i) - 1), (fun i -> Cs.Vec.V.fromInt (i + 1)), 0,
	  ParamCoeff.mkCst Scalar.Rat.z,
	  ParamCoeff.Poly.cste Scalar.Rat.z
	;
	  "zero1", (fun i -> (Cs.Vec.V.toInt i) - 1), (fun i -> Cs.Vec.V.fromInt (i + 1)), 1,
	  ParamCoeff.mkSparse 1 [] Scalar.Rat.z,
	  ParamCoeff.Poly.cste Scalar.Rat.z
	;
	  "one", (fun i -> (Cs.Vec.V.toInt i) - 1), (fun i -> Cs.Vec.V.fromInt (i + 1)), 0,
	  ParamCoeff.mkCst Scalar.Rat.u,
	  ParamCoeff.Poly.cste Scalar.Rat.u
	;
	  "unit", (fun i -> (Cs.Vec.V.toInt i) - 1), (fun i -> Cs.Vec.V.fromInt (i + 1)), 1,
	  ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] Scalar.Rat.u,
	  ParamCoeff.Poly.mk2_cste [([x],Scalar.Rat.u)] Scalar.Rat.u
	;
	  "multi", (fun i -> (Cs.Vec.V.toInt i) - 1), (fun i -> Cs.Vec.V.fromInt (i + 1)), 2,
	  ParamCoeff.mkSparse 2 [0, Scalar.Rat.u; 1, Scalar.Rat.mk1 2] Scalar.Rat.u,
	  ParamCoeff.Poly.mk2_cste [([x],Scalar.Rat.u) ; ([y],Scalar.Rat.mk1 2)] Scalar.Rat.u
	;
	  "shift", (fun i -> (Cs.Vec.V.toInt i) - 2), (fun i -> Cs.Vec.V.fromInt (i + 2)), 3,
	  ParamCoeff.mkSparse 3 [1, Scalar.Rat.u; 2, Scalar.Rat.mk1 2] Scalar.Rat.u,
	  ParamCoeff.Poly.mk2_cste [([z],Scalar.Rat.u) ; ([t],Scalar.Rat.mk1 2)] Scalar.Rat.u
	]

	let ofPolyTs : T.testT
	  = let chk : string * (Cs.Vec.V.t -> int) *
			(int -> Cs.Vec.V.t) * int *
			  ParamCoeff.t * ParamCoeff.Poly.t -> T.stateT -> T.stateT
		   = fun (nm, tr, _, i, c, p) state ->
		   let c' = ParamCoeff.ofPoly tr i p in
		   T.equals nm ParamCoeff.to_string ParamCoeff.equal c c' state
		 in
		 paramCoeffPolyTcs
		 |> List.map chk
		 |> T.suite "ofPoly"

	let toPolyTs : T.testT
	  = let chk : string * (Cs.Vec.V.t -> int) *
			(int -> Cs.Vec.V.t) * int *
			  ParamCoeff.t * ParamCoeff.Poly.t -> T.stateT -> T.stateT
		   = fun (nm, _, tr, _, c, p) state ->
		   let p' = ParamCoeff.toPoly tr c in
		   T.equals nm ParamCoeff.Poly.to_string ParamCoeff.Poly.equal p p' state
		 in
		 paramCoeffPolyTcs
		 |> List.map chk
		 |> T.suite "toPoly"

	let addTs : T.testT
	  = let chk : string * ParamCoeff.t * ParamCoeff.t * ParamCoeff.t -> T.stateT -> T.stateT
		   = fun (nm, c1, c2, c) state ->
		   let c' = ParamCoeff.add c1 c2 in
		   T.equals nm ParamCoeff.to_string ParamCoeff.equal c c' state
		 in
		 [
		   "zero",
		   ParamCoeff.mkCst Scalar.Rat.z,
		   ParamCoeff.mkCst Scalar.Rat.z,
		   ParamCoeff.mkCst Scalar.Rat.z
		 ;
		   "one",
		   ParamCoeff.mkCst Scalar.Rat.u,
		   ParamCoeff.mkCst Scalar.Rat.z,
		   ParamCoeff.mkCst Scalar.Rat.u
		 ;
		   "two",
		   ParamCoeff.mkCst Scalar.Rat.u,
		   ParamCoeff.mkCst Scalar.Rat.u,
		   ParamCoeff.mkCst (Scalar.Rat.mk1 2)
		 ;
		   "unit",
		   ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] Scalar.Rat.z,
		   ParamCoeff.mkSparse 1 [] Scalar.Rat.z,
		   ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] Scalar.Rat.z
		 ;
		   "units",
		   ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] Scalar.Rat.z,
		   ParamCoeff.mkSparse 1 [0, Scalar.Rat.mk1 2] Scalar.Rat.z,
		   ParamCoeff.mkSparse 1 [0, Scalar.Rat.mk1 3] Scalar.Rat.z
		 ;
		   "diff",
		   ParamCoeff.mkSparse 2 [1, Scalar.Rat.u] Scalar.Rat.z,
		   ParamCoeff.mkSparse 2 [0, Scalar.Rat.mk1 2] Scalar.Rat.z,
		   ParamCoeff.mkSparse 2 [0, Scalar.Rat.mk1 2; 1, Scalar.Rat.u] Scalar.Rat.z
		 ]
		 |> List.map chk
		 |> T.suite "add"

	let mulTs : T.testT
	  = let chk : string * Scalar.Rat.t * ParamCoeff.t * ParamCoeff.t -> T.stateT -> T.stateT
		   = fun (nm, a, c, c') state ->
		   let c'' = ParamCoeff.mul a c in
		   T.equals nm ParamCoeff.to_string ParamCoeff.equal c' c'' state
		 in
		 [
		   "zero", Scalar.Rat.z,
		   ParamCoeff.mkCst Scalar.Rat.z,
		   ParamCoeff.mkCst Scalar.Rat.z
		 ;
		   "zero1", Scalar.Rat.z,
		   ParamCoeff.mkCst Scalar.Rat.u,
		   ParamCoeff.mkCst Scalar.Rat.z
		 ;
		   "zero2", Scalar.Rat.z,
		   ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] Scalar.Rat.z,
		   ParamCoeff.mkSparse 1 [] Scalar.Rat.z
		 ;
		   "unit", Scalar.Rat.u,
		   ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] Scalar.Rat.u,
		   ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] Scalar.Rat.u
		 ;
		   "two", Scalar.Rat.mk1 2,
		   ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] (Scalar.Rat.mk1 2),
		   ParamCoeff.mkSparse 1 [0, Scalar.Rat.mk1 2] (Scalar.Rat.mk1 4)
		 ]
		 |> List.map chk
		 |> T.suite "mul"

	let is_constant_ts : T.testT
	  = let chk : string * bool * ParamCoeff.t -> T.stateT -> T.stateT
		   = fun (nm, r, c) state ->
		   let r' = ParamCoeff.is_constant c in
		   T.equals nm Pervasives.string_of_bool (=) r r' state
		 in
		 [
			 "zero_no_param", true, ParamCoeff.mkCst Scalar.Rat.z;
			 "zero", true, ParamCoeff.mkSparse 1 [] Scalar.Rat.z;
			 "constant_no_param", true, ParamCoeff.mkCst Scalar.Rat.u;
			 "constant", true, ParamCoeff.mkSparse 1 [] Scalar.Rat.u;
			 "one_param", false, ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] Scalar.Rat.z;
			 "one_param'", false, ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] Scalar.Rat.u;
			 "params", false, ParamCoeff.mkSparse 2 [0, Scalar.Rat.u; 1, Scalar.Rat.negU] Scalar.Rat.u
		 ]
		 |> List.map chk
		 |> T.suite "is_constant"

	let eval_ts : T.testT
	=	let chk : string * Q.t * ParamCoeff.t * (int -> Q.t) -> T.stateT -> T.stateT
		=	fun (nm, r, c, f) state ->
			let r' = ParamCoeff.eval c f in
			T.equals nm Q.to_string Q.equal r r' state
		in
		[
			"no_param", Q.one, ParamCoeff.mkCst Q.one, List.nth [];
			"one_param", Q.of_int 4, ParamCoeff.mk [Q.of_int 2] Q.zero, List.nth [Q.of_int 2];
			"one_param_cst", Q.of_int 3, ParamCoeff.mk [Q.one] Q.one, List.nth [Q.of_int 2]
		]
		|> List.map chk
		|> T.suite "eval"

	let ts : T.testT
	  = [
	  equalTs;
	  mkSparseTs;
	  ofPolyTs;
	  toPolyTs;
	  addTs;
	  mulTs;
	  is_constant_ts;
		eval_ts
	] |> T.suite Cs.Vec.V.name
end

module Positive = Test(Cstr.Rat.Positive)

module Int = Test(Cstr.Rat.Int)

let ts : T.testT
	  = [
	  Positive.ts;
	  Int.ts
	] |> T.suite "ParamCoeff"
