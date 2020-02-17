open Vpl

module Cs = Cstr.Rat

let x = Var.fromInt 1
let y = Var.fromInt 2
let z = Var.fromInt 3
let t = Var.fromInt 4

let equalTs : Test.t
  = fun () ->
  let chk : string * bool * ParamCoeff.t * ParamCoeff.t -> Test.stateT -> Test.stateT
	   = fun (nm, r, c, c') state ->
	   if r = ParamCoeff.equal c c'
	   then Test.succeed state
	   else
		let e = Printf.sprintf "%s: %s is not equal to %s"
						 nm
						 (ParamCoeff.to_string c)
						 (ParamCoeff.to_string c')
		in
		Test.fail nm e state
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
	 |> Test.suite "equal"

let mkSparsesucceedTs : Test.t
  = fun () ->
   let chk : string * ParamCoeff.t * ParamCoeff.t -> Test.stateT -> Test.stateT
	   = fun (nm, c, c') state ->
	   Test.equals nm ParamCoeff.to_string ParamCoeff.equal c' c state
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
	 |> Test.suite "succeed"

let mkSparseFailureTs : Test.t
  = fun () ->
  let chk : string * int * (int * Scalar.Rat.t) list * Scalar.Rat.t -> Test.stateT -> Test.stateT
	   = fun (nm, i, l, a) state ->
	   try
	Stdlib.ignore (ParamCoeff.mkSparse i l a);
	let e = Printf.sprintf "%s: expected Invalid_argument" nm in
		Test.fail nm e state
	   with Invalid_argument _ -> Test.succeed state
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
	 |> Test.suite "failure"

let mkSparseTs : Test.t
  = fun () ->
  [mkSparsesucceedTs(); mkSparseFailureTs()] |> Test.suite "mkSparse"

let paramCoeffPolyTcs : (string * (Var.t -> int) *
				(int -> Var.t) * int *
				  ParamCoeff.t * ParamCoeff.Poly.t) list
  = [
  "zero", (fun i -> (Var.toInt i) - 1), (fun i -> Var.fromInt (i + 1)), 0,
  ParamCoeff.mkCst Scalar.Rat.z,
  ParamCoeff.Poly.cste Scalar.Rat.z
;
  "zero1", (fun i -> (Var.toInt i) - 1), (fun i -> Var.fromInt (i + 1)), 1,
  ParamCoeff.mkSparse 1 [] Scalar.Rat.z,
  ParamCoeff.Poly.cste Scalar.Rat.z
;
  "one", (fun i -> (Var.toInt i) - 1), (fun i -> Var.fromInt (i + 1)), 0,
  ParamCoeff.mkCst Scalar.Rat.u,
  ParamCoeff.Poly.cste Scalar.Rat.u
;
  "unit", (fun i -> (Var.toInt i) - 1), (fun i -> Var.fromInt (i + 1)), 1,
  ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] Scalar.Rat.u,
  ParamCoeff.Poly.mk_list [([x,1],Scalar.Rat.u) ; ([],Scalar.Rat.u)]
;
  "multi", (fun i -> (Var.toInt i) - 1), (fun i -> Var.fromInt (i + 1)), 2,
  ParamCoeff.mkSparse 2 [0, Scalar.Rat.u; 1, Scalar.Rat.of_int 2] Scalar.Rat.u,
  ParamCoeff.Poly.mk_list [([x,1],Scalar.Rat.u) ; ([y,1],Scalar.Rat.of_int 2) ; ([],Scalar.Rat.u)]
;
  "shift", (fun i -> (Var.toInt i) - 2), (fun i -> Var.fromInt (i + 2)), 3,
  ParamCoeff.mkSparse 3 [1, Scalar.Rat.u; 2, Scalar.Rat.of_int 2] Scalar.Rat.u,
  ParamCoeff.Poly.mk_list [([z,1],Scalar.Rat.u) ; ([t,1],Scalar.Rat.of_int 2) ; ([],Scalar.Rat.u)]
]

let ofPolyTs : Test.t
  = fun () ->
  let chk : string * (Var.t -> int) *
		(int -> Var.t) * int *
		  ParamCoeff.t * ParamCoeff.Poly.t -> Test.stateT -> Test.stateT
	   = fun (nm, tr, _, i, c, p) state ->
	   let c' = ParamCoeff.ofPoly tr i p in
	   Test.equals nm ParamCoeff.to_string ParamCoeff.equal c c' state
	 in
	 paramCoeffPolyTcs
	 |> List.map chk
	 |> Test.suite "ofPoly"

let toPolyTs : Test.t
  = fun () ->
  let chk : string * (Var.t -> int) *
		(int -> Var.t) * int *
		  ParamCoeff.t * ParamCoeff.Poly.t -> Test.stateT -> Test.stateT
	   = fun (nm, _, tr, _, c, p) state ->
	   let p' = ParamCoeff.toPoly tr c in
	   Test.equals nm ParamCoeff.Poly.to_string ParamCoeff.Poly.equal p p' state
	 in
	 paramCoeffPolyTcs
	 |> List.map chk
	 |> Test.suite "toPoly"

let addTs : Test.t
  = fun () ->
  let chk : string * ParamCoeff.t * ParamCoeff.t * ParamCoeff.t -> Test.stateT -> Test.stateT
	   = fun (nm, c1, c2, c) state ->
	   let c' = ParamCoeff.add c1 c2 in
	   Test.equals nm ParamCoeff.to_string ParamCoeff.equal c c' state
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
	   ParamCoeff.mkCst (Scalar.Rat.of_int 2)
	 ;
	   "unit",
	   ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] Scalar.Rat.z,
	   ParamCoeff.mkSparse 1 [] Scalar.Rat.z,
	   ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] Scalar.Rat.z
	 ;
	   "units",
	   ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] Scalar.Rat.z,
	   ParamCoeff.mkSparse 1 [0, Scalar.Rat.of_int 2] Scalar.Rat.z,
	   ParamCoeff.mkSparse 1 [0, Scalar.Rat.of_int 3] Scalar.Rat.z
	 ;
	   "diff",
	   ParamCoeff.mkSparse 2 [1, Scalar.Rat.u] Scalar.Rat.z,
	   ParamCoeff.mkSparse 2 [0, Scalar.Rat.of_int 2] Scalar.Rat.z,
	   ParamCoeff.mkSparse 2 [0, Scalar.Rat.of_int 2; 1, Scalar.Rat.u] Scalar.Rat.z
	 ]
	 |> List.map chk
	 |> Test.suite "add"

let mulTs : Test.t
  = fun () ->
  let chk : string * Scalar.Rat.t * ParamCoeff.t * ParamCoeff.t -> Test.stateT -> Test.stateT
	   = fun (nm, a, c, c') state ->
	   let c'' = ParamCoeff.mul a c in
	   Test.equals nm ParamCoeff.to_string ParamCoeff.equal c' c'' state
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
	   "two", Scalar.Rat.of_int 2,
	   ParamCoeff.mkSparse 1 [0, Scalar.Rat.u] (Scalar.Rat.of_int 2),
	   ParamCoeff.mkSparse 1 [0, Scalar.Rat.of_int 2] (Scalar.Rat.of_int 4)
	 ]
	 |> List.map chk
	 |> Test.suite "mul"

let is_constant_ts : Test.t
  = fun () ->
  let chk : string * bool * ParamCoeff.t -> Test.stateT -> Test.stateT
	   = fun (nm, r, c) state ->
	   let r' = ParamCoeff.is_constant c in
	   Test.equals nm Stdlib.string_of_bool (=) r r' state
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
	 |> Test.suite "is_constant"

let eval_ts : Test.t
= fun () ->
let chk : string * Q.t * ParamCoeff.t * (int -> Q.t) -> Test.stateT -> Test.stateT
	=	fun (nm, r, c, f) state ->
		let r' = ParamCoeff.eval c f in
		Test.equals nm Q.to_string Q.equal r r' state
	in
	[
		"no_param", Q.one, ParamCoeff.mkCst Q.one, List.nth [];
		"one_param", Q.of_int 4, ParamCoeff.mk [Q.of_int 2] Q.zero, List.nth [Q.of_int 2];
		"one_param_cst", Q.of_int 3, ParamCoeff.mk [Q.one] Q.one, List.nth [Q.of_int 2]
	]
	|> List.map chk
	|> Test.suite "eval"

let ts : Test.t
  = fun () ->
  List.map Test.run [
  equalTs;
  mkSparseTs;
  ofPolyTs;
  toPolyTs;
  addTs;
  mulTs;
  is_constant_ts;
	eval_ts
] |> Test.suite "ParamCoeff"
