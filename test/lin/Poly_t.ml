open Vpl

module Test (Vec : Vector.Rat.Type) = struct

	module Poly = Poly.Poly(Vec)	
	module Coeff = Poly.Coeff
	module V = Poly.V
	
	let x = V.fromInt 1
	let y = V.fromInt 2
	let z = V.fromInt 3
	
	module Invariant_t
	  = struct

	  module Monomial_t
		 = struct

		 let ts : T.testT
		   = let chk (nm, a, m) = fun state -> 
		  if Poly.Invariant.Monom.check m = a
		  then T.succeed state
		  else 
		  	let e = Printf.sprintf "%s: expected %B for %s" nm a (Poly.Monomial.to_string m) in
		  	T.fail nm e state
		in
		let tcs : (string * bool * Poly.Monomial.t) list
		  = [
		  "zero1", false, ([x], Coeff.z);
		  "zero2", false, ([x; y], Coeff.z);
		  "zero_const0", false, ([], Coeff.z);
		  "zero_const1", false, ([x], Coeff.z);
		  "const0", true, ([], Coeff.u);
		  "const1", true, ([x], Coeff.u);
		  "const_mixed0", true, ([x; x], Coeff.u);
		  "const_mixed1", true, ([x; x], Coeff.u);
		  "sorted0", true, ([x; y], Coeff.u);
		  "sorted1", false, ([y; x], Coeff.u);
		  "sorted2", true, ([x; y; z], Coeff.u);
		  "sorted3", false, ([x; z; y], Coeff.u);
		  "sorted4", false, ([z; y; x], Coeff.u);
		  "sorted5", false, ([y; z; x], Coeff.u);
		  "multi0", true, ([x; x], Coeff.u);
		  "multi1", true, ([x; y; y], Coeff.u);
		  "multi2", false, ([x; y; x], Coeff.u);
		] in
		List.map chk tcs |> T.suite "Monomial"

	  end

	  let ts : T.testT
		 = let chk : string * bool * Poly.t -> T.testT
		= fun (nm, a, p) -> fun state ->
		if Poly.Invariant.check p = a
		then T.succeed state
		else let e = Printf.sprintf "%s: expected %B for %s" nm a (Poly.to_string p)
			in T.fail nm e state
		in
		   let tcs : (string * bool * Poly.t) list
		= [
		"empty", true, [[], Coeff.u];
		"const1", true, [[x], Coeff.u];
		"const2", true, [[], Coeff.u; [x], Coeff.u];
		"sorted4", false, [[x], Coeff.u; [], Coeff.u];
		"sorted0", true, [[x], Coeff.u; [y], Coeff.u];
		"sorted1", false, [[y], Coeff.u; [x], Coeff.u];
		"sorted2", true, [[x], Coeff.u; [y], Coeff.u];
		"sorted3", true, [[x], Coeff.u; [x; y], Coeff.u];
		"dup0", false, [[x], Coeff.u; [x], Coeff.u];
		"dup1", false, [[x], Coeff.u; [y], Coeff.u; [x], Coeff.u];
		   ] in
		   Monomial_t.ts :: List.map chk tcs |> T.suite "Invariant"

	end

	module Monomial_t
	  = struct

	  let canonTs : T.testT
		 =
		 let eq : Poly.Monomial.t option -> Poly.Monomial.t option -> bool
		   = fun om om' ->
		   match om, om' with
		   | None, Some _ | Some _, None -> false
		   | None, None -> true
		   | Some m, Some m' -> Poly.Monomial.equal m m'
		 in
		 let chkResult : string * Poly.Monomial.t * Poly.Monomial.t option -> T.testT
		   = fun (nm, m, em) state -> 
		   let am = Poly.Monomial.canonO m in
		   if eq em am
		   then T.succeed state
		   else T.fail nm "" state
		 in
		 let chkInvariant : string * Poly.Monomial.t * Poly.Monomial.t option -> T.testT
		   = fun (nm, m, _) state -> 
		   match Poly.Monomial.canonO m with
		   | None -> T.succeed state
		   | Some am ->
		 if Poly.Invariant.Monom.check am
		 then T.succeed state
		 else let e = Printf.sprintf "%s: chkInvariant" nm in
		 	 T.fail nm e state
		 in
		 [
		   "const0", ([], Coeff.u), Some ([], Coeff.u);
		   "mono0", ([x], Coeff.u), Some ([x], Coeff.u);
		   "mono1", ([y], Coeff.u), Some ([y], Coeff.u);
		   "mono2", ([x], Coeff.mk1 2), Some ([x], Coeff.mk1 2);
		   "multi0", ([x; y], Coeff.u), Some ([x; y], Coeff.u);
		   "multi1", ([y; x], Coeff.u), Some ([x; y], Coeff.u);
		   "zero0", ([], Coeff.z), None;
		   "zero1", ([], Coeff.z), None;
		   "zero2", ([x], Coeff.z), None;(*
		   "bad_const0", ([x; x], Coeff.u), None;
		   "bad_const1", ([x; x], Coeff.u), None*)
		 ]
		 |> fun tcs -> List.map chkResult tcs @ List.map chkInvariant tcs
			  |> T.suite "canonMonomial"

	  let compare_ts : T.testT
		 = let chk : string * Poly.Monomial.t * Poly.Monomial.t * int -> T.testT
		= fun (nm, m, m', er) -> fun state ->
		let ar = Poly.Monomial.compare m m' in
		T.equals nm string_of_int (=) er ar state
		   in
		   let tcs : (string * Poly.Monomial.t * Poly.Monomial.t * int) list
		=
		Poly.Monomial.([
						 "equal_syn", canon ([x; y], Coeff.mk ~-2 3), canon ([x; y], Coeff.mk ~-2 3), 0;
						 "equal", canon ([x; y], Coeff.mk ~-2 3), canon ([x; y], Coeff.mk1 12), 0;
						 "neq", canon ([x; y], Coeff.mk ~-2 3), canon ([y], Coeff.mk ~-2 3), -1;
		])
		   in
		   T.suite "compare_monomial" (List.map chk tcs)

	  let equal_ts : T.testT
		 = let chk : string * Poly.Monomial.t * Poly.Monomial.t * bool -> T.testT
		= fun (nm, m, m', er) state ->
		let ar = Poly.Monomial.equal m m' in
		T.equals nm string_of_bool (=) er ar state
		   in
		   let tcs : (string * Poly.Monomial.t * Poly.Monomial.t * bool) list
		=
		Poly.Monomial.([
						 "equal", canon ([x; y], Coeff.mk ~-2 3), canon ([x; y], Coeff.mk ~-2 3), true;
						 "neq0", canon ([x; y], Coeff.mk ~-2 3), canon ([x; y], Coeff.mk1 12), false;
						 "neq1", canon ([x; y], Coeff.mk ~-2 3), canon ([y], Coeff.mk ~-2 3), false;
		])
		   in
		   T.suite "Poly.Monomial.equal" (List.map chk tcs)

	  let ts : T.testT
		 = [
		 canonTs;
		 compare_ts;
		 equal_ts
	  ] |> T.suite "Monomial"

	end

	module Poly_t
	  = struct

	  let canonTs : T.testT
		 = let eq : Poly.t -> Poly.t -> bool
		= fun p p' ->
		if List.length p <> List.length p' then false
		else List.for_all2 Poly.Monomial.equal p p'
		   in
		   let chkResult : string * Poly.t * Poly.t -> T.testT
		= fun (nm, p, ep) state ->
		let ap = Poly.canon p in
		T.equals nm Poly.to_string  eq ep ap state
		   in
		   let chkInvariant : string * Poly.t * Poly.t -> T.testT
		= fun (nm, p, _) state ->
		let am = Poly.canon p in
		if Poly.Invariant.check am
		then T.succeed state
		else let e = Printf.sprintf "%s: chkInvariant" nm
			in T.fail nm e state
		   in
		   [
		"empty0", [], [[], Coeff.z];
		"empty1", [[x], Coeff.z], [[], Coeff.z];
		"empty2", [[x], Coeff.z], [[], Coeff.z];
		"const0", [[], Coeff.u], [[], Coeff.u];
		"const1", [[x], Coeff.u], [[x], Coeff.u];
		"const_add0", [[], Coeff.u; [], Coeff.u], [[], Coeff.mk1 2];
		"const_cancel", [[], Coeff.u; [], Coeff.negU], [[], Coeff.z];
		"mono0", [[x], Coeff.u], [[x], Coeff.u];
		"mono1", [[], Coeff.u; [x], Coeff.u], [[], Coeff.u; [x], Coeff.u];
		"mono_unsorted", [[x], Coeff.u; [], Coeff.mk1 2], [[], Coeff.mk1 2; [x], Coeff.u];
		"dup0", [[x], Coeff.u; [x], Coeff.mk1 2], [[x], Coeff.mk1 3];
		"dup1", [[x; y], Coeff.u; [y; x], Coeff.u], [[x; y], Coeff.mk1 2];
		"dup_cancel", [[x], Coeff.u; [y], Coeff.u; [x], Coeff.negU], [[y], Coeff.u]
		   ]
		   |> fun tcs -> List.map chkResult tcs @ List.map chkInvariant tcs
				 |> T.suite "canonPoly_imp"

	  let equal_ts : T.testT
		 = let chk : string * Poly.t * Poly.t * bool -> T.testT
		= fun (nm, p, p', er) state ->
		let ar = Poly.equal p p' in
		T.equals nm string_of_bool (=) er ar state
		   in
		   let tcs : (string * Poly.t * Poly.t * bool) list
		= [
		"equal", Poly.of_string "-2/3*x1*x2", Poly.of_string "-2/3*x1*x2", true;
		"neq0", Poly.of_string "-2/3*x1*x2", Poly.of_string "12*x1*x2", false;
		"neq1", Poly.of_string "-2/3*x1*x2", Poly.of_string "-2/3*x2", false
		   ] in
		   T.suite "Poly.equal" (List.map chk tcs)

	  let is_constant_ts : T.testT
		 = let chk : string * Poly.t * bool -> T.testT
		= fun (nm, p, er) state ->
		let ar = Poly.is_constant p in
		T.equals nm Pervasives.string_of_bool (=) er ar state
		   in
		   let tcs : (string * Poly.t * bool) list
		= [
		"constant", Poly.of_string "-2/3", true;
		"empty", Poly.of_string "", true;
		"mono0", Poly.of_string "-2/3*x2", false;
		"mono1", Poly.of_string "x1", false
		   ] in
		   T.suite "is_constant" (List.map chk tcs)

	  let is_zero_ts : T.testT
		 = let chk : string * Poly.t * bool -> T.testT
		= fun (nm, p, er) state ->
		let ar = Poly.isZ p in
		T.equals nm Pervasives.string_of_bool (=) er ar state
		   in
		   let tcs : (string * Poly.t * bool) list
		= [
		"constant", Poly.of_string "-2/3", false;
		"empty", Poly.of_string "", true;
		"mono0", Poly.of_string "-2/3*x2", false;
		"mono1", Poly.of_string "x1", false
		   ] in
		   T.suite "is_constant" (List.map chk tcs)
		   
	  let get_constant_ts : T.testT
		 = let chk : string * Poly.t * Coeff.t -> T.testT
		= fun (nm, p, ec) state ->
		let ac = Poly.get_constant p in
		T.equals nm Coeff.to_string Coeff.equal ec ac state
		   in
		   let tcs : (string * Poly.t * Coeff.t) list
		= [
		"empty_basis", Poly.of_string "-2/3", Coeff.of_string "-2/3";
		"regular", Poly.of_string "-2/3", Coeff.of_string "-2/3";
		"empty", Poly.of_string "", Coeff.z;
		"poly", Poly.of_string "-1*x1+4*x4+-2", Coeff.mk1 (-2);
		"mono", Poly.of_string "x1", Coeff.z
		   ] in
		   T.suite "get_constant" (List.map chk tcs)

	  let add_ts : T.testT
		 = let chk : string * Poly.t * Poly.t * Poly.t -> T.testT
		= fun (nm, p, p', ep) state ->
		let ap = Poly.add p p' in
		T.equals nm Poly.to_string  Poly.equal ep ap state
		   in
		   let tcs : (string * Poly.t * Poly.t * Poly.t) list
		= [
		"same_varset",
		Poly.of_string "-2/3*x1*x2",
		Poly.of_string "-2/3*x1*x2",
		Poly.of_string "-4/3*x1*x2";
		"diff_varset",
		Poly.of_string "x1*x3+-1*x2*x1*x3+-3*x1",
		Poly.of_string "-1*x1+4*x4",
		Poly.of_string "x1*x3+-1*x2*x1*x3+-4*x1+4*x4";
		"vanish",
		Poly.of_string "-2/3*x1*x2",
		Poly.of_string "2/3*x1*x2",
		Poly.of_string ""
		   ] in
		   T.suite "polynomial_add" (List.map chk tcs)

	  let mult_ts : T.testT
		 = let chk : string * Poly.t * Poly.t * Poly.t -> T.testT
		= fun (nm, p, p', ep) state ->
		let ap = Poly.mul p p' in
		T.equals nm Poly.to_string  Poly.equal ep ap state
		   in
		   let tcs : (string * Poly.t * Poly.t * Poly.t) list
		= [
		"same_varset",
		Poly.of_string "-2/3*x1*x2",
		Poly.of_string "-2/3*x1*x2",
		Poly.of_string "4/9*x1*x2*x1*x2";
		"diff_varset",
		Poly.of_string "x1*x3+-1*x2*x1*x3+-3*x1",
		Poly.of_string "-1*x1+4*x4+-2",
		Poly.of_string "-1*x1*x3*x1+x2*x1*x1*x3+3*x1*x1+4*x4*x1*x3+-4*x2*x1*x3*x4+-3*4*x1*x4+-2*x1*x3+2*x2*x1*x3+-2*-3*x1";
		"vanish",
		Poly.of_string "-2/3*x2*x2",
		Poly.of_string "",
		Poly.of_string ""
		   ] in
		   T.suite "polynomial_mult" (List.map chk tcs)

	  let monomial_coefficient_ts : T.testT
		 = let chk : string * Poly.t * Poly.MonomialBasis.t * Coeff.t -> T.testT
		= fun (nm, p, mb, ec) state ->
		let ac = Poly.monomial_coefficient p mb in
		T.equals nm Coeff.to_string Coeff.equal ec ac state
		   in
		   let tcs : (string * Poly.t * Poly.MonomialBasis.t * Coeff.t) list
		= [
		"missing", Poly.of_string "-2/3*x1*x2", [x], Coeff.z;
		"found", Poly.of_string "-2/3*x1*x2", [x;y], Coeff.of_string "-2/3"
		   ] in
		   T.suite "monomial_coefficient" (List.map chk tcs)

	  let monomial_coefficient_other_ts : T.testT
		 = let chk : string * Poly.t * Poly.MonomialBasis.t * Poly.t -> T.testT
		= fun (nm, p, mb, ep) state ->
		let ap = Poly.monomial_coefficient_poly p mb in
		T.equals nm Poly.to_string  Poly.equal ep ap state
		   in
		   let tcs : (string * Poly.t * Poly.MonomialBasis.t * Poly.t) list
		= [
		"full", Poly.of_string "-2/3*x1*x2", [x], [([y],Coeff.of_string "-2/3")];
		"partial", Poly.of_string "-2/3*x1*x2", [x;y], [([],Coeff.of_string "-2/3")]
		   ] in
		   T.suite "monomial_coefficient_other" (List.map chk tcs)

	  let is_affine_ts : T.testT
		 = let chk : string * bool * Poly.t -> T.testT
		= fun (nm, r, p) state ->
		let r' = Poly.is_affine p in
		T.equals nm Pervasives.string_of_bool (=) r r' state
		   in
		[
		  "zero", true, Poly.mk_cste [] Coeff.z;
		  "constant", true, Poly.mk_cste [] (Coeff.mk1 2);
		  "linear1", true, Poly.mk_cste [[x], Coeff.u] Coeff.z;
		  "linear2", true, Poly.mk_cste [[x], Coeff.u; [y], Coeff.mk1 3] Coeff.z;
		  "affine", true, Poly.mk_cste [[x], Coeff.negU] Coeff.u;
		  "affine2", true, Poly.mk_cste [[x], Coeff.negU; [z],Coeff.u] Coeff.u;
		  "multi", false, Poly.mk_cste [[x; y], Coeff.u] Coeff.z;
		  "mixed", false, Poly.mk_cste [[y; z], Coeff.mk1 2; [x], Coeff.u] Coeff.z
		   ]
		   |> List.map chk
		   |> T.suite "is_affine"
		
	let of_string_ts : T.testT
    = let chk : string * string * Poly.t -> T.testT
	= fun (nm, s, ep) state ->
	let ap = Poly.of_string s in
	T.equals nm Poly.to_string  Poly.equal ep ap state
      in
      let tcs : (string * string * Poly.t) list
	= Poly.
	  ([
	    "empty", "", mk_cste [] Coeff.z;
	    "constant", "3", mk_cste [] (Coeff.mk1 3);
	    "linear", "x1", mk_cste [[x], Coeff.u] Coeff.z;
	    "product", "x1*x2", mk_cste [[x; y], Coeff.u] Coeff.z;
	    "affine", "-1 * x1 + 1", mk_cste [[x], Coeff.negU] Coeff.u;
	    "simple", "-2/3*x2*x1", mk_cste [[x; y], Coeff.mk 3 ~-2] Coeff.z;
	  ]) in
      T.suite "of_string" (List.map chk tcs)
  
	let toCstr_ts : T.testT
		= let chk : string * Poly.t * (Vec.t * Coeff.t) -> T.testT
			= fun (nm, p, (ev,ec)) state ->
			let (v,c) = Poly.toCstr p in
			T.equals nm 
			(fun (v,c) -> Printf.sprintf "(%s,%s)" (Vec.to_string V.to_string v) (Coeff.to_string c))
			(fun (v1,c1) (v2,c2)-> Vec.equal v1 v2 && Coeff.equal c1 c2) (ev,ec) (v,c) state
		in
		let var = Poly.V.fromInt in
		let tcs : (string * Poly.t * (Vec.t * Coeff.t)) list
		= Poly.([
	  		"nil",
	  		Poly.z,
	  		(Vec.nil, Coeff.z)
	  	;
	  		"linear polynomial",
	  		Poly.of_string "x1 + 2*x3 + -2*x2",
	  		(Vec.mk [(Coeff.u, var 1) ; (Coeff.of_string "-2", var 2) ;  (Coeff.of_string "2", var 3)],
	  		Coeff.z)
	  	;
	  		"constant polynomial",
	  		Poly.cste (Coeff.of_string "2/3"),
	  		(Vec.nil, Coeff.of_string "2/3")
	  	;
	  		"affine polynomial",
	  		Poly.of_string "x1 + 2*x3 + -2/3 + -2*x2",
	  		(Vec.mk [(Coeff.u, var 1) ; (Coeff.of_string "-2", var 2) ;  (Coeff.of_string "2", var 3)],
	  		Coeff.of_string "-2/3")
	  	]) 
		in
		T.suite "toCstr" (List.map chk tcs)
	
	let ofCstr_ts : T.testT
		= let chk : string * Poly.t * (Vec.t * Coeff.t) -> T.testT
			= fun (nm, ep, (v,c)) state ->
			let ap = Poly.ofCstr v c in
			T.equals nm Poly.to_string Poly.equal ep ap state
		in
		let var = Poly.V.fromInt in
		let tcs : (string * Poly.t * (Vec.t * Coeff.t)) list
		= Poly.([
	  		"nil",
	  		Poly.z,
	  		(Vec.nil, Coeff.z)
	  	;
	  		"linear polynomial",
	  		Poly.of_string "x1 + 2*x3 + -2*x2",
	  		(Vec.mk [(Coeff.u, var 1) ; (Coeff.of_string "-2", var 2) ;  (Coeff.of_string "2", var 3)],
	  		Coeff.z)
	  	;
	  		"constant polynomial",
	  		Poly.cste (Coeff.of_string "2/3"),
	  		(Vec.nil, Coeff.of_string "2/3")
	  	;
	  		"affine polynomial",
	  		Poly.of_string "x1 + 2*x3 + -2/3 + -2*x2",
	  		(Vec.mk [(Coeff.u, var 1) ; (Coeff.of_string "-2", var 2) ;  (Coeff.of_string "2", var 3)],
	  		Coeff.of_string "-2/3")
	  	]) 
		in
		T.suite "ofCstr" (List.map chk tcs)
 
	  let ts : T.testT
		 = [
		 canonTs;
		 equal_ts;
		 is_constant_ts;
		 is_zero_ts;
		 get_constant_ts;
		 add_ts;
		 mult_ts;
		 monomial_coefficient_ts;
		 monomial_coefficient_other_ts;
		 is_affine_ts;
		 of_string_ts;
		 toCstr_ts;
		 ofCstr_ts;
	  ] |> T.suite "Poly"

	end

	let ts : T.testT
	  = T.suite (Coeff.name ^ ":" ^ V.name) [
			  Invariant_t.ts;
			  Monomial_t.ts;
			  Poly_t.ts;
			]
end

module Rat_Int = Test(Vector.Rat.Int)

module Rat_Pos = Test(Vector.Rat.Positive)

let ts : T.testT
	  = T.suite "Poly" [
			  Rat_Int.ts;
			  Rat_Pos.ts
			]
