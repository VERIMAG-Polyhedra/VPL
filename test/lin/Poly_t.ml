open Vpl

module Make_Tests (Vec : Vector.Type) = struct

	module Poly = Poly.Make(Vec)
	module Coeff = Poly.Coeff

	let x = Var.fromInt 1
	let y = Var.fromInt 2
	let z = Var.fromInt 3

    let mk_mb = Poly.MonomialBasis.mk
    let mk_m = Poly.Monomial.mk_list
    let mk_p = Poly.mk_list

	module Invariant_t = struct

        module Monomial_t = struct

            let ts : Test.t
                = fun () ->
                let chk (nm, a, m) = fun state ->
                    try
                        if Poly.Invariant.Monom.check m = a
                        then Test.succeed state
                        else let e = Printf.sprintf "%s: expected %B for %s" nm a (Poly.Monomial.to_string m) in
                        Test.fail nm e state
                    with Invalid_argument _ ->
                        if not a
                        then Test.succeed state
                        else let e = Printf.sprintf "%s: expected %B, got exception" nm a in
                        Test.fail nm e state
                in
                let tcs = [
                    "zero1", false, ([x,1], Coeff.z);
                    "zero2", false, ([x,1; y,1], Coeff.z);
                    "zero_const0", false, ([], Coeff.z);
                    "zero_const1", false, ([x,1], Coeff.z);
                    "const0", true, ([], Coeff.u);
                    "const1", true, ([x,1], Coeff.u);
                    "const_mixed0", true, ([x,2], Coeff.u);
                    "const_mixed1", true, ([x,2], Coeff.u);
                    "sorted0", true, ([x,1; y,1], Coeff.u);
                    "sorted1", false, ([y,1; x,1], Coeff.u);
                    "sorted2", true, ([x,1; y,1; z,1], Coeff.u);
                    "sorted3", false, ([x,1; z,1; y,1], Coeff.u);
                    "sorted4", false, ([z,1; y,1; x,1], Coeff.u);
                    "sorted5", false, ([y,1; z,1; x,1], Coeff.u);
                    "multi0", true, ([x,2], Coeff.u);
                    "multi1", true, ([x,1; y,2], Coeff.u);
                    "multi2", false, ([x,1; y,1; x,1], Coeff.u);
                ] in
                List.map chk tcs |> Test.suite "Monomial"
        end

        let ts : Test.t
            = fun () ->
            let chk : string * bool * Poly.t -> (Test.stateT -> Test.stateT)
                = fun (nm, a, p) -> fun state ->
                if Poly.Invariant.check p = a
                then Test.succeed state
                else let e = Printf.sprintf "%s: expected %B for %s" nm a (Poly.to_string p)
                in Test.fail nm e state
            in
            let tcs : (string * bool * Poly.t) list = [
                "empty", true, [[], Coeff.u];
                "const1", true, [[x,1], Coeff.u];
                "const2", true, [[], Coeff.u; [x,1], Coeff.u];
                "sorted0", true, [[x,1], Coeff.u; [y,1], Coeff.u];
                "sorted1", false, [[y,1], Coeff.u; [x,1], Coeff.u];
                "sorted2", true, [[x,1], Coeff.u; [y,1], Coeff.u];
                "sorted3", true, [[x,1], Coeff.u; [x,1; y,1], Coeff.u];
                "sorted4", false, [[x,1], Coeff.u; [], Coeff.u];
                "dup0", false, [[x,1], Coeff.u; [x,1], Coeff.u];
                "dup1", false, [[x,1], Coeff.u; [y,1], Coeff.u; [x,1], Coeff.u];
            ] in
            Monomial_t.ts() :: List.map chk tcs |> Test.suite "Invariant"

	end

	module Monomial_t = struct

        let canonTs : Test.t
            = fun () ->
            let eq : Poly.Monomial.t option -> Poly.Monomial.t option -> bool
                = fun om om' ->
                match om, om' with
                | None, Some _ | Some _, None -> false
                | None, None -> true
                | Some m, Some m' -> Poly.Monomial.equal m m'
            in
            let chkResult : string * Poly.Monomial.t * Poly.Monomial.t option -> (Test.stateT -> Test.stateT)
                = fun (nm, m, em) state ->
                let am = Poly.Monomial.canonO m in
                if eq em am
                then Test.succeed state
                else Test.fail nm "" state
            in
            let chkInvariant : string * Poly.Monomial.t * Poly.Monomial.t option -> (Test.stateT -> Test.stateT)
                = fun (nm, m, _) state ->
                match Poly.Monomial.canonO m with
                | None -> Test.succeed state
                | Some am ->
                    if Poly.Invariant.Monom.check am
                    then Test.succeed state
                    else let e = Printf.sprintf "%s: chkInvariant" nm in
                    Test.fail nm e state
            in
            [
                "const0", mk_m [] Coeff.u, Some (mk_m [] Coeff.u);
                "mono0", mk_m [x,1] Coeff.u, Some (mk_m [x,1] Coeff.u);
                "mono1", mk_m [y,1] Coeff.u, Some (mk_m [y,1] Coeff.u);
                "mono2", mk_m [x,1] (Coeff.of_int 2), Some (mk_m [x,1] (Coeff.of_int 2));
                "multi0", mk_m [x,1; y,1] Coeff.u, Some (mk_m [x,1; y,1] Coeff.u);
                "multi1", mk_m [y,1; x,1] Coeff.u, Some (mk_m [x,1; y,1] Coeff.u);
            ]
            |> fun tcs -> List.map chkResult tcs @ List.map chkInvariant tcs
            |> Test.suite "canonMonomial"

        let compare_ts : Test.t
            = fun () ->
            let chk : string * Poly.Monomial.t * Poly.Monomial.t * int -> (Test.stateT -> Test.stateT)
                = fun (nm, m, m', er) -> fun state ->
                let ar = Poly.Monomial.compare m m' in
                Test.equals nm string_of_int (=) er ar state
            in
            let tcs : (string * Poly.Monomial.t * Poly.Monomial.t * int) list =
                Poly.Monomial.([
                "equal_syn", canon (mk_m [x,1; y,1] (Coeff.mk ~-2 3)), canon (mk_m [x,1; y,1] (Coeff.mk ~-2 3)), 0;
                "neq", canon (mk_m [x,1; y,1] (Coeff.mk ~-2 3)), canon (mk_m [y,1] (Coeff.mk ~-2 3)), -1;
                ])
            in
            Test.suite "compare_monomial" (List.map chk tcs)

        let equal_ts : Test.t
            = fun () ->
            let chk : string * Poly.Monomial.t * Poly.Monomial.t * bool -> (Test.stateT -> Test.stateT)
                = fun (nm, m, m', er) state ->
                let ar = Poly.Monomial.equal m m' in
                Test.equals nm string_of_bool (=) er ar state
            in
            let tcs : (string * Poly.Monomial.t * Poly.Monomial.t * bool) list =
                Poly.Monomial.([
                "equal", canon (mk_m [x,1; y,1] (Coeff.mk ~-2 3)), canon (mk_m [x,1; y,1] (Coeff.mk ~-2 3)), true;
                "neq0", canon (mk_m [x,1; y,1] (Coeff.mk ~-2 3)), canon (mk_m [x,1; y,1] (Coeff.of_int 12)), false;
                "neq1", canon (mk_m [x,1; y,1] (Coeff.mk ~-2 3)), canon (mk_m [y,1] (Coeff.mk ~-2 3)), false;
                ])
            in
            Test.suite "Poly.Monomial.equal" (List.map chk tcs)

        let mul_ts : Test.t
            = fun () ->
            let chk : string * Poly.Monomial.t * Poly.Monomial.t * Poly.Monomial.t -> (Test.stateT -> Test.stateT)
            = fun (nm, m, m', ep) state ->
            let ap = Poly.Monomial.mul m m' in
            Test.equals nm Poly.Monomial.to_string Poly.Monomial.equal ep ap state
            in
            let tcs : (string * Poly.Monomial.t * Poly.Monomial.t * Poly.Monomial.t) list = [
                "same_varset",
                (mk_m [x,1 ; y,1] (Coeff.mk ~-2 3)),
                (mk_m [x,1 ; y,1] (Coeff.mk ~-2 3)),
                (mk_m [x,2 ; y,2] (Coeff.mk 4 9))
            ;
                "different_varset",
                (mk_m [x,2 ; z,3] (Coeff.mk ~-2 3)),
                (mk_m [y,4] (Coeff.mk 4 5)),
                (mk_m [x,2 ; y,4 ; z, 3] (Coeff.mk ~-8 15))
            ;
                "some_common_varset",
                (mk_m [x,2 ; y,1 ; z,3] (Coeff.mk ~-2 3)),
                (mk_m [y,2] (Coeff.mk 4 5)),
                (mk_m [x,2 ; y,3 ; z, 3] (Coeff.mk ~-8 15))
            ] in
            Test.suite "monomial_mul" (List.map chk tcs)

        let ts : Test.t
            = fun () -> [
                canonTs();
                compare_ts();
                equal_ts();
                mul_ts()
            ]
            |> Test.suite "Monomial"
	end

	module Poly_t = struct

        let canonTs : Test.t
            = fun () ->
            let eq : Poly.t -> Poly.t -> bool
                = fun p p' ->
                if List.length p <> List.length p' then false
                else List.for_all2 Poly.Monomial.equal p p'
            in
            let chkResult : string * Poly.t * Poly.t -> (Test.stateT -> Test.stateT)
                = fun (nm, p, ep) state ->
                let ap = Poly.canon p in
                Test.equals nm Poly.to_string  eq ep ap state
            in
            let chkInvariant : string * Poly.t * Poly.t -> (Test.stateT -> Test.stateT)
                = fun (nm, p, _) state ->
                let am = Poly.canon p in
                if Poly.Invariant.check am
                then Test.succeed state
                else let e = Printf.sprintf "%s: chkInvariant" nm
                in Test.fail nm e state
            in [
            "empty0", [], [[], Coeff.z];
            "empty1", [[x,1], Coeff.z], [[], Coeff.z];
            "empty2", [[x,1], Coeff.z], [[], Coeff.z];
            "const0", [[], Coeff.u], [[], Coeff.u];
            "const1", [[x,1], Coeff.u], [[x,1], Coeff.u];
            "const_add0", [[], Coeff.u; [], Coeff.u], [[], Coeff.of_int 2];
            "const_cancel", [[], Coeff.u; [], Coeff.negU], [[], Coeff.z];
            "mono0", [[x,1], Coeff.u], [[x,1], Coeff.u];
            "mono1", [[], Coeff.u; [x,1], Coeff.u], [[], Coeff.u; [x,1], Coeff.u];
            "mono_unsorted", [[x,1], Coeff.u; [], Coeff.of_int 2], [[], Coeff.of_int 2; [x,1], Coeff.u];
            "dup0", [[x,1], Coeff.u; [x,1], Coeff.of_int 2], [[x,1], Coeff.of_int 3];
            "dup1", [[x,1; y,1], Coeff.u; [y,1; x,1], Coeff.u], [[x,1; y,1], Coeff.of_int 2];
            "dup_cancel", [[x,1], Coeff.u; [y,1], Coeff.u; [x,1], Coeff.negU], [[y,1], Coeff.u]
            ]
            |> List.map (fun (nm,p1,p2) -> (nm, mk_p p1, mk_p p2))
            |> fun tcs -> List.map chkResult tcs @ List.map chkInvariant tcs
            |> Test.suite "canonPoly_imp"

        let equal_ts : Test.t
            = fun () ->
            let chk : string * Poly.t * Poly.t * bool -> (Test.stateT -> Test.stateT)
                = fun (nm, p, p', er) state ->
                let ar = Poly.equal p p' in
                if er = ar
                then Test.succeed state
                else let s_error = Printf.sprintf "equal %s %s gave %B instead of %B"
                    (Poly.to_string p)
                    (Poly.to_string p')
                    ar er
                    in
                    Test.fail nm s_error state
            in
            let tcs : (string * Poly.t * Poly.t * bool) list = [
                "equal", Poly.of_string "-2/3*x1*x2", Poly.of_string "-2/3*x1*x2", true;
                "neq0", Poly.of_string "-2/3*x1*x2", Poly.of_string "12*x1*x2", false;
                "neq1", Poly.of_string "-2/3*x1*x2", Poly.of_string "-2/3*x2", false
                ]
            in
            Test.suite "Poly.equal" (List.map chk tcs)

        let is_constant_ts : Test.t
            = fun () ->
            let chk : string * Poly.t * bool -> (Test.stateT -> Test.stateT)
                = fun (nm, p, er) state ->
                let ar = Poly.is_constant p in
                Test.equals nm Stdlib.string_of_bool (=) er ar state
            in
            let tcs : (string * Poly.t * bool) list = [
                "constant", Poly.of_string "-2/3", true;
                "empty", Poly.of_string "", true;
                "mono0", Poly.of_string "-2/3*x2", false;
                "mono1", Poly.of_string "x1", false
            ] in
            Test.suite "is_constant" (List.map chk tcs)

        let is_zero_ts : Test.t
            = fun () ->
            let chk : string * Poly.t * bool -> (Test.stateT -> Test.stateT)
                = fun (nm, p, er) state ->
                let ar = Poly.isZ p in
                Test.equals nm Stdlib.string_of_bool (=) er ar state
            in
            let tcs : (string * Poly.t * bool) list = [
                "constant", Poly.of_string "-2/3", false;
                "empty", Poly.of_string "", true;
                "mono0", Poly.of_string "-2/3*x2", false;
                "mono1", Poly.of_string "x1", false
            ] in
            Test.suite "is_constant" (List.map chk tcs)

        let monomial_coefficient_ts : Test.t
            = fun () ->
            let chk : string * Poly.t * Poly.MonomialBasis.t * Coeff.t -> (Test.stateT -> Test.stateT)
                = fun (nm, p, mb, ec) state ->
                let ac = Poly.monomial_coefficient p mb in
                Test.equals nm Coeff.to_string Coeff.equal ec ac state
            in
            let tcs : (string * Poly.t * Poly.MonomialBasis.t * Coeff.t) list = [
                "missing", Poly.of_string "-2/3*x1*x2", mk_mb [x,1], Coeff.z;
                "found", Poly.of_string "-2/3*x1*x2", mk_mb [x,1;y,1], Coeff.of_string "-2/3"
            ] in
            Test.suite "monomial_coefficient" (List.map chk tcs)

        let get_constant_ts : Test.t
            = fun () ->
            let chk : string * Poly.t * Coeff.t -> (Test.stateT -> Test.stateT)
                = fun (nm, p, ec) state ->
                let ac = Poly.get_constant p in
                Test.equals nm Coeff.to_string Coeff.equal ec ac state
            in
            let tcs : (string * Poly.t * Coeff.t) list = [
                "empty_basis", Poly.of_string "-2/3", Coeff.of_string "-2/3";
                "regular", Poly.of_string "-2/3", Coeff.of_string "-2/3";
                "empty", Poly.of_string "", Coeff.z;
                "poly", Poly.of_string "-1*x1+4*x4+-2", Coeff.of_int (-2);
                "mono", Poly.of_string "x1", Coeff.z
            ] in
            Test.suite "get_constant" (List.map chk tcs)

        let add_ts : Test.t
            = fun () ->
            let chk : string * Poly.t * Poly.t * Poly.t -> (Test.stateT -> Test.stateT)
                = fun (nm, p, p', ep) state ->
                let ap = Poly.add p p' in
                Test.equals nm Poly.to_string  Poly.equal ep ap state
            in
            let tcs : (string * Poly.t * Poly.t * Poly.t) list = [
                "same_varset",
                Poly.of_string "-2/3*x1*x2",
                Poly.of_string "-2/3*x1*x2",
                Poly.of_string "-4/3*x1*x2"
            ;
                "diff_varset",
                Poly.of_string "x1*x3+-1*x2*x1*x3+-3*x1",
                Poly.of_string "-1*x1+4*x4",
                Poly.of_string "x1*x3+-1*x2*x1*x3+-4*x1+4*x4"
            ;
                "vanish",
                Poly.of_string "-2/3*x1*x2",
                Poly.of_string "2/3*x1*x2",
                Poly.of_string ""
            ] in
            Test.suite "add" (List.map chk tcs)

        let mul_ts : Test.t
            = fun () ->
            let chk : string * Poly.t * Poly.t * Poly.t -> (Test.stateT -> Test.stateT)
                = fun (nm, p, p', ep) state ->
                let ap = Poly.mul p p' in
                Test.equals nm Poly.to_string  Poly.equal ep ap state
            in
            let tcs : (string * Poly.t * Poly.t * Poly.t) list = [
                "same_varset",
                Poly.of_string "-2/3*x1*x2",
                Poly.of_string "-2/3*x1*x2",
                Poly.of_string "4/9*x1*x2*x1*x2"
            ;
                "diff_varset",
                Poly.of_string "x1*x3+-1*x2*x1*x3+-3*x1",
                Poly.of_string "-1*x1+4*x4+-2",
                Poly.of_string "-1*x1*x3*x1+x2*x1*x1*x3+3*x1*x1+4*x4*x1*x3+-4*x2*x1*x3*x4+-3*4*x1*x4+-2*x1*x3+2*x2*x1*x3+-2*-3*x1"
            ;
                "vanish",
                Poly.of_string "-2/3*x2*x2",
                Poly.of_string "",
                Poly.of_string ""
            ] in
            Test.suite "mul" (List.map chk tcs)

        let monomial_coefficient_other_ts : Test.t
            = fun () ->
            let chk : string * Poly.t * Poly.MonomialBasis.t * Poly.t -> (Test.stateT -> Test.stateT)
                = fun (nm, p, mb, ep) state ->
                let ap = Poly.monomial_coefficient_poly p mb in
                Test.equals nm Poly.to_string  Poly.equal ep ap state
            in
            let tcs : (string * Poly.t * Poly.MonomialBasis.t * Poly.t) list = [
                "full", Poly.of_string "-2/3*x1*x2", mk_mb [x,1], mk_p [([y,1],Coeff.of_string "-2/3")];
                "partial", Poly.of_string "-2/3*x1*x2", mk_mb [x,1;y,1], mk_p [([],Coeff.of_string "-2/3")]
            ] in
            Test.suite "monomial_coefficient_other" (List.map chk tcs)

        let is_linear_ts : Test.t
            = fun () ->
            let chk : string * bool * Poly.t -> (Test.stateT -> Test.stateT)
                = fun (nm, r, p) state ->
                let r' = Poly.is_linear p in
                Test.equals nm Stdlib.string_of_bool (=) r r' state
            in [
                "zero", true, Poly.add_cste [] Coeff.z;
                "constant", true, Poly.add_cste [] (Coeff.of_int 2);
                "linear1", true, Poly.add_cste (mk_p [[x,1], Coeff.u]) Coeff.z;
                "linear2", true, Poly.add_cste (mk_p [[x,1], Coeff.u; [y,1], Coeff.of_int 3]) Coeff.z;
                "affine", true, Poly.add_cste (mk_p [[x,1], Coeff.negU]) Coeff.u;
                "affine2", true, Poly.add_cste (mk_p [[x,1], Coeff.negU; [z,1],Coeff.u]) Coeff.u;
                "multi", false, Poly.add_cste (mk_p [[x,1; y,1], Coeff.u]) Coeff.z;
                "mixed", false, Poly.add_cste (mk_p [[y,1; z,1], Coeff.of_int 2; [x,1], Coeff.u]) Coeff.z
            ]
            |> List.map chk
            |> Test.suite "is_linear"

        let of_string_ts : Test.t
            = fun () ->
            let chk : string * string * Poly.t -> (Test.stateT -> Test.stateT)
                = fun (nm, s, ep) state ->
                let ap = Poly.of_string s in
                Test.equals nm Poly.to_string  Poly.equal ep ap state
            in
            let tcs : (string * string * Poly.t) list = Poly.([
                "empty", "", add_cste [] Coeff.z;
                "constant", "3", add_cste [] (Coeff.of_int 3);
                "linear", "x1", add_cste (mk_p [[x,1], Coeff.u]) Coeff.z;
                "product", "x1*x2", add_cste (mk_p [[x,1; y,1], Coeff.u]) Coeff.z;
                "affine", "-1 * x1 + 1", add_cste (mk_p [[x,1], Coeff.negU]) Coeff.u;
                "simple", "-2/3*x2*x1", add_cste (mk_p [[x,1; y,1], Coeff.mk 3 ~-2]) Coeff.z;
            ]) in
            Test.suite "of_string" (List.map chk tcs)

        let toCstr_ts : Test.t
            = fun () ->
            let chk : string * Poly.t * (Vec.t * Coeff.t) -> (Test.stateT -> Test.stateT)
                = fun (nm, p, (ev,ec)) state ->
                let (v,c) = Poly.toCstr p in
                Test.equals nm (fun (v,c) ->
                    Printf.sprintf "(%s,%s)" (Vec.to_string Var.to_string v) (Coeff.to_string c))
                    (fun (v1,c1) (v2,c2)-> Vec.equal v1 v2 && Coeff.equal c1 c2) (ev,ec) (v,c) state
            in
            let var = Var.fromInt in
            let tcs : (string * Poly.t * (Vec.t * Coeff.t)) list = Poly.([
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
            Test.suite "toCstr" (List.map chk tcs)

	let ofCstr_ts : Test.t
		= fun () ->
        let chk : string * Poly.t * (Vec.t * Coeff.t) -> (Test.stateT -> Test.stateT)
			= fun (nm, ep, (v,c)) state ->
			let ap = Poly.ofCstr v c in
			Test.equals nm Poly.to_string Poly.equal ep ap state
		in
		let var = Var.fromInt in
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
		Test.suite "ofCstr" (List.map chk tcs)

        let ts : Test.t
            = fun () ->
            List.map Test.run [
                canonTs;
                equal_ts;
                is_constant_ts;
                is_zero_ts;
                get_constant_ts;
                monomial_coefficient_ts;
                add_ts;
                mul_ts;
                monomial_coefficient_other_ts;
                is_linear_ts;
                of_string_ts;
                toCstr_ts;
                ofCstr_ts;
            ] |> Test.suite "Poly"

	end

	let ts : Test.t
	  = fun () ->
      Test.suite (Coeff.name) [
			  Invariant_t.ts();
			  Monomial_t.ts();
			  Poly_t.ts();
			]
end

module Rat_Pos = Make_Tests(Vector.Rat)

let ts : Test.t
    = fun () ->
    Test.suite "Polynomials" [
     Rat_Pos.ts()
	]
