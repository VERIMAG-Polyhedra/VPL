open Vpl

module Test (Cs : Cstr.Type) = struct
	let x = Cs.Vec.V.fromInt 1
	let y = Cs.Vec.V.fromInt 2
	let z = Cs.Vec.V.fromInt 3

	let varPr: Cs.Vec.V.t -> string
	= fun _x ->
		let vars: (Cs.Vec.V.t * string) list
		= [x, "x"; y, "y"; z, "z"]
		in
		try
			List.assoc _x vars
		with
		| Not_found -> "v" ^ (Cs.Vec.V.to_string _x)

	let mkc t v c =
		Cs.mk t (List.map (fun (i, v) -> (Cs.Vec.Coeff.mk1 i, v)) v) (Cs.Vec.Coeff.mk1 c)

	let eq = mkc Cstr.Eq
	let le = mkc Cstr.Le
	let lt = mkc Cstr.Lt

	let v iv = Cs.Vec.mk (List.map (fun (i, v) -> (Cs.Vec.Coeff.mk1 i, v)) iv)

	(* Cs.eval *)
	let evalTs: T.testT
	=
		let chk (name, c, p, r) = fun state ->
			if r = Cs.eval c p then
				T.succeed state
			else
				let err =
					if r then
						(Cs.Vec.to_string varPr p) ^ " does not satisfy " ^ (Cs.to_string varPr c)
					else
						(Cs.Vec.to_string varPr p) ^ " satisfies " ^ (Cs.to_string varPr c)
				in
				T.fail name err state
		in
		let tcs = [
			"unit_le0", le [1, x] 2, v [1, x], true;
			"unit_eq0", eq [1, x] 2, v [2, x], true;
			"le0", le [1, x] 2, v [1, x; 2, y], true;
			"eq0", eq [1, x] 2, v [2, x; 2, y], true;
		] in
		T.suite "eval" (List.map chk tcs)

	(* Cs.compl *)
	let complOkTs: T.testT
	=
		let chk (name, c, inpts, outpts) = fun state ->
			let cbar: Cs.t = Cs.compl c in
			let r_in = List.fold_left
				(fun r pt -> r && Cs.eval cbar pt) true inpts
			in
			let r_out = List.fold_left
				(fun r pt -> r && not (Cs.eval cbar pt)) true outpts
			in
			if r_in && r_out then
				T.succeed state
			else
				let err: string = (Cs.to_string varPr cbar) ^ ": " ^
					(if r_in then "in ok" else "bad in") ^ ", " ^
					(if r_out then "out ok" else "bad out")
				in
				T.fail name err state
		in
		let tcs = [
			"simple0", le [1, x] 1, [v [2, x]; v [2, x; 1, y]], [v [1, x]]
		] in
		T.suite "ok" (List.map chk tcs)

	let complKoTs: T.testT
	=
		let chk (name, c) = fun state ->
			try
				let _ = Cs.compl c in
				T.fail name "no exception raised" state
			with
			| Invalid_argument "Cstr.compl" -> T.succeed state
			| _ -> T.fail name "unexpected exception raised" state
		in
		let tcs = [
			"simple0", eq [1, x] 1
		] in
		T.suite "ko" (List.map chk tcs)

	let complTs: T.testT
	= T.suite "compl" [complOkTs; complKoTs]

	(* Cs.split *)
	let splitOkTs: T.testT
	=
		let chk (t, c) = fun state ->
			let (c1, c2) = Cs.split c in
			if Cs.equalSyn (Cs.add c1 c2) (le [] 0) then
				T.succeed state
			else
				let e = "c1: " ^ (Cs.to_string varPr c1) ^ "\nc2: " ^ (Cs.to_string varPr c2) in
				T.fail t e state
		in
		let tcs = [
			"0", eq [1, x] 1;
			"1", eq [3, x; 4, y] 2
		] in
		T.suite "ok" (List.map chk tcs)

	let splitKoTs: T.testT
	=
		let chk (name, c) = fun state ->
			try
				let _ = Cs.split c in
				T.fail name "no exception raised" state
			with
			| Invalid_argument "Cstr.split" -> T.succeed state
			| _ -> T.fail name "unexpected exception raised" state
		in
		let tcs = [
			"0", le [1, x] 0
		] in
		T.suite "ko" (List.map chk tcs)

	let splitTs: T.testT
	= T.suite "split" [splitOkTs; splitKoTs]

	(* Cs.inclSyn *)
	let isInclSynTs =
		let chk (t, c1, c2, r) = fun state ->
			if r then
				if Cs.inclSyn c1 c2 then
					T.succeed state
				else
					T.fail t "no implication" state
			else
				if not (Cs.inclSyn c1 c2) then
					T.succeed state
				else
					T.fail t "implication" state
		in
		let tcs = [
			"ideq0", eq [1, x] 1, eq [1, x] 1, true;
			"ideq1", eq [1, x; 1, y] 1, eq [1, x; 1, y] 1, true;
			"ideq2", eq [2, x; 1, y] 1, eq [2, x; 1, y] 1, true;
			"ideq3", eq [1, x; 2, y] 1, eq [1, x; 2, y] 1, true;
			"ideq4", eq [1, x; 1, y] 2, eq [1, x; 1, y] 2, true;
			"idle0", le [1, x] 1, le [1, x] 1, true;
			"idle1", le [1, x; 1, y] 1, le [1, x; 1, y] 1, true;
			"idle2", le [2, x; 1, y] 1, le [2, x; 1, y] 1, true;
			"idle3", le [1, x; 2, y] 1, le [1, x; 2, y] 1, true;
			"idle4", le [1, x; 1, y] 2, le [1, x; 1, y] 2, true;
			"idlt0", lt [1, x] 1, lt [1, x] 1, true;
			"idlt1", lt [1, x; 1, y] 1, lt [1, x; 1, y] 1, true;
			"idlt2", lt [2, x; 1, y] 1, lt [2, x; 1, y] 1, true;
			"idlt3", lt [1, x; 2, y] 1, lt [1, x; 2, y] 1, true;
			"idlt4", lt [1, x; 1, y] 2, lt [1, x; 1, y] 2, true;
			"csteq0", eq [1, x] 1, eq [1, x] 2, false;
			"csteq1", eq [1, x] 2, eq [1, x] 1, false;
			"cstle0", le [1, x] 1, le [1, x] 2, true;
			"cstle1", le [1, x] 2, le [1, x] 1, false;
			"cstlt0", lt [1, x] 1, lt [1, x] 2, true;
			"cstlt0", lt [1, x] 2, lt [1, x] 1, false;
			"lineq0", eq [1, x] 1, eq [2, x] 1, false;
			"lineq1", eq [1, x] 1, eq [1, y] 1, false;
			"lineq2", eq [1, x; 1, y] 1, eq [1, y] 1, false;
			"lineq3", eq [1, x] 1, eq [1, x; 1, y] 1, false;
			"linle0", le [1, x] 1, le [2, x] 1, false;
			"linle1", le [1, x] 1, le [1, y] 1, false;
			"linle2", le [1, x; 1, y] 1, le [1, y] 1, false;
			"linle3", le [1, x] 1, le [1, x; 1, y] 1, false;
			"eqle0", eq [1, x] 1, le [1, x] 1, true;
			"eqle1", eq [1, x; 1, y] 1, le [1, x; 1, y] 1, true;
			"eqle2", eq [1, x] 1, le [1, x] 2, true;
			"eqle3", eq [1, x] 2, le [1, x] 1, false;
			"eqlt0", eq [1, x] 1, lt [1, x] 1, false;
			"eqlt1", eq [1, x; 1, y] 1, lt [1, x; 1, y] 1, false;
			"eqlt2", eq [1, x] 1, lt [1, x] 2, true;
			"eqlt3", eq [1, x] 2, lt [1, x] 1, false;
			"leeq0", le [1, x] 1, eq [1, x] 1, false;
			"leeq1", le [1, x; 1, y] 1, eq [1, x; 1, y] 1, false;
			"leeq2", le [1, x] 1, eq [1, x] 2, false;
			"leeq3", le [1, x] 2, eq [1, x] 1, false;
			"lteq0", lt [1, x] 1, eq [1, x] 1, false;
			"lteq1", lt [1, x; 1, y] 1, eq [1, x; 1, y] 1, false;
			"lteq2", lt [1, x] 1, eq [1, x] 2, false;
			"lteq3", lt [1, x] 2, eq [1, x] 1, false;
			"multle0", le [1, x] 1, le [2, x] 2, false;
			"multle1", le [1, x] 1, le [2, x] 3, false;
			"multle2", le [1, x; 2, y] 2, le [3, x; 6, y] 7, false;
			"multle3", le [2, x] 2, le [1, x] 1, false;
			"multle4", le [2, x] 2, le [1, x] 2, false;
			"multle5", le [3, x; 6, y] 6, le [1, x; 2, y] 3, false;
			"multle6", le [3, x; 6, y] 6, le [1, x; 2, y] 1, false;
			"multle7", le [-3, x; -6, y] (-6), le [1, x; 2, y] 1, false;
			"multeq0", eq [1, x] 0, eq [3, x] 0, false;
			"multeq1", eq [-1, x] 0, eq [3, x] 0, false;
			"c2contrad0", le [1, x] 1, le [] (-1), false;
			"c2contrad1", le [] 1, le [] (-1), false;
			"c2contrad2", le [] (-1), le [] (-1), true;
			"c2contrad3", le [] (-2), le [] (-1), true;
			"c2contrad4", eq [1, x] 1, eq [] 1, false;
			"c2contrad5", eq [1, x] 1, eq [] (-1), false;
			"c2contrad6", eq [] 1, eq [] 1, true;
			"c2contrad7", eq [] 1, eq [] (-1), false;
			"c2trivial0", le [1, x] 1, le [] 1, false;
			"c2trivial1", le [] 1, le [] 1, true;
			"c2trivial2", le [] 2, le [] 1, false;
			"c2trivial3", le [] (-1), le [] 1, true;
			"c1contrad0", le [] (-1), le [1, x] 1, false;
			"c1trivial0", le [] 1, le [1, x] 1, false
		] in
		T.suite "isInclSyn" (List.map chk tcs)

	(* Cs.equal *)
	let isEqTs: T.testT
	=
		let chkRes (name, r, c1, c2) = fun state ->
			let r1 = Cs.equal c1 c2 in
			if r = r1 then
				T.succeed state
			else
				let err = (if r then "not equal" else "equal") ^
					"\nc1: " ^ (Cs.to_string varPr c1) ^ "\nc2: " ^ (Cs.to_string varPr c2)
				in
				T.fail name err state
		in
		let chkSym (name, _, c1, c2) = fun state ->
			let r1 = Cs.equal c1 c2 in
			let r2 = Cs.equal c2 c1 in
			if r1 = r2 then
				T.succeed state
			else
				T.fail name "not symmetric" state
		in
		let tcs = [
			"triv0", true, eq [] 0, eq [] 0;
			"triv1", true, le [] 0, le [] 0;
			"triv2", true, le [] 1, le [] 1;
			"triv3", false, le [] 2, le [] 1;
			"triv4", true, lt [] 0, lt [] 0;
			"triv5", true, lt [] 1, lt [] 1;
			"triv6", false, lt [] 2, lt [] 0;
			"triv7", false, eq [] 0, le [] 0;
			"triv8", false, eq [] 0, le [] 1;
			"triv9", false, le [] 0, lt [] 0;
			"triv10", false, le [] 0, lt [] 1;
			"lin0", true, eq [1, x] 0, eq [1, x] 0;
			"lin1", false, eq [1, x] 0, eq [1, x] 2;
			"lin2", true, eq [1, x; 1, y] 0, eq [1, x; 1, y] 0;
			"lin3", false, eq [1, x; 1, y] 0, eq [1, x; 1, y] 1;
			"cst0", false, eq [1, x] 1, eq [1, y] 1;
			"cst1", false, eq [1, x] 1, eq [1, y] 0
		] in
		T.suite "isEq" [
			T.suite "res" (List.map chkRes tcs);
			T.suite "sym" (List.map chkSym tcs)
		]

	(* Cs.incl *)
	let isInclTs: T.testT
	=
		let chk (t, c1, c2, r) = fun state ->
			let r1 = Cs.incl c1 c2 in
			if r then
				if  r1 then
					T.succeed state
				else
					T.fail t "no implication" state
			else
				if not r1 then
					T.succeed state
				else
					T.fail t "implication" state
		in
		let tcs = [
			"ideq0", eq [1, x] 1, eq [1, x] 1, true;
			"ideq1", eq [1, x; 1, y] 1, eq [1, x; 1, y] 1, true;
			"ideq2", eq [2, x; 1, y] 1, eq [2, x; 1, y] 1, true;
			"ideq3", eq [1, x; 2, y] 1, eq [1, x; 2, y] 1, true;
			"ideq4", eq [1, x; 1, y] 2, eq [1, x; 1, y] 2, true;
			"idle0", le [1, x] 1, le [1, x] 1, true;
			"idle1", le [1, x; 1, y] 1, le [1, x; 1, y] 1, true;
			"idle2", le [2, x; 1, y] 1, le [2, x; 1, y] 1, true;
			"idle3", le [1, x; 2, y] 1, le [1, x; 2, y] 1, true;
			"idle4", le [1, x; 1, y] 2, le [1, x; 1, y] 2, true;
			"csteq0", eq [1, x] 1, eq [1, x] 2, false;
			"csteq1", eq [1, x] 2, eq [1, x] 1, false;
			"cstle0", le [1, x] 1, le [1, x] 2, true;
			"cstle1", le [1, x] 2, le [1, x] 1, false;
			"lineq0", eq [1, x] 1, eq [2, x] 1, false;
			"lineq1", eq [1, x] 1, eq [1, y] 1, false;
			"lineq2", eq [1, x; 1, y] 1, eq [1, y] 1, false;
			"lineq3", eq [1, x] 1, eq [1, x; 1, y] 1, false;
			"linle0", le [1, x] 1, le [2, x] 1, false;
			"linle1", le [1, x] 1, le [1, y] 1, false;
			"linle2", le [1, x; 1, y] 1, le [1, y] 1, false;
			"linle3", le [1, x] 1, le [1, x; 1, y] 1, false;
			"eqle0", eq [1, x] 1, le [1, x] 1, true;
			"eqle1", eq [1, x; 1, y] 1, le [1, x; 1, y] 1, true;
			"eqle2", eq [1, x] 1, le [1, x] 2, true;
			"eqle3", eq [1, x] 2, le [1, x] 1, false;
			"leeq0", le [1, x] 1, eq [1, x] 1, false;
			"leeq1", le [1, x; 1, y] 1, eq [1, x; 1, y] 1, false;
			"leeq2", le [1, x] 1, eq [1, x] 2, false;
			"leeq3", le [1, x] 2, eq [1, x] 1, false;
			"multle0", le [1, x] 1, le [2, x] 2, true;
			"multle1", le [1, x] 1, le [2, x] 3, true;
			"multle2", le [1, x; 2, y] 2, le [3, x; 6, y] 7, true;
			"multle3", le [2, x] 2, le [1, x] 1, true;
			"multle4", le [2, x] 2, le [1, x] 2, true;
			"multle5", le [3, x; 6, y] 6, le [1, x; 2, y] 3, true;
			"multle6", le [3, x; 6, y] 6, le [1, x; 2, y] 1, false;
			"multle7", le [-3, x; -6, y] (-6), le [1, x; 2, y] 1, false;
			"multeq0", eq [1, x] 0, eq [3, x] 0, true;
			"multeq1", eq [-1, x] 0, eq [3, x] 0, true;
			"c2contrad0", le [1, x] 1, le [] (-1), false;
			"c2contrad1", le [] 1, le [] (-1), false;
			"c2contrad2", le [] (-1), le [] (-1), false;
			"c2contrad3", le [] (-2), le [] (-1), false;
			"c2contrad4", eq [1, x] 1, eq [] 1, false;
			"c2contrad5", eq [1, x] 1, eq [] (-1), false;
			"c2contrad6", eq [] 1, eq [] 1, false;
			"c2contrad7", eq [] 1, eq [] (-1), false;
			"c2trivial0", le [1, x] 1, le [] 1, true;
			"c2trivial1", le [] 1, le [] 1, true;
			"c2trivial2", le [] 2, le [] 1, true;
			"c2trivial3", le [] (-1), le [] 1, true;
			"c1contrad0", le [] (-1), le [1, x] 1, false;
			"c1trivial0", le [] 1, le [1, x] 1, false
		] in
		T.suite "isIncl" (List.map chk tcs)

	(* Cs.tellProp *)
	let tellPropTs: T.testT
	=
		let chk (name, c, r) = fun state ->
			if r = Cs.tellProp c then
				T.succeed state
			else
				T.fail name "not equal" state
		in
		let tcs = [
			"eq0", eq [] 1, Cs.Contrad;
			"eq1", eq [1, x] 1, Cs.Nothing;
			"eq2", eq [] 0, Cs.Trivial;
			"eq3", eq [] (-1), Cs.Contrad;
			"eq4", eq [] (-2), Cs.Contrad;
			"le0", le [] 1, Cs.Trivial;
			"le1", le [] 0, Cs.Trivial;
			"le2", le [] (-1), Cs.Contrad;
			"le3", le [] (-2), Cs.Contrad;
			"le4", le [1, x] 1, Cs.Nothing;
			"le5", le [1, x] (-1), Cs.Nothing;
			"lt0", lt [] 1, Cs.Trivial;
			"lt1", lt [] 0, Cs.Contrad;
			"lt2", lt [] (-1), Cs.Contrad;
			"lt3", lt [] (-2), Cs.Contrad;
			"lt4", lt [1, x] 1, Cs.Nothing;
			"lt5", lt [1, x] 0, Cs.Nothing
		] in
		T.suite "tellProp" (List.map chk tcs)

	let ts: T.testT
	= T.suite Cs.Vec.Coeff.name [evalTs; complTs; splitTs; isInclSynTs; isEqTs; isInclTs; tellPropTs]
end

module Rat = struct

	module Test (Cstr : Cstr.Rat.Type) = struct
		include Test(Cstr)
		
		(* Cs.elim *)
		let elimNoElimTs: T.testT
		=
			let chk (name, c1, c2) = fun state ->
				try
					let _ = Cstr.elim c1 c2 x in
					T.fail name "Cs.NoElim not raised" state
				with
				| Cstr.NoElim -> T.succeed state
				| _ -> T.fail name "unexpected exception raised" state
			in
			let tcs = [
				"eqnil0", eq [] 0, eq [] 0;
				"eqnil1", eq [] 0, eq [1, y] 0;
				"eqone0", eq [1, z] 0, eq [1, y] 0;
				"eqone1", eq [1, y] 0, eq [1, z] 0;
				"eqone2", eq [1, y] 0, eq [1, y] 0;
				"lenil0", le [] 0, le [] 0;
				"lenil1", le [] 0, le [1, z] 0;
				"leone0", le [1, z] 0, le [1, y] 0;
				"leone1", le [1, y] 0, le [1, z] 0;
				"leone2", le [1, y] 0, le [1, y] 0;
				"eqlenil0", eq [] 0, le [] 0;
				"eqlenil1", eq [] 0, le [1, y] 0;
				"eqleone0", eq [1, z] 0, le [1, y] 0;
				"eqleone1", eq [1, y] 0, le [1, z] 0
			] in
			T.suite "noElim" (List.map chk tcs)

		let elimCannotElimTs: T.testT
		=
			let chk (name, c1, c2) = fun state ->
				try
					let _ = Cstr.elim c1 c2 x in
					T.fail name "Cs.CannotElim not raised" state
				with
				| Cstr.CannotElim -> T.succeed state
				| _ -> T.fail name "unexpected exception raised" state
			in
			let tcs = [
				"eqone0", eq [1, x] 0, eq [] 0;
				"eqone1", eq [1, x] 0, eq [1, y] 0;
				"eqone2", eq [1, x] 0, eq [1, y] 0;
				"eqone3", eq [1, x; 1, y] 0, eq [1, y] 0;
				"eqone4", eq [1, y] 0, eq [1, x] 0;
				"leone0", le [1, x] 0, le [] 0;
				"leone1", le [1, x] 0, le [1, y] 0;
				"leone2", le [1, x] 0, le [1, y] 0;
				"leone3", le [1, x; 1, y] 0, le [1, y] 0;
				"leone4", le [1, y] 0, le [1, x] 0;
				"eqleone0", eq [1, x] 0, le [] 0;
				"eqleone1", eq [1, x] 0, le [1, y] 0;
				"eqleone2", eq [1, x] 0, le [1, y] 0;
				"eqleone3", eq [1, x; 1, y] 0, le [1, y] 0;
				"eqleone4", eq [1, y] 0, le [1, x] 0;
				"leeqone0", le [1, x] 0, eq [] 0;
				"leeqone1", le [1, x] 0, eq [1, y] 0;
				"leeqone2", le [1, x] 0, eq [1, y] 0;
				"leeqone3", le [1, x; 1, y] 0, eq [1, y] 0;
				"leeqone4", le [1, y] 0, eq [1, x] 0;
				"le0", le [1, x] 0, le [1, x] 0;
				"le1", le [-1, x] 0, le [-1, x] 0;
				"le2", le [-1, x] 0, le [-2, x] 0;
				"le3", le [1, x] 0, le [2, x] 0;
				"le4", le [1, x; 1, y] 0, le [1, x; 1, y] 0;
				"le5", le [1, x; -1, y] 0, le [1, x; 1, y] 0
			] in
			T.suite "cannotElim" (List.map chk tcs)

		let elimOkTs: T.testT
		=
			let chkRes (name, c1, c2) = fun state -> 
				let (c, _, _) = Cstr.elim c1 c2 x in
				let a = Cstr.Vec.get (Cstr.get_v c) x in
				if Cstr.Vec.Coeff.cmpz a = 0 then
					T.succeed state
				else
					T.fail name "x has non-zero coefficient" state
			in
			let chkCoef (name, c1, c2) = fun state ->
				let (c, x1, x2) = Cstr.elim c1 c2 x in
				let c' = Cstr.add (Cstr.mulc x1 c1) (Cstr.mulc x2 c2) in
				if Cstr.equalSyn c c' then
					T.succeed state
				else
					let e = "bad coefficients\n" ^
						"x1 = " ^ (Cstr.Vec.Coeff.to_string x1) ^ ", x2 = " ^ (Cstr.Vec.Coeff.to_string x2)
					in
					T.fail name e state
			in
			let tcs = [
				"eqeq0", eq [1, x; 1, y] 0, eq [1, x] 1;
				"eqeq1", eq [2, x; 1, y] 0, eq [1, x] 1;
				"eqeq2", eq [1, x; 1, y] 0, eq [2, x] 1;
				"eqeq3", eq [1, x; 2, y] 0, eq [1, x; 1, y; 2, z] 1;
				"leeq0", le [1, x; 1, y] 0, eq [1, x] 1;
				"leeq1", le [1, x; 2, y] 1, eq [1, x] 1;
				"leeq2", le [2, x; 1, y] 1, eq [1, x] 1;
				"leeq3", le [1, x; 1, y] 1, eq [2, x] 1;
				"eqle0", eq [1, x; 1, y] 0, le [1, x] 1;
				"eqle1", eq [1, x; 2, y] 1, le [1, x] 1;
				"eqle2", eq [2, x; 1, y] 1, le [1, x] 1;
				"eqle3", eq [1, x; 1, y] 1, le [2, x] 1;
				"lele0", le [1, x] 1, le [-1, x; 1, y] 0;
				"lele1", le [2, x] 1, le [-1, x; 1, y] 0;
				"lele2", le [1, x] 1, le [-2, x; 1, y] 0;
				"lele3", le [1, x; 1, z] 1, le [-1, x; 1, y] 0;
				"lele4", le [-1, x] 1, le [1, x; 1, y] 0;
				"lele5", le [-2, x] 1, le [1, x; 1, y] 0;
				"lele6", le [-1, x] 1, le [2, x; 1, y] 0;
				"lele7", le [-1, x; 1, z] 1, le [1, x; 1, y] 0;
			] in
			T.suite "ok" [
				T.suite "res" (List.map chkRes tcs);
				T.suite "coef" (List.map chkCoef tcs)
			]

		let elimTs: T.testT
		= T.suite "elim" [elimCannotElimTs; elimNoElimTs; elimOkTs]

		let ts : T.testT
			= T.suite Cstr.Vec.Coeff.name [evalTs; complTs; elimTs; splitTs ; isInclSynTs; isEqTs; isInclTs; tellPropTs]
	end
	
	module Positive = Test(Cstr.Rat.Positive)
	
	module Int = Test(Cstr.Rat.Int)
	
	let ts : T.testT
			= T.suite "Rat" [Positive.ts ; Int.ts] 
end

module Float = struct
	module Cstr = Cstr.Float
	
	module Positive = struct
		module Cstr = Cstr.Positive
		include Test(Cstr)
	end
	
	module Int = struct
		module Cstr = Cstr.Int
		include Test(Cstr)
	end
	
	let ts : T.testT
		= T.suite "Float" [Positive.ts ; Int.ts] 
end

let ts : T.testT
	= T.suite "Cstr" [Rat.ts ; Float.ts]
