open Vpl

module Test (Cs : Cstr.Rat.Type) = struct
	
	module Cs = Cstr.Rat.Positive (* XXX: à retirer pour généraliser à Var.Int *)
	module Var = Cs.Vec.V
	module Vec = Cs.Vec
	
	let x = Var.fromInt 1
	let y = Var.fromInt 2
	let z = Var.fromInt 3
	let sx1 = Var.fromInt 4
	let sx2 = Var.fromInt 6
	let sx3 = Var.fromInt 5
	let sx4 = Var.fromInt 7
	let nxt = sx1

	let varPr: Var.t -> string
	= fun _x ->
		let vars
		= [x, "x"; y, "y"; z, "z"; sx1, "sx1"; sx2, "sx2"; sx3, "sx3"; sx4, "sx4"]
		in
		try List.assoc _x vars with Not_found -> "v" ^ (Var.to_string _x)

	let mkc t v c =
		Cs.mk t (List.map (fun (c, v) -> (Vec.Coeff.mk1 c, v)) v) (Vec.Coeff.mk1 c)

	let eq = mkc Cstr.Eq
	let le = mkc Cstr.Le
	let lt = mkc Cstr.Lt
	
	let checknbasic s =
		let checkv st =
			(match Splx.get_low st with
			| Some l -> Scalar.Symbolic.cmp (Splx.get_bv l) (Splx.get_v st) <= 0
			|None -> true) &&
			(match Splx.get_up st with
			| Some u -> Scalar.Symbolic.cmp (Splx.get_bv u) (Splx.get_v st) >= 0
			|None -> true)
		in
		let rec _check m st =
			Cs.Vec.M.for_all2
				(fun mopt nopt -> match mopt,nopt with
				| Some _, _ -> true
				| None, None -> true
				| None, Some n -> checkv n) m st
		in
		_check (Splx.get_mat s) (Splx.get_state s)
	
	let checkmat s =
		let rec eval a ve st =
			Cs.Vec.M.fold2 
				(fun _ a x y -> Scalar.Symbolic.add a (Scalar.Symbolic.mulr x (Splx.get_v y)))
				a ve st
		in
		Cs.Vec.M.fold
			(fun _ a n ->
				match n with
				| None -> a
				| Some ve ->
					if a then
						let v = eval Scalar.Symbolic.z ve (Splx.get_state s) in
						Scalar.Symbolic.cmp Scalar.Symbolic.z v = 0
					else
						false)
			true (Splx.get_mat s)

	let checkBasic : Splx.t -> bool
	  = fun sx ->
	  Var.Set.fold
		(fun x b ->
		 if not b then b
		 else
		   Cs.Vec.M.fold
		 (fun _ b' ->
		  if not b' then fun _ -> b'
		  else
			function
			| None -> b'
			| Some v -> Vec.Coeff.cmpz (Cs.Vec.get v x) = 0
		 )
		 true
		 (Cs.Vec.M.set None (Splx.get_mat sx) x None)
		)
		(Cs.Vec.M.mskBuild (function None -> false | Some _ -> true) [Splx.get_mat sx]
		 |> Cs.Vec.M.pathsGet)
		true

	let checkinv s = checknbasic s && checkmat s && checkBasic s

	type invChkT
	= InvOk of Splx.t Splx.mayUnsatT | BadInv of Splx.t
	
	
	let ccheck : Splx.t -> invChkT
	= fun s0 ->
		let rec loop i s =
			match Splx.step Splx.Steep s with
			| Splx.StepCont sx ->
				if checkinv sx
				then loop (i+1) sx
				else BadInv sx
			| Splx.StepSat sx -> InvOk (Splx.IsOk sx)
			| Splx.StepUnsat w -> InvOk (Splx.IsUnsat w)
		in
		let s = Splx.Preprocessing.presimpl s0 in
		if checkinv s
		then loop 1 s
		else BadInv s
	
	let ccheckFromAdd : Splx.t Splx.mayUnsatT -> invChkT
	= function
		| Splx.IsUnsat _ -> Pervasives.invalid_arg "Splx_t.ccheckFromAdd"
		| Splx.IsOk sx -> ccheck sx
	
	
	let asgEquiv : Scalar.Symbolic.t Cs.Vec.M.t -> (Var.t * Scalar.Symbolic.t) list -> bool
	  = fun t l ->
	  let xs = List.map Pervasives.fst l |> Var.Set.of_list in
	  if not
		   (List.fold_left
		  (fun b (x, a) ->
		   if Var.Set.mem x xs then b
		   else b && Scalar.Symbolic.cmp Scalar.Symbolic.z a = 0
		  )
		  true (Cs.Vec.M.toList t)
		   )
	  then false
	  else List.for_all (fun (x, a) -> Scalar.Symbolic.cmp a (Cs.Vec.M.get Scalar.Symbolic.z t x) = 0) l

	(* Splx.mergeB *)
	let mergeBTs: T.testT
	=
		let mkV i = Scalar.Symbolic.ofRat (Vec.Coeff.mk1 i) in
		let mkN i = Vec.Coeff.mk1 i in
		let mkB id sc v = Some {Splx.id = id; Splx.scale = sc; Splx.bv = v} in
		let chk (t, st, stA, stB) = fun state ->
			let actualSt = Splx.mergeB stA stB in
			if Splx.state_equal st actualSt
			then T.succeed state
			else
				let e = Printf.sprintf "expected:\n%s\nactual:\n%s\n"
					(Splx.prSt st) (Splx.prSt actualSt)
				in
				T.fail t e state
		in
		let tcs = [
			"nil0",
				{Splx.v = mkV 0; Splx.low = None; Splx.up = None},
				{Splx.v = mkV 0; Splx.low = None; Splx.up = None},
				{Splx.v = mkV 0; Splx.low = None; Splx.up = None};

			"nil1", (* resulting value is that of the first argument *)
				{Splx.v = mkV 2; Splx.low = None; Splx.up = None},
				{Splx.v = mkV 2; Splx.low = None; Splx.up = None},
				{Splx.v = mkV 0; Splx.low = None; Splx.up = None};

			"nil2", (* resulting value is that of the first argument *)
				{Splx.v = mkV 0; Splx.low = None; Splx.up = None},
				{Splx.v = mkV 0; Splx.low = None; Splx.up = None},
				{Splx.v = mkV 2; Splx.low = None; Splx.up = None};

			"noMergeLow0",
				{Splx.v = mkV 0; Splx.low = mkB 1 (mkN 1) (mkV 2); Splx.up = None},
				{Splx.v = mkV 0; Splx.low = None; Splx.up = None},
				{Splx.v = mkV 1; Splx.low = mkB 1 (mkN 1) (mkV 2); Splx.up = None};

			"noMergeLow1",
				{Splx.v = mkV 0; Splx.low = mkB 1 (mkN 1) (mkV 2); Splx.up = None},
				{Splx.v = mkV 0; Splx.low = mkB 1 (mkN 1) (mkV 2); Splx.up = None},
				{Splx.v = mkV 0; Splx.low = None; Splx.up = None};

			"noMergeUp0",
				{Splx.v = mkV 1; Splx.low = None; Splx.up = mkB 1 (mkN 1) (mkV 2)},
				{Splx.v = mkV 1; Splx.low = None; Splx.up = None},
				{Splx.v = mkV 0; Splx.low = None; Splx.up = mkB 1 (mkN 1) (mkV 2)};

			"noMergeUp1",
				{Splx.v = mkV 0; Splx.low = None; Splx.up = mkB 1 (mkN 1) (mkV 2)},
				{Splx.v = mkV 0; Splx.low = None; Splx.up = mkB 1 (mkN 1) (mkV 2)},
				{Splx.v = mkV 0; Splx.low = None; Splx.up = None};

			"noMerge0",
				{Splx.v = mkV 0; Splx.low = mkB 1 (mkN 1) (mkV 2); Splx.up = mkB 2 (mkN 1) (mkV 3)},
				{Splx.v = mkV 0; Splx.low = mkB 1 (mkN 1) (mkV 2); Splx.up = None},
				{Splx.v = mkV 1; Splx.low = None; Splx.up = mkB 2 (mkN 1) (mkV 3)};

			"noMerge1",
				{Splx.v = mkV 1; Splx.low = mkB 1 (mkN 1) (mkV 2); Splx.up = mkB 2 (mkN 1) (mkV 3)},
				{Splx.v = mkV 1; Splx.low = None; Splx.up = mkB 2 (mkN 1) (mkV 3)},
				{Splx.v = mkV 0; Splx.low = mkB 1 (mkN 1) (mkV 2); Splx.up = None};

			"mergeLow0", (* resulting value is out-of-bounds *)
				{Splx.v = mkV 2; Splx.low = mkB 2 (mkN 2) (mkV 3); Splx.up = None},
				{Splx.v = mkV 2; Splx.low = mkB 1 (mkN 1) (mkV 2); Splx.up = None},
				{Splx.v = mkV 3; Splx.low = mkB 2 (mkN 2) (mkV 3); Splx.up = None};

			"mergeLow1",
				{Splx.v = mkV 3; Splx.low = mkB 1 (mkN 1) (mkV 3); Splx.up = None},
				{Splx.v = mkV 3; Splx.low = mkB 1 (mkN 1) (mkV 3); Splx.up = None},
				{Splx.v = mkV 2; Splx.low = mkB 2 (mkN 2) (mkV 2); Splx.up = None};

			"mergeLow2", (* upper bound is preserved *)
				{Splx.v = mkV 3; Splx.low = mkB 1 (mkN 1) (mkV 3); Splx.up = mkB 3 (mkN 2) (mkV 4)},
				{Splx.v = mkV 3; Splx.low = mkB 1 (mkN 1) (mkV 3); Splx.up = mkB 3 (mkN 2) (mkV 4)},
				{Splx.v = mkV 2; Splx.low = mkB 2 (mkN 2) (mkV 2); Splx.up = None};

			"mergeLow3", (* upper bound is preserved *)
				{Splx.v = mkV 3; Splx.low = mkB 1 (mkN 1) (mkV 3); Splx.up = mkB 3 (mkN 1) (mkV 4)},
				{Splx.v = mkV 3; Splx.low = mkB 1 (mkN 1) (mkV 3); Splx.up = None},
				{Splx.v = mkV 2; Splx.low = mkB 2 (mkN 2) (mkV 2); Splx.up = mkB 3 (mkN 1) (mkV 4)};

			"mergeUp0",
				{Splx.v = mkV 2; Splx.low = None; Splx.up = mkB 2 (mkN 1) (mkV 2)},
				{Splx.v = mkV 2; Splx.low = None; Splx.up = mkB 1 (mkN 1) (mkV 3)},
				{Splx.v = mkV 1; Splx.low = None; Splx.up = mkB 2 (mkN 1) (mkV 2)};

			"mergeUp1",
				{Splx.v = mkV 2; Splx.low = None; Splx.up = mkB 1 (mkN 1) (mkV 2)},
				{Splx.v = mkV 2; Splx.low = None; Splx.up = mkB 1 (mkN 1) (mkV 2)},
				{Splx.v = mkV 1; Splx.low = None; Splx.up = mkB 2 (mkN 1) (mkV 3)};

			"mergeUp2", (* lower bound is preserved *)
				{Splx.v = mkV 2; Splx.low = mkB 3 (mkN 1) (mkV 1); Splx.up = mkB 2 (mkN 1) (mkV 2)},
				{Splx.v = mkV 2; Splx.low = mkB 3 (mkN 1) (mkV 1); Splx.up = mkB 1 (mkN 1) (mkV 3)},
				{Splx.v = mkV 1; Splx.low = None; Splx.up = mkB 2 (mkN 1) (mkV 2)};

			"mergeUp3", (* lower bound is preserved *)
				{Splx.v = mkV 2; Splx.low = mkB 3 (mkN 1) (mkV 1); Splx.up = mkB 2 (mkN 1) (mkV 2)},
				{Splx.v = mkV 2; Splx.low = None; Splx.up = mkB 1 (mkN 1) (mkV 3)},
				{Splx.v = mkV 1; Splx.low = mkB 3 (mkN 1) (mkV 1); Splx.up = mkB 2 (mkN 1) (mkV 2)};

			"merge0", (* both bounds from the same operand *)
				{Splx.v = mkV 2; Splx.low = mkB 1 (mkN 1) (mkV 1); Splx.up = mkB 2 (mkN 1) (mkV 2)},
				{Splx.v = mkV 2; Splx.low = mkB 1 (mkN 1) (mkV 1); Splx.up = mkB 2 (mkN 1) (mkV 2)},
				{Splx.v = mkV 1; Splx.low = mkB 3 (mkN 1) (mkV 0); Splx.up = mkB 4 (mkN 1) (mkV 4)};

			"merge1", (* both bounds from the same operand *)
				{Splx.v = mkV 1; Splx.low = mkB 1 (mkN 1) (mkV 1); Splx.up = mkB 2 (mkN 1) (mkV 2)},
				{Splx.v = mkV 1; Splx.low = mkB 3 (mkN 1) (mkV 0); Splx.up = mkB 4 (mkN 1) (mkV 4)},
				{Splx.v = mkV 2; Splx.low = mkB 1 (mkN 1) (mkV 1); Splx.up = mkB 2 (mkN 1) (mkV 2)};

			"merge2", (* one bound from each operand *)
				{Splx.v = mkV 1; Splx.low = mkB 3 (mkN 1) (mkV 1); Splx.up = mkB 2 (mkN 1) (mkV 2)},
				{Splx.v = mkV 1; Splx.low = mkB 3 (mkN 1) (mkV 1); Splx.up = mkB 4 (mkN 1) (mkV 4)},
				{Splx.v = mkV 2; Splx.low = mkB 1 (mkN 1) (mkV 0); Splx.up = mkB 2 (mkN 1) (mkV 2)};

			"merge3", (* one bound from each operand *)
				{Splx.v = mkV 1; Splx.low = mkB 1 (mkN 1) (mkV 1); Splx.up = mkB 4 (mkN 1) (mkV 2)},
				{Splx.v = mkV 1; Splx.low = mkB 3 (mkN 1) (mkV 0); Splx.up = mkB 4 (mkN 1) (mkV 2)},
				{Splx.v = mkV 2; Splx.low = mkB 1 (mkN 1) (mkV 1); Splx.up = mkB 2 (mkN 1) (mkV 4)};

			"merge4", (* keep previous if equal: lower bound *)
				{Splx.v = mkV 1; Splx.low = mkB 3 (mkN 1) (mkV 1); Splx.up = mkB 4 (mkN 1) (mkV 2)},
				{Splx.v = mkV 1; Splx.low = mkB 3 (mkN 1) (mkV 1); Splx.up = mkB 4 (mkN 1) (mkV 2)},
				{Splx.v = mkV 2; Splx.low = mkB 1 (mkN 1) (mkV 1); Splx.up = mkB 2 (mkN 1) (mkV 4)};

			"merge4", (* keep previous if equal: upper bound *)
				{Splx.v = mkV 1; Splx.low = mkB 1 (mkN 1) (mkV 1); Splx.up = mkB 4 (mkN 1) (mkV 2)},
				{Splx.v = mkV 1; Splx.low = mkB 3 (mkN 1) (mkV 0); Splx.up = mkB 4 (mkN 1) (mkV 2)},
				{Splx.v = mkV 2; Splx.low = mkB 1 (mkN 1) (mkV 1); Splx.up = mkB 2 (mkN 1) (mkV 2)};

			(* results specifying empty intervals *)
			"already0", (* one of the two operands is self-contradictory *)
				{Splx.v = mkV 1; Splx.low = mkB 1 (mkN 2) (mkV 2); Splx.up = mkB 2 (mkN 1) (mkV 1)},
				{Splx.v = mkV 1; Splx.low = mkB 1 (mkN 2) (mkV 2); Splx.up = mkB 2 (mkN 1) (mkV 1)},
				{Splx.v = mkV 0; Splx.low = None; Splx.up = None};

			"already1", (* one of the two operands is self-contradictory *)
				{Splx.v = mkV 1; Splx.low = mkB 1 (mkN 2) (mkV 2); Splx.up = mkB 3 (mkN 1) (mkV 1)},
				{Splx.v = mkV 1; Splx.low = mkB 1 (mkN 2) (mkV 2); Splx.up = mkB 3 (mkN 1) (mkV 1)},
				{Splx.v = mkV 0; Splx.low = mkB 2 (mkN 1) (mkV 0); Splx.up = mkB 4 (mkN 1) (mkV 3)};

			"already2", (* one of the two operands is self-contradictory *)
				{Splx.v = mkV 0; Splx.low = mkB 1 (mkN 2) (mkV 2); Splx.up = mkB 2 (mkN 1) (mkV 1)},
				{Splx.v = mkV 0; Splx.low = None; Splx.up = None},
				{Splx.v = mkV 1; Splx.low = mkB 1 (mkN 2) (mkV 2); Splx.up = mkB 2 (mkN 1) (mkV 1)};

			"already3", (* one of the two operands is self-contradictory *)
				{Splx.v = mkV 0; Splx.low = mkB 1 (mkN 2) (mkV 2); Splx.up = mkB 3 (mkN 1) (mkV 1)},
				{Splx.v = mkV 0; Splx.low = mkB 2 (mkN 1) (mkV 0); Splx.up = mkB 4 (mkN 1) (mkV 3)},
				{Splx.v = mkV 1; Splx.low = mkB 1 (mkN 2) (mkV 2); Splx.up = mkB 3 (mkN 1) (mkV 1)};

			"merge0", (* the merge leads to empty interval *)
				{Splx.v = mkV 0; Splx.low = mkB 1 (mkN 3) (mkV 2); Splx.up = mkB 2 (mkN 2) (mkV 1)},
				{Splx.v = mkV 0; Splx.low = mkB 1 (mkN 3) (mkV 2); Splx.up = None},
				{Splx.v = mkV 0; Splx.low = None; Splx.up = mkB 2 (mkN 2) (mkV 1)}
		] in
		T.suite "ok" (List.map chk tcs)

	(* Splx.setSymbolic *)
	let setSymbolicTs: T.testT
	=
		let chk (t, res, ve, st) = fun state ->
			let aRes = Splx.setSymbolic ve st in
			let valEq v1 v2 = if Scalar.Symbolic.cmp v1 v2 = 0 then true else false in
			if valEq res aRes then
				T.succeed state
			else
				T.fail t (Scalar.Symbolic.to_string aRes) state
		in
		let mapMk = Cs.Vec.M.mk {Splx.v = Scalar.Symbolic.z; Splx.low = None; Splx.up = None} in
		let stMk v = {Splx.v = v; Splx.low = None; Splx.up = None} in
		let valMk i = Scalar.Symbolic.ofRat (Vec.Coeff.mk1 i) in
		let tcs = [
			"nil0", valMk 0, Cs.Vec.nil, Cs.Vec.M.empty;
			"nil1", valMk 0, Cs.Vec.mk [Vec.Coeff.u, x], Cs.Vec.M.empty;
			"nil2", valMk 0, Cs.Vec.nil, mapMk [x, stMk (valMk 1)];
			"u0", valMk (-1), Cs.Vec.mk [Vec.Coeff.u, x], mapMk [x, stMk (valMk 1)];
			"u1", valMk (-2), Cs.Vec.mk [Vec.Coeff.u, x], mapMk [x, stMk (valMk 2)];
			"u2", valMk 1, Cs.Vec.mk [Vec.Coeff.mk1 (-1), x], mapMk [x, stMk (valMk 1)];
			"u3", valMk 2, Cs.Vec.mk [Vec.Coeff.u, x], mapMk [x, stMk (valMk (-2))];
			"m0", valMk 1, Cs.Vec.mk [Vec.Coeff.mk1 2, x; Vec.Coeff.mk1 3, y; Vec.Coeff.u, z],
				mapMk [x, stMk (valMk 1); y, stMk (valMk (-1))]
		] in
		T.suite "setSymbolic" (List.map chk tcs)

	(* Splx.fitBnd *)
	let fitBndTs: T.testT
	=
		let mkV i = Scalar.Symbolic.ofRat (Vec.Coeff.mk1 i) in
		let mkB v = Some {Splx.id = 1; Splx.scale = Vec.Coeff.mk1 0; Splx.bv = v} in
		let chkBnd (t, s) = fun state ->
			match Splx.fitBnd s with
			| Splx.BndUnsat _ -> T.fail t "unsat" state
			| Splx.FitBnd (_, s1) ->
				if Splx.obnd_equal (Splx.get_low s) (Splx.get_low s1)
					&& Splx.obnd_equal (Splx.get_up s) (Splx.get_up s1)
				then T.succeed state
				else
					let e = Splx.prSt s1 in
					T.fail t e state
		in
		let chkFit (t, s) = fun state ->
			match Splx.fitBnd s with
			| Splx.BndUnsat _ -> T.fail t "unsat" state
			| Splx.FitBnd (_, s1) ->
				let satL =
					match (Splx.get_low s1) with
					| None -> true
					| Some l -> Scalar.Symbolic.cmp (Splx.get_bv l) (Splx.get_v s1) <= 0 in
				let satU =
					match (Splx.get_up s1) with
					| None -> true
					| Some u -> Scalar.Symbolic.cmp (Splx.get_v s1) (Splx.get_bv u) <= 0 in
				if satL && satU
				then T.succeed state
				else T.fail t (Splx.prSt s1) state
		in
		let chkDelta (t, s) = fun state ->
			match Splx.fitBnd s with
			| Splx.BndUnsat _ -> T.fail t "unsat" state
			| Splx.FitBnd (d, s1) ->
				let vEq v1 v2 = (Scalar.Symbolic.cmp v1 v2 = 0) in
				if vEq (Splx.get_v s1) (Scalar.Symbolic.add (Splx.get_v s) d)
				then T.succeed state
				else
					let err = Printf.sprintf "s1: %s, d: %s\n"
						(Scalar.Symbolic.to_string (Splx.get_v s1)) (Scalar.Symbolic.to_string d)
					in
					T.fail t err state
		in
		let tcs = [
			"none0", {Splx.v = mkV 1; Splx.low = None; Splx.up = None};
			"upSat0", {Splx.v = mkV 1; Splx.low = None; Splx.up = mkB (mkV 1)};
			"upSat1", {Splx.v = mkV 1; Splx.low = None; Splx.up = mkB (mkV 2)};
			"upFit0", {Splx.v = mkV 3; Splx.low = None; Splx.up = mkB (mkV 2)};
			"lowSat0", {Splx.v = mkV 1; Splx.low = mkB (mkV 1); Splx.up = None};
			"lowSat1", {Splx.v = mkV 1; Splx.low = mkB (mkV 0); Splx.up = None};
			"lowFit0", {Splx.v = mkV 0; Splx.low = mkB (mkV 1); Splx.up = None}
		] in
		T.suite "fitBnd" [
			T.suite "bnd" (List.map chkBnd tcs);
			T.suite "fit" (List.map chkFit tcs);
			T.suite "delta" (List.map chkDelta tcs)
		]

	(* Splx.add *)
	let addOkTs: T.testT
	=
		let mkV i = Scalar.Symbolic.ofRat (Vec.Coeff.mk1 i) in
		let mkVn i = Scalar.Symbolic.ndelta (Vec.Coeff.mk1 i) in
		let mkN i = Vec.Coeff.mk1 i in
		let mkB id sc v = Some {Splx.id = id; Splx.scale = sc; Splx.bv = v} in
		let chkSt (t, i, c, s, _, v, l, u) = fun state ->
			match Splx.addAcc s (i, c) with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s1 ->
				let stV =
					let state_z = {
						Splx.v = Scalar.Symbolic.z;
						Splx.low = None;
						Splx.up = None
					} in
					Cs.Vec.M.get state_z (Splx.get_state s1) v
				in
				if Splx.obnd_equal l (Splx.get_low stV) && Splx.obnd_equal u (Splx.get_up stV)
				then T.succeed state
				else
					let err = Splx.prSt stV in
					T.fail t err state
		in
		let chkC (t, i, c, s, e, v, _, _) = fun state ->
			match Splx.addAcc s (i, c) with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s1 ->
				let e1 = Cs.Vec.M.get None (Splx.get_mat s1) v in
				match e with
				| None ->
					if e1 = None
					then T.succeed state
					else T.fail t "not equal" state
				| Some e2 ->
					match e1 with
					| None -> T.fail t "constraint expected" state
					| Some e3 ->
						if Cs.Vec.equal e2 e3
						then T.succeed state
						else
							let err = Printf.sprintf "e2: %s, e3: %s\n"
								(Cs.Vec.to_string varPr e2) (Cs.Vec.to_string varPr e3)
							in
							T.fail t err state
		in
		let chkInv (t, i, c, s, _, _, _, _) = fun state ->
			match Splx.addAcc s (i, c) with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s1 ->
				if checkinv s1
				then T.succeed state
				else
					let err = Printf.sprintf "invariant\n%s\n" (Splx.pr varPr s1) in
					T.fail t err state
		in
		let tcs = [
		(* no overriding *)
			(* single variable *)
			"sle0", 1, le [1, x] 1, Splx.mk nxt [],
				None, x, None, mkB 1 (mkN 1) (mkV 1);

			"sle1", 3, le [2, x] 4, Splx.mk nxt [],
				None, x, None, mkB 3 (Vec.Coeff.mk 2 1) (mkV 2);

			"sle2", 4, le [2, x] (-4), Splx.mk nxt [],
				None, x, None, mkB 4 (Vec.Coeff.mk 2 1) (mkV (-2));

			"sle3", 2, le [-2, x] (-4), Splx.mk nxt [],
				None, x, mkB 2 (Vec.Coeff.mk 2 1) (mkV 2), None;

			"sle4", 1, le [1, x] 1, Splx.mk nxt [2, le [1, y] 1],
				None, x, None, mkB 1 (mkN 1) (mkV 1);

			"sle5", 1, le [1, x] 1, Splx.mk nxt [2, le [1, x; 1, y] 5],
				None, x, None, mkB 1 (mkN 1) (mkV 1);

			"sle6", 1, le [1, x] 1, Splx.mk nxt [2, le [-1, x] 1],
				None, x, mkB 2 (mkN 1) (mkV (-1)), mkB 1 (mkN 1) (mkV 1);

			"slt0", 2, lt [1, y] 1, Splx.mk nxt [],
				None, y, None, mkB 2 (mkN 1) (mkVn 1);

			"seq0", 2, eq [1, z] 1, Splx.mk nxt [],
				None, z, mkB 2 (mkN (-1)) (mkV 1), mkB 2 (mkN 1) (mkV 1);

			"sle7", 1, le [-1, x] (-1), Splx.mk nxt [
				2, eq [1, x; 1, z] 1;
				3, le [1, z; 1, y] 2;
				4, le [1, x; -1, y; 2, z] 2 ],
				None, x, mkB 1 (mkN 1) (mkV 1), None;

			(* multiple variables *)
			"mle0", 2, le [1, x; 1, y] 1, Splx.mk nxt [],
				Some (Cs.Vec.mk [mkN 1, sx1; mkN (-1), x; mkN (-1), y]),
				sx1, None, mkB 2 (mkN 1) (mkV 1);

			"mle1", 1, le [1, x; 1, y] 1, Splx.mk nxt [
				2, eq [1, x; 1, z] 1;
				3, le [1, z; 1, y] 2;
				4, le [1, x; -1, y; 2, z] 2 ],
				Some (Cs.Vec.mk [mkN 1, sx4; mkN (-1), x; mkN (-1), y]),
				sx4, None, mkB 1 (mkN 1) (mkV 1);

		(* overriding *)
			"overLe0", 1, le [1, x] 2, Splx.mk nxt [2, le [1, x] 3],
				None, x, None, mkB 1 (mkN 1) (mkV 2);

			"overEq0", 1, eq [2, y] 2, Splx.mk nxt [2, le [1, y] 2],
				None, y, mkB 1 (Vec.Coeff.mk 2 (-1)) (mkV 1), mkB 1 (Vec.Coeff.mk 2 1) (mkV 1);

		(* bug evidence *)
			"decompress0", 2, le [-1, x] (-17), Splx.mk nxt [
				0, le [-1, y] (-1);
				1, le [-1, x; 16, y] 15 ],
				None, x, mkB 2 (mkN 1) (mkV 17), None
		] in
		T.suite "Ok" [
			T.suite "state" (List.map chkSt tcs);
			T.suite "cons" (List.map chkC tcs);
			T.suite "inv" (List.map chkInv tcs)
		]

	let addInvalTs: T.testT
	=	let chk : string * int * Cs.t * Splx.t Splx.mayUnsatT
			-> T.stateT -> T.stateT
		= fun (nm, i, c, sx) st ->
			match Splx.addAcc sx (i, c) with
			| Splx.IsOk _ ->
				if Cs.Trivial = Cs.tellProp c
				then T.succeed st
				else T.fail nm "ok on non-trivial constraint" st
			| Splx.IsUnsat _ ->
				if Cs.Contrad = Cs.tellProp c
				then T.succeed st
				else T.fail nm "unsat on non-contradictory constraint" st
		in
		let tcs = [
			"triv0", 1, le [] 1, Splx.mk nxt [];
			"triv1", 1, lt [] 1, Splx.mk nxt [];
			"triv2", 1, eq [] 0, Splx.mk nxt [];
			"triv3", 1, le [] 1, Splx.mk nxt [2, le [1, x] 2];
			"contrad0", 1, le [] (-1), Splx.mk nxt [];
			"contrad1", 2, eq [] 1, Splx.mk nxt [1, le [1, x; 1, y] 2]
		] in
		T.suite "inval" (List.map chk tcs)

	let addUnsatTs: T.testT
	=
		let witnessEq : Splx.witness_t -> Splx.witness_t -> bool
		=	let sort = List.sort (fun (i, _) (i', _) -> Pervasives.compare i i') in
			let coefEq = fun (i, n) (i', n') -> i = i' && Vec.Coeff.cmp n n' = 0 in
			fun w w' -> List.for_all2 coefEq (sort w) (sort w')
		in
		let chk (t, i, c, s, w) = fun state ->
			match s with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk sx ->
				match Splx.add sx (i, c) with
				| Splx.IsOk sx -> T.fail t "got Splx.IsOk _" state
				| Splx.IsUnsat actualW ->
					if witnessEq w actualW
					then T.succeed state
					else T.fail t "not equal" state
		in
		let tcs = [
			"le0", 1, le [1, x] 1, Splx.mk nxt [2, le [-1, x] (-2)],
				[1, Vec.Coeff.u; 2, Vec.Coeff.u]
		] in
		T.suite "unsat" (List.map chk tcs)
	
	
	let addIncrTs : T.testT
	  = let chk : string * Splx.t Splx.mayUnsatT * (Var.t * Scalar.Symbolic.t) list -> T.stateT -> T.stateT
		  = fun (nm, osx, asg) st ->
		  match osx with
		  | Splx.IsUnsat _ -> T.fail nm "unsat" st
		  | Splx.IsOk sx ->
		 if not (checkinv sx) then T.fail nm "invariant" st
		 else
		   let asg' = Splx.getAsg sx in
		   if asgEquiv asg' asg then T.succeed st
		   else
			 let e =
			   Printf.sprintf "got: %s\n\n%s\n"
					  (Cs.Vec.M.to_string "; " (fun a s -> s ^ ": " ^ Scalar.Symbolic.to_string a) varPr asg')
					  (Splx.pr varPr sx)
			 in
			 T.fail nm e st
		in
		[
		  "equality",
		  (Splx.mk nxt [
			 1, eq [1, x; -1, y] 0;
			 2, le [1, x] 1;
			   ]
		   |> Splx.checkFromAdd
		   |> (fun sx -> Splx.addAcc sx (3, le [-1, y] ~-1))
		   |> Splx.checkFromAdd),
		  [x, Scalar.Symbolic.ofRat Vec.Coeff.u; y, Scalar.Symbolic.ofRat Vec.Coeff.u]
		;
		  "basic_in_def_insertBack",
		  (Splx.mk nxt [
			 1, eq [1, y] 0;
			 2, le [1, x] 0;
			 3, le [1, x; 1, y] ~-1;
			 4, le [1, x; 1, z] 1
			   ]
		   |> Splx.checkFromAdd
		   |> (fun sx -> Splx.addAcc sx (5, le [1, z] 1))),
		  [x, Scalar.Symbolic.ofRat Vec.Coeff.negU; y, Scalar.Symbolic.z; z, Scalar.Symbolic.ofRat Vec.Coeff.u]
		;
		  "basic_in_def_rewrite",
		  (Splx.mk nxt [
			 1, eq [1, y] 0;
			 2, le [1, x] 0;
			 3, le [1, x; 1, y] ~-1;
			 4, le [1, x; 1, z] 1
			   ]
		   |> Splx.checkFromAdd
		   |> (fun sx -> Splx.addAcc sx (5, le [1, y; 1, z] 1))),
		  [x, Scalar.Symbolic.ofRat Vec.Coeff.negU; y, Scalar.Symbolic.z; z, Scalar.Symbolic.ofRat Vec.Coeff.u]
		]
		|> List.map chk
		|> T.suite "incr"
	
	
	let addTs: T.testT
	= T.suite "add" [addOkTs; addInvalTs; addUnsatTs; addIncrTs]

	(* Splx.pickbasic_steep *)
	let pickbasic_steepTs : T.testT
	=
		let proj : Splx.choiceT option -> Var.t option
		= function
			| Some ch -> Some ch.Splx.var
			| None -> None
		in
		let pr : Var.t option -> string
		= function
			| Some x -> "Some " ^ Var.to_string x
			| None -> "None"
		in
		let chk : string * Var.t option * Splx.t Splx.mayUnsatT -> T.stateT -> T.stateT
		= fun (nm, v, msx) st ->
			match msx with
			| Splx.IsUnsat _ -> T.fail nm "unsat" st
			| Splx.IsOk sx ->
				let res = proj (Splx.Steep.pickBasic sx) in
				if v = res
				then T.succeed st
				else
					let e = Printf.sprintf "expected %s and got %s" (pr v) (pr res) in
					T.fail nm e st
		in
		let tcs : (string * Var.t option * Splx.t Splx.mayUnsatT) list
		=	let sx0 = nxt in
			let sx1 = Var.next sx0 in
			[
				(* trivial case *)
				"empty", None, Splx.mk nxt [];

				(* lower bound violated *)
				"lower0", Some sx1, Splx.mk nxt [
					0, eq [1, x; 1, y] 2;
					1, eq [1, x; -1, y] 3
				];

				(* lower bound violated, order changed *)
				"lower1", Some sx0, Splx.mk nxt [
					0, eq [1, x; 1, y] 4;
					1, eq [1, x; -1, y] 3
				];

				(* upper bound violated *)
				"upper0", Some sx0, Splx.mk nxt [
					0, le [1, x; 1, y] (-2);
					1, le [-1, x; 1, y] (-1)
				];

				(* upper bound violated, order changed *)
				"upper0", Some sx1, Splx.mk nxt [
					0, le [1, x; 1, y] (-1);
					1, le [-1, x; 1, y] (-2)
				];

				"uplow0", Some sx1, Splx.mk nxt [
					0, le [1, x; 1, y] (-1);
					1, eq [1, y; 2, z] 2
				];
			]
		in
		T.suite "pickbasic_steep" (List.map chk tcs)
	
	(* Splx.pickbasic_bland *)
	let pickbasic_blandFinishedTs: T.testT
	=
		let chk (name, msx) = fun state ->
			match msx with
			| Splx.IsUnsat _ -> T.fail name "unsat" state
			| Splx.IsOk sx ->
				match Splx.Bland.pickBasic sx with
				| None -> T.succeed state
				| Some _ -> T.fail name "found a basic variable" state
		in
		let tcs = [
			"empty", Splx.mk nxt [];
			"sat_bounds", Splx.mk nxt [0, eq [1, x] 0; 1, le [1, x; 1, y] 1]
		] in
		T.suite "finished" (List.map chk tcs)

	let pickbasic_blandTs: T.testT
	= T.suite "pickbasic_bland" [pickbasic_blandFinishedTs]

	(* Splx.pivot *)
	let pivotTs: T.testT
	=
		let chkB (t, xB, xN, msx) = fun state ->
			match msx with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s ->
				let m = Splx.pivot (Splx.get_mat s) xB xN in
				match Cs.Vec.M.get None m xB with
				| None -> T.succeed state
				| Some _ -> T.fail t "basic still basic" state
		in
		let chkN (t, xB, xN, msx) = fun state ->
			match msx with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s ->
				let m = Splx.pivot (Splx.get_mat s) xB xN in
				let nodeB = Cs.Vec.M.get None (Splx.get_mat s) xB in
				let nodeN = Cs.Vec.M.get None m xN in
				match nodeB, nodeN with
				| None, _ | _, None -> T.fail t "not basic" state
				| Some consB, Some consN ->
					match Cs.Vec.isomorph consB consN with
					| None -> T.fail t "bad normalization" state
					| Some n ->
						if Vec.Coeff.cmp (Cs.Vec.get consB xN) n = 0 then
							T.succeed state
						else
							T.fail t (Vec.Coeff.to_string n) state
		in
		let chkProj (t, xB, xN, msx) = fun state ->
			match msx with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s ->
				let m = Splx.pivot (Splx.get_mat s) xB xN in
				let cnt i = function
					| None -> i
					| Some cons -> if Vec.Coeff.cmpz (Cs.Vec.get cons xN) = 0 then i else i + 1
				in
			if Cs.Vec.M.fold (fun _ -> cnt) 0 m = 1
			then T.succeed state
			else T.fail t "cnt > 1" state
		in
		let tcs = [
		"basic", sx1, x, Splx.mk nxt [
			0, le [-1, x; -1, y] (-1);
			1, le [1, x] 1;
			2, eq [-1, x; -1, z] 0;
			3, le [1, z] 2;
			4, le [1, y] 2];

		"coef0", sx1, x, Splx.mk nxt [
			0, le [-1, x; -1, y] (-1);
			1, le [1, x] 1;
			2, eq [-2, x; -1, z] 0;
			3, le [1, z] 2;
			4, le [1, y] 2];

		"coef1", sx1, x, Splx.mk nxt [
			0, le [-2, x; -1, y] (-1);
			1, le [1, x] 1;
			2, eq [-1, x; -1, z] 0;
			3, le [1, z] 2;
			4, le [1, y] 2];

		"noX0", sx1, x, Splx.mk nxt [
			0, le [-2, x; 1, y] (-1);
			1, le [1, y; 2, z] (-2);
			2, le [1, x] 1 ];

		"noX1", sx1, x, Splx.mk nxt [
			0, le [1, x; 1, z] (-1);
			1, le [1, y; 2, z] 3;
			2, le [1, x; 2, y] 3;
			3, le [1, x] 2 ]
		] in
		T.suite "pivot" [
			T.suite "basic" (List.map chkB tcs);
			T.suite "non-basis" (List.map chkN tcs);
			T.suite "proj" (List.map chkProj tcs)
		]
	
	let stepUnsatTs : T.testT
	=
		let chk : Splx.strategyT -> string * Splx.t Splx.mayUnsatT
			-> T.stateT -> T.stateT
		= fun strgy (nm, msx) state ->
			match msx with
			| Splx.IsUnsat _ -> T.fail nm "unsat" state
			| Splx.IsOk sx ->
				match Splx.step strgy sx with
				| Splx.StepCont _
				| Splx.StepSat _ -> T.fail nm "not StepUnsat" state
				| Splx.StepUnsat _ -> T.succeed state
		in
		let tcs = [
		"unit0", Splx.mk nxt [
			0, le [1, x] 0;
			1, le [1, y] 0;
			2, eq [1, x; 1, y] 1]

		] in
		T.suite "Unsat" (List.map (chk Splx.Steep) tcs)

	let stepTs : T.testT
	= T.suite "step" [stepUnsatTs]

	let checkSatTs =
		let chk (t, s0) = fun state ->
			match Splx.checkFromAdd s0 with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk _ ->
				match ccheckFromAdd s0 with
				| InvOk _ -> T.succeed state
				| BadInv _ -> T.fail t "invariant" state
		in
		let tcs = [
			"decompress0", Splx.mk nxt [
				0, le [-1, y] (-1);
				1, le [-1, x; 16, y] 15;
				2, le [-1, x] (-17)];

			"decompress1", Splx.mk nxt [
				0, le [-1, y] (-1);
				1, le [-1, x; 16, y] (-18);
				2, le [-1, x] (-17)];

			"decompress2", Splx.mk nxt [
				0, le [-1, y] (-1);
				1, le [-1, x; 16, y] 15;
				2, le [-1, x] (-17);
				3, le [-1, x; 16, y] (-18)]
		] in
		T.suite "sat" (List.map chk tcs)

	let checkUnsatTs: T.testT
	=
		let chk (t, w, sx) = fun state ->
			let print_witness (l: Splx.witness_t): string =
				let pretty1 ((i: int), (s: Vec.Coeff.t)): string =
					(Vec.Coeff.to_string s) ^ " * eq" ^ (string_of_int i) in
				let rec p (s: string) (l': Splx.witness_t): string =
					match l' with [] -> s
					| h::t -> if String.length s = 0 then p (pretty1 h) t
						else p (s ^ ", " ^ (pretty1 h)) t in
				"got " ^ (p "" l)
			in
			let check_witness (w: Splx.witness_t) (w': Splx.witness_t): bool =
				let p (acc: bool) ((i: int), (s: Vec.Coeff.t)): bool =
					let p' ((i': int), (s': Vec.Coeff.t)): bool =
						(i = i') && Vec.Coeff.cmp s s' = 0
					in
					acc && (try let _ = List.find p' w' in true
						with Not_found -> false)
				in
				List.fold_left p true w
			in
			match Splx.checkFromAdd sx with
			| Splx.IsOk _ -> T.fail t "sat" state
			| Splx.IsUnsat f ->
				if check_witness w f
				then
					match ccheckFromAdd sx with
					| BadInv _ -> T.fail t "invariant" state
					| InvOk (Splx.IsOk _) -> T.fail t "ccheck sat" state
					| InvOk (Splx.IsUnsat f) ->
						if check_witness w f
						then T.succeed state
						else T.fail t "ccheck unsat" state
				else T.fail t (print_witness f) state
		in
		let tcs = [
			"simple_le0", [0, Vec.Coeff.u; 1, Vec.Coeff.u; 2, Vec.Coeff.mk1 2], Splx.mk nxt [
				0, le  [-1, x] (-3);
				1, le [1, x; 2, y] 3;
				2, le [-1, y] (-1)];

			"simple_le1", [0, Vec.Coeff.u; 1, Vec.Coeff.u; 2, Vec.Coeff.u], Splx.mk nxt [
				0, le [-1, x] (-3);
				1, le [1, x; 2, y] 3;
				2, le [-2, y] (-2)];

			"simple_eq0", [0, Vec.Coeff.u; 1, Vec.Coeff.u; 2, Vec.Coeff.mk1 2], Splx.mk nxt [
				0, le  [-1, x] (-3);
				1, eq [1, x; 2, y] 3;
				2, le [-1, y] (-1)];

			"simple_eq1", [0, Vec.Coeff.u; 1, Vec.Coeff.negU; 2, Vec.Coeff.mk1 2], Splx.mk nxt [
				0, le [-1, x] (-3);
				1, eq [-1, x; -2, y] (-3);
				2, le [-1, y] (-1)];

			"simple_eq2", [0, Vec.Coeff.u; 1, Vec.Coeff.u; 2, Vec.Coeff.mk1 2], Splx.mk nxt [
				0, le [-1, x] (-3);
				1, eq [1, x; 2, y] 3;
				2, le [-1, y] (-1)];

			"useless_l0", [0, Vec.Coeff.u; 2, Vec.Coeff.u; 3, Vec.Coeff.mk1 2], Splx.mk nxt [
				0, le [-1, x] (-3);
				1, le [1, x; 1, y] 4;
				2, eq [1, x; 2, y] 3;
				3, le [-1, y] (-1)];

			"useless_l1", [0, Vec.Coeff.u; 2, Vec.Coeff.u; 3, Vec.Coeff.mk1 2], Splx.mk nxt [
				0, le [-1, x] (-3);
				1, le [1, x] 4;
				2, eq [1, x; 2, y] 3;
				3, le [-1, y] (-1)]
		] in
		T.suite "unsat" (List.map chk tcs)

	let checkTs: T.testT
	= T.suite "check" [checkSatTs; checkUnsatTs]

	let getAsgTs : T.testT
	  = let chk : (string * Splx.t Splx.mayUnsatT * (Var.t * Scalar.Symbolic.t) list) -> T.stateT -> T.stateT
		  = fun (nm, sx, asg) st ->
		  match Splx.checkFromAdd sx with
		  | Splx.IsUnsat _ -> T.fail nm "unsat" st
		  | Splx.IsOk sx' ->
		 let asg' = Splx.getAsg sx' in
		 if asgEquiv asg' asg
		 then T.succeed st
		 else
		   let estr =
			 Printf.sprintf "assignement: %s\n\nfinal tableau:\n%s\n"
					(Cs.Vec.M.to_string "; " (fun a s -> s ^ ": " ^ Scalar.Symbolic.to_string a) varPr asg')
					(Splx.pr varPr sx')
		   in
		   T.fail nm estr st
		in
		[
		  "empty",
		  Splx.mk Var.u [], [];

		  "bounded",
		  Splx.mk nxt
			  [
			1, le [1, x] 1
			  ],
		  [x, Scalar.Symbolic.z];

		  "unbounded_immediate",
		  Splx.mk nxt
			  [
			1, le [1, x; 1, y] 1
			  ],
		  [x, Scalar.Symbolic.z; y, Scalar.Symbolic.z];

		  "one_unbounded",
		  Splx.mk nxt
			  [
			1, le [-1, x] ~-1;
			2, le [1, x; 1, y] 1;
			3, le [1, x; 1, z] ~-2;
			4, le [1, z] 1
			  ],
		  [x, Scalar.Symbolic.ofRat Vec.Coeff.u; y, Scalar.Symbolic.z; z, Scalar.Symbolic.ofRat (Vec.Coeff.mk1 ~-3)];

		  "two_unbounded",
		  Splx.mk nxt
			  [
			1, le [-1, x] ~-1;
			2, le [1, x; 1, y] 1;
			3, le [1, x; 1, z] ~-2
			  ],
		  [x, Scalar.Symbolic.ofRat Vec.Coeff.u; y, Scalar.Symbolic.z; z, Scalar.Symbolic.ofRat (Vec.Coeff.mk1 ~-3)];

		  (* When [y] is eliminated, [x] disappears from the matrix of constraints. *)
		  "vanishing_var",
		  Splx.mk nxt
			  [
			1, le [-1, x] ~-1;
			2, le [1, x; 1, y] 1;
			3, le [-1, x; -1, y; 1, z] 1;
			4, le [1, z] 1
			  ],
		  [x, Scalar.Symbolic.ofRat Vec.Coeff.u; y, Scalar.Symbolic.z; z, Scalar.Symbolic.z]
		]
		|> List.map chk
		|> T.suite "getAsg"

	let insertBackTs : T.testT
	= let chkInv : string * Var.t * Cs.Vec.t * Splx.t -> T.stateT -> T.stateT
		= fun (nm, x, _, sx) st ->
		let sx' = Splx.insertBack x sx in
		if checkinv sx' then T.succeed st
		else
		  let e = Printf.sprintf "%s" (Splx.pr varPr sx') in
		  T.fail nm e st
	  in
	  let chkCons : string * Var.t * Cs.Vec.t * Splx.t -> T.stateT -> T.stateT
		= fun (nm, x, v, sx) st ->
		let sx' = Splx.insertBack x sx in
		match Cs.Vec.M.get None (Splx.get_mat sx') x with
		| None ->
		   let e = Printf.sprintf "not a basic variable\n%s" (Splx.pr varPr sx') in
		   T.fail nm e st
		| Some v' ->
		   if Cs.Vec.equal v v' then T.succeed st
		   else
		   let e = Printf.sprintf "incorrect constraint\n%s" (Splx.pr varPr sx') in
		   T.fail nm e st
	  in
	  let chkDef : string * Var.t * Cs.Vec.t * Splx.t -> T.stateT -> T.stateT
		= fun (nm, x, _, sx) st ->
		let sx' = Splx.insertBack x sx in
		match Splx.Defs.getDef sx'.Splx.defs x with
		| None -> T.succeed st
		| Some _ ->
		   let e = Printf.sprintf "%s" (Splx.pr varPr sx') in
		   T.fail nm e st
	  in
	  let out : Splx.t Splx.mayUnsatT -> Splx.t
		= function
		| Splx.IsUnsat _ -> Pervasives.failwith "Splx_t.insertBackTs"
		| Splx.IsOk sx -> sx
	  in
	  [
		"simple", z, eq [1, x; 1, z; -1, sx2] 0 |> Cs.get_v,
		(Splx.mk nxt [
			   1, le [-1, x] ~-1;
			   2, le [-1, y] ~-1;
			   3, le [1, x; 1, y] 2;
			   4, le [1, x; 1, z] 2
			 ]
		 |> Splx.checkFromAdd |> out)
	  ;
		"eliminated_later", x, eq [1, x; -1, z; -1, sx1; 1, sx2] 0 |> Cs.get_v,
		(Splx.mk nxt [1, le [1, x; 1, y] 1] |> out
		 |> Splx.Preprocessing.elim x sx1
		 |> (fun sx -> Splx.add sx (2, le [1, y; 1, z] 1)) |> out
		 |> Splx.Preprocessing.elim y sx2)
	  ]
	  |> List.map (fun tc -> List.map (fun f -> f tc) [chkInv; chkCons; chkDef])
	  |> List.concat
	  |> T.suite "insertBackTs"

	let complOkTs: T.testT
	=
		let getstate s x =
			let z = { Splx.v = Scalar.Symbolic.z; Splx.low = None; Splx.up = None } in
				Cs.Vec.M.get z (Splx.get_state s) x
		in
		let chk_inv (t, id, _, s) = fun state ->
			match Splx.checkFromAdd s with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s ->
				if checkinv s
				then
					let s1 = Splx.compl s id in
					if checkinv s1
					then T.succeed state
					else
						let e = "invariant\n" ^ (Splx.pr varPr s1) in
						T.fail t e state
				else failwith "Splx_t.compl_ok_ts"
		in
		let chk_same (t, id, x, s) = fun state ->
			match Splx.checkFromAdd s with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s ->
				let s1 = Splx.compl s id in
				let xb0 = getstate s x in
				let xb1 = getstate s1 x in
				match (Splx.get_low xb0) with
				| Some l when (Splx.get_id l) = id ->
					if None = (Splx.get_low xb1)
					then T.succeed state
					else T.fail t "not equal" state
				| _ ->
					match (Splx.get_up xb0) with
					| Some u when (Splx.get_id u) = id ->
						if None = (Splx.get_up xb1)
						then T.succeed state
						else T.fail t "not equal" state
					| _ -> failwith "Splx_t.compl_ok_ts"
		in
		let chk_compl (t, id, x, s) = fun state ->
			match Splx.checkFromAdd s with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s ->
				let s1 = Splx.compl s id in
				let xb0 = getstate s x in
				let xb1 = getstate s1 x in
				let cmp v1 optv2 =
					match optv2 with
					| None -> ("None", false)
					| Some v2 ->
						let e = "Some (" ^ (Scalar.Symbolic.to_string (Splx.get_bv v2)) ^ ")" in
						(e, Scalar.Symbolic.cmp v1 (Splx.get_bv v2) = 0)
				in
				match Splx.get_low xb0 with
				| Some l when (Splx.get_id l) = id ->
					let (e, r) = cmp (Scalar.Symbolic.subdelta (Splx.get_bv l))
						(Splx.get_up xb1)
					in
					if r
					then T.succeed state
					else T.fail t e state
				| _ ->
					match Splx.get_up xb0 with
					| Some u when (Splx.get_id u) = id ->
						let (e, r) = cmp (Scalar.Symbolic.adddelta (Splx.get_bv u))
							(Splx.get_low xb1)
						in
						if r
						then T.succeed state
						else T.fail t e state
					| _ -> failwith "Splx_t.compl_ok_ts"
		in
		let tcs = [
			"one0", 0, x, Splx.mk nxt [
				0, le [1, x] 1
			];

			"one0", 0, sx1, Splx.mk nxt [
				0, le [1, x; 2, y] 2
			];

			"m0", 1, y, Splx.mk nxt [
				0, le [1, x; 2, y] 2;
				1, le [1, y] 1;
				2, le [-1, x] 1
			];

			"m1", 2, sx2, Splx.mk nxt [
				0, le [1, x; 2, y] 2;
				1, le [1, y] 1;
				2, le [1, y; 2, z] 2;
				3, le [1, z] 30;
				4, le [-1, x] 1
			]

		] in
		T.suite "ok" [
			T.suite "inv" (List.map chk_inv tcs);
			T.suite "same" (List.map chk_same tcs);
			T.suite "compl" (List.map chk_compl tcs)
		]

	let complKoTs: T.testT
	=
		let chk (t, id, s) = fun state ->
			match Splx.checkFromAdd s with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s ->
				try
					let _ = Splx.compl s id in
					T.fail t "no exception raised" state
				with
				| Invalid_argument "Splx.compl" -> T.succeed state
				| _ -> T.fail t "unexpected exception raised" state
		in
		let tcs = [
			"empty", 0, Splx.mk nxt [];
			"one0", 1, Splx.mk nxt [
				0, le [1, x] 1];

			"one1", 1, Splx.mk nxt [
				0, le [1, x; 1, y] 1];

			"0", 2, Splx.mk nxt [
				0, le [1, x] 1;
				1, le [1, x; 1, y] 1];

			"hole0", 1, Splx.mk nxt [
				0, le [1, x] 1;
				2, le [1, x; 1, y] 1]
		] in
		T.suite "ko" (List.map chk tcs)

	let complTs: T.testT
	= T.suite "compl" [complKoTs; complOkTs]

	let strictenOkTs: T.testT
	=
		let chk_inv (t, id, _, s) = fun state ->
			match Splx.checkFromAdd s with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s1 ->
				if checkinv s1
				then
					match Splx.stricten s1 id with
					| Splx.IsUnsat _ -> T.fail t "unsat" state
					| Splx.IsOk s2 ->
						if checkinv s2
						then T.succeed state
						else
							let e = "invariant\n" ^ (Splx.pr varPr s2) in
							T.fail t e state
				else failwith "Splx_t.stricten_ok_ts"
		in
		let chk_bnd (t, id, x, s) = fun state ->
			let getstate s x =
				let z = { Splx.v = Scalar.Symbolic.z; Splx.low = None; Splx.up = None }
				in
					Cs.Vec.M.get z (Splx.get_state s) x
			in
			match s with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s ->
				match Splx.stricten s id with
				| Splx.IsUnsat _ -> T.fail t "unsat" state
				| Splx.IsOk s2 ->
					let st1 = getstate s x in
					let st2 = getstate s2 x in
					let pr = fun v1 v2 -> Printf.sprintf "expected: %s, got: %s"
						(Scalar.Symbolic.to_string v1) (Scalar.Symbolic.to_string v2)
					in
					match (Splx.get_low st1), (Splx.get_low st2) with
					| Some l1, Some l2 when (Splx.get_id l1) = id ->
						if Scalar.Symbolic.hasDelta (Splx.get_bv l1) then
							T.fail t "original bound has a delta" state
						else
							let v = Scalar.Symbolic.adddelta (Splx.get_bv l1) in
							if Scalar.Symbolic.cmp v (Splx.get_bv l2) = 0 then
								T.succeed state
							else
								T.fail t (pr v (Splx.get_bv l2)) state
					| None, Some _ -> T.fail t "bound appeared" state
					| Some _, None -> T.fail t "bound disappeared" state
					| Some _, Some _ | None, None ->

					match (Splx.get_up st1), (Splx.get_up st2) with
					| Some u1, Some u2 when (Splx.get_id u1) = id ->
						if Scalar.Symbolic.hasDelta (Splx.get_bv u1) then
							T.fail t "original bound has a delta" state
						else
							let v = Scalar.Symbolic.subdelta (Splx.get_bv u1) in
							if Scalar.Symbolic.cmp v (Splx.get_bv u2) = 0 then
								T.succeed state
							else
								T.fail t (pr v (Splx.get_bv u2)) state
					| Some _, Some _ -> T.fail t "invalid id" state
					| None, Some _ -> T.fail t "bound appeared" state
					| Some _, None -> T.fail t "bound disappeared" state
					| None, None -> T.fail t "no bound" state
		in
		let tcs = [
			"up0", 0, x, Splx.mk nxt [
				0, le [1, x] 0 ];

			"up1", 0, x, Splx.mk nxt [
				0, le [1, x] 1 ];

			"up2", 0, sx1, Splx.mk nxt [
				0, le [1, x; 1, y] 1 ];

			"up3", 0, x, Splx.mk nxt [
				0, le [1, x] (-1) ];

			"low0", 0, x, Splx.mk nxt [
				0, le [-1, x] 0 ];

			"m0", 1, sx1, Splx.mk nxt [
				0, le [1, x] 0;
				1, le [1, x; 1, y] 1;
				2, le [1, y; 1, z] 0;
				3, le [1, y] 15;
				4, le [1, z] 15 ];

			"m1", 0, x, Splx.mk nxt [
				0, le [1, x] 0;
				1, le [1, x; 1, y] 1;
				2, le [1, y; 1, z] 0 ]
		] in
		T.suite "ok" [
			T.suite "inv" (List.map chk_inv tcs);
			T.suite "bnd" (List.map chk_bnd tcs)
		]

	let strictenInvalTs: T.testT
	=
		let chk (t, id, s) = fun state ->
			match s with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s ->
				try
					let _ = Splx.stricten s id in
					T.fail t "no exception raised" state
				with
				| Invalid_argument "Splx.stricten" -> T.succeed state
				| _ -> T.fail t "unexpected exception raised" state
		in
		let tcs = [
		(* missing identifier *)
			"empty", 0, Splx.mk nxt [];
			"one0", 1, Splx.mk nxt [
				0, le [1, x] 1];

			"one1", 1, Splx.mk nxt [
				0, le [1, x; 1, y] 1];

			"0", 2, Splx.mk nxt [
				0, le [1, x] 1;
				1, le [1, x; 1, y] 1];

			"hole0", 1, Splx.mk nxt [
				0, le [1, x] 1;
				2, le [1, x; 1, y] 1];

		(* already strict bound *)
			"up0", 0, Splx.mk nxt [
				0, lt [1, x] 0 ];

			"up1", 0, Splx.mk nxt [
				0, lt [1, x] 1 ];

			"up2", 0, Splx.mk nxt [
				0, lt [1, x; 1, y] 1 ];

			"up3", 0, Splx.mk nxt [
				0, lt [1, x] (-1) ];

			"low0", 0, Splx.mk nxt [
				0, lt [-1, x] 0 ];

			"m0", 1, Splx.mk nxt [
				0, le [1, x] 0;
				1, lt [1, x; 1, y] 1;
				2, le [1, y; 1, z] 0;
				3, le [1, y] 15;
				4, le [1, z] 15 ];

			"m1", 0, Splx.mk nxt [
				0, lt [1, x] 0;
				1, le [1, x; 1, y] 1;
				2, le [1, y; 1, z] 0 ]
		] in
		T.suite "inval" (List.map chk tcs)
	
	let strictenUnsatTs: T.testT
		= let cmp (i1,_) (i2,_) =
			match Pervasives.compare i1 i2 with
			| 0 -> invalid_arg "Cert.cmp"
			| n -> n
		in
		let sort a = List.sort cmp a
		in
		let isEqF (f1: Splx.witness_t) (f2: Splx.witness_t): bool =
			List.length f1 = List.length f2 &&
			List.for_all2 (fun (i1, n1) (i2, n2) -> i1 = i2 && Cs.Vec.Coeff.cmp n1 n2 = 0)
			(sort f1) (sort f2)
		in
		let chk (t, id, w, s) = fun state ->
			match s with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s ->
				match Splx.stricten s id with
				| Splx.IsOk _ -> T.fail t "not unsat" state
				| Splx.IsUnsat w1 ->
					if isEqF w w1
					then T.succeed state
					else T.fail t ("bad cert" ^ (Splx.Witness.to_string w1)) state
		in
		let tcs = [
			"simpl0", 0, [0, Vec.Coeff.u; 1, Vec.Coeff.u], Splx.mk nxt [
				0, le [1, x] 0;
				1, le [-1, x] 0 ];

			"scale0", 0, [0, Vec.Coeff.mk 2 1; 1, Vec.Coeff.u], Splx.mk nxt [
				0, le [2, y] 0;
				1, le [-1, y] 0 ]
		] in
		T.suite "unsat" (List.map chk tcs)

	let strictenTs: T.testT
	= T.suite "stricten" [strictenOkTs; strictenInvalTs; strictenUnsatTs]

	let forgetOkTs: T.testT
	=
		let chk_inv (t, id, _, s) = fun state ->
			match s with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s ->
				if checkinv s
				then
					let s2 = Splx.forget s id in
					if checkinv s2
					then T.succeed state
					else
						let e = "invariant\n" ^ (Splx.pr varPr s2) in
						T.fail t e state
				else failwith "Splx_t.forget_ok_ts"
		in
		let chk_bnd (t, id, x, s) = fun state ->
			let getbnd s x = Cs.Vec.M.get
				{ Splx.v = Scalar.Symbolic.z; Splx.low = None; Splx.up = None }
					(Splx.get_state s) x
			in
			let chkbnd b1 b2 =
				match b1, b2 with
				| Some b, None when (Splx.get_id b) = id -> true
				| Some b, Some _ when (Splx.get_id b) = id -> false
				| None, None -> true
				| None, Some _ -> false
				| Some _, _ -> true
			in
			match s with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s ->
				let s2 = Splx.forget s id in
				let b1 = getbnd s x in
				let b2 = getbnd s2 x in
				let rup = chkbnd (Splx.get_up b1) (Splx.get_up b2) in
				let rlow = chkbnd (Splx.get_low b1) (Splx.get_low b2) in
				if rlow && rup
				then T.succeed state
				else
					let e = "bound not forgotten\n" ^ (Splx.pr varPr s2) in
					T.fail t e state
		in
		let chk_mat (t, id, x, s) = fun state ->
			match s with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s ->
				let xbnd = Cs.Vec.M.get
					{ Splx.v = Scalar.Symbolic.z; Splx.low = None; Splx.up = None }
						(Splx.get_state s) x
				in
				if (Splx.get_up xbnd) = None || (Splx.get_low xbnd) = None then
					let count = Cs.Vec.M.fold
						(fun _ n -> function None -> n | Some _ -> n + 1) 0
					in
					let s2 = Splx.forget s id in
					let cnt1 = count (Splx.get_mat s) in
					let cnt2 = count (Splx.get_mat s2) in
					if cnt1 - cnt2 = 1
					then T.succeed state
					else
						let e = "cnt1 - cnt2 = " ^ (string_of_int (cnt1 - cnt2)) in
						T.fail t e state
				else T.succeed state
		in
		let tcs = [
			"one0", 0, x, Splx.mk nxt [
				0, le [1, x] 1;
				1, le [-1, x] 0];

			"cons0", 0, x, Splx.mk nxt [
				0, le [1, x] 1;
				1, le [1, x; 1, y] 0;
				2, le [1, x; 1, z] 0;
				3, le [1, y] 0;
				4, le [1, z] 0];

			"cons1", 0, x, Splx.mk nxt [
				0, le [1, x] 1;
				1, le [1, x; 1, y] 0]
		] in
		T.suite "ok" [
			T.suite "inv" (List.map chk_inv tcs);
			T.suite "bnd" (List.map chk_bnd tcs);
			T.suite "mat" (List.map chk_mat tcs)
		]

	let forgetKoTs: T.testT
	=
		let chk (t, id, s) = fun state ->
			match Splx.checkFromAdd s with
			| Splx.IsUnsat _ -> T.fail t "unsat" state
			| Splx.IsOk s -> try
					let _ = Splx.forget s id in
					T.fail t "no exception raised" state
				with
				| Invalid_argument "Splx.forget" -> T.succeed state
				| _ -> T.fail t "unexpected exception raised" state
		in
		let tcs = [
			"empty", 0, Splx.mk nxt [];
			"one0", 1, Splx.mk nxt [
				0, le [1, x] 1];

			"one1", 1, Splx.mk nxt [
				0, le [1, x; 1, y] 1];

			"0", 2, Splx.mk nxt [
				0, le [1, x] 1;
				1, le [1, x; 1, y] 1];

			"hole0", 1, Splx.mk nxt [
				0, le [1, x] 1;
				2, le [1, x; 1, y] 1]
		] in
		T.suite "ko" (List.map chk tcs)

	let forgetTs: T.testT
	= T.suite "forget" [forgetOkTs; forgetKoTs]
	
	let ts: T.testT
	= T.suite Cs.Vec.V.name [
		mergeBTs; setSymbolicTs; fitBndTs; addTs;
		pickbasic_blandTs;
		pickbasic_steepTs;
		stepTs;
		getAsgTs;
		insertBackTs;
		checkTs; complTs; strictenTs; forgetTs
	]
end

module Positive = Test(Cstr.Rat.Positive)
(*
module Int = Test(Cstr.Rat.Int)

let ts : T.testT
	= T.suite "Splx" [Positive.ts ; Int.ts] 
*)

let ts : T.testT
	= T.suite "Splx" [Positive.ts] 

