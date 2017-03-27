(* Test suite for Opt. *)
open Vpl
open Splx

let x = V.fromInt 1
let y = V.fromInt 2
let z = V.fromInt 3
let t = V.fromInt 4

let nxt = V.fromInt 5

let varPr: V.t -> string
= fun x ->
	let vars = [x, "x"; y, "y"; z, "z"; t, "t"] in
	try
		List.assoc x vars
	with
	| Not_found -> "v" ^ (V.to_string x)

let mkc t v c =
	Cs.mk t (List.map (fun (c, v) -> (Scalar.Rat.mk1 c, v)) v) (Scalar.Rat.mk1 c)

let sxLift : Splx.t Splx.mayUnsatT -> Splx.t
= function
	| Splx.IsOk sx -> sx
	| Splx.IsUnsat _ -> Pervasives.invalid_arg "Opt_t.sxLift"

let eq = mkc Cstr.Eq
let le = mkc Cstr.Le
let lt = mkc Cstr.Lt

let eqProgress b1 b2 =
	match b1, b2 with
	| Opt.Unbnd, Opt.Unbnd -> true
	| Opt.NoChange, Opt.NoChange -> true
	| Opt.UpTo n1, Opt.UpTo n2 -> Scalar.Symbolic.cmp n1 n2 = 0
	| _, _ -> false

let eqNext n1 n2 =
	match n1, n2 with
	| Opt.GoOn _, Opt.GoOn _ -> failwith "Opt_t.eqNext: unimplemented case"
	| Opt.OptUnbnd, Opt.OptUnbnd -> true
	| Opt.OptFinite v1, Opt.OptFinite v2 -> Scalar.Symbolic.cmp v1 v2 = 0
	| Opt.GoOn _, Opt.OptUnbnd
	| Opt.GoOn _, Opt.OptFinite _
	| Opt.OptUnbnd, Opt.GoOn _
	| Opt.OptUnbnd, Opt.OptFinite _
	| Opt.OptFinite _, Opt.OptUnbnd
	| Opt.OptFinite _, Opt.GoOn _ -> false
			
(* XXX: does not handle the tableau part of the Finite and Sup constructors *)
let eqOpt o1 o2 =
	match o1, o2 with
	| Opt.Finite (_, n1, w1), Opt.Finite (_, n2, w2)
	| Opt.Sup (_, n1, w1), Opt.Sup (_, n2, w2) -> Scalar.Rat.cmp n1 n2 = 0 && Splx.Witness.equal w1 w2
	| Opt.Infty, Opt.Infty -> true
	| Opt.Finite (_, _, _), Opt.Sup (_, _, _)
	| Opt.Finite (_, _, _), Opt.Infty
	| Opt.Sup (_, _, _), Opt.Finite (_, _, _)
	| Opt.Sup (_, _, _), Opt.Infty
	| Opt.Infty, Opt.Finite (_, _, _)
	| Opt.Infty, Opt.Sup (_, _, _) -> false

(* The invariant on the simplex tableau which is maintained during the optimization is stronger
than that of the satisfiability algorithm.
All the constraints of the constraint matrix should be satisfied and
all the variables, basic or non-basic, should be within their bounds. *)
let chkInv s0 =
	let chkBnds s =
		let chkBnd st =
			(match Splx.get_low st with
			| Some l -> Scalar.Symbolic.cmp (Splx.get_bv l) (Splx.get_v st) <= 0
			|None -> true) &&
			(match Splx.get_up st with
			| Some u -> Scalar.Symbolic.cmp (Splx.get_bv u) (Splx.get_v st) >= 0
			|None -> true)
		in
		Rtree.fold (fun _ a n -> a && (chkBnd n)) true (Splx.get_state s) in
	let chkMat s =
		let chkCons maybeC =
			let rec eval a ve st =
				match ve, st with
				| Rtree.Nil, _ | _, Rtree.Nil-> a
				| Rtree.Sub (l1, n1, r1), Rtree.Sub (l2, n2, r2) ->
					let incr = Scalar.Symbolic.add a (Scalar.Symbolic.mulr n1 (Splx.get_v n2)) in
					eval (eval incr l1 l2) r1 r2
			in
			match maybeC with
			| None -> true
			| Some c -> Scalar.Symbolic.cmp Scalar.Symbolic.z (eval Scalar.Symbolic.z c (Splx.get_state s)) = 0
		in
		Rtree.fold (fun _ a n -> a && (chkCons n)) true (Splx.get_mat s)
	in
	chkMat s0 && chkBnds s0

(* Opt.setObj *)
let setObjTs: T.testT
=
	let chkVar (t, obj, s) = fun state ->
		let (oVar, s1) = Opt.setObj (sxLift s) obj in
		match Rtree.get None (Splx.get_mat s1) oVar with
		| None -> T.fail t "no constraint associated with z" state
		| Some cons -> (* This is a light check. *)
			if Scalar.Rat.cmp (Vec.get cons oVar) Scalar.Rat.u = 0 then
				T.succeed state
			else
				T.fail t "not equal" state
	in
	let chkInv (t, obj, s) = fun state ->
		let (_, s1) = Opt.setObj (sxLift s) obj in
		if chkInv s1 then
			T.succeed state
		else
			let msg = Printf.sprintf "invariant\n%s" (Splx.pr varPr s1) in
			T.fail t msg state
	in
	let chkBnd (t, obj, s) = fun state ->
		let (oVar, s1) = Opt.setObj (sxLift s) obj in
		let oSt = Rtree.get {Splx.v = Scalar.Symbolic.z; Splx.low = None; Splx.up = None} (Splx.get_state s1) oVar in
		if (Splx.get_low oSt) = None && (Splx.get_up oSt) = None then
			T.succeed state
		else
			T.fail t "bad bounds" state
	in
	let tcs = [
		"nil0", Vec.nil, Splx.mk nxt [];
		"nil1", Vec.mk [Scalar.Rat.u, x], Splx.mk nxt [];
		"nil2", Vec.mk [Scalar.Rat.u, x; Scalar.Rat.u, y], Splx.mk nxt [];
		"u0", Vec.mk [Scalar.Rat.u, x], Splx.mk nxt [ 0, le [1, x] 1 ];
		"u1", Vec.mk [Scalar.Rat.u, x], Splx.mk nxt [ 0, le [-1, x] (-1) ];
		"m0", Vec.mk [Scalar.Rat.u, x; Scalar.Rat.u, y], Splx.mk nxt [
			0, eq [1, y] 3;
			1, le [-1, x] (1) ]
	] in
	T.suite "setObj" [
		T.suite "var" (List.map chkVar tcs);
		T.suite "invariant" (List.map chkInv tcs);
		T.suite "bnd" (List.map chkBnd tcs)
	]

(* Opt.pickNBasic *)
let pickNBasicTs: T.testT
=
	let chk (t, a, (o, s)) = fun state ->
		let isMoveEq (v1, d1, b1) (v2, d2, b2) = eqProgress b1 b2 && v1 = v2 && d1 = d2 in
		match Opt.pickNBasic o s with
		| Opt.Done ->
			if a = None then
				T.succeed state
			else
				T.fail t "not equal to None" state
		| Opt.Move (v, d, b) as r ->
			let msg = Printf.sprintf "pickNBasic found %s" (Opt.prAction varPr r) in
			match a with
			| None -> T.fail t msg state
			| Some aL ->
				if List.exists (isMoveEq (v, d, b)) aL then
					T.succeed state
				else
					T.fail t msg state
	in
	let tcs = [
		"nilV0", None, Opt.setObj (sxLift (Splx.mk nxt []))
			Vec.nil;

		"nilV1", None, Opt.setObj (sxLift (Splx.mk nxt [
				0, le [1, x; 1, y] 2 ]))
			Vec.nil;

		"nilS0", Some [
				x, Opt.Incr, Opt.Unbnd
			], Opt.setObj (sxLift (Splx.mk nxt []))
			(Vec.mk [Scalar.Rat.u, x]);

		"nilS1", Some [
				x, Opt.Incr, Opt.Unbnd;
				y, Opt.Incr, Opt.Unbnd
			], Opt.setObj (sxLift (Splx.mk nxt []))
			(Vec.mk [Scalar.Rat.u, x; Scalar.Rat.u, y]);

		"nilS2", Some [
				x, Opt.Decr, Opt.Unbnd;
				y, Opt.Incr, Opt.Unbnd
			], Opt.setObj (sxLift (Splx.mk nxt []))
			(Vec.mk [Scalar.Rat.negU, x; Scalar.Rat.u, y]);

		"bnd0", Some [
				x, Opt.Incr, Opt.UpTo (Scalar.Symbolic.ofRat (Scalar.Rat.mk1 2))
			], Opt.setObj (sxLift (Splx.mk nxt [
				0, le [1, x] 2 ]))
			(Vec.mk [Scalar.Rat.u, x]);

		"bnd1", Some [
				x, Opt.Incr, Opt.Unbnd
			], Opt.setObj (sxLift (Splx.mk nxt [
				0, le [1, x; 1, y] 2;
				1, le [1, y] 1 ]))
			(Vec.mk [Scalar.Rat.u, x]);

		"ubnd0", Some [
				x, Opt.Decr, Opt.Unbnd
			], Opt.setObj (sxLift (Splx.mk nxt [
				0, le [1, x] 2 ]))
			(Vec.mk [Scalar.Rat.negU, x]);

		"notFirst0", Some [
				y, Opt.Incr, Opt.Unbnd
			], Opt.setObj (sxLift (Splx.mk nxt [
				0, eq [1, x] 0 ]))
			(Vec.mk [Scalar.Rat.u, x; Scalar.Rat.u, y]);

		"notFirst1", Some [
				y, Opt.Incr, Opt.UpTo (Scalar.Symbolic.ofRat (Scalar.Rat.mk1 2))
			], Opt.setObj (sxLift (Splx.mk nxt [
				0, eq [1, x] 0;
				1, le [1, y] 2 ]))
			(Vec.mk [Scalar.Rat.u, x; Scalar.Rat.u, y]);

		"none0", None, Opt.setObj (sxLift (Splx.mk nxt [
				0, eq [1, x] 0;
				1, le [1, y] (-1) ]))
			(Vec.mk [Scalar.Rat.u, x; Scalar.Rat.u, y])
	] in
	T.suite "pickNBasic" (List.map chk tcs)

(* Opt.pickBasic *)
let pickBasicTs: T.testT
=
	let chkV (t, xN, inBnd, dir, xB, _, (_, s)) = fun state ->
		let (xB1, _) = Opt.pickBasic s xN inBnd dir in
		if xB = xB1 then
			T.succeed state
		else
			T.fail t (varPr xB1) state
	in
	let chkP (t, xN, inBnd, dir, _, xNBnd, (_, s)) = fun state ->
		let (_, xNBnd1) = Opt.pickBasic s xN inBnd dir in
		if eqProgress xNBnd xNBnd1 then
			T.succeed state
		else
			T.fail t (Opt.prProgress xNBnd1) state
	in
	let sx1 = nxt in
	let sx2 = V.next sx1 in
	let tcs = [
		"nil0", x, Opt.Unbnd, Opt.Incr, x, Opt.Unbnd,
			Opt.setObj (sxLift (Splx.mk nxt []))
			(Vec.mk [Scalar.Rat.u, x]);

		"up0", x, Opt.Unbnd, Opt.Decr, sx1, Opt.UpTo (Scalar.Symbolic.ofRat (Scalar.Rat.mk1 3)),
			Opt.setObj (sxLift (Splx.mk nxt [
				0, le [-1, x; 1, y] 3 ]))
			(Vec.mk [Scalar.Rat.u, x]);

		"up1", x, Opt.Unbnd, Opt.Decr, sx1, Opt.UpTo (Scalar.Symbolic.ofRat (Scalar.Rat.mk1 2)),
			Opt.setObj (sxLift (Splx.mk nxt [
				0, le [-2, x; 1, y] 4 ]))
			(Vec.mk [Scalar.Rat.u, x]);

		"up2", x, Opt.Unbnd, Opt.Decr, sx2, Opt.NoChange,
			Opt.setObj (sxLift (Splx.mk nxt [
				0, le [-2, x; 1, y] 4;
				1, le [-1, x; 1, z] 0 ]))
			(Vec.mk [Scalar.Rat.u, x]);

		"nbasic0", x, Opt.UpTo (Scalar.Symbolic.ofRat (Scalar.Rat.mk1 1)), Opt.Decr,
			x, Opt.UpTo (Scalar.Symbolic.ofRat (Scalar.Rat.mk1 1)),
			Opt.setObj (sxLift (Splx.mk nxt [
				0, le [-2, x; 1, y] 4 ]))
			(Vec.mk [Scalar.Rat.u, x])
	] in
	T.suite "pickBasic" [
		T.suite "var" (List.map chkV tcs);
		T.suite "progress" (List.map chkP tcs)
	]

(* Opt.step *)
let stepEndTs: T.testT
=
	let chk (t, next, (z, s)) = fun state ->
		let next1 = Opt.step z s in
		match next with
		| Opt.GoOn _ -> T.fail t "bad test case" state
		| _ ->
			if eqNext next next1 then
				T.succeed state
			else
				T.fail t (Opt.prNext varPr next1) state
	in
	let tcs = [
		"nil0", Opt.OptUnbnd, Opt.setObj (sxLift (Splx.mk nxt []))
			(Vec.mk [Scalar.Rat.u, x]);

		"u0", Opt.OptFinite (Scalar.Symbolic.ofRat (Scalar.Rat.mk1 (-2))),
			Opt.setObj (sxLift (Splx.mk nxt [
				0, le [-1, x] (-2) ]))
			(Vec.mk [Scalar.Rat.negU, x])
	] in
	T.suite "end" (List.map chk tcs)

let stepGoOnTs: T.testT
=
	let chkInv (t, _, (z, s)) = fun state ->
		match Opt.step z s with
		| Opt.OptFinite _
		| Opt.OptUnbnd as r -> T.fail t (Opt.prNext varPr r) state
		| Opt.GoOn s1 ->
			if chkInv s1 then
				T.succeed state
			else
				let msg = Printf.sprintf "broken invariant\n%s\n" (Splx.pr varPr s1) in
				T.fail t msg state
	in
	let chkObj (t, zV, (z, s)) = fun state ->
		match Opt.step z s with
		| Opt.OptFinite _
		| Opt.OptUnbnd as r -> T.fail t (Opt.prNext varPr r) state
		| Opt.GoOn s1 ->
			let zV1 =
				let zSt = Rtree.get {Splx.v = Scalar.Symbolic.z; Splx.low = None; Splx.up = None} (Splx.get_state s1) z in
				Splx.get_v zSt
			in
			if Scalar.Symbolic.cmp zV zV1 = 0 then
				T.succeed state
			else
				T.fail t (Scalar.Symbolic.to_string zV1) state
	in
	let tcs = [
		"own0", Scalar.Symbolic.ofRat (Scalar.Rat.mk1 2), Opt.setObj (sxLift (Splx.mk nxt [
				0, le [1, x] 2 ]))
			(Vec.mk [Scalar.Rat.u, x]);

		"other0", Scalar.Symbolic.ofRat (Scalar.Rat.mk1 2), Opt.setObj
			(sxLift (Splx.mk nxt [ 0, le [1, x; 1, y] 2 ]))
			(Vec.mk [Scalar.Rat.u, x])
	] in
	T.suite "goOn" [
		T.suite "inv" (List.map chkInv tcs);
		T.suite "obj" (List.map chkObj tcs)
	]

let stepTs: T.testT
= T.suite "step" [stepEndTs; stepGoOnTs]

(* Opt.max *)
let maxTs: T.testT
=
	let chk (t, v, obj, s) = fun state ->
		match Opt.max' s obj with
		| Splx.IsUnsat _ -> T.fail t "unsat" state
		| Splx.IsOk opt ->
			if eqOpt v opt
			then T.succeed state
			else T.fail t (Opt.prOpt opt) state
	in
	(* XXX: should do without dummySx *)
	let tcs
	  = let dummySx
	      = match Splx.mk V.XH [] with
	      | Splx.IsUnsat _ -> Pervasives.failwith "Opt_t.maxTs"
	      | Splx.IsOk sx -> sx
	    in
	    [
		"nil0", Opt.Finite (dummySx, Scalar.Rat.z, []), Vec.mk [], Splx.mk nxt [];
		"nil1", Opt.Infty, Vec.mk [Scalar.Rat.u, x], Splx.mk nxt [];
		"own0", Opt.Finite (dummySx, Scalar.Rat.mk1 2, [0, Scalar.Rat.u]), Vec.mk [Scalar.Rat.u, x], Splx.mk nxt [
			0, le [1, x] 2 ];

		"ownSup0", Opt.Sup (dummySx, Scalar.Rat.mk1 2, [0, Scalar.Rat.u]),
			Vec.mk [Scalar.Rat.u, x], Splx.mk nxt [
				0, lt [1, x] 2 ];

		"comb1", Opt.Finite (dummySx, Scalar.Rat.mk1 2, [0, Scalar.Rat.u; 1, Scalar.Rat.u]),
			Vec.mk [Scalar.Rat.u, x], Splx.mk nxt [
				0, le [1, x; 1, y] 2;
				1, le [-1, y] 0 ];

		"comb2", Opt.Sup (dummySx, Scalar.Rat.mk1 2, [0, Scalar.Rat.u; 1, Scalar.Rat.u]),
			Vec.mk [Scalar.Rat.u, x], Splx.mk nxt [
				0, le [1, x; 1, y] 2;
				1, lt [-1, y] 0 ];

		"m0", Opt.Finite (dummySx, Scalar.Rat.mk1 6, [0, Scalar.Rat.mk 2 1; 1, Scalar.Rat.mk 2 1]),
			Vec.mk [Scalar.Rat.u, x], Splx.mk nxt [
				0, le [-1, y] 4;
				1, le [2, x; 1, y] 8;
				2, le [1, x; 1, y] 3 ];

		"red0", Opt.Finite (dummySx, Scalar.Rat.mk1 4, [0, Scalar.Rat.u; 1, Scalar.Rat.u]),
			Vec.mk [Scalar.Rat.u, x; Scalar.Rat.u, y], Splx.mk nxt [
				0, le [1, x] 2;
				1, le [1, y] 2;
				2, le [1, x; 1, y] 5 ];

		"distant_nxt0", Opt.Infty, Vec.mk [Scalar.Rat.negU, x],
			Splx.mk (V.XI (V.XO (V.XO V.XH))) [
				1, Cs.eq [Scalar.Rat.u, x; Scalar.Rat.u, y; Scalar.Rat.negU, z] Scalar.Rat.z;
				2, Cs.eq [Scalar.Rat.u, y; Scalar.Rat.negU, t] Scalar.Rat.negU;
				3, Cs.le [Scalar.Rat.negU, z] Scalar.Rat.u;
				4, Cs.le [Scalar.Rat.negU, z; Scalar.Rat.negU, t] Scalar.Rat.negU]
	] in
	T.suite "max" (List.map chk tcs)
(*
let getAsgTs : T.testT
	= let check : string * (Cs
*)
let ts: T.testT
= T.suite "Opt" [setObjTs; pickNBasicTs; pickBasicTs; stepTs; maxTs]
