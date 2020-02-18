open Vpl

module Cs = Cstr.Rat

let factory : Cs.t Factory.t = {
	Factory.name = "Cstr";
	Factory.top = (Cs.mk Cstr_type.Eq [] Scalar.Rat.z);
	Factory.triv = (fun cmp n -> Cs.mk cmp [] n);
	Factory.add = Cs.add;
	Factory.mul = Cs.mulc;
	Factory.to_le = (fun c -> {c with Cs.typ = Cstr_type.Le});
	Factory.merge = (fun c1 c2 ->
		let c1' = {c1 with Cs.typ = Cstr_type.Eq}
		and c2' = {c2 with Cs.typ = Cstr_type.Eq} in
		if Cs.equal c1' c2'
		then c1'
		else failwith "merge");
	Factory.to_string = Cs.to_string Var.to_string;
	Factory.rename = Cs.rename;
}


(* EqSet *)
let x = Var.fromInt 1
let y = Var.fromInt 2
let z = Var.fromInt 3
let t = Var.fromInt 4
let nxt = 5

let varPr: Var.t -> string
= fun _x ->
	let vars: (Var.t * string) list
	= [x, "x"; y, "y"; z, "z"; t, "t"]
	in
	try
		List.assoc _x vars
	with
	| Not_found -> "v" ^ (Var.to_string _x)

let mkc t v c =
	Cs.mk t (List.map (fun (n, x) -> (Cs.Vec.Coeff.of_int n, x)) v) (Cs.Vec.Coeff.of_int c)

let eq = mkc Cstr_type.Eq
let le = mkc Cstr_type.Le
let lt = mkc Cstr_type.Lt

let mask l =
	List.fold_left (fun m x -> Rtree.set None m x (Some x)) Rtree.empty l

(* XXX: This function does not do any check on the inputs. *)

let mkCons : Cs.t -> Cs.t Cons.t
	= fun c ->
		(c, c)

let mk: (Var.t * Cs.t) list -> Cs.t EqSet.t
	= fun l ->
	List.fold_left
		(fun s (x,c) -> (x, (mkCons c))::s)
		(EqSet.nil) l

let mks: Cs.t list -> Cs.t EqSet.t
= fun l ->
	match EqSet.addM factory EqSet.nil (List.map (fun x -> (x,x)) l) with
	| EqSet.Added s -> s
	| EqSet.Bot _ -> invalid_arg "EqSet_t.mks"

let mkl: Cs.t list -> Cs.t EqSet.t
= fun l ->
	mks (List.rev l)

let optxpr =
	function
	| None -> "None"
	| Some x -> "Some " ^ (varPr x)

let prProp = function
| Cs.Trivial -> "trivial"
| Cs.Contrad -> "contrad"
| Cs.Nothing -> "nothing"

let impliesEq: Cs.t list -> Cs.t list -> bool
= fun l1 l2 ->
	Misc.list_eq2 Cs.equal l1 l2

let impliesPr: Cs.t list -> string = Cs.list_to_string

let relEq: Cs.t EqSet.rel_t -> Cs.t EqSet.rel_t -> bool
= fun r1 r2 ->
	match r1, r2 with
	| EqSet.NoIncl, EqSet.NoIncl -> true
	| EqSet.Incl l1, EqSet.Incl l2 -> impliesEq l1 l2
	| EqSet.NoIncl, EqSet.Incl _
	| EqSet.Incl _, EqSet.NoIncl -> false

let relPr: Cs.t EqSet.rel_t -> string
= function
	| EqSet.NoIncl -> "NoIncl"
	| EqSet.Incl l -> Printf.sprintf "Incl l with l =\n%s" (impliesPr l)

(* EqSet.filter *)
let filterTs: Test.t
= fun () ->
	let chkNo (name, _, c, s, _) = fun state ->
		let (c1,cert1) = EqSet.filter factory s c in
		let vars = List.map (fun (x, _) -> x) s in
		let zcoef x = Cs.Vec.Coeff.cmpz (Cs.Vec.get (Cs.get_v c1) x) = 0 in
		if List.for_all zcoef vars then
			Test.succeed state
		else
			let err = Printf.sprintf "some variable has non-zero coefficient in %s\n"
				(Cons.to_string_ext factory varPr (c1,cert1)) in
			Test.fail name err state
	in
	let chkProp (name, p, c, s, _) = fun state ->
		let (c1,cert1) = EqSet.filter factory s c in
		let p1 = Cs.tellProp c1 in
		if p = p1 then
			Test.succeed state
		else
			let e = (prProp p) ^ " != " ^ (prProp p1) in
			Test.fail name e state
	in
	let chkLin (name, _, c, s, cert) = fun state ->
		let (c1,cert1) = EqSet.filter factory s c in
		if Cs.equal cert1 cert then
			Test.succeed state
		else
			let err = Printf.sprintf "expected: %s\nbut got: %s" (factory.Factory.to_string cert) (factory.Factory.to_string cert1) in
			Test.fail name err state
	in
	let tcs = [
		"nil0", Cs.Nothing, mkCons (eq [1, x] 0), mkl [], (eq [1, x] 0);
		"nil1", Cs.Nothing, mkCons (eq [1, x] 1), mkl [], (eq [1, x] 1);
		"nil2", Cs.Nothing, mkCons (eq [1, x; 2, y] 0), mkl [], (eq [1, x; 2, y] 0);

		"one0", Cs.Nothing, mkCons (eq [2, x; 3, y] 1), mkl [eq [1, y] 1 ], (eq [2, x] (-2));

		"incl0", Cs.Trivial, mkCons (eq [2, x; 3, y] 1), mkl [
			eq [1, x; 1, y] 1;
			eq [1, y] (-1) ],
			eq [] 0 ;

		"incl1", Cs.Trivial, mkCons (le [2, x; 3, y] 1), mkl [
			eq [1, x; 1, y] 1;
			eq [1, y] (-1) ],
			le [] 0 ;

		"triv0", Cs.Trivial, mkCons (le [] 1), mkl [
			eq [1, x] 1;
			eq [1, y] 2 ],
			le [] 1;

		"contrad0", Cs.Contrad, mkCons (eq [2, x; 1, z] 1), mkl [
			eq [1, x; 1, z] 0;
			eq [1, x] 2 ],
			Cs.eq [] (Q.of_float (-1.))

	] in
	Test.suite "filter" [
		Test.suite "no" (List.map chkNo tcs);
		Test.suite "prop" (List.map chkProp tcs);
		Test.suite "lin" (List.map chkLin tcs)
	]

(* EqSet.addM2 *)
(* XXX: check that all is fine when adding multiple equalities *)
let addMTs: Test.t
= fun () ->
	let chk (nm, s, l, r)
	= fun st -> try
		let ar = EqSet.addM factory s l in
		if EqSet.meetEq r ar
		then Test.succeed st
		else
			let estr = Printf.sprintf "expected:\n%s\ngot:\n%s\n"
				(EqSet.meet_to_string factory varPr r) (EqSet.meet_to_string factory varPr ar)
			in
			Test.fail nm estr st
		with Failure s when s = "EqSet.addM" -> Test.fail nm "caught Failure exception" st
	in
	let tcs: (string * 'c EqSet.t * Cs.t Cons.t list * 'c EqSet.meetT) list
	= [
		"concat0", EqSet.nil, [],
			EqSet.Added EqSet.nil;

		"concat1", mks [], [eq [1, x] 0],
			EqSet.Added (mks [eq [1, x] 0]);

		"concat2", mks [eq [1, x] 0], [],
			EqSet.Added (mks [eq [1, x] 0]);

		"concat3", mks [eq [1, y] 1], [eq [1, x] 0],
			EqSet.Added (mks [
				eq [1, y] 1;
				eq [1, x] 0]);

		"adj0", mks [], [eq [2, x] 0],
			EqSet.Added (mks [eq [1, x] 0]);

		"adj1", mks [], [eq [-1, x] 0],
			EqSet.Added (mks [eq [1, x] 0]);

		"comb0", mks [eq [1, x] 0],
			[eq [1, x; 1, y] 0],
			EqSet.Added (mks [
				eq [1, x] 0;
				eq [1, y] 0]);

		"comb+adj0", mks [eq [1, x] 0],
			[eq [1, x; 2, y] 0],
			EqSet.Added (mks [
				eq [1, x] 0;
				eq [1, y] 0]);

		"redundant0", mks [eq [1, x] 0],
			[eq [1, x] 0],
			EqSet.Added (mks [eq [1, x] 0]);

		"redundant1", mks [
			eq [1, x] 0;
			eq [1, y] 1],
			[eq [1, x; 2, y] 2],
			EqSet.Added (mks [
				eq [1, x] 0;
				eq [1, y] 1]);

		"contrad0", mks [],
			[eq [] 1],
			EqSet.Bot (eq [] 1);

		"contrad1", mks [eq [1, x] 0],
			[eq [1, x] 1],
			EqSet.Bot (eq [] (-1));

		"contrad2", mks [
			eq [1, x] 0;
			eq [1, y] 0],
			[eq [1, x; 2, y] 1],
			EqSet.Bot (eq [] 1);
	]
	|> List.map
		(fun (nm, s, l, r) -> nm, s, (List.map mkCons l), r)
	in
	Test.suite "addM" (List.map chk tcs)

(* EqSet.pick *)
let pickTs: Test.t
= fun () ->
	let chk (name, msk, c, r) = fun state ->
		let r1 = EqSet.pick msk c in
		if r = r1 then
			Test.succeed state
		else
			let e = "expected " ^ (optxpr r) ^ " but got " ^ (optxpr r1) in
			Test.fail name e state
	in
	let tcs = [
		"nil0", mask [], mkCons (eq [] 0), None;
		"nil1", mask [x], mkCons (eq [] 0), None;
		"nil2", mask [y], mkCons (eq [] 0), None;
		"nil3", mask [], mkCons (eq [1, x] 0), None;
		"diff0", mask [x], mkCons (eq [1, y] 0), None;
		"one0", mask [x], mkCons (eq [1, x] 0), Some x;
		"one1", mask [x], mkCons (eq [1, x; 1, y] 0), Some x;
		"one2", mask [y], mkCons (eq [1, x; 1, y] 0), Some y;
		"m0", mask [x; y], mkCons (eq [1, y] 0), Some y;
		"m1", mask [x; y], mkCons (eq [1, y; 1, z] 0), Some y;

		(* implementation detail *)
		"m2", mask [x; y], mkCons (eq [1, x; 1, y] 0), Some x
	] in
	Test.suite "pick" (List.map chk tcs)

(* EqSet.subst *)
let substTs: Test.t
= fun () ->
	let chkCons (name, x, c, s, s1) = fun state ->
		let c1 = mkCons c in
		let s2 = EqSet.subst factory x c1 s in
		if EqSet.equal s1 s2 then
			Test.succeed state
		else
			let e =
				"expected:\n" ^  (EqSet.to_string_ext factory varPr s1) ^ "but got:\n" ^ (EqSet.to_string_ext factory varPr s2)
			in
			Test.fail name e state
	in
	let tcs = [
		"nil0", x, eq [1, x] 0, mk [], mk [];
		"nil1", y, eq [1, x] 0, mk [], mk [];
		"nil2", y, eq [1, x] 0, mk [], mk [];
		"one0", x, eq [1, x] 0,
			mk [y, eq [1, x; 1, y] 1],
			mk [y, eq [1, y] 1];

		"one1", x, eq [1, x] 0,
			mk [y, eq [1, x; 1, y] 1],
			mk [y, eq [1, y] 1];

		"no0", x, eq [1, x] 0, mk [y, eq [1, y] 1], mk [y, eq [1, y] 1];
		"m0", x, eq [1, x] 0, mk [
			z, eq [1, z] 1;
			y, eq [1, x; 1, y] 1
		], mk [
			z, eq [1, z] 1;
			y, eq [1, y] 1 ];
	] in
	Test.suite "subst" [
		Test.suite "cons" (List.map chkCons tcs)
	]

(* EqSet.tryDefs *)
let tryDefsTs: Test.t
= fun () ->
	let chk_res (t, r, msk, s) = fun state ->
		let (def, _) = EqSet.tryDefs factory msk s in
		let a = if def = None then false else true in
		if a = r then
			Test.succeed state
		else
			Test.fail t "bad conclusion" state
	in
	let chk_def (t, _, msk, s) = fun state ->
		let (def, _) = EqSet.tryDefs factory msk s in
		match def with
		| None -> Test.succeed state
		| Some (_, x) ->
			if List.exists (fun (x1, _) -> x = x1) s then
				Test.succeed state
			else
				let e = "not found" in
				Test.fail t e state
	in
	let chk_nox (t, _, msk, s) = fun state ->
		let (def, s1) = EqSet.tryDefs factory msk s in
		match def with
		| None -> Test.succeed state
		| Some (_, x) ->
			let noXPred c = Cs.Vec.Coeff.cmpz (Cs.Vec.get (Cs.get_v (Cons.get_c c)) x) = 0 in
			if List.for_all (fun (_, c) -> noXPred c) s1 then
				Test.succeed state
			else
				Test.fail t "x remaining" state
	in
	let tcs = [
		"nil0", false, mask [], mkl [];
		"nil1", false, mask [x], mkl [];
		"one0", true, mask [x], mkl [eq [1, x] 0];
		"one0", true, mask [x], mkl [
			eq [1, x] 0;
			eq [1, x; 1, y] 0 ];

		"no0", false, mask [z], mkl [
			eq [1, x; 1, z] 0;
			eq [1, y] 0 ]
	] in
	Test.suite "tryDefs" [
		Test.suite "res" (List.map chk_res tcs);
		Test.suite "def" (List.map chk_def tcs);
		Test.suite "nox" (List.map chk_nox tcs)
	]

(* EqSet.trySubstM *)
let trySubstMTs: Test.t
= fun () ->
	let chk_var (t, x, msk, s) = fun state ->
		let (optx1, _) = EqSet.trySubstM factory msk s in
		match optx1 with
		| None ->
			if x = None then
				Test.succeed state
			else
				let e = "expected " ^ (optxpr x) ^ " but got None" in
				Test.fail t e state
		| Some (_, x1) ->
			if x = Some x1 then
				Test.succeed state
			else
				let e = "expected " ^ (optxpr x) ^ " but got " ^ (optxpr (Some x1)) in
				Test.fail t e state
	in
	let chk_nox (t, x, msk, s) = fun state ->
		let (e, s1, s2) =
			let (_, s1) = EqSet.trySubstM factory msk s in
			match x with
			| None -> ("trySubstM did change s", s, s1)
			| Some x ->
				let c = mkCons (eq [1, x] 0) in
				let s2 = EqSet.subst factory x c s1 in
				let e =
					"still some " ^ (varPr x) ^ " in\n" ^ (EqSet.to_string_ext factory varPr s1)
				in
				(e, s1, s2)
		in
		if EqSet.equal s1 s2 then
			Test.succeed state
		else
			Test.fail t e state
	in
	let tcs = [
		"x0", Some x, mask [x], mk [
			y, eq [1, x; 1, y] 0;
			x, eq [1, x] 0 ];

		"no0", None, mask [z], mk [
			x, eq [1, x] 0 ]
	] in
	Test.suite "trySubstM" [
		Test.suite "var" (List.map chk_var tcs);
		Test.suite "nox" (List.map chk_nox tcs)
	]
(*
(* EqSet.joinSetup *)
let joinSetupTs: Test.t
=
	let alpha = Var.fromInt nxt in
	let nxt' = nxt + 1 in
	let chk_id0 dup (t, idOff, s) = fun state ->
		let (_, _, s1) = EqSet.joinSetup dup (Var.fromInt nxt') Rtree.empty alpha idOff s in
		let sorted =
			let ids = List.map (fun c -> (Cons.get_id c)) (EqSet.list s) in
			List.sort Stdlib.compare ids
		in
		let sorted1 =
			let ids1 = List.map (fun c -> (Cons.get_id c) - idOff) (EqSet.list s1) in
			List.sort Stdlib.compare ids1
		in
		if sorted = sorted1 then
			Test.succeed state
		else
			Test.fail t "not equal" state
	in
	let chk_def0 dup (t, i0, s) = fun state ->
		let (_, reloc, s1) = EqSet.joinSetup dup (Var.fromInt nxt') Rtree.empty alpha i0 s in
		let a =
			let chk (x1, _) (x2, _) =
				if dup then
					x1 = x2
				else
					Rtree.get None reloc x1 = Some x2
			in
			List.for_all2 chk s s1
		in
		if a then
			Test.succeed state
		else
			let err =
				Printf.sprintf "wrong defined variables: s:\n%s\ns1:\n%s\n"
				(EqSet.to_string_ext varPr s) (EqSet.to_string_ext varPr s1)
			in
			Test.fail t err state
	in
	let chk_frag0 dup (t, i0, s) = fun state ->
		let (_, _, s1) = EqSet.joinSetup dup (Var.fromInt nxt') Rtree.empty alpha i0 s in
		let a =
			let chk c =
				match Cons.get_f c with
				| [] -> false
				| (id, coef)::[] -> id = (Cons.get_id c) && Cs.Vec.Coeff.cmp Cs.Vec.Coeff.u coef = 0
				| _ -> false
			in
			List.for_all chk (EqSet.list s1)
		in
		if a then
			Test.succeed state
		else
			Test.fail t "bad fragments" state
	in
	let tcs = [
		"one0", 10, mk [ x, eq [1, x] 1 ];
		"one1", 10, mk [ x, eq [1, x; 1, y] 1 ];
		"one2", 10, mk [ y, eq [1, x; 1, y] 1 ];
		"m0", 10, mk [
			x, eq [1, x; 1, y] 1;
			y, eq [2, y; 1, z] 1;
			z, eq [1, z; 2, t] 1 ]
	] in
	let chk_id = chk_id0 false in
	let chk_idd = chk_id0 true in
	let chk_def = chk_def0 false in
	let chk_frag = chk_frag0 false in
	let chk_defd = chk_def0 true in
	let chk_fragd = chk_frag0 true in
	Test.suite "joinSetup" [
		Test.suite "id" (List.map chk_id tcs);
		Test.suite "idd" (List.map chk_idd tcs);
		Test.suite "def" (List.map chk_def tcs);
		Test.suite "defd" (List.map chk_defd tcs);
		Test.suite "frag" (List.map chk_frag tcs);
		Test.suite "fragd" (List.map chk_fragd tcs)
	]
*)
(* EqSet.incl *)
let inclTs: Test.t
= fun () ->
	let chk (name, s1, s2, expected) = fun state ->
		let actual = EqSet.leq factory s1 s2 in
		if relEq actual expected then
			Test.succeed state
		else
			Test.fail name (relPr actual) state
	in
	let tcs = [
		"id0",
        mks [eq [1, x] 0 ],
        mks [eq [1, x] 0 ],
        EqSet.Incl [eq [1, x] 0];

		"id1",
        mks [
			eq [1, y] 0;
			eq [1, x] 0],
        mks [eq [1, x] 0 ],
        EqSet.Incl [eq [1, x] 0];

		"bin0", mks [
			eq [1, y] 0;
			eq [1, x] 0
		], mks [eq [1, x; 1, y] 0 ], EqSet.Incl [eq [1, x; 1, y] 0]
	] in
	Test.suite "incl" (List.map chk tcs)

(* EqSet.rename *)
let renameTs: Test.t
= fun () ->
	let x' = Var.fromInt nxt in
	let chkVar (t, fromX, toY, s0) = fun state ->
		let defVars s = List.map (fun (v, _) -> v) s in
		let s = EqSet.rename factory s0 fromX toY in
		if List.for_all (fun v -> v <> fromX) (defVars s) then
			Test.succeed state
		else
			let e = (varPr fromX) ^ " remaining\n" ^ (EqSet.to_string_ext factory varPr s) in
			Test.fail t e state
	in
	let chkCoef (t, fromX, toY, s0) = fun state ->
		let aX s =
			let coef (c,cert) x = Cs.Vec.get (Cs.get_v c) x in
			List.map (fun (_, c) -> coef c fromX) s
		in
		let s = EqSet.rename factory s0 fromX toY in
		if List.for_all (fun a -> Cs.Vec.Coeff.cmpz a = 0) (aX s) then
			Test.succeed state
		else
			let e = (varPr fromX) ^ " remaining\n" ^ (EqSet.to_string_ext factory varPr s) in
			Test.fail t e state
	in
	let tcs = [
		"one0", x', x, mkl [ eq [1, x'] 0 ];
		"one1", x', x, mkl [ eq [1, x'; 1, y] 0 ];
		"no0", x', x, mkl [ eq [1, y] 0 ];
		"m0", x', x ,mkl [
			eq [1, x'; 1, y] 1;
			eq [1, x'; 2, y] 2 ];
	] in
	Test.suite "rename" [
		Test.suite "var" (List.map chkVar tcs);
		Test.suite "coef" (List.map chkCoef tcs)
	]

let ts: Test.t
    = fun () ->
    List.map Test.run [filterTs; addMTs; pickTs; substTs; tryDefsTs; trySubstMTs; (*joinSetupTs;*) inclTs; renameTs]
    |> Test.suite "EqSet"
