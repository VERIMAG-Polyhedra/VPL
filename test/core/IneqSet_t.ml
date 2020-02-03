open Vpl

module Cs = Cstr.Rat
module Vec = Cs.Vec

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

let x = Var.fromInt 1
let y = Var.fromInt 2
let z = Var.fromInt 3
let t1 = Var.fromInt 4
let t3 = Var.fromInt 5
let t2 = Var.fromInt 6

let nxt = Var.fromInt 7

let varPr: Var.t -> string
= fun _x ->
	let vars: (Var.t * string) list
	= [x, "x"; y, "y"; z, "z"; t1, "t1"; t2, "t2"; t3, "t3"]
	in
	try
		List.assoc _x vars
	with
	| Not_found -> "v" ^ (Var.to_string _x)

let mkc t v c =
	Cs.mk t (List.map (fun (n, x) -> (Scalar.Rat.of_int n, x)) v) (Scalar.Rat.of_int c)

let eq = mkc Cstr_type.Eq
let le = mkc Cstr_type.Le
let lt = mkc Cstr_type.Lt

let mask l =
	List.fold_left (fun m x -> Rtree.set None m x (Some x)) Rtree.empty l

let optxpr =
	function
	| None -> "None"
	| Some x -> "Some " ^ (varPr x)
(*
let mkN = List.map (fun (i, n) -> (i, Scalar.Rat.of_int n))
*)
let propPr = function
	| IneqSet.Empty f -> Printf.sprintf "Empty: %s" (factory.Factory.to_string f)
	| IneqSet.Trivial -> "Trivial"
	| IneqSet.Implied c -> Printf.sprintf "Implied %s" (factory.Factory.to_string c)
	| IneqSet.Check c -> Printf.sprintf "Check: %s" (Cons.to_string_ext factory varPr c)

let mkCons : Cs.t -> Cs.t Cons.t
	= fun c ->
	(c, c)

let mkEqSet: Cs.t list -> Cs.t EqSet.t
	= fun l ->
	let conss = List.map (fun c -> c,c) l in
	match EqSet.addM factory EqSet.nil conss with
	| EqSet.Added s -> s
	| EqSet.Bot _ -> failwith "IneqSet_t.mkEqSet"

let iset: Cs.t list -> Cs.t IneqSet.t
	= fun l ->
	let l_stricten = List.mapi (fun i c -> i,{c with Cs.typ = Cstr_type.Lt}) l in
	match Splx.checkFromAdd (Splx.mk nxt l_stricten) with
	| Splx.IsUnsat _ -> Stdlib.failwith "IneqSet_t.iset: unexpected empty interior"
	| Splx.IsOk sx_strict ->
		let conss = List.map (fun c -> c,c) l in
		IneqSet.assume nxt IneqSet.top conss (Splx.getAsg sx_strict)

let relEq: Cs.t IneqSet.rel_t -> Cs.t IneqSet.rel_t -> bool
= fun r1 r2 ->
	match r1, r2 with
	| IneqSet.NoIncl, IneqSet.NoIncl -> true
	| IneqSet.Incl l1, IneqSet.Incl l2 -> Misc.list_eq2 Cs.equal l1 l2
	| IneqSet.NoIncl, IneqSet.Incl _
	| IneqSet.Incl _, IneqSet.NoIncl -> false

let relPr: Cs.t IneqSet.rel_t -> string
= function
	| IneqSet.NoIncl -> "NoIncl"
	| IneqSet.Incl l -> Printf.sprintf "Incl l with l:\n%s" (Misc.list_to_string (Cs.to_string Var.to_string) l " ; ")

(* IneqSet.synIncl *)
let synInclCheckTs: Test.t
= fun () ->
	let chk (t, es, s, cIn, cOut) = fun state ->
		let eq : Cs.t IneqSet.prop_t -> Cs.t IneqSet.prop_t -> bool
			= fun c1 c2 ->
			match c1, c2 with
			| IneqSet.Check (c1,cert1), IneqSet.Check (c2,cert2) ->
				Cs.equal c1 c2 &&
				Cs.equal cert1 cert2

			| _, _ -> false
		in
		let result = IneqSet.synIncl factory es s cIn in
		if eq (IneqSet.Check cOut) result then
			Test.succeed state
		else
			let s = Printf.sprintf "Expected %s\ngot %s"
				(propPr (IneqSet.Check cOut))
				(propPr result)
			in
			Test.fail t s state
	in
	let tcs = [
		"subst0",
			mkEqSet [eq [1, y] 0 ], iset [],
			le [1, x; 1, y] 0,
			(le [1, x] 0, eq [1, y] 0) ;

		"substlt",
			mkEqSet [eq [1, y] 0 ], iset [],
			lt [1, x; 1, y] 2,
			(lt [1, x] 2, eq [1, y] 0) ;

	] in
	Test.suite "check" (List.map chk tcs)

let synInclImpliedTs: Test.t
= fun () ->
	let chk (t, es, s, cIn, cOut) = fun state ->
		let eq : Cs.t IneqSet.prop_t -> Cs.t IneqSet.prop_t -> bool
			= fun c1 c2 ->
			match c1, c2 with
			| IneqSet.Implied cert1, IneqSet.Implied cert2 ->
				Cs.equal cert1 cert2

			| _, _ -> false
		in
		let result = IneqSet.synIncl factory es s cIn in
		if eq (IneqSet.Implied cOut) result then
			Test.succeed state
		else
			let s = Printf.sprintf "Expected %s\ngot %s"
				(propPr (IneqSet.Implied cOut))
				(propPr result)
			in
			Test.fail t s state
	in
	let tcs = [

		"subst1",
			mkEqSet [eq [1, x] 0 ], iset [],
			le [1, x] 1,
			(le [1, x] 1) ;

        "simpl:lt",
			mkEqSet [], iset [ lt [1, y] 3 ],
			lt [1, y] 3,
			(lt [1, y] 3) ;

	] in
	Test.suite "implied" (List.map chk tcs)

let synInclTs: Test.t
= fun () ->
Test.suite "synIncl" [synInclCheckTs() ; synInclImpliedTs()]

let equal s1 s2 =
	let incl l1 = List.for_all
		(fun (c2,_) ->
		List.exists (fun (c1,_) -> Cs.inclSyn c1 c2) l1.IneqSet.ineqs)
	in
incl s1 s2.IneqSet.ineqs && incl s2 s1.IneqSet.ineqs

(* IneqSet.assume *)
let addMTs: Test.t
= fun () ->
let chk (nm, s, l, r)
	= fun st ->
		let ilist = List.mapi (fun i cs -> i, cs) (l @ (List.map Stdlib.fst s.IneqSet.ineqs))
		in
		let l_stricten = List.map (fun (i,c) -> i, {c with Cs.typ = Cstr_type.Lt}) ilist in
		match Splx.checkFromAdd (Splx.mk nxt l_stricten) with
		| Splx.IsUnsat _ -> Stdlib.failwith "IneqSet_t.iset: unexpected empty interior"
		| Splx.IsOk sx_strict ->
			let conss = List.map mkCons l in
			let s' = IneqSet.assume nxt s conss (Splx.getAsg sx_strict) in
			if equal r s'
			then Test.succeed st
			else
				let estr = Printf.sprintf "expected:\n%s\ngot:\n%s\n"
					(IneqSet.to_string varPr r) (IneqSet.to_string varPr s')
				in
				Test.fail nm estr st
	in
	let tcs: (string * Cs.t IneqSet.t * Cs.t list * Cs.t IneqSet.t) list
	= [
	"triv0", IneqSet.top, [], IneqSet.top;

	"triv1", iset [le [1, x] 0], [], iset [le [1, x] 0];

	"concat0", iset [], [le [1, x] 0], iset [le [1, x] 0];

	"concat1", iset [le [1, x] 0], [le [1, y] 0],
		iset [
			le [1, x] 0;
			le [1, y] 0];

	"synred0", iset [le [1, x] 0], [le [1, x] 0],
		iset [le [1, x] 0];

	"synred1", iset [le [1, x] 0], [le [1, x] 1],
		iset [le [1, x] 0];

	"synred2", iset [le [1, x] 0], [le [2, x] 0],
		iset [le [1, x] 0];

	"synred3", iset [le [1, x] 1], [le [1, x] 0],
		iset [le [1, x] 0];

	"red0", iset [
		le [1, x] 1;
		le [1, y] 2],
		[le [1, x; 2, y] 5],
		iset [
			le [1, x] 1;
			le [1, y] 2];

	"red1", iset [
		le [1, x] 1;
		le [1, y] 2;
		le [1, x; -1, y] 2],
		[le [1, x; 2, y] 5],
		iset [
			le [1, x] 1;
			le [1, y] 2;
			le [1, x; -1, y] 2];

	"red2", iset [
		le [1, x] 1;
		le [2, x; 1, y] 3],
		[le [1, y] 1],
		iset [
			le [1, x] 1;
			le [1, y] 1];

    "raytracing_bug", iset [
        Cs.mk Cstr_type.Le [Scalar.Rat.of_string "1/2" |> Cs.Vec.Coeff.ofQ, x] (Cs.Vec.Coeff.of_int 1);
        Cs.mk Cstr_type.Le [Scalar.Rat.of_string "-5/2" |> Cs.Vec.Coeff.ofQ, x] (Scalar.Rat.of_string "5404319552844595/4503599627370496" |> Cs.Vec.Coeff.ofQ);
        Cs.mk Cstr_type.Le [Scalar.Rat.of_string "-5" |> Cs.Vec.Coeff.ofQ, x] (Scalar.Rat.of_string "5404319552844595/2251799813685248" |> Cs.Vec.Coeff.ofQ);
        le [2, x] 1],
        [],
        iset [
            Cs.mk Cstr_type.Le [Scalar.Rat.of_string "-5/2" |> Cs.Vec.Coeff.ofQ, x] (Scalar.Rat.of_string "5404319552844595/4503599627370496" |> Cs.Vec.Coeff.ofQ);
            le [2, x] 1;
            ];
	] in
	Test.suite "addM" (List.map chk tcs)


(* IneqSet.pick *)
let pickTs: Test.t
= fun () ->
	let chk (t, msk, x, l) = fun state ->
		let s = iset l in
		let x1 = IneqSet.pick msk s in
		if x = x1 then
			Test.succeed state
		else
			Test.fail t (optxpr x1) state
	in
	let tcs = [
		"nil0", mask [], None, [];
		"nil1", mask [x], None, [];
		"nil2", mask [y], None, [];
		"no0", mask [], None, [le [1, x] 0];
		"no1", mask [y], None, [le [1, x] 0];
		"no2", mask [x], None, [le [1, z] 0];
		"one0", mask [x], Some x, [le [1, x] 0];
		"one1", mask [x], Some x, [
			le [1, x] 0;
			le [1, y] 0 ];

		"one2", mask [x; y], Some x, [
			le [1, x] 0;
			le [1, x; 1, y] 0 ];

		"m0", mask [x; y], Some y, [
			le [1, x; 2, z] 0;
			le [1, x; 1, z] 0;
			le [-1, x; 1, y] 0;
			le [2, y; 1, z] 0 ]
	] in
	Test.suite "pick" (List.map chk tcs)

(* IneqSet.subst *)
let substTs: Test.t
= fun () ->
	let chk_res (t, x, e, s, s1) = fun state ->
		let s2 = iset s in
		let s3 = iset s1 in
		let s4 = IneqSet.subst factory nxt EqSet.nil x e s2 in
		if equal s3 s4 then
			Test.succeed state
		else
			Test.fail t (IneqSet.to_string_ext factory varPr s4) state
	in
	let tcs = [
		"nil0", x, mkCons (eq [1, x] 0), [], [];

		"other0", x, mkCons (eq [1, x] 0), [
			le [1, y] 0
		], [
			le [1, y] 0 ];

		"other1", y, mkCons (eq [1, y] 0), [
			le [1, x] 0;
			le [1, x; 1, z] 0
		], [
			le [1, x] 0;
			le [1, x; 1, z] 0 ];

		"one0", x, mkCons (eq [1, x] 0), [
			le [1, x; 1, y] 0;
		], [
			le [1, y] 0 ];

		"mixed0", x, mkCons (eq [1, x] 0), [
			le [1, x; 1, y] 0;
			le [1, z] 0
		], [
			le [1, y] 0;
			le [1, z] 0 ];

		"simpl0", x, mkCons (eq [1, x] 0), [
			le [1, x; 1, y] 0;
			le [1, y] 0
		], [
			le [1, y] 0 ]
	] in
	Test.suite "subst" [
		Test.suite "res" (List.map chk_res tcs)
	]

(* IneqSet.fmElim *)
let fmElimTs: Test.t
= fun () ->
	let chk_res (t, x, s, s1) = fun state ->
		let s2 = iset s in
		let s3 = iset s1 in
		let s4 = IneqSet.fmElim_one factory nxt EqSet.nil x s2 in
		if equal s3 s4 then
			Test.succeed state
		else
			Test.fail t (IneqSet.to_string_ext factory varPr s4) state
	in
	let tcs = [
		"nil0", x, [], [];
		"other0", x, [
			le [1, y] 0
		], [
			le [1, y] 0 ];

		"plus0", x, [
			le [1, x] 0;
			le [1, y] 0
		], [
			le [1, y] 0 ];

		"plus1", x, [
			le [1, x] 0;
			le [1, x; 1, y] 0;
			le [1, y] 0
		], [
			le [1, y] 0 ];

		"plus2", x, [
			le [1, x] 0
		], [];

		"neg0", x, [
			le [-1, x] 0;
			le [1, y] 0
		], [
			le [1, y] 0 ];

		"m0", x, [
			le [1, x; 1, y] 0;
			le [-1, x] 0
		], [
			le [1, y] 0
		];

		"m1", x, [
			le [1, x] 1;
			le [-1, x] 0
		], [];

		"m2", x, [
			le [1, x; 1, y] 0;
			le [1, x; 1, z] 0;
			le [-1, x] 0
		], [
			le [1, y] 0;
			le [1, z] 0 ];

		"mixed0", x, [
			le [-1, x] 0;
			le [1, x; 1, y] 0;
			le [1, z] 0
		], [
			le [1, y] 0;
			le [1, z] 0 ]
	] in
	Test.suite "fmElim_one" [
		Test.suite "res" (List.map chk_res tcs)
	]

(* IneqSet.fmElimM *)
let fmElimMTs: Test.t
= fun () ->
	let chk_res (t, msk, cList, cList1) =fun state ->
		let s = iset cList in
		let s1 = iset cList1 in
		let s2 = IneqSet.fmElim factory nxt EqSet.nil msk s in
		if equal s1 s2 then
			Test.succeed state
		else
			Test.fail t (IneqSet.to_string_ext factory varPr s2) state
	in
	let tcs = [
		"nil0", mask [], [], [];
		"nil1", mask [x], [], [];
		"nil2", mask [], [
			le [1, x] 0
		], [
			le [1, x] 0 ];

		"two0", mask [x; y], [
			le [1, x] 0
		], [];

		"two1", mask [x; y], [
			le [1, x] 0;
			le [-1, x; 1, y] 0
		], [];

		"two2", mask [x; y], [
			le [1, x] 0;
			le [-1, x; 1, y] 0;
			le [-1, y; 1, z] 0
		], [
			le [1, z] 0 ]
	] in
	Test.suite "fmElim" [
		Test.suite "res" (List.map chk_res tcs)
	]

(* IneqSet.incl *)
let isInclTs: Test.t
= fun () ->
	let chk (name, es, s1, s2, expected) = fun state ->
		let actual = IneqSet.incl factory nxt es s1 s2 in
		if relEq actual expected then
			Test.succeed state
		else
			let err = Printf.sprintf "expected:\n%s\nactual:\n%s" (relPr expected) (relPr actual) in
			Test.fail name err state
	in
	let tcs = [
		"simpl0", EqSet.nil, iset [
			le [1, x] 1
		], iset [
			le [1, x] 2 ],
		IneqSet.Incl ([le [1, x] 2 ]);

		"simpl1", EqSet.nil, iset [
			le [1, x] 2
		], iset [
			le [1, x] 1 ],
		IneqSet.NoIncl;

		"simpl_lt", EqSet.nil, iset [
			lt [1, x] 1
		], iset [
			le [1, x] 2 ],
		IneqSet.Incl ([le [1, x] 2]);

        "simpl_lt2", EqSet.nil, iset [
			lt [-1, x] (-1)
		], iset [
			lt [-1, x] (-1) ],
		IneqSet.Incl ([lt [-1, x] (-1)]);

		"comb0", EqSet.nil, iset [
			le [1, y] 1;
			le [1, z] 1
		], iset [
			le [1, y; 1, z] 2 ],
		IneqSet.Incl [le [1, y; 1, z] 2];

		"substSimpl0", mkEqSet [eq [1, x] 0], iset [
			le [1, y] 1
		], iset [
			le [1, x; 1, y] 1 ],
		IneqSet.Incl [le [1, x; 1, y] 1 ];

		"substTriv0", mkEqSet [eq [1, x] 0], iset [], iset [
			le [-1, x] 0 ],
		IneqSet.Incl [le [-1, x] 0];

		"substLt", mkEqSet [eq [1, x] 0], iset [], iset [
			lt [1, x] 1 ],
		IneqSet.Incl [lt [1, x] 1];

		"substTriv1", mkEqSet [eq [1, x] 0], iset [
			le [1, y] 0
		], iset [
			le [1, y] 0;
			le [-1, x] 0 ],
		IneqSet.Incl [le [1, y] 0; le [-1, x] 0];

		"subst_comb0", mkEqSet [eq [1, z] 0], iset [
			le [-1, x; 1, y] 5;
			le [-1, x; -1, y] (-7)
		], iset [le [-1, x; 1, z] 0],
		IneqSet.Incl [le [-1, x; 1, z] 0]
	] in
	Test.suite "isIncl" (List.map chk tcs)

let ts: Test.t
    = fun () ->
    List.map Test.run [synInclTs; addMTs; substTs; isInclTs ; pickTs ; fmElimTs; fmElimMTs]
    |> Test.suite "IneqSet"
