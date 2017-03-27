open Vpl

module Cs = Cstr.Rat.Positive
module EqSet = IneqSet.EqSet
module Cons = EqSet.Cons
module Cert = Cons.Cert
module Vec = Cs.Vec
module V = Vec.V

let factory : Cs.t Cert.t = { 
	Cert.name = "Cstr"; 
	Cert.top = (Cs.mk Cstr.Eq [] Scalar.Rat.z);
	Cert.triv = (fun cmp n -> Cs.mk cmp [] n);
	Cert.add = Cs.add;    
	Cert.mul = Cs.mulc;
	Cert.to_le = (fun c -> {c with Cs.typ = Cstr.Le});
	Cert.merge = (fun c1 c2 ->
		let c1' = {c1 with Cs.typ = Cstr.Eq}
		and c2' = {c2 with Cs.typ = Cstr.Eq} in
		if Cs.equal c1' c2'
		then c1'
		else failwith "merge"); 
	Cert.to_string = Cs.to_string Cs.Vec.V.to_string;
	Cert.rename = Cs.rename;
}
	
let x = V.fromInt 1
let y = V.fromInt 2
let z = V.fromInt 3
let t1 = V.fromInt 4
let t3 = V.fromInt 5
let t2 = V.fromInt 6

let nxt = V.fromInt 7

let varPr: V.t -> string
= fun _x ->
	let vars: (V.t * string) list
	= [x, "x"; y, "y"; z, "z"; t1, "t1"; t2, "t2"; t3, "t3"]
	in
	try
		List.assoc _x vars
	with
	| Not_found -> "v" ^ (V.to_string _x)

let mkc t v c =
	Cs.mk t (List.map (fun (n, x) -> (Scalar.Rat.mk1 n, x)) v) (Scalar.Rat.mk1 c)

let eq = mkc Cstr.Eq
let le = mkc Cstr.Le
let lt = mkc Cstr.Lt

let mask l =
	List.fold_left (fun m x -> Vec.M.set None m x (Some x)) Vec.M.empty l

let optxpr =
	function
	| None -> "None"
	| Some x -> "Some " ^ (varPr x)
(*
let mkN = List.map (fun (i, n) -> (i, Scalar.Rat.mk1 n))
*)
let propPr = function
	| IneqSet.Empty f -> Printf.sprintf "Empty: %s" (factory.Cert.to_string f)
	| IneqSet.Trivial -> "Trivial"
	| IneqSet.Implied c -> Printf.sprintf "Implied %s" (factory.Cert.to_string c)
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
	let l_stricten = List.mapi (fun i c -> i,{c with Cs.typ = Cstr.Lt}) l in
	match Splx.checkFromAdd (Splx.mk nxt l_stricten) with
	| Splx.IsUnsat _ -> Pervasives.failwith "IneqSet_t.iset: unexpected empty interior"
	| Splx.IsOk sx_strict -> 
		let conss = List.map (fun c -> c,c) l in
		IneqSet.addM nxt IneqSet.nil conss (Splx.getAsg sx_strict)

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
	| IneqSet.Incl l -> Printf.sprintf "Incl l with l:\n%s" (Misc.list_to_string (Cs.to_string Cs.Vec.V.to_string) l " ; ")

(* IneqSet.synIncl *)
let synInclCheckTs: T.testT
=
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
			T.succeed state
		else
			let s = Printf.sprintf "Expected %s\ngot %s"
				(propPr (IneqSet.Check cOut))
				(propPr result)
			in
			T.fail t s state
	in
	let tcs = [
		"subst0",
			mkEqSet [eq [1, y] 0 ], iset [],
			le [1, x; 1, y] 0,
			(le [1, x] 0, eq [1, y] 0) ; 
				
		"substlt",
			mkEqSet [eq [1, y] 0 ], iset [],
			lt [1, x; 1, y] 2,
			(lt [1, x] 2, eq [1, y] 0)
			
	] in
	T.suite "check" (List.map chk tcs)

let synInclImpliedTs: T.testT
=	
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
			T.succeed state
		else
			let s = Printf.sprintf "Expected %s\ngot %s"
				(propPr (IneqSet.Implied cOut))
				(propPr result)
			in
			T.fail t s state
	in
	let tcs = [
			
		"subst1",
			mkEqSet [eq [1, x] 0 ], iset [],
			le [1, x] 1,
			(le [1, x] 1) ;
			
	] in
	T.suite "implied" (List.map chk tcs)
	
let synInclTs: T.testT
= T.suite "synIncl" [synInclCheckTs ; synInclImpliedTs]

(* IneqSet.addM *)
let addMTs: T.testT
=
let chk (nm, s, l, r)
	= fun st ->
		let ilist = List.mapi (fun i cs -> i, cs) (l @ (List.map Pervasives.fst s))
		in
		let l_stricten = List.map (fun (i,c) -> i, {c with Cs.typ = Cstr.Lt}) ilist in
		match Splx.checkFromAdd (Splx.mk nxt l_stricten) with
		| Splx.IsUnsat _ -> Pervasives.failwith "IneqSet_t.iset: unexpected empty interior"
		| Splx.IsOk sx_strict -> 
			let conss = List.map mkCons l in
			let s' = IneqSet.addM nxt s conss (Splx.getAsg sx_strict) in
			if IneqSet.equal r s'
			then T.succeed st
			else
				let estr = Printf.sprintf "expected:\n%s\ngot:\n%s\n"
					(IneqSet.to_string varPr r) (IneqSet.to_string varPr s')
				in
				T.fail nm estr st
	in
	let tcs: (string * Cs.t IneqSet.t * Cs.t list * Cs.t IneqSet.t) list
	= [
	"triv0", IneqSet.nil, [], IneqSet.nil;

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
	] in
	T.suite "addM" (List.map chk tcs)
	

(* IneqSet.pick *)
let pickTs: T.testT
=
	let chk (t, msk, x, l) = fun state ->
		let s = iset l in
		let x1 = IneqSet.pick msk s in
		if x = x1 then
			T.succeed state
		else
			T.fail t (optxpr x1) state
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
	T.suite "pick" (List.map chk tcs)

(* IneqSet.subst *)
let substTs: T.testT
=
	let chk_res (t, x, e, s, s1) = fun state ->
		let s2 = iset s in
		let s3 = iset s1 in
		let s4 = IneqSet.subst factory nxt EqSet.nil x e s2 in
		if IneqSet.equal s3 s4 then
			T.succeed state
		else
			T.fail t (IneqSet.to_string_ext factory varPr s4) state
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
	T.suite "subst" [
		T.suite "res" (List.map chk_res tcs)
	]

(* IneqSet.fmElim *)
let fmElimTs: T.testT
=
	let chk_res (t, x, s, s1) = fun state ->
		let s2 = iset s in
		let s3 = iset s1 in
		let s4 = IneqSet.fmElim factory nxt EqSet.nil x s2 in
		if IneqSet.equal s3 s4 then
			T.succeed state
		else
			T.fail t (IneqSet.to_string_ext factory varPr s4) state
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
	T.suite "fmElim" [
		T.suite "res" (List.map chk_res tcs)
	]

(* IneqSet.fmElimM *)
let fmElimMTs: T.testT
=
	let chk_res (t, msk, cList, cList1) =fun state ->
		let s = iset cList in
		let s1 = iset cList1 in
		let s2 = IneqSet.fmElimM factory nxt EqSet.nil msk s in
		if IneqSet.equal s1 s2 then
			T.succeed state
		else
			T.fail t (IneqSet.to_string_ext factory varPr s2) state
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
	T.suite "fmElimM" [
		T.suite "res" (List.map chk_res tcs)
	]

(* IneqSet.incl *)
let isInclTs: T.testT
=
	let chk (name, es, s1, s2, expected) = fun state ->
		let actual = IneqSet.incl factory nxt es s1 s2 in
		if relEq actual expected then
			T.succeed state
		else
			let err = Printf.sprintf "expected:\n%s\nactual:\n%s" (relPr expected) (relPr actual) in
			T.fail name err state
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
	T.suite "isIncl" (List.map chk tcs)

let ts: T.testT
= T.suite Vec.V.name [synInclTs; addMTs; substTs; isInclTs ; pickTs ; fmElimTs; fmElimMTs]

