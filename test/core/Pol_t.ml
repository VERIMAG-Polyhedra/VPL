(*
Vpl.Pol.Debug.enable_all();;
Vpl.Pol.Debug.print_enable();;
*)
(*Vpl.Join.Debug.enable_all();;
Vpl.Join.Debug.print_enable();;
Vpl.Proj.Debug.enable_all();;
Vpl.Proj.Debug.print_enable();;
Vpl.PLPCore2.Debug.enable_all();;
Vpl.PLPCore2.Debug.print_enable();;
Vpl.PLPCore.Debug.enable_all();;
Vpl.PLPCore.Debug.print_enable();;
Vpl.PSplx2.Debug.enable [Title ; MInput ; MOutput];;
Vpl.PSplx2.Debug.print_enable();;
Vpl.PSplx.Debug.enable [Title ; MInput ; MOutput];;
Vpl.PSplx.Debug.print_enable();;
*)
open Vpl

module Cs = Cstr.Rat
module Vec = Cs.Vec

let x = Var.fromInt 1
let y = Var.fromInt 2
let z = Var.fromInt 3
let a = Var.fromInt 4
let c = Var.fromInt 6
let d = Var.fromInt 5
let e = Var.fromInt 7

let varPr: Var.t -> string
	= fun _x ->
		let vars: (Var.t * string) list
		= [x, "x"; y, "y"; z, "z"; a, "a"; c, "c"; d, "d"; e, "e"]
		in
		List.assoc _x vars

module Factory = struct

	type cert = Pol.Cs.t

	let factory = {
		Factory.name = "Cstr";
		Factory.top = (Cs.mk Cstr_type.Eq [] Scalar.Rat.z);
		Factory.triv = (fun cmp n -> if
			match cmp with
			| Cstr_type.Le -> Scalar.Rat.le Scalar.Rat.z n
			| Cstr_type.Lt -> Scalar.Rat.lt Scalar.Rat.z n
			| Cstr_type.Eq -> Scalar.Rat.equal n Scalar.Rat.z
			then ()
			else (Printf.sprintf "triv %s %s 0"
			 	(Cstr_type.(match cmp with | Le -> "<=" | Lt -> "<" | Eq -> "="))
				(Scalar.Rat.to_string n)
				|> print_endline;
				Stdlib.failwith "Factory.Cstr.triv")
			;
			Cs.mk cmp [] n);
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

    let mk : Pol.Cs.t -> cert
        = fun cs -> cs

	let equal : Pol.Cs.t -> cert -> bool
		= fun cs cert ->
		Cs.equalSyn cs cert

    let mkCons : Cstr.Rat.t -> cert Cons.t
		= fun cs -> (cs, mk cs)

    let convert : 'c Pol.t -> cert Pol.t
        = fun p -> {
            Pol.eqs = List.map (fun (v,(cstr,_)) ->
                (v,mkCons cstr)
            ) p.Pol.eqs;
            Pol.ineqs = List.map (fun (cstr,_) ->
                mkCons cstr
            ) p.Pol.ineqs.ineqs
            |> IneqSet.of_list;
            Pol.point = p.Pol.point;
        }

    let check : cert Pol.t -> bool
		= fun p ->
		List.for_all (fun (c,cert) ->
            equal c cert
        ) (Pol.get_cons p)

    let to_string = factory.to_string
end

let factory = Factory.factory

let mkc t v c =
	Cs.mk t (List.map (function (i, x) -> (Scalar.Rat.of_int i, x)) v) (Scalar.Rat.of_int c)

let eq = mkc Cstr_type.Eq
let le = mkc Cstr_type.Le
let lt = mkc Cstr_type.Lt

let mkCons : Cs.t -> Cs.t Cons.t
	= fun c -> (c,c)

let p (l: Cs.t list): Cs.t Pol.t =
	match Pol.mk factory (List.map mkCons l) with
	| None -> failwith "Pol.mk returned None"
	| Some poly -> poly

let p': (Var.t * Cs.t) list -> Cs.t list -> Cs.t Pol.t
	= fun eqs ineqs -> {
		Pol.eqs = List.map (fun (x, c) -> (x, mkCons c)) eqs;
		Pol.ineqs = List.map mkCons ineqs |> IneqSet.of_list;
        Pol.point = None}

let check_certificates : Cs.t Pol.t -> bool
	= fun p ->
	List.for_all (fun (c,cert) -> Cs.equal c cert)
	(EqSet.list p.Pol.eqs @ p.Pol.ineqs.ineqs)


module Make_Tests (F : sig
	val set : unit -> unit
	end) = struct

	let set : unit = F.set()

	let contrad : Cs.t = Cs.eq [] Scalar.Rat.negU

	let eqBnd: Pol.bndT -> Pol.bndT -> bool = fun bnd1 bnd2 ->
		match bnd1, bnd2 with
		| Pol.Infty, Pol.Infty -> true
		| Pol.Open n1, Pol.Open n2
		| Pol.Closed n1, Pol.Closed n2 -> Scalar.Rat.cmp n1 n2 = 0
		| _, _ -> false

	let eqItv: Pol.itvT -> Pol.itvT -> bool = fun itv1 itv2 ->
		eqBnd (Pol.get_low itv1) (Pol.get_low itv2) &&
		eqBnd (Pol.get_up itv1) (Pol.get_up itv2)

	(* Pol.add *)
	let addTcs =
		[
		"noEqs0", false, le [1, x] 0, p [
			le [1, y] 0 ],
			[Pol.Added (p' [] [
				le [1, y] 0;
				le [1, x] 0])];

		"eqs0", false, le [1, x; 1, y] 0, p [
			eq [1, x; -1, y] 0 ],
			[Pol.Added (p' [
				x, eq [1, x; -1, y] 0]
				[le [1, y] 0])];

		"eqs1", false, le [1, x; 1, y] 0, p [
			eq [1, x; -1, z] 0],
			[Pol.Added (p' [
				x, eq [1, x; -1, z] 0]
				[le [1, y; 1, z] 0])];

		"eqs2", false, eq [1, x; 1, y] 1, p [
			eq [1, x] 0],
			 [Pol.Added (p' [
				x, eq [1, x] 0;
				y, eq [1, y] 1] [])];

		"eqs3", false, eq [1, x; 1, y] 1, p [
			eq [1, x] 0;
			le [1, y; 1, z] 0],
			[Pol.Added (p' [
				x, eq [1, x] 0;
				y, eq [1, y] 1]
				[le [1, z] (-1)])];

		"newEq0", false, eq [1, x] 0, p [
			le [1, x; 1, y] 2 ],
			[Pol.Added (p' [
				x, eq [1, x] 0]
				[le [1, y] 2])];

		"implicitEq0", false, le [1, x] 0, p [
			le [1, y] 0;
			le [-1, x] 0 ],
			[Pol.Added (p' [
				x, eq [1, x] 0]
				[le [1, y] 0])];

		(* substitutions in the building blocks of an implicit equality *)
		"implicitEqs2", false, le [1, x; 1, y] 0, p [
			eq [1, y] 0;
			le [-1, x] 0 ],
			[Pol.Added (p' [
				x, eq [1, x] 0;
				y, eq [1, y] 0] [])];

		"emptyEq0", true, eq [-1, z] 2, p [
			eq [-1, x] 2;
			eq [-1, y] 0;
			le [-1, z] (-6) ],
			[Pol.Contrad contrad];

		"emptyLe0", true, le [1, x] 0, p [
			le [-1, x] (-1) ],
			[Pol.Contrad contrad];

		"emptyLe1", true, le [1, x; 1, y] 0, p [
			le [-1, x] (-1);
			eq [1, y] 0 ],
			[Pol.Contrad contrad];

		"emptyLe2", true, le [-1, x] (-3), p [
			eq [1, x] 2;
			eq [1, y] 1],
			[Pol.Contrad contrad];

		"trans_post4_first_side", true, le [1, x ; -1, y] (-1), p [
			le [-1, x] 0;
			le [1, x ; -1, y] 0;
			le [2, y ; -1, z] 0;
			le [1, z ; -1, x] 0;
			],
			[Pol.Contrad contrad];

		"trans_post4_other_side", true, le [-1, x ; 1, y] (-1), p [
			le [-1, x] 0;
			le [1, x ; -1, y] 0;
			le [2, y ; -1, z] 0;
			le [1, z ; -1, x] 0;
			],
			[Pol.Contrad contrad];

		"trans_post4_other_side_2", true, le [-1, x ; 1, y] (-1), p [
			eq [1, y] 0;
			eq [1, z] 0;
			eq [1, x ; -1, z] 0;
			],
			[Pol.Contrad contrad];

		"eqs4", false, eq [1, x] 0, p [
		le [1, x; 1, y] 1;
		le [1, y] 1],
		[Pol.Added (p'
			[x, eq [1, x] 0]
			[le [1, y] 1])];

		"implicitEqs0", false, le [1, z; -1, x] 0, p [
			le [1, y; -1, z] 0;
			le [1, x; -1, y] 0 ],
			[Pol.Added (p' [
				y, eq [1, y; -1, z] 0;
				x, eq [1, x; -1, z] 0] []);
			 Pol.Added (p' [
				y, eq [1, y; -1, z] 0;
				x, eq [1, x; -1, y] 0] [])];

		"implicitEqs1", false, le [1, z; -1, x] 0, p [
			le [1, y; -1, z] 0;
			le [1, x; -1, y] 0;
			le [1, x; 1, a] 1 ],
			[Pol.Added (p'
				[x, eq [1, x; -1, z] 0;
				y, eq [1, y; -1, z] 0]
				[le [1, z; 1, a] 1]);
			 Pol.Added (p'
				[x, eq [1, x; -1, y] 0;
				y, eq [1, y; -1, z] 0]
				[le [1, z; 1, a] 1])]

	]
	|> List.map (fun (a,b,c,d,e) -> a,b, mkCons c,d,e)

	let addTs: Test.t
	= fun () ->
		let chkBot (t, empty, c, p, _) = fun state ->
			match Pol.add factory p c with
			| Pol.Added _ ->
				if not empty then
					Test.succeed state
				else
					Test.fail t "should be bottom" state
			| Pol.Contrad _ ->
				if empty then
					Test.succeed state
				else
					Test.fail t "should not be bottom" state
		in
		let chk_subst (t, isEmpty, c, p, _) = fun state ->
			match Pol.add factory p c, isEmpty with
			| Pol.Contrad _, true
			| Pol.Contrad _, false
			| Pol.Added _, true -> Test.succeed state
			| Pol.Added p1, false ->

			let defs = List.map (fun (x, _) -> x) (Pol.get_eqs p1) in
			let chk1 c = List.for_all
				(fun x -> Scalar.Rat.cmpz (Vec.get (Cs.get_v (Cons.get_c c)) x) = 0) defs
			in
			if List.for_all chk1 (Pol.get_ineqs p1) then
				Test.succeed state
			else
				let err = "defined variables in inequality set\n" ^ (Pol.to_string_ext factory varPr p1) in
				Test.fail t err state
		in
		let chk_inv (nm, _, c, p, _)
		= fun st ->
			match Pol.add factory p c with
			| Pol.Contrad _ -> Test.succeed st
			| Pol.Added p' ->
				match Pol.invChk factory p' with
				| true, _ -> Test.succeed st
				| false, e -> Test.fail nm ((Pol.to_string_ext_raw factory p') ^ "\n" ^ e) st
		in
		let chkCert : string * bool * Cs.t Cons.t * Cs.t Pol.t * Cs.t Pol.meetT list -> Test.stateT -> Test.stateT
			= let handleContrad: Cs.t -> string option
				= fun a_ce ->
				if Cs.tellProp a_ce = Cs.Contrad
				then None
				else Some "bad certificate: contradiction expected"
			in
			let mkplist p =
				let e = EqSet.list p.Pol.eqs in
				let i = p.Pol.ineqs.ineqs in
				List.append e i
			in
			(* TODO: remplacer ces tests par des tests d'inclusion*)
			let handleAdded : Cs.t Cons.t list -> Cs.t Cons.t list -> string option
				= fun a_conss e_conss ->
				if List.for_all
					(fun cons -> Cs.equalSyn (Cons.get_c cons) (Cons.get_cert cons))
					a_conss
				  &&
				  	Misc.list_eq2
				  	(fun cons1 cons2 -> Cs.equalSyn (Cons.get_c cons1) (Cons.get_c cons2))
				  	a_conss e_conss
				then None
				else let e = Printf.sprintf "bad certificate:\nExpected %s\ngot %s"
					(Misc.list_to_string (Cons.to_string_ext factory Var.to_string) e_conss "\n")
					(Misc.list_to_string (Cons.to_string_ext factory Var.to_string) a_conss "\n")
					in
					Some e
			in
			let chk (c,p,r) =
				(match Pol.add factory p c, r with
				| Pol.Added p, Pol.Added q -> handleAdded (mkplist p) (mkplist q)
				| Pol.Contrad ce, Pol.Contrad ce' -> handleContrad ce
				| Pol.Added _, Pol.Contrad _
				| Pol.Contrad _, Pol.Added _-> Some "wrong certificate type")
			in
			fun (nm, _, c, p, certs) st ->
			if List.exists (fun cert -> chk (c, p, cert) = None) certs
			then Test.succeed st
			else
				match chk (c, p, List.hd certs) with
				| None -> assert false
				| Some e -> Test.fail nm e st
		in
		Test.suite "add" [
			Test.suite "bot" (List.map chkBot addTcs);
			Test.suite "subst" (List.map chk_subst addTcs);
			Test.suite "inv" (List.map chk_inv addTcs);
			Test.suite "forward_cert" (List.map chkCert addTcs)
		]

	(* Pol.meet *)
	let meetTcs: (string * Cs.t Pol.t * Cs.t Pol.t * Cs.t Pol.t option) list
	= [
		"trivial0", p [], p [], Some (p []);

	(* simple preservation of the constraints in the inputs *)
		"trivial1", p [
			le [1, x] 0
		], p [], Some (p [
			le [1, x] 0
		]);

		"trivial2", p [], p [
			le [1, x] 0
		], Some (p [
			le [1, x] 0
		]);

		"trivial3", p [
			eq [1, x] 0
		], p [], Some (p [
			eq [1, x] 0
		]);

		"trivial4", p [], p [
			eq [1, x] 0
		], Some (p [
			eq [1, x] 0
		]);

		"trivial5", p [
			le [1, x] 0
		], p [
			le [1, y] 0
		], Some (p [
			le [1, x] 0;
			le [1, y] 0
		]);

		"trivial6", p [
			eq [1, x] 0
		], p [
			le [1, y] 0
		], Some (p [
			eq [1, x] 0;
			le [1, y] 0
		]);

		"trivial7", p [
			le [1, x] 0
		], p [
			eq [1, y] 0
		], Some (p [
			le [1, x] 0;
			eq [1, y] 0
		]);

		"trivial6", p [
			eq [1, x] 0
		], p [
			eq [1, y] 0
		], Some (p [
			eq [1, x] 0;
			eq [1, y] 0
		]);

	(* redundant constraint elimination *)
		"red0", p [
			le [1, x] 0
		], p [
			le [1, x] 1
		], Some (p [
			le [1, x] 0
		]);

		"red1", p [
			le [1, x] 0;
			le [1, y] 1
		], p [
			le [1, x; 1, y] 1
		], Some (p [
			le [1, x] 0;
			le [1, y] 1
		]);

		"red2", p [
			le [1, x] 1
		], p [
			le [1, x] 0
		], Some (p [
			le [1, x] 0
		]);

		"red3", p [
			le [1, x; 1, y] 1
		], p [
			le [1, x] 0;
			le [1, y] 1
		], Some (p [
			le [1, x] 0;
			le [1, y] 1
		]);

		"red4",
			p [eq [1, x] 0],
			p [le [1, x] 0],
			Some (p [eq [1, x] 0]);

	(* variable substitutions using equalities *)
		"eq0", p [
			eq [1, x] 0
		], p [
			le [1, x; 1, y] 1
		], Some (p [
			eq [1, x] 0;
			le [1, y] 1
		]);

		"eq1", p [
			eq [1, x] 0
		], p [
			le [1, x] 1
		], Some (p [
			eq [1, x] 0
		]);

		"eq2", p [
			eq [1, x] 0;
			le [1, y; 1, z] 1
		], p [
			eq [1, y] 2;
			le [1, x; 1, z] 1
		], Some (p [
			eq [1, x] 0;
			eq [1, y] 2;
			le [1, z] (-1)
		]);

	(* new implicit equalities *)
		"implicit0", p [
			le [1, x] 0
		], p [
			le [-1, x] 0
		], Some (p [
			eq [1, x] 0
		]);

		"implicit1", p [
			le [1, x] 0
		], p [
			le [-1, x] 0;
			le [1, x; 1, z] 2
		], Some (p [
			eq [1, x] 0;
			le [1, z] 2
		]);

		"implicit2", p [
			le [1, x] 0;
			eq [1, y] 2
		], p [
			le [-1, x] 0;
			le [1, x; 1, y; 1, z] 2
		], Some (p [
			eq [1, y] 2;
			eq [1, x] 0;
			le [1, z] 0
		]);

	(* empty polyhedra *)
		"empty0", p [
			le [1, x] 0
		], p [
			le [-1, x] (-2)
		], None;

		"empty1", p [
			eq [1, x] 0
		], p [
			le [-1, x] (-2)
		], None;

		"empty2", p [
			le [1, x] 0
		], p [
			eq [-1, x] (-2)
		], None;

		"empty3", p [
			eq [1, x] 0
		], p [
			eq [1, x] (-2)
		], None;

		"empty4", p [
			le [1, x; 1, y] 1;
			le [-1, y] (-2)
		], p [
			le [-1, x] 0
		], None;

		"empty5", p [
			le [1, x; 1, y] 1
		], p [
			eq [1, x] 0;
			le [-1, y] (-2)
		], None
	]

	let meetTs: Test.t
	= fun () ->
    let chk (nm, p1, p2, maybeP)
		= fun st ->
			let noneExpected: Cs.t Pol.meetT -> Test.stateT
			= function
				| Pol.Contrad _ -> Test.succeed st
				| Pol.Added _ ->
					let estr = "expected None but got Some _" in
					Test.fail nm estr st
			in
			let someExpected: Cs.t Pol.t -> Cs.t Pol.meetT -> Test.stateT
			= fun p -> function
				| Pol.Contrad _ ->
					let estr = "expected Some _ but got None" in
					Test.fail nm estr st
				| Pol.Added p' ->
					if Pol.equal factory factory p p'
					then Test.succeed st
					else
						let estr = Printf.sprintf "expected\n%s\ngot\n%s\n"
							(Pol.to_string_ext factory varPr p) (Pol.to_string_ext factory varPr p')
						in
						Test.fail nm estr st
			in
			let res = Pol.meet factory p1 p2 in
			match maybeP with
			| None -> noneExpected res
			| Some p -> someExpected p res
		in
		Test.suite "meet" (List.map chk meetTcs)

	(* Pol.incl *)
	let inclTcs = [
		"simple0", true, p [
			le [1, x; 1, y] 2
		], p [
			le [1, x; 1, y] 3 ];

		"simple1", true, p [
			le [1, x] 2
		], p [
			le [1, x] 3 ];

		"simple2", true, p [
			le [1, x] 2
		], p [
			le [2, x] 5 ];

		"eq0", true, p [
			le [1, x] 1;
			le [-1, x] (-1)
		], p [
			eq [1, x] 1 ];

		"fail0", false, p [
			le [1, x] 2
		], p [
			le [1, x] 1 ];

		"eq1", true, p [
			eq [1, x] 0
		], p [
			eq [1, x] 0 ];

		"eq2", true, p [
			eq [-1, x] 0
		], p [
			eq [-1, x] 0 ];

		"decompress", false, p [
			le [-1, y] (-1);
			le [-1, x; 16, y] 15;
			le [-1, x] (-17)
		], p [
			le [1, x; -16, y] 17 ];

		"sylvain", false, p [
			le [2, x] 3
		], p [
			le [1, x] 1 ];

		"splx0", true, p [
			le [1, x] 2;
			le [2, y] 1;
		], p [
			le [1, x; 2, y] 3 ];

		"order", true, p [
			le [1, x] 2;
			le [1, y] 3
		], p [
			le [1, y] 3;
			le [1, x] 2];

		"branch", true, {
		Pol.eqs = [
			(x, mkCons (eq [1, x] 3))];
		Pol.ineqs = IneqSet.top;
        Pol.point = None;
		}, {
		Pol.eqs = [];
		Pol.ineqs = [
			mkCons (le [-1, x] 0);
			mkCons (le [1, x] 3)] |> IneqSet.of_list;
        Pol.point = None;};

		"unsat_eq", false, {
		Pol.eqs = [
			(x, mkCons (eq [1, x] 1))];
		Pol.ineqs = [
			mkCons (le [-1, z] (-1));
			mkCons (le [1, y] 0);
			]|> IneqSet.of_list;
        Pol.point = None;
		}, {
		Pol.eqs = [];
		Pol.ineqs = [
			mkCons (le [1, x] 0);
			mkCons (le [-1, z] (-1));
			mkCons (le [-1, x ; 1, y] (-1))
			]|> IneqSet.of_list;
        Pol.point = None;};
		(*
		"sylvain_while2", true, {
		Pol.eqs = [
			(x, mkCons (Cs.mk Cstr_type.Eq [Scalar.Rat.u, x ; Scalar.Rat.of_string "-1/3", y ; Scalar.Rat.of_string "1/3", z] Scalar.Rat.z))];
		Pol.ineqs = [
			mkCons (le [2, y] 20);
			mkCons (le [-1, y] (-12));
			mkCons (le [1, z] 2);
			mkCons (le [-1, z] 0);
		]
		}, {
		Pol.eqs = [];
		Pol.ineqs = [
			mkCons (le [-1, x] (-4));
			mkCons (le [1, x] 6)]};*)
	]

	let inclTs: Test.t
	= fun () ->
		let chk_res (name, r, p1, p2) = fun state ->
			let (ok1, e1) = Pol.invChk factory p1 in
			if not ok1 || not (check_certificates p1)
			then Test.fail name ("bad p1:\n" ^ e1) state
			else
				let (ok2, e2) = Pol.invChk factory p2 in
				if not ok2 || not (check_certificates p2)
				then Test.fail name ("bad p2:\n" ^ e2) state
				else
					let dump: Cs.t Pol.t -> Cs.t Pol.t -> string
					= fun p1 p2 ->
						Printf.sprintf "p1:\n%s\np2:\n%s"
							(Pol.to_string_ext factory varPr p1) (Pol.to_string_ext factory varPr p2)
					in
					match Pol.incl factory p1 p2 with
					| Pol.NoIncl ->
						if r
						then Test.fail name ("no inclusion\n" ^ (dump p1 p2)) state
						else Test.succeed state
					| Pol.Incl _ ->
						if r
						then Test.succeed state
						else Test.fail name ("unexpected inclusion\n" ^ (dump p1 p2)) state
		in
		Test.suite "incl" (List.map chk_res inclTcs)

	(* Pol.project *)
	let projectTcs = [
		"empty", x, p [], p [];
		"no_x0", x, p [eq [1, y] 0], p [eq [1, y] 0];
		"no_x1", x, p [eq [1, y] 0; eq [2, y; 1, z] 0],
			p [eq [1, y] 0; eq [2, y; 1, z] 0];
		"only_x", x, p [eq [1, x] 0], p [];
		"simple0", x, p [le [1, x; 2, y] 1; le [1, y] 2], p [le [1, y] 2];
		"subst0", x, p [eq [1, x; -1, y] 0; le [1, x] 1], p [le [1, y] 1];
		"fme0", x, p [le [1, x] 0; le [-1, x; 1, y] 1], p [le [1, y] 1];
		"trivial0", x, p [le [1, x; 1, y] 0; le [-1, x; -1, y] 1], p [];
	]

	(* XXX: Add syntactic criteria for the certificates *)
	let projectTs: Test.t
	= fun () ->
		let chk_res (t, v, p, r) = fun state ->
			let p1 = Pol.project factory p [v] in
			if Pol.equal factory factory p1 r && check_certificates p1 then
				Test.succeed state
			else
				let estr =
					"projecting x from polyhedron P:\n" ^ (Pol.to_string_ext factory varPr p) ^
					"\ngives polyhedron P1:\n" ^ (Pol.to_string_ext factory varPr p1) ^
					"\nwhich is not equal to the expected R:\n" ^
						(Pol.to_string_ext factory varPr r)
				in
				Test.fail t estr state
		in
		Test.suite "project" (List.map chk_res projectTcs)

	(* Pol.projectM *)
	let projectMTcs = [
		"empty", [x], p [], p [];
		"no_x0", [x], p [
			eq [1, y] 0
		], p [
			eq [1, y] 0 ];

		"no_x1", [x], p [
			eq [1, y] 0;
			eq [2, y; 1, z] 0
		], p [
			eq [1, y] 0;
			eq [2, y; 1, z] 0 ];

		"only_x", [x], p [
			eq [1, x] 0
		], p [];

		"simple0", [x], p [
			le [1, x; 2, y] 1;
			le [1, y] 2
		], p [
			le [1, y] 2 ];

		"subst0", [x], p [
			eq [1, x; -1, y] 0;
			le [1, x] 1
		], p [
			le [1, y] 1 ];

		"fme0", [x], p [
			le [1, x] 0;
			le [-1, x; 1, y] 1
		], p [
			le [1, y] 1 ];

		"trivial0", [x], p [
			le [1, x; 1, y] 0;
			le [-1, x; -1, y] 1
		], p [];

		"le0", [x; z], p [
			le [1, x; 2, y; -3, z] 0;
			le [-2, x; -1, z] 0;
			le [3, y; 1, z] 0
		], p [
			le [1, y] 0 ];

		"eq0", [x; z], p [
			le [1, x; 2, y; -3, z] 0;
			le [-2, x; -1, z] 0;
			eq [1, y; -1, z] 0
		], p [
			le [-1, y] 0 ];

		"eq0_reversed_order", [z; x], p [
			le [1, x; 2, y; -3, z] 0;
			le [-2, x; -1, z] 0;
			eq [1, y; -1, z] 0
		], p [
			le [-1, y] 0 ];

		(*"Kohler_failure", [a; c; e; d],
		p [
			 le [120150, a; 112894, c; 47, e; -122500, d] 0;
			 le [-1, a; -1, c; 1, e] 0;
			 le [-1, a; -1, c; 1, d] 0;
			 le [51, a; 47, c; -51, d] 0;
			 le [122453, a; 112847, c; 47, e; -122500, d] 0;
			 le [1, x; 49, a; -1, c] 49;
			 le [-1, x; 1, a; 1, c; 1, z; -1, e] 1;
			 le [-1, a] 0;
			 le [-1, x; 1, y; 1, a; 1, c; -1, d] 1;
			 le [-1, y; 1, d] 0;
			 le [-2499, a; 2401, c; 1, e; -2500, d] 0;
			 le [51, a; 47, c; -51, e] 0;
			 le [-51, a; 49, c; -51, e] 0;
			 le [-1201, a; 1152, c; 49, e; -1250, d] 0;
			 le [5884803, a; 5529503, c; 2303, e; -6000000, d] 0
		  ],
		p [
			 le [49, x; -51, y] 2401;
			 le [-1, x; 1, y] 1;
			 le [-1, y] 0;
			 le [2401, x; -2500, y; 1, z] 117699;
			 le [-1, x; 1, z] 1
		  ]*)
	]

	let projectMTs: Test.t
	= fun () ->
		let chk_res (t, l, p, r) = fun state ->
			(*Proj.Debug.enable_all();
			Proj.Debug.print_enable();
			PLPCore.Debug.enable_all();
			PLPCore.Debug.print_enable();
			PSplxExec.Debug.enable_all();
			PSplxExec.Debug.print_enable();*)
			let p1 = Pol.project factory p l in
			if Pol.equal factory factory p1 r && check_certificates p1 then
				Test.succeed state
			else
				let estr =
					"projecting" ^
					(List.fold_left (fun s x -> s ^ " " ^ (varPr x)) "" l) ^
	 				" from polyhedron P:\n" ^ (Pol.to_string_ext factory varPr p) ^
					"\ngives polyhedron P1:\n" ^ (Pol.to_string_ext factory varPr p1) ^
					"\nwhich is not syntactically equal to the expected R:\n" ^
						(Pol.to_string_ext factory varPr r)
				in
				Test.fail t estr state
		in
		Test.suite "projectM" (List.map chk_res projectMTcs)
(*
	(* Pol.joinSetup *)
	let joinSetupTs: Test.t
	= fun () ->
		let x = Var.fromInt 1 in
		let y = Var.fromInt 2 in
		let alpha = Var.fromInt 3 in
		let x1 = Var.fromInt 4 in
		let y1 = Var.fromInt 6 in
		let chk_pol (t, _, p1, p2, p0) = fun state ->
			let (_, _, p, _) = Pol.joinSetup (Var.next y) p1 p2 in
			if Pol.equal p0 p then
				Test.succeed state
			else
				let estr =
					"expected p:\n" ^ (Pol.to_string_ext varPr p0) ^
					"\n\nactual p:\n" ^ (Pol.to_string_ext varPr p)
				in
				Test.fail t estr state
		in
		let chk_vars (t, l0, p1, p2, _) = fun state ->
			let (_, _, p, l) = Pol.joinSetup (Var.next y) p1 p2 in
			let incl l1 l2 =
				List.for_all (fun x -> List.exists ((=) x) l2) l1
			in
			if incl l l0 && incl l0 l then
				Test.succeed state
			else
				Test.fail t "bad var list" state
		in
		let chk_frag (t, _, p1, p2, _) = fun state ->
			let (_, _, p, _) = Pol.joinSetup (Var.next y) p1 p2 in
			let initP = { p with
				Pol.eqs = EqSet.initc (Pol.get_eqs p);
				Pol.ineqs = IneqSet.initc (Pol.get_ineqs p) }
			in
			let chk =
				List.for_all2 (fun c1 c2 ->
					List.for_all2 (fun (x1, n1) (x2, n2) ->
						x1 = x2 && Scalar.Rat.cmp n1 n2 = 0) (Cons.get_f c1) (Cons.get_f c2))
			in
			if chk (EqSet.list (Pol.get_eqs p)) (EqSet.list (Pol.get_eqs initP)) &&
				chk (IneqSet.list (Pol.get_ineqs p)) (IneqSet.list (Pol.get_ineqs initP)) then
				Test.succeed state
			else
				let e = "not equal\np:\n" ^ (Pol.to_string_ext varPr p) ^ "\ninitP:\n" ^ (Pol.to_string_ext varPr initP) in
				Test.fail t e state
		in
		let tcs = [
			"one", [alpha; x1; y1], p [
				eq [1, x] 1;
				le [-1, y] 1
			], p [
				eq [1, x] 1;
				le [1, y] 2
			], p [
				eq [1, x; -1, x1; 1, alpha] 1;
				eq [1, x1; -1, alpha] 0;
				le [-1, y1; -1, alpha] 0;
				le [1, y; -1, y1; 2, alpha] 2;
				le [-1, alpha] 0;
				le [1, alpha] 1 ];

			"all", [alpha; x1; y1], p [
				eq [1, x] 1
			], p [
				eq [1, y] 1
			], p [
				eq [1, y; -1, y1; 1, alpha] 1;
				eq [1, x1; -1, alpha] 0;
				le [-1, alpha] 0;
				le [1, alpha] 1 ]
		] in
		Test.suite "joinSetup" [
			Test.suite "pol" (List.map chk_pol tcs);
			Test.suite "vars" (List.map chk_vars tcs);
			Test.suite "frag" (List.map chk_frag tcs)
		]
*)
	(* Pol.join *)
	let joinTcs: (string * Cs.t Pol.t * Cs.t Pol.t * Cs.t Pol.t) list
	= [
		"one", p [
			eq [1, x] 1;
			le [-1, y] 1
		], p [
			eq [1, x] 1;
			le [1, y] 2
		], p [
			eq [1, x] 1
		];

		"all",p [
			eq [1, x] 1
		], p [
			eq [1, y] 1
		], p [];

		"chull969", p [
			eq [1, c; -1, d] (-4080);
			eq [-1, e] (-15);
			le [1, z] 6;
			le [1, c] 15;
			le [-1, a] (-256);
			le [-1, z] (-2);
			le [-1, y] (-1);
			le [-1, x; 1, z] 0
		], p [
			le [-16, e; 1, d] 3855
		], p [
			le [1, d; -16, e] 3855
		];

		"sylvain", p [
			eq [-1, x; 1, y] 5;
			le [1, x] 5;
			le [-1, x] (-1)
		], p [
			eq [1, x; -1, y] 5;
			le [1, x] 10;
			le [-1, x] (-6)
		], p [
			le [1, x; -1, y] 5;
			le [1, x; 1, y] 15;
			le [-1, x; -1, y] (-7);
			le [-1, x; 1, y] 5
		];

		"jdphuff_chull777", p [
			eq [-1, x] 0;
			le [-1, y] 0;
			le [-1, z; 1, a] 1
		], p [
			eq [-1, x] 0;
			le [-1, y] 0;
			eq [-1, z; 1, a] 1
		], p [
			eq [1, x] 0;
			le [-1, y] 0;
			le [1, a; -1, z] 1
		];
		(* known to fail*)

		(*"notClosed0", p [
			lt [1, y] 3;
		], p [
			le [1, x; 1, y] 5;
			le [-1, x; 1, y] 1
		], p [
			le [1, y] 3
		];*)
        (*
        "similarities", p [
            le [-1, x ; 1, y] 0;
            le [1, x ; 1, y] 5;
            le [56, x ; 20, y] 237;
            le [-1, y] 0;
            lt [-586, x ; -280, y] (-922)
        ], p [
            le [-1, x] (-1);
            le [-1, y] 2;
            le [56, x ; 20, y] 237;
            lt [1, y] 0;
            lt [-333, x ; 112, y] (-501)
        ], p [
            le [56, x ; 20, y] 237;
            lt [-1, x ; 1, y] 0;
            lt [1, x ; 1, y] 5;
            lt [-1, y] 2;
            lt [-1, x] (-1);
            lt [-2221, x ; 56, y] (-2305)
        ];
        *)

		"dots0", p [
			eq [1, x] 0;
			eq [1, y] 0
		], p [
			eq [1, x] 1;
			eq [1, y] 2
		], p [
			eq [2, x; -1, y] 0;
			le [1, y] 2;
			le [-1, y] 0
		];
		"chk_guess0", p [
			eq [1, y] 0
		], p [
			le [-1, y] (-1)
		], p [
			le [-1, y] 0
		]
	]

	let joinTs: Test.t
	= fun () ->
		let chk1 (name, p1, p2, _) = fun state ->
			let (p, _) = Pol.join factory factory p1 p2 in
            (*Debug.enable();
            Debug.print_enable();
            Debug.set_colors();
            Pol.Debug.enable DebugTypes.([MInput ; Title ; MOutput ; Normal ; Detail]);*)
			match Pol.incl factory p1 p, check_certificates p with
			| Pol.Incl _, true -> Test.succeed state
			| Pol.NoIncl, _ ->
				let err = Printf.sprintf "p1 not in p\np:\n%s\np1:%s"
                    (Pol.to_string_ext factory varPr p)
                    (Pol.to_string_ext factory varPr p1) in
				Test.fail name err state
			| _, false ->
				let err = Printf.sprintf "wrong certificate in p1 : %s" (Pol.to_string_ext factory varPr p) in
				Test.fail name err state
		in
		let chk2 (name, p1, p2, _) = fun state ->
			let (_,p) = Pol.join factory factory p1 p2 in
			match Pol.incl factory p2 p, check_certificates p with
			| Pol.Incl _, true -> Test.succeed state
			| Pol.NoIncl, _ ->
				let err = Printf.sprintf "p2 not in p\np:\n%s\np2:%s"
                    (Pol.to_string_ext factory varPr p)
                    (Pol.to_string_ext factory varPr p2) in
				Test.fail name err state
			| _, false ->
				let err = Printf.sprintf "wrong certificate in p2 : %s" (Pol.to_string_ext factory varPr p) in
				Test.fail name err state
		in
		let chkR (name, p1, p2, pR) = fun state ->
			let (p1', p2') = Pol.join factory factory p1 p2 in
			match Pol.incl factory p1' pR with
			| Pol.Incl _ -> begin
				match Pol.incl factory p2' pR with
				| Pol.Incl _ -> Test.succeed state
				| Pol.NoIncl ->
					let err =
						Printf.sprintf "expected:\n%s\ngot:\n%s\n"
							(Pol.to_string_ext factory varPr pR) (Pol.to_string_ext factory varPr p2')
					in
					Test.fail name err state
				end
			| Pol.NoIncl ->
				let err =
					Printf.sprintf "expected:\n%s\ngot:\n%s\n"
						(Pol.to_string_ext factory varPr pR) (Pol.to_string_ext factory varPr p1')
				in
				Test.fail name err state
		in
		Test.suite "join" [
			Test.suite "result" (List.map chkR joinTcs);
			Test.suite "1" (List.map chk1 joinTcs);
			Test.suite "2" (List.map chk2 joinTcs);
		]

	(* Pol.widen *)
	let widenTs: Test.t
	= fun () ->
		let chk (t, p1, p2, pR) = fun state ->
			if Pol.incl factory p1 p2 = Pol.NoIncl then
				failwith "Pol_t.widen_ts"
			else
				let p = Pol.widen factory p1 p2 in
				if Pol.equal factory factory p pR then
					Test.succeed state
				else
					let err =
						"p is different from expected pR" ^
						"\np:\n" ^ (Pol.to_string_ext factory varPr p) ^ "\npR:\n" ^ (Pol.to_string_ext factory varPr pR)
					in
					Test.fail t err state
		in
		let tcs = [
			"halbwachs0", p [
				eq [1, y] 0;
				le [-1, x; 1, y] 0;
				le [1, x; -1, y] 1
			], p [
				le [-1, y] 0;
				le [-1, x; 1, y] 0;
				le [1, x; 1, y] 2
			], p [
				le [-1, y] 0;
				le [-1, x; 1, y] 0
			];

			"halbwachs1", p [
				eq [1, y] 0;
				le [-1, x] 0;
				le [1, x] 1
			], p [
				le [-1, y] 0;
				le [-1, x; 1, y] 0;
				le [1, x; 1, y] 2
			], p [
				le [-1, y] 0;
				le [-1, x; 1, y] 0
			];

			(* XXX: pourquoi ce test ne passe pas? C'était déjà le cas dans la vpl1.0 *)
			(*
			"dots0", p [
				eq [1, x] 0;
				eq [1, y] 0
			], p [
				eq [2, x; -1, y] 0;
				le [1, y] 2;
				le [-1, y] 0
			], p [
				eq [2, x; -1, y] 0;
				le [-1, y] 0
			]*)
		] in
		Test.suite "widen" (List.map chk tcs)

	(* Pol.itvize *)
	let itvizeTcs: (string * Vec.t * Pol.itvT * Cs.t Pol.t) list
	= [
		"nil0", Vec.mk [Scalar.Rat.u, x], {Pol.low = Pol.Infty; Pol.up = Pol.Infty}, p [];
		"direct0", Vec.mk [Scalar.Rat.u, x], {Pol.low = Pol.Infty; Pol.up = Pol.Closed (Scalar.Rat.of_int 2)}, p [
			le [1, x] 2 ];

		"direct1", Vec.mk [Scalar.Rat.u, x], {Pol.low = Pol.Infty; Pol.up = Pol.Open (Scalar.Rat.of_int 2)}, p [
			lt [1, x] 2 ];

		"direct2", Vec.mk [Scalar.Rat.u, x], {Pol.low = Pol.Closed (Scalar.Rat.of_int (-2)); Pol.up = Pol.Infty}, p [
			le [-1, x] 2 ];

		"direct3", Vec.mk [Scalar.Rat.u, x], {Pol.low = Pol.Open (Scalar.Rat.of_int (-2)); Pol.up = Pol.Infty}, p [
			lt [-1, x] 2 ];

		"direct4", Vec.mk [Scalar.Rat.u, x], {Pol.low = Pol.Closed (Scalar.Rat.of_int 2); Pol.up = Pol.Closed (Scalar.Rat.of_int 2)}, p [
			eq [1, x] 2 ];

		"comb0", Vec.mk [Scalar.Rat.u, x], {Pol.low = Pol.Infty; Pol.up = Pol.Closed (Scalar.Rat.of_int 2)}, p [
			le [1, y] 2;
			le [2, x; -1, y] 2 ];

		"comb1", Vec.mk [Scalar.Rat.u, x], {Pol.low = Pol.Infty; Pol.up = Pol.Open (Scalar.Rat.of_int 2)}, p [
			lt [1, y] 2;
			le [2, x; -1, y] 2 ];

		"comb2", Vec.mk [Scalar.Rat.u, x], {Pol.low = Pol.Infty; Pol.up = Pol.Open (Scalar.Rat.of_int 2)}, p [
			le [1, y] 2;
			lt [2, x; -1, y] 2 ];

		"eq0", Vec.mk [Scalar.Rat.u, x], {Pol.low = Pol.Infty; Pol.up = Pol.Closed Scalar.Rat.z}, p [
			eq [1, y] 2;
			le [1, x; 1, y] 2 ]
	]

	let chk_upper_bound : Var.t -> Pol.bndT -> Cs.t option -> bool
		= fun x bnd cstr_opt ->
		match bnd, cstr_opt with
		| Pol.Infty, None -> true
		| Pol.Closed r, Some cstr ->
			let cstr' = Cs.le [Scalar.Rat.u,x] r in
			Cs.equal cstr cstr'
		| Pol.Open r, Some cstr ->
			let cstr' = Cs.lt [Scalar.Rat.u,x] r in
			Cs.equal cstr cstr'
		| _,_ -> false

	let chk_lower_bound : Var.t -> Pol.bndT -> Cs.t option -> bool
		= fun x bnd cstr_opt ->
		match bnd, cstr_opt with
		| Pol.Infty, None -> true
		| Pol.Closed r, Some cstr ->
			let cstr' = Cs.le [Scalar.Rat.negU,x] (Scalar.Rat.neg r) in
			Cs.equal cstr cstr'
		| Pol.Open r, Some cstr ->
			let cstr' = Cs.lt [Scalar.Rat.negU,x] (Scalar.Rat.neg r) in
			Cs.equal cstr cstr'
		| _,_ -> false

	(* XXX: certificates should be syntactically checked *)
	let itvizeTs: Test.t
	= fun () ->
		let chk (name, v, itv, p) = fun state ->
			let (itv1, lower_cert, upper_cert) = Pol.itvize factory p v in
			let var = Cs.Vec.getVars [v] |> Var.Set.elements |> List.hd in
			if eqItv itv itv1
			 &&
			 	chk_upper_bound var (Pol.get_up itv) upper_cert
			 &&
			 	chk_lower_bound var (Pol.get_low itv) lower_cert
			then Test.succeed state
			else Test.fail name (Pol.to_string_itv varPr v itv1) state
		in
		Test.suite "itvize" (List.map chk itvizeTcs)

	let getUpperBoundTs : Test.t
	  = fun () ->
      let chk : string * Vec.t * Pol.itvT * Cs.t Pol.t -> Test.stateT -> Test.stateT
		   = fun (nm, v, i, p) st ->
		   let b = Pol.getUpperBound factory p v |> Stdlib.fst in
		   if eqBnd (Pol.get_up i) b
		   then Test.succeed st
		   else Test.fail nm "not equal" st
		 in
		 List.map chk itvizeTcs
		 |> Test.suite "getUpperBound"

	let getLowerBoundTs : Test.t
	  = fun () ->
      let chk : string * Vec.t * Pol.itvT * Cs.t Pol.t -> Test.stateT -> Test.stateT
		   = fun (nm, v, i, p) st ->
		   let b = Pol.getLowerBound factory p v |> Stdlib.fst in
		   if eqBnd (Pol.get_low i) b
		   then Test.succeed st
		   else Test.fail nm "not equal" st
		 in
		 List.map chk itvizeTcs
		 |> Test.suite "getLowerBound"

	(* Pol.rename *)
	let renameTs : Test.t
	   = fun () ->
       let chk : string * Var.t * Var.t * Cs.t Pol.t * Cs.t Pol.t option -> Test.stateT -> Test.stateT
		= fun (nm, x, y, p, op) st ->
			match op with
			| Some p' ->
				let p_r = Pol.rename factory x y p in
				if Pol.equal factory factory p_r p' && check_certificates p_r
				then Test.succeed st
				else
					let e : string = "failed" in
					Test.fail nm e st
			| None -> try
				let _ = Pol.rename factory x y p in
				let e : string = "failed" in
				Test.fail nm e st
				with Assert_failure (_, _, _) -> Test.succeed st
		in
		let tcs : (string * Var.t * Var.t * Cs.t Pol.t * Cs.t Pol.t option) list
		= [
			"nil0", x, y, p [], Some (p []);

			"ok0", x, y, p [le [1, x] 0], Some (p [le [1, y] 0]);

			"ok1", x, y, p [eq [1, x] 0], Some (p [eq [1, y] 0]);

			"ko0", x, y, p [le [1, y] 0], None;

			"ko1", x, y, p [eq [1, y] 0], None
		] in
		Test.suite "rename" (List.map chk tcs)

	let ts : Test.t =
        fun () ->
        List.map Test.run [
    		addTs;
    		meetTs;
    		inclTs;
    		projectTs;
    		projectMTs;
    		(*joinSetupTs;*)
    		joinTs;
    		widenTs;
    		itvizeTs;
    		getUpperBoundTs;
    		getLowerBoundTs;
    		renameTs
    	]
        |> Test.suite (Printf.sprintf "%s:%s"
        (Flags.proj_to_string())
        (Flags.join_to_string()))
end

module Classic = Make_Tests
	(struct
		let set : unit -> unit
			= fun () ->
			Flags.proj := Flags.FM;
			Flags.join := Flags.Baryc;
	end)

let ts1
	= Test.suite "Pol" [ Classic.ts() ] Test.stateZ

module PLP_Rat = Make_Tests
	(struct
		let set : unit -> unit
			= fun () ->
			Flags.proj := Flags.Proj_PLP Flags.Rat;
			Flags.join := Flags.Join_PLP Flags.Rat;
	end)

let ts2
	= Test.suite "Pol" [ PLP_Rat.ts() ] ts1

let ts = Test.prState "Pol" ts2
