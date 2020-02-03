module Cs = Cstr.Rat

type 'c t = (Var.t * 'c Cons.t) list

let to_string: (Var.t -> string) -> 'c t -> string
	= fun varPr e ->
		List.fold_right
		(fun (_, c) s -> s ^ (Cons.to_string varPr c) ^ "\n") e ""

let to_string_ext: 'c Factory.t -> (Var.t -> string) -> 'c t -> string
	= fun factory varPr e->
		List.fold_right
		(fun (x, c) s -> Printf.sprintf "%s(%s, %s)\n"
			s (varPr x) (Cons.to_string_ext factory varPr c)) e ""

type 'c rel_t =
	| NoIncl
	| Incl of 'c list

let nil : 'c t = []

let isTop (s : 'c t) = (s = [])

let list x = List.map Stdlib.snd x

let equal (s1 : 'c1 t) (s2 : 'c2 t) =
	if List.length s1 = List.length s2 then
		List.for_all2
			(fun (x1 ,(c1,_)) (x2, (c2,_)) ->
				x1 = x2 && Cs.inclSyn c1 c2)
			s1 s2
	else
		false

(* L'ordre fold_right est important pour les réécritures *)
let filter : 'c Factory.t -> 'c t -> 'c Cons.t -> 'c Cons.t
	= fun factory s c ->
	let filter1 (x, (c2,cert2)) (c1,cert1) =
		if Cs.Vec.Coeff.cmpz (Cs.Vec.get (Cs.get_v c1) x) = 0 then
			(c1,cert1)
		else
			Cons.elimc factory x (c1,cert1) (c2,cert2)
	in
	List.fold_right filter1 s c

let filter2 : 'c Factory.t -> 'c t -> Cs.t -> Cs.t * 'c Cons.t
	= fun factory s c ->
	let filter1 (c_res,(cons_c, cons_cert)) (x, (c,cert)) =
		let (c_res',(cstr_res',cert_res')) = Cons.elim factory x (c,cert) c_res in
		(c_res', (Cs.add cstr_res' cons_c, factory.Factory.add cert_res' cons_cert))
	in
	List.fold_left filter1 (c, Cons.triv factory) s

let leq : 'c1 Factory.t -> 'c1 t -> 'c2 t -> 'c1 rel_t
	= fun factory s1 s2 ->
	if List.length s1 < List.length s2 then
		NoIncl
	else
		let rec _incl certs =
		function
		| [] -> Incl certs
		| (_, (c,_))::t ->
			let (c2,(_,cert2)) = filter2 factory s1 c in
			match Cs.tellProp c2 with
			| Cs.Contrad | Cs.Nothing -> NoIncl
			| Cs.Trivial ->
				_incl (cert2::certs) t
		in
		_incl [] s2


let choose : Cs.t -> Var.t * Cs.Vec.Coeff.t
	= fun c ->
	match Rtree.findPred (fun n -> Cs.Vec.Coeff.cmpz n <> 0) (Cs.get_v c) with
	| None -> failwith "EqSet.choose"
	| Some (x, a) -> (x, a)

let rename factory s fromX toY =
	let rename1 (x, c) =
		((if x = fromX then toY else x), Cons.rename factory fromX toY c)
	in
	List.map rename1 s

let pick (msk : Var.t option Rtree.t) ((e,_) : 'c Cons.t) =
	match Rtree.findPred2
		(fun n1 n2 -> n1 <> None && not (Cs.Vec.Coeff.cmpz n2 = 0))
		msk (Cs.get_v e)
	with
	| Some (_,Some n1,_) -> Some n1
	| _ -> None

let rec subst : 'c Factory.t -> Var.t -> 'c Cons.t -> 'c t -> 'c t
	= fun factory x e ->
	function
	| [] -> []
	| (x1, (e1,cert1))::l1 ->
		if Var.equal x1 x then
			failwith "EqSet.subst"
		else
			let e2 =
				if Cs.Vec.Coeff.cmpz (Cs.Vec.get (Cs.get_v e1) x) = 0 then
					(e1,cert1)
				else
					Cons.elimc factory x e (e1,cert1)
			in
			(x1,e2) :: subst factory x e l1

let rec tryDefs : 'c Factory.t -> Var.t option Rtree.t -> 'c t -> ('c Cons.t * Var.t) option * 'c t
	= fun factory msk ->
	function
	| [] -> (None, [])
	| (x, e)::l ->
		if Rtree.get None msk x = None
		then
			let (def, l1) = tryDefs factory msk l in
			(def, (x, e)::l1)
		else
			let l1 = subst factory x e l in
			(Some (e, x), l1)

let trySubstM : 'c Factory.t -> Var.t option Rtree.t -> 'c t -> ('c Cons.t * Var.t) option * 'c t
	= fun factory msk l ->
	let (def, l1) = tryDefs factory msk l in
	if def = None then
		let rec _trysubstm msk =
			function
			| [] -> (None, [])
			| (x, e)::l ->
				match pick msk e with
				| None ->
					let (def, l1) = _trysubstm msk l in
					(def, (x, e)::l1)
				| Some x ->
					let l1 = subst factory x e l in
					(Some (e, x), l1)
		in
		_trysubstm msk l
	else
		(def, l1)

let trySubst : 'c Factory.t -> Var.t -> 'c t -> 'c Cons.t option * 'c t
	= fun factory x l ->
	let msk = Rtree.set None Rtree.empty x (Some x) in
	let (optx, s1) = trySubstM factory msk l in
	match optx with
	| None -> (None, s1)
	| Some (e, _) -> (Some e, s1)

type 'c meetT =
| Added of 'c t
| Bot of 'c

(* XXX: doit on comparer les certificats? *)
let meetEq: 'c meetT -> 'c meetT -> bool
	= fun ar ar' ->
	match ar, ar' with
	| Added e, Added e' -> equal e e'
	| Bot _, Bot _ -> true
	| Added _, Bot _
	| Bot _, Added _ -> false

let meet_to_string : 'c Factory.t -> (Var.t -> string) -> 'c meetT -> string
= fun factory varPr -> function
	| Added e ->
		Printf.sprintf "Added %s" (to_string_ext factory varPr e)
	| Bot f -> Printf.sprintf "Bot : %s" (factory.Factory.to_string f)

let addM: 'c Factory.t -> 'c t -> 'c Cons.t list -> 'c meetT
	= fun factory s conss ->
	let add : 'c Cons.t -> 'c meetT -> 'c meetT
		= fun (c,cert) ->
		function
		| Bot _ as r -> r
		| Added s ->
			match Cs.get_typ c with
			| Cstr_type.Le | Cstr_type.Lt -> failwith "EqSet.addM"
			| Cstr_type.Eq ->
				let (c1,cert1) = filter factory s (c,cert) in
			 	match Cs.tellProp c1 with
				| Cs.Trivial -> Added s
				| Cs.Contrad -> Bot cert1
				| Cs.Nothing ->
					let (x, ax) = choose c1 in
					let c2 = Cs.mulc (Cs.Vec.Coeff.inv ax) c1
					and cert2 = factory.Factory.mul (Cs.Vec.Coeff.inv ax) cert1 in
					(* rewritting of the rest of the equality set with the new one. *)
					let s' = if !Flags.row_echelon_equalities
						then subst factory x (c2,cert2) s
						else s
					in
					Added ((x, (c2,cert2)) :: s')
	in
	List.fold_left (fun res c -> add c res) (Added s) conss


let add: 'c Factory.t -> 'c t -> 'c Cons.t -> 'c meetT
	= fun factory s c ->
	addM factory s [c]

let joinSetup_1: 'c2 Factory.t -> Var.t -> Var.t option Rtree.t -> Var.t -> 'c1 t
	-> Var.t * Var.t option Rtree.t * (Var.t * (('c1,'c2) Cons.discr_t) Cons.t) list
	= fun factory2 nxt relocTbl alpha s ->
	let apply (x, c) (nxt1, relocTbl1, s1) =
        let (nxt2, relocTbl2, c1) = Cons.joinSetup_1 factory2 nxt1 relocTbl1 alpha c in
        let x1 = match Rtree.get None relocTbl2 x with
            | None -> failwith "EqSet.joinSetup_1"
            | Some x1 -> x1
        in
        (nxt2, relocTbl2, (x1, c1)::s1)
	in
    (* List.fold_right is necessary because order needs to be preserved (echelon form) *)
	List.fold_right apply s (nxt, relocTbl, nil)

let joinSetup_2: 'c1 Factory.t -> Var.t -> Var.t option Rtree.t -> Var.t -> 'c2 t
	-> Var.t * Var.t option Rtree.t * (Var.t * (('c1,'c2) Cons.discr_t) Cons.t) list
	= fun factory1 nxt relocTbl alpha s ->
	let apply (x, c) (nxt1, relocTbl1, s1) =
		let (nxt2, relocTbl2, c1) = Cons.joinSetup_2 factory1 nxt1 relocTbl1 alpha c in
		let x1 = x in
		(nxt2, relocTbl2, (x1, c1)::s1)
	in
    (* List.fold_right is necessary because order needs to be preserved (echelon form) *)
	List.fold_right apply s (nxt, relocTbl, nil)

let minkowskiSetup_1: 'c2 Factory.t -> Var.t -> Var.t option Rtree.t -> 'c1 t
	-> Var.t * Var.t option Rtree.t * (Var.t * (('c1,'c2) Cons.discr_t) Cons.t) list
	= fun factory2 nxt relocTbl s ->
	let apply (x, c) (nxt1, relocTbl1, s1) =
		let (nxt2, relocTbl2, c1) = Cons.minkowskiSetup_1 factory2 nxt1 relocTbl1 c in
		let x1 =	match Rtree.get None relocTbl2 x with
				| None -> failwith "EqSet.minkowskiSetup_1"
				| Some x1 -> x1
		in
		(nxt2, relocTbl2, (x1, c1)::s1)
	in
(* List.fold_right is necessary because order needs to be preserved (echelon form) *)
	List.fold_right apply s (nxt, relocTbl, nil)

let minkowskiSetup_2: 'c1 Factory.t -> Var.t -> Var.t option Rtree.t -> 'c2 t
	-> Var.t * Var.t option Rtree.t * (Var.t * (('c1,'c2) Cons.discr_t) Cons.t) list
	= fun factory1 nxt relocTbl s ->
	let apply (x, c) (nxt1, relocTbl1, s1) =
		let (nxt2, relocTbl2, c1) = Cons.minkowskiSetup_2 factory1 nxt1 relocTbl1 c in
		let x1 = x in
		(nxt2, relocTbl2, (x1, c1)::s1)
	in
(* List.fold_right is necessary because order needs to be preserved (echelon form) *)
	List.fold_right apply s (nxt, relocTbl, nil)

let satisfy : 'c t -> Cs.Vec.t -> bool
    = fun eqs point ->
    List.for_all
        (fun (_,cons) ->
            Cons.get_c cons
            |> Cs.satisfy point)
        eqs
