module Cs = Cstr.Rat
module Vec = Cs.Vec

module Debug = Opt.Debug

type 'c t = {
    ineqs : 'c Cons.t list;
    regions : 'c PLP.Region.t list option;
}

let is_top s = (s.ineqs = [])

let top : 'c t = {
    ineqs = [];
    regions = None;
}

let of_list : 'c Cons.t list -> 'c t
    = fun cstrs -> { top with
        ineqs = cstrs;
    }

let to_string: (Var.t -> string) -> 'c t -> string
    = fun varPr s ->
    List.map (Cons.to_string varPr) s.ineqs
    |> String.concat "\n"

let to_string_ext: 'c Factory.t -> (Var.t -> string) -> 'c t -> string
    = fun factory varPr s ->
    List.map (Cons.to_string_ext factory varPr) s.ineqs
    |> String.concat "\n"

let to_string_raw : 'c t -> string
    = fun s ->
    List.map (Cons.to_string Var.to_string) s.ineqs
    |> String.concat "\n"

let copy : 'c t -> 'c t
    = fun s -> {
        ineqs = s.ineqs;
        regions = match s.regions with
            | None -> None
            | Some regs -> Some (List.map PLP.Region.copy regs);
    }

let map : ('c1 -> 'c2) -> 'c1 t -> 'c2 t
    = fun f s ->
    let ineqs' = List.map (fun (cstr,cert) -> (cstr, f cert)) s.ineqs
    and regions' = match s.regions with
        | None -> None
        | Some regions -> let regions' = List.map (fun reg ->
            let sx' = {reg.PLP.Region.sx with
                cstrs = List.map (fun (cstr,cert) -> (cstr, f cert)) reg.sx.cstrs ;
            } in {reg with
                sx = sx';
            }
        ) regions
        in Some regions'
    in {
        ineqs = ineqs';
        regions = regions';
    }

type 'c prop_t =
| Empty of 'c
| Trivial
| Implied of 'c
| Check of 'c Cons.t

let synIncl : 'c1 Factory.t -> 'c1 EqSet.t -> 'c1 t -> Cs.t -> 'c1 prop_t
	= fun factory es s c ->
	match Cs.tellProp c with
	| Cs.Trivial -> Trivial
	| Cs.Contrad -> Empty (factory.Factory.top)
	| Cs.Nothing -> begin
		let (cstr1, cons1) = EqSet.filter2 factory es c in
		let cert1 = try Cons.adjust_cert_constant factory cons1 c
			with Invalid_argument _ -> Cons.get_cert cons1
		in
		match Cs.tellProp cstr1 with
		| Cs.Trivial -> Implied cert1
		| Cs.Contrad -> Empty cert1
		| Cs.Nothing ->
		try
			let consI = List.find (fun (cstr2,_) -> Cs.incl cstr2 cstr1) s.ineqs in
			Implied (factory.Factory.add cert1 (Cons.adjust_cert_constant factory consI cstr1))
		with Not_found -> Check (cstr1,cert1)
		end

(* the coefficient whose id is -1 is the constraint to compute*)
let mkCert : 'c Factory.t -> 'c Cons.t list -> Cs.t -> (int * Scalar.Rat.t) list -> 'c
	= fun factory conss cstr combination ->
	try
		let a0 = List.assoc (-1) combination in
		let combination' = List.fold_left
			(fun res (i,a) ->
				if i = -1
				then res
				else (i,Scalar.Rat.div a a0) :: res)
			[] combination
		in
		let cons = Cons.linear_combination_cons factory conss combination'
		in
		Cons.adjust_cert_constant factory cons cstr
	with
		Not_found -> Stdlib.failwith "IneqSet.mkCert"

type 'c rel_t =
| NoIncl
| Incl of 'c list

let incl: 'c1 Factory.t -> Var.t -> 'c1 EqSet.t -> 'c1 t ->  'c2 t -> 'c1 rel_t
	= fun factory nxt es s1 s2 ->
	let rec _isIncl
		= fun certs optSx ->
		function
		| [] -> Incl certs
		| c::t ->
			match synIncl factory es s1 (Cons.get_c c) with
			| Empty _ -> NoIncl
			| Trivial -> failwith "IneqSet.incl"
			| Implied c -> _isIncl (c::certs) optSx t
			| Check c1 ->
				let sx =
					match optSx with
					| Some sx -> sx
					| None -> Splx.mk nxt (List.mapi (fun i (c,_) -> i,c) s1.ineqs)
				in
				let c1' = Cs.compl (Cons.get_c c1) in
				match Splx.checkFromAdd (Splx.addAcc sx (-1, c1')) with
				| Splx.IsOk _ -> NoIncl
				| Splx.IsUnsat w ->
					let cert = mkCert factory s1.ineqs (Cons.get_c c1) w
						|> factory.Factory.add (Cons.get_cert c1)
					in
					_isIncl (cert::certs) (Some sx) t
	in
	_isIncl [] None s2.ineqs

(*
type 'c satChkT = Sat of Splx.t | Unsat of 'c

(** [chkFeasibility nvar s] checks whether [s] is satisfiable and returns a
simplex object with a satisfied state if it is. If it is not satisfiable, then
a linear combination of the input constraints is returned. [nvar] is used for
fresh variable generation. *)
let chkFeasibility: 'c Factory.t -> Var.t -> 'c t -> 'c satChkT
= fun factory nvar s ->
	let cs = List.mapi (fun i c -> (i, Cons.get_c c)) s.ineqs in
	match Splx.checkFromAdd (Splx.mk nvar cs) with
	| Splx.IsOk sx -> Sat sx
	| Splx.IsUnsat w -> Unsat (Cons.linear_combination_cert factory s.ineqs w)
*)

let rename factory s fromX toY =
    let ineqs' = List.map (Cons.rename factory fromX toY) s.ineqs in {
        ineqs = ineqs';
        regions = None;
    }

let pick : Var.t option Rtree.t -> 'c t -> Var.t option
	= fun msk s ->
	let update v n p m =
		match Scalar.Rat.cmpz n with
		| 0 -> (v, p, m)
		| n when n < 0 -> (v, p + 1, m)
		| _ -> (v, p, m + 1)
	in
	let rec build m cnt ve =
		match m, cnt, ve with
		| Rtree.Nil, _, _ -> Rtree.Nil
		| _, rt, Rtree.Nil -> rt
		| Rtree.Sub (l1, n1, r1), Rtree.Nil, Rtree.Sub (l2, n2, r2) ->
			let l = build l1 Rtree.Nil l2 in
			let r = build r1 Rtree.Nil r2 in
			let n =
				if n1 = None then
					(None, 0, 0)
				else
					update n1 n2 0 0
			in
			Rtree.Sub (l, n, r)

		| Rtree.Sub (l1,n1,r1), Rtree.Sub (l2,n2,r2), Rtree.Sub (l3,n3,r3) ->
			let l = build l1 l2 l3 in
			let r = build r1 r2 r3 in
			let n =
				if n1 = None then
					n2
				else
					let (_, p, m) = n2 in
					update n1 n3 p m
			in
			Rtree.Sub (l, n, r)
	in
	let scores = List.fold_left (fun a c -> build msk a (Cs.get_v (Cons.get_c c)))
		Rtree.Nil s.ineqs
	in
	let (optv, _) =
		let choose (best, sc) (v, p, m) =
			if v = None || p = 0 && m = 0 then
				(best, sc)
			else
				let nsc = p * m - (p + m) in
				if best = None then
					(v, nsc)
				else
					if nsc < sc then
						(v, nsc)
					else
						(best, sc)
		in
		Rtree.fold (fun _ -> choose) (None, -1) scores
	in
	optv

let trim : 'c Cons.t list -> Splx.t -> 'c Cons.t list * Splx.t
	= fun s0 sx0 ->
	let check i c (s, sx) =
		let sx1 = Splx.compl sx i in
		match Splx.check sx1 with
		| Splx.IsOk _ -> c::s, sx
		| Splx.IsUnsat _ -> s, Splx.forget sx i
	in
	List.fold_right2 check (Misc.range 0 (List.length s0)) s0 ([], sx0)

let trimSet : Var.t -> 'c Cons.t list -> 'c Cons.t list
	= fun nxt s ->
	let cl = List.mapi (fun i c -> i, Cons.get_c c) s in
	match Splx.checkFromAdd (Splx.mk nxt cl) with
	| Splx.IsUnsat _ -> Stdlib.failwith "IneqSet.trimSet"
	| Splx.IsOk sx -> Stdlib.fst (trim s sx)

(* XXX: À revoir?
Cette fonction n'est utilisée que dans la projection?
A priori, si synIncl renvoie Check c, c n'aura pas été réécrit car il vient d'une contrainte déjà présente dans le polyèdre.*)
let synAdd : 'c Factory.t -> 'c EqSet.t -> 'c t -> 'c Cons.t -> 'c t
	= fun factory es s cons ->
	match synIncl factory es s (Cons.get_c cons) with
	| Trivial | Implied _ -> s
	| Empty _ -> failwith "IneqSet.synAdd"
	| Check _ -> {
        regions = None;
		ineqs = cons::(List.filter (fun c2 -> not (Cons.implies cons c2)) s.ineqs);
    }

let subst: 'c Factory.t -> Var.t -> 'c EqSet.t -> Var.t -> 'c Cons.t -> 'c t -> 'c t
	= fun factory nxt es x e s ->
	let gen s c =
		let c1 =
			if Scalar.Rat.cmpz (Vec.get (Cs.get_v (Cons.get_c c)) x) = 0
			then c
			else Cons.elimc factory x e c
		in
		synAdd factory es s c1
	in
	let s' = List.fold_left gen top s.ineqs in {
        ineqs = trimSet nxt s'.ineqs;
        regions = None;
    }

let fmElim_one: 'c Factory.t -> Var.t -> 'c EqSet.t -> Var.t ->  'c t -> 'c t
	= fun factory nxt es x s ->
	let (pos, z, neg) = List.fold_left (fun (p, z, n) c ->
		match Scalar.Rat.cmpz (Vec.get (Cs.get_v (Cons.get_c c)) x) with
		| 0 -> (p, c::z, n)
		| a when a < 0 -> (c::p, z, n)
		| _ -> (p, z, c::n)) ([], [], []) s.ineqs
	in
	let zs = List.rev z in
	let add : 'c Factory.t -> 'c t -> 'c Cons.t -> 'c t
	  = fun factory s c ->
	  synAdd factory es s c
	in
	let apply factory s0 c = List.fold_left
		(fun s c1 -> add factory s (Cons.elimc factory x c c1)) s0 neg
	in
    List.fold_left (apply factory) {ineqs = zs; regions = None} pos
	|> fun s -> trimSet nxt s.ineqs
    |> fun ineqs -> {ineqs = ineqs; regions = None}

let plpElim : 'c Factory.t -> Cs.Vec.t -> Var.t list -> 'c t -> 'c t
	= fun factory normalization_point xs s ->
    let res = Proj.proj factory normalization_point xs s.ineqs in
    let (regs,ineqs) = List.split res in
    let ineqs' = List.filter (fun cons ->
        Cs.tellProp (Cons.get_c cons) <> Cs.Trivial
    ) ineqs
    |> Misc.rem_dupl Cons.equal
    in {
        ineqs = ineqs';
        regions = Some regs;
    }

(* TODO: use two factories *)
let proj_incl : 'c Factory.t -> Cs.Vec.t -> Var.t list -> 'c EqSet.t -> 'c t -> 'c t -> 'c t option
    = fun factory normalization_point xs p2_eqs p1_ineqs p2_ineqs ->
    let p1_ineqs' = List.map (EqSet.filter factory p2_eqs) p1_ineqs.ineqs in
    match ProjIncl.proj_incl factory factory normalization_point xs p1_ineqs' p2_ineqs.ineqs with
    | None -> None
    | Some res ->
        let (regs,ineqs) = List.split res in
        let ineqs' = List.filter (fun cons ->
            Cs.tellProp (Cons.get_c cons) <> Cs.Trivial
        ) ineqs
        |> Misc.rem_dupl Cons.equal
        in
        Some {
            ineqs = ineqs';
            regions = Some regs;
        }

let fmElim: 'c Factory.t -> Var.t -> 'c EqSet.t -> Var.t option Rtree.t -> 'c t -> 'c t
    = fun factory nxt es msk s ->
	let rec elim s1 =
		match pick msk s1 with
		| None -> s1
		| Some x ->
			let s2 = fmElim_one factory nxt es x s1 in
			elim s2
	in
	elim s

let joinSetup_1: 'c2 Factory.t -> Var.t -> Var.t option Rtree.t -> Var.t -> 'c1 t
	-> Var.t * Var.t option Rtree.t * (('c1,'c2) Cons.discr_t) Cons.t list
	= fun factory2 nxt relocTbl alpha s ->
	let apply (nxt1, relocTbl1, s1) c =
		let (nxt2, relocTbl2, c1) = Cons.joinSetup_1 factory2 nxt1 relocTbl1 alpha c in
		(nxt2, relocTbl2, c1::s1)
	in
	List.fold_left apply (nxt, relocTbl, []) s.ineqs

let joinSetup_2: 'c1 Factory.t -> Var.t -> Var.t option Rtree.t -> Var.t -> 'c2 t
	-> Var.t * Var.t option Rtree.t * (('c1,'c2) Cons.discr_t) Cons.t list
	= fun factory1 nxt relocTbl alpha s ->
	let apply (nxt1, relocTbl1, s1) c =
		let (nxt2, relocTbl2, c1) = Cons.joinSetup_2 factory1 nxt1 relocTbl1 alpha c in
		(nxt2, relocTbl2, c1::s1)
	in
	List.fold_left apply (nxt, relocTbl, []) s.ineqs

let minkowskiSetup_1: 'c2 Factory.t -> Var.t -> Var.t option Rtree.t -> 'c1 t
	-> Var.t * Var.t option Rtree.t * (('c1,'c2) Cons.discr_t) Cons.t list
	= fun factory2 nxt relocTbl s ->
	let apply (nxt1, relocTbl1, s1) c =
		let (nxt2, relocTbl2, c1) = Cons.minkowskiSetup_1 factory2 nxt1 relocTbl1 c in
		(nxt2, relocTbl2, c1::s1)
	in
	List.fold_left apply (nxt, relocTbl, []) s.ineqs

let minkowskiSetup_2: 'c1 Factory.t -> Var.t -> Var.t option Rtree.t -> 'c2 t
	-> Var.t * Var.t option Rtree.t * (('c1,'c2) Cons.discr_t) Cons.t list
	= fun factory1 nxt relocTbl s ->
	let apply (nxt1, relocTbl1, s1) c =
		let (nxt2, relocTbl2, c1) = Cons.minkowskiSetup_2 factory1 nxt1 relocTbl1 c in
		(nxt2, relocTbl2, c1::s1)
	in
	List.fold_left apply (nxt, relocTbl, []) s.ineqs


(** [isRed sx conss i] checks whether the constraint identified by [i] is redundant in
the constraint set represented by [sx]. *)
let isRed: Splx.t -> int -> bool
	= fun sx i ->
	let sx' = Splx.compl sx i in
	match Splx.check sx' with
	| Splx.IsOk _ -> false
	| Splx.IsUnsat _ -> true

let min_classic : Splx.t -> (int * 'c Cons.t) list -> 'c t
	= fun sx conss ->
	let classic (sx, s) (i,cons) =
		if isRed sx i
		then (Splx.forget sx i, s)
		else (sx, cons::s)
	in
	let ineqs' = List.fold_left classic (sx, []) conss
	|> Stdlib.snd in {
        ineqs = ineqs';
        regions = None;
    }

(** [rmRed s sx] removes all the redundancy from [s]. The simplex object [sx] is
assumed to be in a statisfied state (it has been returned by [Splx.check]) and
to represent [s]. The returned simplex object is [sx] with all the redundant
bounds [Splx.forgot]ten. The returned certificate list contains a certificate
for all the removed constraints. *)
let rmRedAux : 'c t -> Splx.t -> Scalar.Symbolic.t Rtree.t -> 'c t
    = fun s sx point ->
    if List.length s.ineqs <= 1
    then s
    else
        let conss = List.mapi (fun i c -> (i,c)) s.ineqs in
        min_classic sx conss

(** [isIncl s c] checks whether [c] is implied by [s] on syntactic grounds.*)
let inclSyn: 'c t -> 'c Cons.t -> bool
	= fun s c ->
	match List.filter (fun c' -> Cons.implies c' c) s.ineqs with
	| [] -> false
	| [_] -> true
	| _ -> failwith "IneqSet.addM"

(** [rmSynRed2 s l] removes syntactically redundant constraints from the union
of [s] and [l].*)
let rmRedSyn: 'c t -> 'c Cons.t list -> 'c t
	= let rm1 s c =
		if inclSyn s c
		then s
		else let (_, s2) = List.partition (fun c' -> Cons.implies c c') s.ineqs in
			{ineqs = c::s2; regions = None;}
	in
	fun s l ->
    List.fold_left rm1 top (s.ineqs @ l)

(* precondition :
	- s @ l has non empty interior
	- l contains no trivial constraints
*)
let assume: Var.t -> 'c t -> 'c Cons.t list -> Scalar.Symbolic.t Rtree.t -> 'c t
	= fun nvar s conss point ->
	let s2 = rmRedSyn s conss in
	if List.length s2.ineqs <= 1
	then s2
	else
		let ilist = List.mapi (fun i c -> (i, Cons.get_c c)) s2.ineqs in
		match Splx.checkFromAdd (Splx.mk nvar ilist) with
		| Splx.IsUnsat _ -> Stdlib.failwith "IneqSet.addM: unexpected unsat set"
		| Splx.IsOk sx -> rmRedAux s2 sx point

let assume_back : 'c Factory.t -> 'c t -> 'c Cons.t -> Vector.Symbolic.t -> ('c t * Vector.Symbolic.t) option
    = fun factory s cons old_point ->
    let s' = copy s in
    match s'.regions with
    | Some regs -> begin try
            let ineqs = List.map (fun (ineq,_) -> ineq) s.ineqs in
            let (sols, new_point) = PLP.add_column factory PLP.std_config ineqs regs cons old_point in
            let (regs', ineqs') = List.split sols in Some ({
                ineqs = ineqs';
                regions = Some regs';
            }, new_point)
        with PSplxExec.Infeasible_problem -> None
        end
    | None -> failwith "no stored regions"

(*
(*TEST FOR EFFICIENCY : *)
let addM: Var.t -> 'c t -> 'c Cons.t list -> Scalar.Symbolic.t Rtree.t -> 'c t
	= fun nvar s conss point ->
	let apply : Flags.min_method -> 'c t
		= fun min ->
		let s' = s @ conss in
		if List.length s' <= 1
		then s'
		else
			match min with
			| Flags.Raytracing (Flags.Splx)->
				RmRedAux.splx s' point
			| Flags.Raytracing (Flags.Glpk)->
				RmRedAux.glpk s'
			| Flags.Classic -> begin
				let s2 = rmRedSyn s conss in
				let ilist = List.mapi (fun i c -> (i, Cons.get_c c)) s2 in
				match Splx.checkFromAdd (Splx.mk nvar ilist) with
				| Splx.IsUnsat _ -> Stdlib.failwith "IneqSet.addM: unexpected unsat set"
				| Splx.IsOk sx -> begin
					Stat.base_n_cstr := List.length s2;
					let conss = List.mapi (fun i c -> (i,c)) s2 in
					RmRedAux.classic sx conss
					end
				end
			| _ -> Stdlib.invalid_arg "IneqSet.rmRedAux"
	in
	match !Flags.min with
	| Flags.MHeuristic -> apply (Heuristic.min (List.map Cons.get_c (s @ conss)))
	| m -> apply m
*)


(** Returns the partition into regions of the given polyhedron, according to the given normalization point.
    Certificates are lost during the process: frontiers of regions have no certificate.
*)
let get_regions_from_point : 'c Factory.t -> 'c t -> Vec.t -> ('c Cons.t list * Vector.Symbolic.t) list
    = fun factory p point ->
    let regions = PoltoPLP.to_plp factory point p.ineqs in
    List.map (fun (reg,cons) ->
        let ineqs = List.map (fun cstr -> cstr, factory.top)
            ((Cons.get_c cons) :: PLP.Region.get_cstrs reg)
        and point = Vector.Symbolic.ofRat reg.PLP.Region.point
        in
        (ineqs, point)
    ) regions.PoltoPLP.mapping

let satisfy : 'c t -> Vec.t -> bool
    = fun s point ->
    List.for_all
        (fun cons -> Cons.get_c cons |> Cs.satisfy point)
        s.ineqs
