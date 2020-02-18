module Cs = Cstr.Rat
module Vec = Cs.Vec

module Debug = IneqSet.Debug
module Profile = Profile.Profile(struct let name = "Pol" end)

type 'c t = {
	eqs: 'c EqSet.t;
	ineqs: 'c IneqSet.t;
    point: Vector.Symbolic.t option;
}

let top: 'c t
= {
	eqs = EqSet.nil;
	ineqs = IneqSet.top;
    point = Some Vector.Symbolic.nil;
}

let get_eqs (x : 'c t) = x.eqs
let get_ineqs (x : 'c t) = x.ineqs.ineqs

let mkplist p =
	let e = EqSet.list p.eqs in
	let i = p.ineqs.ineqs in
	List.map (fun c -> (Cons.get_c c)) (List.append e i)

let varSet: 'c t -> Var.Set.t
	= fun p -> Cs.getVars (mkplist p)

let to_string: (Var.t -> string) -> 'c t -> string
	= fun varPr p ->
	if EqSet.isTop p.eqs && IneqSet.is_top p.ineqs
	then "top"
	else Printf.sprintf "%s%s\nPoint: %s"
        (EqSet.to_string varPr p.eqs)
        (IneqSet.to_string varPr p.ineqs)
        (match p.point with
            | None -> "None"
            | Some point -> Vector.Symbolic.to_string varPr point)

let to_string_raw: 'c t -> string
	= fun p -> to_string Var.to_string  p

let to_string_ext: 'c Factory.t -> (Var.t -> string) -> 'c t -> string
	= fun factory varPr p ->
	let estr =
		if EqSet.isTop p.eqs
		then " top"
		else "\n" ^ (EqSet.to_string_ext factory varPr p.eqs)
	in
	let istr =
		if IneqSet.is_top p.ineqs
		then " top"
		else "\n" ^ (IneqSet.to_string_ext factory varPr p.ineqs)
	in
    let pstr = match p.point with
        | None -> "None"
        | Some point -> Vector.Symbolic.to_string varPr point
    in
	Printf.sprintf "{\n\teqs = %s;\n\tineqs = %s;\n\tpoint = %s}" estr istr pstr

let to_string_ext_raw: 'c Factory.t -> 'c t -> string
	= fun factory p -> to_string_ext factory Var.to_string  p

let get_point : 'c t -> Vector.Symbolic.t
    = fun p ->
	Debug.log DebugTypes.Title (lazy (Printf.sprintf
		"Getting point of polyhedron %s"
		(to_string_raw p)));
    match p.point with
        | Some point ->
			Debug.exec point DebugTypes.Normal (lazy (Printf.sprintf
			"Point already computed: %s"
			(Vector.Symbolic.to_string Var.to_string point)))
        | None -> begin
            let ineqs = List.map Cons.get_c p.ineqs.ineqs in
            let horizon = Cs.getVars ineqs |> Var.horizon in
            match Splx.getPointInside_cone horizon ineqs with
            | Some point -> Debug.exec point DebugTypes.Normal (lazy (Printf.sprintf
			"Computed by Splx.getPointInside_code: %s"
			(Vector.Symbolic.to_string Var.to_string point)))
            | None -> match Opt.getAsg horizon (List.mapi (fun i cstr -> i, cstr) ineqs) with
                | None -> failwith "Pol.get_point: Unexpected empty polyhedron"
                | Some point -> Debug.exec point DebugTypes.Normal (lazy (Printf.sprintf
				"Computed by Opt.getAsg: %s"
				(Vector.Symbolic.to_string Var.to_string point)))
            end

type 'c rel_t =
| NoIncl
| Incl of 'c list

type bndT =
	| Infty
	| Open of Scalar.Rat.t
	| Closed of Scalar.Rat.t

type itvT = { low: bndT; up: bndT }

let bnd_to_string : bndT -> string
    = function
    | Infty -> "infty"
	| Open r -> Printf.sprintf "Open(%s)" (Scalar.Rat.to_string r)
	| Closed r ->  Printf.sprintf "Closed (%s)" (Scalar.Rat.to_string r)

let get_up (i: itvT) = i.up
let get_low (i: itvT) = i.low

let length_itv : itvT -> Scalar.Rat.t option
    = fun itv ->
    match get_low itv, get_up itv with
    | Infty, _ | _, Infty-> None
	| Open l, Open u | Open l, Closed u | Closed l, Open u | Closed l, Closed u -> Some (Scalar.Rat.sub u l)

type cstT = S of Scalar.Rat.t | I of itvT

(** The description of an assignment operation.
[var] is given the value [lin] + [cst]. *)
type assign_t = {
	var: Var.t;
	lin: (Scalar.Rat.t * Var.t) list;
	cst: cstT }

let getVar: assign_t -> Var.t
= fun a -> a.var

let getLin: assign_t -> (Scalar.Rat.t * Var.t) list
= fun a -> a.lin

let getCst: assign_t -> cstT
= fun a -> a.cst

let get_cons p : 'c Cons.t list =
	EqSet.list p.eqs @ p.ineqs.ineqs

let get_cstr p : Cs.t list =
  (List.map (fun (_,c) -> Cons.get_c c) (get_eqs p)) @
  (List.map Cons.get_c (get_ineqs p))

let get_cstr_ineqs p : Cs.t list =
  (List.map Cons.get_c (get_ineqs p))

let to_string_itv: (Var.t -> string) -> Vec.t -> itvT -> string
= fun varPr v itv ->
	let prLow = function
		| Infty -> "-infty < "
		| Open n -> (Scalar.Rat.to_string n) ^ " < "
		| Closed n -> (Scalar.Rat.to_string n) ^ " <= "
	in
	let prUp = function
		| Infty -> " < +infty"
		| Open n -> " < " ^ (Scalar.Rat.to_string n)
		| Closed n -> " <= " ^ (Scalar.Rat.to_string n)
	in
	(prLow itv.low) ^ (Vec.to_string varPr v) ^ (prUp itv.up)

let to_string_itv_raw: Vec.t -> itvT -> string
= fun v itv -> to_string_itv Var.to_string  v itv

let equalSyn p1 p2: bool =
	let incl l1 l2 = List.for_all
		(fun c2 -> List.exists
			(fun c1 -> Cs.inclSyn (Cons.get_c c1) (Cons.get_c c2)) l1) l2
	in
	let eq l1 l2 = incl l1 l2 && incl l2 l1 in
	EqSet.equal p1.eqs p2.eqs && eq p1.ineqs.ineqs p2.ineqs.ineqs

type 'c ependingT =
	| EMin of 'c EqSet.t
	| EAdd of 'c Cons.t list * 'c ependingT

type 'c ipendingT =
	| IMin of 'c IneqSet.t
	| IAdd of 'c Cons.t list * 'c ipendingT
	| IAddRw of 'c Cons.t list * 'c ipendingT
	| IRed of 'c IneqSet.t
	| IRw of 'c IneqSet.t

let rec needsRewrite: 'c ipendingT -> 'c ipendingT
= function
	| IMin iset -> IRw iset
	| IAdd (l, ip) -> IAddRw (l, needsRewrite ip)
	| IAddRw (l, ip) -> IAddRw (l, needsRewrite ip)
	| IRed iset -> IRw iset
	| IRw _ as ip -> ip

type 'c rewriteT =
	| Rewritten of 'c Cons.t list
	| RewriteBot of 'c

(** [rewriteIneqs es s] projects from [s] all variables defined in [es].
The produced certificates prove that the result [s'] is implied by [s].
No redundancy elimination is performed on [s']. *)
let rewriteIneqs: 'c Factory.t -> 'c EqSet.t -> 'c Cons.t list -> 'c rewriteT
=	let rewrite: 'c Factory.t -> 'c EqSet.t -> 'c rewriteT -> 'c Cons.t -> 'c rewriteT
	= fun factory es -> function
		| RewriteBot _ as r -> fun _ -> r
		| Rewritten s -> fun c ->
			let c' = EqSet.filter factory es c in
			match Cs.tellProp (Cons.get_c c') with
			| Cs.Trivial -> Rewritten s
			| Cs.Contrad -> RewriteBot (Cons.get_cert c')
			| Cs.Nothing ->
				Rewritten (c'::s)
	in
	fun factory es s ->
		List.fold_left (rewrite factory es) (Rewritten []) s

type 'c rewriteBisT =
	| Rewritten' of 'c Cons.t list
	| RewriteBot' of 'c

let listRw: 'c Factory.t -> 'c EqSet.t -> 'c Cons.t list -> 'c rewriteBisT
=	let rw1: 'c Factory.t -> 'c EqSet.t -> 'c rewriteBisT -> 'c Cons.t -> 'c rewriteBisT
	= fun factory eset -> function
		| RewriteBot' _ as rw -> fun _ -> rw
		| Rewritten' ilist -> fun cons ->
			let (cstr',cert') = EqSet.filter factory eset cons in
			match Cs.tellProp cstr' with
			| Cs.Contrad -> RewriteBot' cert'
			| Cs.Trivial ->
				Rewritten' ilist
			| Cs.Nothing ->
				Rewritten' ((cstr',cert')::ilist)
	in
	fun factory eset ilist ->
		let rwZ = Rewritten' [] in
		List.fold_left (rw1 factory eset) rwZ ilist

type 'c logT = {
	eset: 'c ependingT;
	iset: 'c ipendingT}

let rec epending_to_string : 'c ependingT -> string
	= function
	| EMin eset -> Printf.sprintf "EMin (%s)" (EqSet.to_string Var.to_string eset)
	| EAdd (l, epending) -> Printf.sprintf "EAdd (%s, %s)"
		(Misc.list_to_string (fun (_,c) -> Cs.to_string Var.to_string c) l " ; ")
		(epending_to_string epending)

let rec ipending_to_string : 'c ipendingT -> string
	= function
	| IMin iset -> Printf.sprintf "IMin (%s)" (IneqSet.to_string Var.to_string iset)
	| IAdd (l, ipending) -> Printf.sprintf "IAdd (%s, %s)"
		(Misc.list_to_string (fun (_,c) -> Cs.to_string Var.to_string c) l " ; ")
		(ipending_to_string ipending)
	| IAddRw (l, ipending) -> Printf.sprintf "IAddRw (%s, %s)"
		(Misc.list_to_string (fun (_,c) -> Cs.to_string Var.to_string c) l " ; ")
		(ipending_to_string ipending)
	| IRed iset -> Printf.sprintf "IRed (%s)" (IneqSet.to_string Var.to_string iset)
	| IRw iset -> Printf.sprintf "IRw (%s)" (IneqSet.to_string Var.to_string iset)

let log_to_string : 'c logT -> string
	= fun log ->
	Printf.sprintf "eset =\n%s\n---\niset =\n%s\n"
	(epending_to_string log.eset)
	(ipending_to_string log.iset)

let logEset: 'c logT -> 'c ependingT -> 'c logT
	= fun lp eset ->
	{  eset = eset;
		iset = needsRewrite lp.iset;
	}

let logIset: 'c logT -> 'c ipendingT -> 'c logT
	= fun lp iset ->
	{lp with iset = iset;}

type 'c meetT =
	| Added of 'c t
	| Contrad of 'c

let meetPr: 'c Factory.t -> (Var.t -> string) -> 'c meetT -> string
= fun factory varPr -> function
	| Contrad ce -> Printf.sprintf "Contrad ce, with ce:\n%s" (factory.Factory.to_string ce)
	| Added p -> Printf.sprintf
		"Added p with p:\n%s"
		(to_string_ext factory varPr p)

let meetEq: 'c meetT -> 'c meetT -> bool
= fun m1 m2 ->
	match m1, m2 with
	| Contrad ce1, Contrad ce2 -> ce1 = ce2
	| Added p1, Added p2 -> equalSyn p1 p2
	| Contrad _, Added _
	| Added _ , Contrad _ -> false

type ('a,'c) mayBotT
	= Ok of 'a | Bot of 'c

let logOut: ('c logT * Vector.Symbolic.t option, 'c) mayBotT -> 'c meetT
	= function
	| Bot ce -> Contrad ce
	| Ok (lp,point) ->
		match lp.iset with
		| IRed _ | IRw _ | IAdd (_, _) | IAddRw (_, _) -> assert false
		| IMin iset ->
			match lp.eset with
			| EAdd _ -> assert false
			| EMin eset ->
				let p = {
						eqs = eset;
						ineqs = iset;
                        point = point;
					} in
					Added p

let logAddEqs: 'c logT -> 'c Cons.t list -> 'c logT
	= fun p l ->
	if List.length l = 0
	then p
	else logEset p (EAdd (l, p.eset))

let logEqSetAddM: 'c Factory.t -> 'c logT -> ('c logT, 'c) mayBotT
	= fun factory lp1 ->
	let rec flatten: 'c Cons.t list -> 'c ependingT -> ('c logT, 'c) mayBotT
		= fun l -> function
		| EAdd (l', lp) -> flatten (l @ l') lp
		| EMin eset as ep ->
			if List.length l = 0
			then Ok {lp1 with eset = ep}
			else
				match EqSet.addM factory eset l with
				| EqSet.Bot c -> Bot c
				| EqSet.Added eset' -> Ok (logEset lp1 (EMin eset'))
	in
	flatten [] lp1.eset

(* XXX: If there is nothing to rewrite, the log should not be changed. *)
let logrewriteIneqs: 'c Factory.t -> 'c logT -> ('c logT, 'c) mayBotT
	= fun factory lp ->
	let rec rw: 'c logT -> 'c EqSet.t -> 'c ipendingT -> ('c ipendingT, 'c) mayBotT
		= fun lp eset ip ->
			match ip with
			| IAdd (l, ip1) -> begin
				match rw lp eset ip1 with
				| Bot _ as r -> r
				| Ok ip2 -> Ok (IAdd (l, ip2))
				end
			| IAddRw (l, ip1) -> begin
				match listRw factory eset l with
				| RewriteBot' f -> Bot f
				| Rewritten' l' ->
					match rw lp eset ip1 with
					| Bot _ as r -> r
					| Ok ip2 ->
						if List.length l' = 0
						then Ok ip2
						else Ok (IAdd (l', ip2))
				end
			| IMin _ | IRed _ ->	Ok ip
			| IRw iset ->
				match rewriteIneqs factory eset iset.ineqs with
				| RewriteBot f -> Bot f
				| Rewritten iset' ->  Ok (IRed (IneqSet.of_list iset'))
	in
	match lp.eset with
	| EAdd (_, _) -> assert false
	| EMin eset ->
		match rw lp eset lp.iset with
		| Bot ce -> Bot ce
		| Ok ip -> Ok (logIset lp ip)

let logIneqSetAddM: Var.t -> Scalar.Symbolic.t Rtree.t -> 'c logT -> ('c logT * Vector.Symbolic.t option, 'c) mayBotT
	= let doAdd: Var.t -> 'c logT -> 'c Cons.t list -> 'c IneqSet.t -> Scalar.Symbolic.t Rtree.t
        -> ('c logT * Vector.Symbolic.t option, 'c) mayBotT
		= fun nvar lp l iset point ->
		let iset' = IneqSet.assume nvar iset l point in
		Ok (logIset lp (IMin iset'), Some point)
	in
	fun nvar point lp ->
		let rec flatten: 'c Cons.t list -> 'c ipendingT -> ('c logT * Vector.Symbolic.t option, 'c) mayBotT
		= fun l -> function
			| IRw _
			| IAddRw (_, _) -> assert false
			| IAdd (l', ip) -> flatten (l @ l') ip
			| IRed iset -> doAdd nvar lp l iset point
			| IMin iset as ip ->
				if List.length l = 0
				then Ok ({lp with iset = ip}, Some point)
				else doAdd nvar lp l iset point
	in
	flatten [] lp.iset

type satChkT = Sat of Splx.t | Unsat of (int * Cs.Vec.Coeff.t) list

(** [chkFeasibility nvar s] checks whether [s] is satisfiable and returns a
simplex object with a satisfied state if it is. If it is not satisfiable, then
a linear combination of the input constraints is returned. [nvar] is used for
fresh variable generation. *)
let chkFeasibility: Var.t -> (int * Cs.t) list -> satChkT
	= let chkFeasibility: Var.t -> (int * Cs.t) list -> satChkT
		= fun nvar cs ->
		match Splx.checkFromAdd (Splx.mk nvar cs) with
		| Splx.IsOk sx -> Sat sx
		| Splx.IsUnsat w -> Unsat w
	in fun nvar cs ->
	Profile.start "chkFeasibility";
	let res = chkFeasibility nvar cs in
	Profile.stop "chkFeasibility";
	res

let rec extract_implicit_eqs' : 'c Factory.t -> Var.t -> 'c logT -> (('c logT, 'c) mayBotT) * (Scalar.Symbolic.t Rtree.t) option
	= fun factory nvar lp ->
	(* TODO: peut-on factoriser le calcul des Is et Js?*)
	let compute_Is : 'c Cons.t list -> (int * Cs.Vec.Coeff.t) list -> (int * 'c Cons.t) list
		= fun conss wit ->
		let (i0, coeff0) = List.hd wit in
		List.fold_left
			(fun res (i, coeff) ->
				let cons = Cons.mul factory coeff (List.nth conss i)
					|> Cons.add factory (List.hd res |> Stdlib.snd)
				in
				(i,cons) :: res)
			[(i0, Cons.mul factory coeff0 (List.nth conss i0))]
			(Misc.sublist wit 1 ((List.length wit)-1))
	in
	let compute_Js : 'c Cons.t list -> (int * Cs.Vec.Coeff.t) list -> (int * 'c) list
		= fun conss wit ->
		let compute_index : int -> int
			= fun i ->
			let n = Misc.findi (fun (j,_) -> i = j) wit in
			List.nth wit (n-1)
			|> Stdlib.fst
		in
		let len = (List.length wit)-1 in
		let (i_n, coeff_n) = List.nth wit len in
		List.fold_right
			(fun (i, coeff) res ->
				let cert = factory.Factory.mul coeff (List.nth conss i |> Cons.get_cert)
					|> factory.Factory.add (List.hd res |> Stdlib.snd)
				in
				(compute_index i,cert) :: res)
			(Misc.sublist wit 1 len)
			[(compute_index i_n,factory.Factory.mul coeff_n (List.nth conss i_n |> Cons.get_cert))]
		|> List.rev
	in
	(* Remove trivial constraints from a given list of constraints. *)
	let rmTriv: 'c Cons.t list -> 'c Cons.t list
		= let isTriv: Cs.t -> bool
			= fun c ->
			Cs.tellProp c = Cs.Trivial
		in
		fun l ->
		List.filter (fun (c,_) -> not (isTriv c)) l
	in
	let rec get_ineqs :'c ipendingT -> 'c Cons.t list
		= fun iset ->
		match iset with
		| IMin iset | IRed iset | IRw iset -> iset.ineqs
		| IAdd (iset,tail) | IAddRw (iset,tail) -> iset @ (get_ineqs tail)
	in
	let conss = get_ineqs lp.iset
		|> rmTriv
	in
	let ilist = List.mapi (fun i (c,_) -> (i,c)) conss in
	match chkFeasibility nvar ilist with
	| Unsat wit ->
		let cert = Cons.linear_combination_cert factory conss wit in
		(Bot cert, None)
	| Sat _ ->
		let ilist_stricten = List.map (fun (i,c) -> i, {c with Cs.typ = Cstr_type.Lt}) ilist in
		match chkFeasibility nvar ilist_stricten with
		| Sat sx_strict -> Ok lp, Some (Splx.getAsg sx_strict)
		| Unsat wit ->
			let is = compute_Is conss wit
			and js = compute_Js conss wit
			in
			let ijs = List.map2
				(fun (i,(cstr,lower_bound)) (j,upper_bound) ->
					assert (i = j);
					(i, ({cstr with Cs.typ = Cstr_type.Eq}, factory.Factory.merge lower_bound upper_bound))
					) is js
			in
			let elist =
				List.fold_left
				(fun res (i,_) ->
					if List.mem_assoc i ijs
					then List.assoc i ijs :: res
					else res)
				[] ilist
			in
			let lp' = logAddEqs lp elist in
			match logEqSetAddM factory lp' with
			| Bot _ as b -> b, None
			| Ok lp ->
				match logrewriteIneqs factory lp with
				| Bot _ as b -> b, None
				| Ok lp -> extract_implicit_eqs' factory nvar lp

let extract_implicit_eqs : 'c Factory.t -> Var.t -> 'c logT -> (('c logT, 'c) mayBotT) * (Scalar.Symbolic.t Rtree.t) option
	= fun factory nvar lp ->
	Profile.start "extract_implicit_eqs";
	let res = extract_implicit_eqs' factory nvar lp in
	Profile.stop "extract_implicit_eqs";
	res

let logInit: 'c t -> 'c Cons.t list -> ('c logT, 'c) mayBotT
	= let split: 'c Cons.t list -> 'c Cons.t list * 'c Cons.t list
		= fun l -> List.partition (fun (c, _) -> Cs.get_typ c = Cstr_type.Eq) l
	in
	fun p l ->
		let (elist, ilist) = split l in
		Ok {
			eset = if elist = []
				then EMin p.eqs
				else EAdd (elist, EMin p.eqs);
			iset = if ilist = []
				then if elist = []
					then IMin p.ineqs
					else IRw p.ineqs
				else IAddRw (ilist, IMin p.ineqs)
		}

let logBind: ('c logT, 'c) mayBotT -> ('c logT -> ('c logT, 'c) mayBotT) -> ('c logT, 'c) mayBotT
= fun mlp f ->
	match mlp with
	| Bot _ as b -> b
	| Ok lp -> f lp

let (<*>) = logBind

(* XXX: it is a bit awkward that [p.nxt] is supposed to take into account
the identifiers used in [l]. *)
let addAux': 'c Factory.t -> Var.t -> 'c t -> 'c Cons.t list -> 'c meetT
	= fun factory nvar p l ->
	logOut
	(logInit p l
	<*> (fun lp -> logEqSetAddM factory lp) (* Removing syntactic equalities *)
	<*> (fun lp -> logrewriteIneqs factory lp)
	|> function
		| Bot _ as b -> b
		| Ok lp ->
			match extract_implicit_eqs factory nvar lp with
			| Bot f, _ -> Bot f
			| Ok _, None -> Stdlib.failwith "Pol.addAux"
			| Ok lp, Some point -> logIneqSetAddM nvar point lp)

let addAux: 'c Factory.t -> Var.t -> 'c t -> 'c Cons.t list -> 'c meetT
	= fun factory nvar p conss ->
	Profile.start "addAux" ;
	let res = addAux' factory nvar p conss in
	Profile.stop "addAux";
	res

let addMSub: 'c Factory.t -> Var.t -> 'c t -> 'c Cons.t list -> 'c meetT
	= fun factory nvar p conss -> addAux factory nvar p conss

let addSub: 'c Factory.t -> Var.t -> 'c t -> 'c Cons.t -> 'c meetT
	= fun factory nvar p c -> addAux factory nvar p [c]

let meetSub: 'c Factory.t -> Var.t -> 'c t -> 'c t -> 'c meetT
	= fun factory nvar p1 p2 ->
	addAux factory nvar p1 (EqSet.list p2.eqs @ p2.ineqs.ineqs)

let mkSub: 'c Factory.t -> Var.t -> 'c Cons.t list -> 'c t option
	= fun factory nvar l ->
	match addMSub factory nvar top l with
	| Added p -> Some p
	| Contrad _ -> None

let projectSubFM: 'c Factory.t -> Var.t -> 'c t -> Var.t list -> 'c t
	= fun factory nxtVar p l ->
	let msk =
		List.fold_left (fun m x -> Rtree.set None m x (Some x)) Rtree.Nil l
	in
	let rec project1 : 'c EqSet.t -> 'c IneqSet.t -> 'c t
	= fun eqs ineqs ->
		let (opte, eqs1) = EqSet.trySubstM factory msk eqs in
		match opte with
		| Some (e, x) ->
			let ineqs1 = IneqSet.subst factory nxtVar eqs1 x e ineqs in
			project1 eqs1 ineqs1
		| None -> begin
			let ineqs1 = IneqSet.fmElim factory nxtVar eqs1 msk ineqs
			in
            let point' = match p.point with
                | None -> None
                | Some point -> Some (Vector.Symbolic.project l point)
            in
			{ineqs = ineqs1; eqs = eqs1; point = point'}
            end
	in
	project1 p.eqs p.ineqs

let projectSubPLP: 'c Factory.t -> Var.t -> 'c t -> Var.t list -> 'c t
	= fun factory nxtVar p vars ->
	let msk =
		List.fold_left (fun m x -> Rtree.set None m x (Some x)) Rtree.Nil vars
	in
	let rec findEq eqs ineqs point vars =
		let (opte, eqs1) = EqSet.trySubstM factory msk eqs in
		match opte with
		| Some (e, x) ->
			let ineqs1 = IneqSet.subst factory nxtVar eqs1 x e ineqs in
            let vec = Cons.get_c e |> Cs.get_v |> Vector.Rat.neg |> Vector.Symbolic.ofRat in
            let point1 = Vector.Symbolic.elim x vec point in
            let vars' = try
                let i = Misc.findi (Var.equal x) vars in
                Misc.popi vars i
                with Not_found -> vars
            in
			findEq eqs1 ineqs1 point1 vars'
		| None -> (eqs, ineqs, point, vars)
	in
	let (eqs1, ineqs1, point, vars') = findEq p.eqs p.ineqs (get_point p) vars in
    if vars' = []
    then {
        ineqs = ineqs1;
        eqs = eqs1;
        point = Some point;
    }
    else
		let point' = Vector.Symbolic.project vars' point in
        let normalization_point = Vector.Symbolic.toRat point' in
    	let ineqs2 = IneqSet.plpElim factory normalization_point vars' ineqs1 in {
            ineqs = ineqs2;
            eqs = eqs1;
            point = Some (point');
        }


let projectSub: 'c Factory.t -> Var.t -> 'c t -> Var.t list -> 'c t
	= fun factory nxt p l ->
    if l = []
    then p
    else match !Flags.proj with
    	| Flags.Proj_PLP _ -> projectSubPLP factory nxt p l
    	| Flags.FM -> projectSubFM factory nxt p l
    	| Flags.PHeuristic -> match Heuristic.proj (List.map Cons.get_c p.ineqs.ineqs) with
    		| Flags.Proj_PLP _ -> projectSubPLP factory nxt p l
    		| Flags.FM -> projectSubFM factory nxt p l
    		| Flags.PHeuristic -> Stdlib.invalid_arg "Pol.projectSub"

(* note: x = y1 + y2 and alpha1 + alpha2 = 1 are substituted on the fly *)
let joinSetup: 'c1 Factory.t -> 'c2 Factory.t -> Var.t -> 'c1 t -> 'c2 t
	-> Var.t * (('c1,'c2) Cons.discr_t) t * Var.t list * ('c1,'c2) Cons.discr_cert
	= fun factory1 factory2 varNxt p1 p2 ->
	Debug.log DebugTypes.Detail (lazy "Entering joinSetup");
	let factory = Cons.discr_factory factory1 factory2 in
	let alpha = varNxt in
	let aIneqs = [
		(Cs.mk Cstr_type.Le [Scalar.Rat.u, alpha] Scalar.Rat.u,
			(factory1.Factory.triv Cstr_type.Le Cs.Vec.Coeff.z, factory2.Factory.triv Cstr_type.Le Cs.Vec.Coeff.u));
		(Cs.mk Cstr_type.Le [Scalar.Rat.negU, alpha] Scalar.Rat.z,
			(factory1.Factory.triv Cstr_type.Le Cs.Vec.Coeff.u, factory2.Factory.triv Cstr_type.Le Cs.Vec.Coeff.z))
		]
	in
	let (varNxt1, r, eqs1) = EqSet.joinSetup_1 factory2 (Var.next varNxt) Rtree.Nil alpha p1.eqs in
	Debug.log DebugTypes.Detail (lazy (Printf.sprintf "Equalities of P1 handled: \n%s"
		(EqSet.to_string_ext factory Var.to_string eqs1)));
	let (varNxt2, r, eqs2) = EqSet.joinSetup_2 factory1 varNxt1 r alpha p2.eqs in
	Debug.log DebugTypes.Detail (lazy (Printf.sprintf "Equalities of P2 handled: \n%s"
		(EqSet.to_string_ext factory Var.to_string eqs2)));
	let (varNxt3, r, ineqs1) = IneqSet.joinSetup_1 factory2 varNxt2 r alpha p1.ineqs in
	Debug.log DebugTypes.Detail (lazy (Printf.sprintf "Inequalities of P1 handled: \n%s"
		(IneqSet.to_string_ext factory Var.to_string (IneqSet.of_list ineqs1))));
	let (varNxt4, r, ineqs2) = IneqSet.joinSetup_2 factory1 varNxt3 r alpha p2.ineqs in
	Debug.log DebugTypes.Detail (lazy (Printf.sprintf "Inequalities of P2 handled: \n%s"
		(IneqSet.to_string_ext factory Var.to_string (IneqSet.of_list ineqs2))));
	let eqs = List.append eqs1 eqs2 in
	let ineqs = List.concat [ineqs1; ineqs2; aIneqs] in
	let vars =
		Rtree.fold (fun _ a -> function None -> a | Some x -> x::a) [alpha] r
	in
    let point' = match p1.point, p2.point with
        | Some point, _ | _, Some point -> Some point
        | _,_ -> None
    in
	(varNxt4, {ineqs = IneqSet.of_list ineqs; eqs = eqs; point = point'}, vars, factory)

let fix_cmp: 'c Factory.t -> 'c Cons.t -> Cs.t list -> 'c Cons.t
	= fun fac (c,cert) inputIneqs ->
	if (Cs.get_typ c) = Cstr_type.Le
	then (c, fac.Factory.to_le cert)
	else
        let c_le = {c with Cs.typ = Cstr_type.Le} in
        if List.exists (Cs.equalSyn c_le) inputIneqs
        then (c_le, fac.Factory.to_le cert)
        else (c, cert)

(**
 * Splits the joined certificates into two lists of certificates.
 * Also relaxes the comparison sign of certificates when necessary.
 *)
let split_certificates : 'c1 Factory.t -> 'c2 Factory.t -> 'c1 t -> 'c2 t -> (('c1,'c2) Cons.discr_t) t -> 'c1 t * 'c2 t
	= fun factory1 factory2 p1 p2 p ->
    let inputIneqs1 = p1.ineqs.ineqs
    and inputIneqs2 = p2.ineqs.ineqs in
	let (ineqs1,ineqs2) =
    let inputIneqs = List.map Cons.get_c inputIneqs1 @ List.map Cons.get_c inputIneqs2 in
	List.fold_left
		(fun (ineqs1,ineqs2) (c, (cert1,cert2)) ->
			((fix_cmp factory1 (c, cert1) inputIneqs)::ineqs1),
			((fix_cmp factory2 (c, cert2) inputIneqs)::ineqs2))
		([],[]) p.ineqs.ineqs
	and (eqs1,eqs2) =
	List.fold_left
		(fun (eqs1,eqs2) (v, (c, (cert1,cert2))) -> (v, (c, cert1))::eqs1, (v, (c, cert2))::eqs2)
		([],[]) p.eqs
	in
	{eqs = eqs1 ; ineqs = IneqSet.of_list ineqs1 ; point = p1.point},
    {eqs = eqs2 ; ineqs = IneqSet.of_list ineqs2 ; point = p2.point}

let joinSub_classic: 'c1 Factory.t -> 'c2 Factory.t -> Var.t -> 'c1 t -> 'c2 t -> 'c1 t * 'c2 t
	= fun factory1 factory2 nxtVar p1 p2 ->
	let (nxtVar1, p0, vars, factory) = joinSetup factory1 factory2 nxtVar p1 p2 in
	let p = projectSub factory nxtVar1 p0 vars in
	split_certificates factory1 factory2 p1 p2 p

module Join_PLP = struct


	(*
    let affine_span :'c1 Factory.t -> 'c2 Factory.t -> Var.t -> 'c1 t -> 'c2 t -> 'c1 t * 'c2 t
        = fun factory1 factory2 varNxt p1 p2 ->
        let factory = Cons.discr_factory factory1 factory2 in
        let alpha = varNxt in
    	let aIneqs = [
    		(Cs.mk Cstr_type.Le [Scalar.Rat.u, alpha] Scalar.Rat.u,
    			(factory1.Factory.triv Cstr_type.Le Cs.Vec.Coeff.z, factory2.Factory.triv Cstr_type.Le Cs.Vec.Coeff.u));
    		(Cs.mk Cstr_type.Le [Scalar.Rat.negU, alpha] Scalar.Rat.z,
    			(factory1.Factory.triv Cstr_type.Le Cs.Vec.Coeff.u, factory2.Factory.triv Cstr_type.Le Cs.Vec.Coeff.z))
    		]
    	in
        let (varNxt1, r, eqs1) = EqSet.joinSetup_1 factory2 (Var.next varNxt) Rtree.Nil alpha p1.eqs in
        Debug.log DebugTypes.Detail (lazy (Printf.sprintf "Equalities of P1 handled: \n%s"
            (EqSet.to_string_ext factory Var.to_string eqs1)));
        let (varNxt2, r, eqs2) = EqSet.joinSetup_2 factory1 varNxt1 r alpha p2.eqs in
        Debug.log DebugTypes.Detail (lazy (Printf.sprintf "Equalities of P2 handled: \n%s"
            (EqSet.to_string_ext factory Var.to_string eqs2)));
        let eqs = List.rev_append eqs1 eqs2 in
        let interior_point = Vector.Symbolic.mk [Scalar.Symbolic.of_float 0.5, alpha] in
        let p' = {ineqs = aIneqs; eqs = eqs; point = Some interior_point} in
        let vars = Rtree.fold (fun _ a ->
            function None -> a | Some x -> x::a
        ) [alpha] r in
        let p_proj = projectSub factory varNxt2 p' vars in
        split_certificates factory1 factory2 p1 p2 p_proj
	*)

	let split_eq : 'c Factory.t -> 'c t -> 'c Cons.t list
		= fun factory p ->
		List.fold_left
			(fun res (_,cons) ->
				let (c1,c2) = Cons.split factory cons in
				c1::c2::res)
			[]
			p.eqs

	(* Version spéciale pour extract_implicit_eq*)
	let logOut: ('c logT * Vector.Symbolic.t option, 'c) mayBotT -> 'c t
		= function
			| Bot _ -> Stdlib.failwith "Pol.join:extract_implicit_eq"
			| Ok (lp, point) -> begin
				match lp.iset with
				| IRed _ | IRw _ | IAdd (_, _) | IAddRw (_, _) -> assert false
				| IMin iset ->
					match lp.eset with
					| EAdd _ -> assert false
					| EMin eset -> {eqs = eset; ineqs = iset ; point = point}
			end

	let extract_implicit_eq : 'c Factory.t -> Var.t -> 'c t -> 'c t
		= fun factory nxtVar p ->
		logOut
		(logInit p []
		|> function
			| Bot _ as b -> b
			| Ok lp ->
				match extract_implicit_eqs factory nxtVar lp with
				| Bot f, _ -> Bot f
				| Ok _, None -> Stdlib.failwith "Pol.join:extract_implicit_eq"
				| Ok lp, Some point -> logIneqSetAddM nxtVar point lp)

	let filter_trivial : 'c Cons.t list -> 'c Cons.t list
		= fun l ->
		List.filter (fun cons ->
			Cs.tellProp (Cons.get_c cons) <> Cs.Trivial
			&& not(Cs.Vec.equal (Cons.get_c cons |> Cs.get_v) Cs.Vec.nil)
		) l

	let add_epsilon : Var.t -> 'c Cons.t list -> 'c Cons.t list
		= fun epsilon l ->
		List.map (fun cons ->
			({(Cons.get_c cons) with Cs.v = Cs.Vec.set (Cons.get_c cons |> Cs.get_v) epsilon Cs.Vec.Coeff.negU},
			 Cons.get_cert cons)
		) l

	let remove_epsilon : Var.t -> 'c Cons.t list -> 'c Cons.t list
		= fun epsilon l ->
		List.map (fun cons -> {(Cons.get_c cons) with
			Cs.v = Cs.Vec.set
				(Cons.get_c cons |> Cs.get_v)
				epsilon Cs.Vec.Coeff.z
			},
			Cons.get_cert cons
		) l

	let compute_point p1_cons p2_cons =
		let p1_point = get_point {
			ineqs = IneqSet.of_list p1_cons;
			eqs = [];
			point = None;
		}
		and p2_point = get_point {
			ineqs = IneqSet.of_list p2_cons;
			eqs = [];
			point = None;
		}
		in
		Vector.Symbolic.add p1_point p2_point
		|> Vector.Symbolic.mulc (Scalar.Symbolic.of_float 0.5)
		|> Vector.Symbolic.toRat

	let joinSub: 'c1 Factory.t -> 'c2 Factory.t -> Var.t -> 'c1 t -> 'c2 t -> 'c1 t * 'c2 t
		= fun factory1 factory2 nxtVar p1 p2 ->
		let epsilon = nxtVar in
		let p1_eqs = split_eq factory1 p1
		and p2_eqs = split_eq factory2 p2 in
		let (p1_cons, p2_cons) =
			if p1_eqs <> [] || p2_eqs <> []
			then (
				(add_epsilon epsilon p1_eqs) @ p1.ineqs.ineqs,
				(add_epsilon epsilon p2_eqs) @ p2.ineqs.ineqs)
			else (p1.ineqs.ineqs, p2.ineqs.ineqs)
		in
		let normalization_point = compute_point p1_cons p2_cons in
		let (p1',p2') =
			let eps_opt = if p1_eqs <> [] || p2_eqs <> []
				then Some epsilon
				else None in
			Join.join factory1 factory2 eps_opt normalization_point p1_cons p2_cons
		in
		let p1'_ineqs = remove_epsilon epsilon p1' |> filter_trivial
		and p2'_ineqs = remove_epsilon epsilon p2' |> filter_trivial
		in
		(extract_implicit_eq factory1 nxtVar {
			ineqs = IneqSet.of_list p1'_ineqs;
			eqs = [];
			point = p1.point;},
		 extract_implicit_eq factory2 nxtVar {
			 ineqs = IneqSet.of_list p2'_ineqs;
			 eqs = [];
			 point = p2.point;}
		)

	(*
	let joinSub : 'c1 Factory.t -> 'c2 Factory.t -> Var.t -> 'c1 t -> 'c2 t -> 'c1 t * 'c2 t
		= fun factory1 factory2 nxtVar p1 p2 ->
        let interior_point = (match p1.point, p2.point with
            | Some point, _ | _, Some point -> point
            | _,_ -> get_point p1
            )
        in
        let (p1_as, p2_as) = affine_span factory1 factory2 nxtVar p1 p2 in
        let (ineqs1,ineqs2) = Join2.join factory1 factory2 (Vector.Symbolic.toRat interior_point) p1.ineqs p2.ineqs in
        let p'1 = {ineqs = ineqs1 @ p1_as.ineqs; eqs = p1_as.eqs ; point = None;}
        and p'2 = {ineqs = ineqs2 @ p2_as.ineqs; eqs = p2_as.eqs ; point = None;} in
        (p'1, p'2)
		*)
end

let inclSub: 'c1 Factory.t -> Var.t -> 'c1 t -> 'c2 t -> 'c1 rel_t
	= fun factory nxtVar p1 p2 ->
	Debug.log DebugTypes.Normal (lazy (Printf.sprintf "Checking inclusion %s ⊂ %s"
		(to_string_ext factory Var.to_string p1)
		(to_string Var.to_string p2)))
	;
	match EqSet.leq factory p1.eqs p2.eqs with
	| EqSet.NoIncl -> NoIncl
	| EqSet.Incl certE -> begin
		Debug.log DebugTypes.Detail (lazy (Printf.sprintf "Inclusion holds for equalities: %s"
			(Misc.list_to_string factory.Factory.to_string certE "\n")));
		match IneqSet.incl factory nxtVar p1.eqs p1.ineqs p2.ineqs with
		| IneqSet.NoIncl -> NoIncl
		| IneqSet.Incl certI -> begin
			Debug.log DebugTypes.Detail (lazy (Printf.sprintf "Inclusion holds for inequalities: %s"
			(Misc.list_to_string factory.Factory.to_string certI "\n")));
			Incl (List.append certI certE)
			end
		end

let joinSub: 'c1 Factory.t -> 'c2 Factory.t -> Var.t -> 'c1 t -> 'c2 t -> 'c1 t * 'c2 t
	= let check_incl
		= fun factory nxtVar p1 p2 ->
		match inclSub factory nxtVar p1 p2 with
		| Incl p2_from_p1 ->
			let ineqs = get_ineqs p2 |> List.rev
			and eqs = get_eqs p2 in
			let len_ineqs = List.length ineqs in
			Some {
				ineqs = List.map2
					(fun (cstr,_) cert -> (cstr,cert))
					ineqs (Misc.sublist p2_from_p1 0 len_ineqs)
                    |> IneqSet.of_list;
				eqs = List.map2
					(fun (v, (cstr,_)) cert -> (v, (cstr,cert)))
					eqs (Misc.sublist p2_from_p1 len_ineqs (List.length p2_from_p1));
                point = p1.point
			}
		| NoIncl -> None
	in
	fun factory1 factory2 nxtVar p1 p2 ->
	match check_incl factory1 nxtVar p1 p2 with
	| Some p1' -> (p1',p2)
	| None -> match check_incl factory2 nxtVar p2 p1 with
    	| Some p2' -> (p1,p2')
    	| None -> begin
    		Debug.log DebugTypes.Normal (lazy "No inclusion found. Computing convex hull. ");
    		match !Flags.join with
    			| Flags.Baryc -> joinSub_classic factory1 factory2 nxtVar p1 p2
    			| Flags.Join_PLP scalar_type -> Join_PLP.joinSub factory1 factory2 nxtVar p1 p2
    			| Flags.Join_fromRegions -> Join_PLP.joinSub factory1 factory2 nxtVar p1 p2
    			| Flags.JHeuristic -> match Heuristic.join (get_cstr p1) (get_cstr p2) with
    				| Flags.Baryc -> joinSub_classic factory1 factory2 nxtVar p1 p2
    				| Flags.Join_PLP scalar_type -> Join_PLP.joinSub factory1 factory2 nxtVar p1 p2
    				| Flags.Join_fromRegions -> Join_PLP.joinSub factory1 factory2 nxtVar p1 p2
    				| Flags.JHeuristic -> Stdlib.invalid_arg "Pol.joinSub"
    		end

let widen : 'c Factory.t -> 'c t -> 'c t -> 'c t
	= fun factory p1 p2 ->
	let elect s c =
		let c1 = EqSet.filter factory p1.eqs c in
		match Cs.tellProp (Cons.get_c c1) with
		| Cs.Trivial -> c::s (* XXX: c shouldn't get in, right? *)
		| Cs.Nothing ->
			if List.exists (fun c2 -> Cons.equal c2 c1) p1.ineqs.ineqs then
				c::s
			else
				s

		| Cs.Contrad -> failwith "Pol.widen"
	in
	{ p2 with ineqs = List.fold_left elect [] p2.ineqs.ineqs |> IneqSet.of_list}

let opt2itv: (Scalar.Rat.t -> Scalar.Rat.t) -> 'c Factory.t -> 'c Cons.t list -> Opt.optT -> bndT * 'c option
	= fun f factory conss ->
	function
	| Opt.Finite (_, n, w) ->
		let cert = Cons.linear_combination_cert factory conss w
			|> factory.Factory.add (factory.Factory.triv Cstr_type.Le Scalar.Rat.z)
		in
		(Closed (f n), Some cert)
	| Opt.Sup (_, n, w) ->
		let cert = Cons.linear_combination_cert factory conss w
			|> factory.Factory.add (factory.Factory.triv Cstr_type.Le Scalar.Rat.z)
		in
		(Open (f n), Some cert)
  | Opt.Infty -> (Infty, None)

let getUpperBoundImpl : 'c Factory.t -> 'c Cons.t list -> Vec.t -> Splx.t Splx.mayUnsatT -> bndT * 'c option
  = fun factory conss v sx ->
  match Opt.max' sx v with
  | Splx.IsUnsat _ -> Stdlib.failwith "Pol.getUpperBoundSub"
  | Splx.IsOk u -> opt2itv (fun n -> n) factory conss u

let getUpperBoundSub : 'c Factory.t -> Var.t -> 'c t -> Vec.t -> bndT * 'c option
  = fun factory x p v ->
  let conss = EqSet.list p.eqs @ p.ineqs.ineqs in
  let i_cstr = List.mapi (fun i cons -> (i, Cons.get_c cons)) conss in
  Splx.mk x i_cstr
    |> getUpperBoundImpl factory conss v

let getLowerBoundImpl : 'c Factory.t -> 'c Cons.t list -> Vec.t -> Splx.t Splx.mayUnsatT -> bndT * 'c option
  = fun factory conss v sx ->
  match Opt.max' sx (Vec.neg v) with
  | Splx.IsUnsat _ -> Stdlib.failwith "Pol.getLowerBoundSub"
  | Splx.IsOk l -> opt2itv Scalar.Rat.neg factory conss l

let getLowerBoundSub : 'c Factory.t -> Var.t -> 'c t -> Vec.t -> bndT * 'c option
  = fun factory x p v ->
  let conss = EqSet.list p.eqs @ p.ineqs.ineqs in
  let i_cstr = List.mapi (fun i cons -> (i, Cons.get_c cons)) conss in
  Splx.mk x i_cstr
    |> getLowerBoundImpl factory conss v

let itvizeSub: 'c Factory.t -> Var.t -> 'c t -> Vec.t -> itvT * 'c option * 'c option
	= fun factory nxtVar p v ->
	let conss = EqSet.list p.eqs @ p.ineqs.ineqs in
	let i_cstr = List.mapi (fun i cons -> (i, Cons.get_c cons)) conss in
	let sx = Splx.mk nxtVar i_cstr in
	let (up, upf) = getUpperBoundImpl factory conss v sx in
	let (low, lpf) = getLowerBoundImpl factory conss v sx in
	{low = low; up = up}, lpf, upf

let add: 'c Factory.t -> 'c t -> 'c Cons.t -> 'c meetT
	= fun factory p c ->
	Debug.log DebugTypes.Title (lazy "Building add");
	Debug.log DebugTypes.MInput (lazy (Printf.sprintf "c = %s\nP = %s"
			(Cons.to_string_ext factory Var.to_string c)
			(to_string_raw p)))
	;
	let nxt = Var.Set.union (varSet p) (Cs.getVars [Cons.get_c c]) |> Var.horizon in
	let res = addSub factory nxt p c in
	Debug.log DebugTypes.MOutput (lazy (Printf.sprintf "%s"
			(match res with
			| Added p -> to_string_raw p
			| Contrad cert -> "Contrad " ^ (factory.Factory.to_string cert))))
	;
	res

let addM: 'c Factory.t -> 'c t -> 'c Cons.t list -> 'c meetT
= fun factory p l ->
	Debug.log DebugTypes.Title (lazy "Building add");
	Debug.log DebugTypes.MInput (lazy (Printf.sprintf "c = %s\nP = %s"
			(Misc.list_to_string (Cons.to_string_ext factory Var.to_string) l " ; ")
			(to_string_raw p)))
	;
	let nxt = Var.Set.union (varSet p) (Cs.getVars (List.map Cons.get_c l)) |> Var.horizon in
	let res = addMSub factory nxt p l in
	Debug.log DebugTypes.MOutput (lazy (Printf.sprintf "%s"
			(match res with
			| Added p -> to_string_raw p
			| Contrad cert -> "Contrad " ^ (factory.Factory.to_string cert))))
	;
	res

let meet: 'c Factory.t -> 'c t -> 'c t -> 'c meetT
	= fun factory p1 p2 ->
	let nxt = Var.Set.union (varSet p1) (varSet p2) |> Var.horizon in
	meetSub factory nxt p1 p2

let mk: 'c Factory.t -> 'c Cons.t list -> 'c t option
	= fun factory l ->
	mkSub factory (List.map Stdlib.fst l |> Cs.getVars |> Var.horizon) l

let project: 'c Factory.t -> 'c t -> Var.t list -> 'c t
	= fun factory p l ->
	Debug.log DebugTypes.Title (lazy "Building Projection");
	Debug.log DebugTypes.MInput (lazy (Printf.sprintf "v = %s\nP = %s"
			(Misc.list_to_string Var.to_string l ";")
			(to_string_raw p)))
	;
	let res = projectSub factory (Var.Set.union (Var.Set.of_list l) (varSet p) |> Var.horizon) p l in
	Debug.log DebugTypes.MOutput (lazy (Printf.sprintf "%s"
			(to_string_raw res)))
	;
	res

let join: 'c1 Factory.t -> 'c2 Factory.t -> 'c1 t -> 'c2 t -> 'c1 t * 'c2 t
	= fun factory1 factory2 p1 p2 ->
	Debug.log DebugTypes.Title (lazy "Building Join");
	Debug.log DebugTypes.MInput (lazy (Printf.sprintf "P1 = %s\nP2 = %s\n"
			(to_string_raw p1)
			(to_string_raw p2)))
	;
	let nxt = Var.Set.union (varSet p1) (varSet p2) |> Var.horizon in
	let res = joinSub factory1 factory2 nxt p1 p2 in
	Debug.log DebugTypes.MOutput (lazy (Printf.sprintf "%s"
			(to_string_raw (fst res))))
	;
	res

let incl: 'c1 Factory.t -> 'c1 t -> 'c2 t -> 'c1 rel_t
	= fun factory p1 p2 ->
    Debug.log DebugTypes.Title (lazy "Testing inclusion");
	Debug.log DebugTypes.MInput (lazy (Printf.sprintf "%s ⊂ %s"
		(to_string_ext factory Var.to_string p1)
		(to_string Var.to_string p2)))
	;
	let nxt = Var.Set.union (varSet p1) (varSet p2) |> Var.horizon in
	let res = inclSub factory nxt p1 p2 in
    Debug.log DebugTypes.MOutput (lazy (match res with
        | NoIncl -> "Inclusion does not hold"
        | Incl certs -> Printf.sprintf "Inclusion holds, the first polyhedron can be expressed as %s"
            (Misc.list_to_string factory.Factory.to_string certs " ; ")
    ));
    res

let itvize: 'c Factory.t -> 'c t -> Vec.t -> itvT * 'c option * 'c option
	= fun factory p v ->
	let nxt = Var.Set.union (Vec.getVars [v]) (varSet p) |> Var.horizon in
	itvizeSub factory nxt p v

let getUpperBound : 'c Factory.t -> 'c t -> Vec.t -> bndT * 'c option
	= fun factory p v ->
	let nxt = Var.Set.union (Vec.getVars [v]) (varSet p) |> Var.horizon in
	getUpperBoundSub factory nxt p v

let getLowerBound : 'c Factory.t -> 'c t -> Vec.t -> bndT * 'c option
	= fun factory p v ->
	let nxt = Var.Set.union (Vec.getVars [v]) (varSet p) |> Var.horizon in
	getLowerBoundSub factory nxt p v

let rename: 'c Factory.t -> Var.t -> Var.t -> 'c t -> 'c t
	= fun factory x y p -> {
	eqs = EqSet.rename factory p.eqs x y;
	ineqs = IneqSet.rename factory p.ineqs x y;
    point = (match p.point with
        | None -> None
        | Some point -> Some (Vector.Symbolic.rename x y point))
    }

(*
let inter factory p1 p2 =
  mk factory ((get_cstr p1) @ (get_cstr p2))
*)
let invChk: 'c Factory.t -> 'c t -> bool * string
	= fun factory p ->
	let chkTriv: 'c t -> bool
		= let chk1: 'c Cons.t -> bool
			= fun c -> Cs.tellProp (Cons.get_c c) = Cs.Nothing
		in
		fun p ->
			List.for_all chk1 p.ineqs.ineqs && List.for_all (fun (_, c) -> chk1 c) p.eqs
	in
	let chkDefU: 'c t -> bool
		= let chk1: Var.t * 'c Cons.t -> bool
			= fun (x, c) ->
			let lin = Cs.get_v (Cons.get_c c) in
			Scalar.Rat.cmp Scalar.Rat.u (Vec.get lin x) = 0
		in
		fun p -> List.for_all chk1 p.eqs
	in
	let chkDefE: 'c t -> bool
		= let chk1: Var.t * 'c Cons.t -> Var.t list * bool -> Var.t list * bool
			= fun (x, c) (l, r) ->
			let lin = Cs.get_v (Cons.get_c c) in
			(x::l, r && List.for_all (fun x' -> Scalar.Rat.cmpz (Vec.get lin x') = 0) l)
		in
		fun p -> snd (List.fold_right chk1 p.eqs ([], true))
	in
	let chkDefI: 'c t -> bool
		= let chk1: Var.t list -> 'c Cons.t -> bool
			= fun l c ->
			let lin = Cs.get_v (Cons.get_c c) in
			List.for_all (fun x -> Scalar.Rat.cmpz (Vec.get lin x) = 0) l
		in
		fun p -> List.for_all (chk1 (List.map fst p.eqs)) p.ineqs.ineqs
	in
	let chkBot: 'c t -> bool
		= let cs: 'c t -> (int * Cs.t) list
			= fun p ->
			List.mapi (fun i c -> (i, Cons.get_c c))
				(List.append p.ineqs.ineqs (List.map snd p.eqs))
		in
		fun p ->
			let h = varSet p |> Var.horizon in
			match Splx.checkFromAdd (Splx.mk h (cs p)) with
			| Splx.IsOk _ -> true
			| Splx.IsUnsat _ -> false
	in
	let chkRed: 'c t -> bool
		= let chkE: 'c EqSet.t -> bool
			= let add: 'c EqSet.t option -> Var.t * 'c Cons.t -> 'c EqSet.t option
				= function
				| None -> fun _ -> None
				| Some es -> fun (_, c) ->
					match EqSet.addM factory es [c] with
					| EqSet.Added es' -> Some es'
					| EqSet.Bot _ -> None
			in
			fun es ->
				match List.fold_left add (Some EqSet.nil) es with
				| None -> false
				| Some es' -> List.length es = List.length es'
		in
		fun p -> chkE p.eqs
	in
	let tr: bool -> string -> bool * string
		= fun r m -> (r, if r then "" else m)
	in
	let log = [
		tr (chkTriv p) "some constraints are trivial or contradictory";
		tr (chkDefU p) "non-unit coefficient for a defined variable";
		tr (chkDefE p) "a defined variable appears in an equality";
		tr (chkDefI p) "a defined variable appears in an inequality";
		tr (chkBot p) "the polyhedron is empty";
		tr (chkRed p) "there are redundant constraints"
	] in
	(List.for_all (fun (r, _) -> r) log,
		let errs = List.map snd log in
		String.concat "\n" (List.filter (fun s -> String.length s > 0) errs))

let equal : 'c1 Factory.t -> 'c2 Factory.t -> 'c1 t -> 'c2 t -> bool
	= fun fac1 fac2 p1 p2 ->
	match (incl fac1 p1 p2, incl fac2 p2 p1) with
	| (Incl _, Incl _) -> true
	| (_,_) -> false

let plot : 'c t -> string
	= fun p ->
	let (eqs, ineqs) = List.partition (fun c -> c.Cs.typ = Cstr_type.Eq) (get_cstr p)
	in
	let vars = Cs.getVars (eqs @ ineqs)
		|> Var.Set.elements
	in
	Printf.sprintf "P = Polyhedron(eqns = %s, ieqs = %s)\n"
		(Misc.list_to_string (Cs.plot vars) eqs ", ")
		(Misc.list_to_string (Cs.plot vars) ineqs ", ")

let plot_opt : 'c t option -> string
	= function
	| None -> "P = Polyhedron(eqns = [1,0])"
	| Some p -> plot p

let to_unit : 'c t -> unit t
	= fun ph ->
	{
		eqs = List.map
			(fun (var, (cstr, _)) -> (var, (cstr,())))
			(get_eqs ph);
		ineqs = List.map
			(fun (cstr, _) -> (cstr,()))
			(get_ineqs ph)
            |> IneqSet.of_list ;
        point = ph.point;
	}

(*
let minkowskiSetup : 'c1 Factory.t -> 'c2 Factory.t -> Var.t -> 'c1 t -> 'c2 t
-> Var.t * (('c1,'c2) Cons.discr_t) t * Var.t list * ('c1,'c2) Cons.discr_cert
	= fun factory1 factory2 nxt p1 p2 ->
	let factory = Cons.discr_factory factory1 factory2 in
	let (varNxt1, r, eqs1) = EqSet.minkowskiSetup_1 factory2 (Var.next nxt) Rtree.Nil p1.eqs in
	let (varNxt2, r, eqs2) = EqSet.minkowskiSetup_2 factory1 varNxt1 r p2.eqs in
	let (varNxt3, r, ineqs1) = IneqSet.minkowskiSetup_1 factory2 varNxt2 r p1.ineqs in
	let (varNxt4, r, ineqs2) = IneqSet.minkowskiSetup_2 factory1 varNxt3 r p2.ineqs in
	let eqs = List.append eqs1 eqs2 in
	let ineqs = List.concat [ineqs1; ineqs2] in
	let vars =
		Rtree.fold (fun _ a -> function None -> a | Some x -> x::a) [] r
	in
    let point' = match p1.point, p2.point with
        | Some point, _ | _, Some point -> Some point
        | _,_ -> None
    in
	(varNxt4, {ineqs = ineqs; eqs = eqs; point = point'}, vars, factory)

let minkowskiSub: 'c1 Factory.t -> 'c2 Factory.t -> Var.t -> 'c1 t -> 'c2 t -> 'c1 t * 'c2 t
	= fun factory1 factory2 nxtVar p1 p2 ->
	let (nxtVar1, p0, vars, factory) = minkowskiSetup factory1 factory2 nxtVar p1 p2 in
	let p = projectSub factory nxtVar1 p0 vars in
	split_certificates factory1 factory2 p1 p2 p
*)
let minkowski : 'c1 Factory.t -> 'c2 Factory.t -> 'c1 t -> 'c2 t -> 'c1 t * 'c2 t
	= fun factory1 factory2 p1 p2 ->
    failwith "minkowski: not implemented"
    (*
	let nxt = Var.Set.union (varSet p1) (varSet p2)
		|> Var.horizon
	in
	minkowskiSub factory1 factory2 nxt p1 p2
    *)

let get_regions_from_point : 'c Factory.t -> 'c t -> Vec.t -> 'c t list
    = fun factory p point ->
    let regions = IneqSet.get_regions_from_point factory p.ineqs point
    and eqs = List.map (fun (var, (cstr,_)) -> (var, (cstr, factory.top))) p.eqs
    in
    List.map
        (fun (ineqs,point) -> {
            eqs = eqs;
            ineqs = IneqSet.of_list ineqs;
            point = Some point;
        })
        regions

let set_point : Vec.t -> 'c t -> 'c t
    = fun point p ->
	Printf.sprintf "setting point %s"
		(Vec.to_string Var.to_string point)
	|> print_endline;
    List.iter
        (fun cstr ->
            let cstr' = { cstr with
                Cs.typ = (match Cs.get_typ cstr with | Cstr_type.Le -> Cstr_type.Lt | typ -> typ);
            } in
            if not (Cs.satisfy point cstr')
            then Stdlib.invalid_arg (Printf.sprintf "Pol.set_point: point %s does not satisfy constraint %s"
                (Vec.to_string Var.to_string point)
                (Cs.to_string Var.to_string cstr))
        )
        (get_cstr p)
    ;
    {p with point = Some (Vector.Symbolic.ofRat point)}

let get_regions : 'c Factory.t -> 'c t -> 'c t list
    = fun factory p ->
	Debug.log DebugTypes.Title (lazy "Getting regions");
	Debug.log DebugTypes.MInput (lazy (Printf.sprintf "P = %s"
			(to_string_raw p)))
	;
    Profile.start "get_regions";
    let point = get_point p |> Rtree.map Vec.ofSymbolic in
    Debug.log DebugTypes.Normal (lazy (Printf.sprintf "Normalization point generated: %s"
			(Vec.to_string Var.to_string point)))
	;
    let regions = get_regions_from_point factory p point in
    Profile.stop "get_regions";
    regions

let size : 'c t -> Scalar.Rat.t option
    = fun p ->
    Debug.log DebugTypes.Title (lazy "Size");
	Debug.log DebugTypes.MInput (lazy (Printf.sprintf "P = %s"
    	(to_string_raw p)));
    Profile.start "size";
    let cstrs = List.map Cons.get_c p.ineqs.ineqs in
    let res = match Opt.getAsg_raw cstrs with
    | Some point ->
        let point' =  Vector.Symbolic.toRat point in
        List.map (Cs.distance_point_cstr point') cstrs
        |> Misc.max Scalar.Rat.cmp
        |> fun x -> Some x
    | _ -> None
    in
    Profile.stop "size";
    Debug.log DebugTypes.MOutput (lazy
        (match res with
            | None -> "none"
            | Some r -> Printf.sprintf "%s: %f" (Scalar.Rat.to_string r) (Scalar.Rat.to_float r)));
    res

let split_in_half : 'c Factory.t -> 'c t -> Cs.t option
    = fun factory p ->
    Debug.log DebugTypes.Title (lazy "Splitting in half");
	Debug.log DebugTypes.MInput (lazy (Printf.sprintf "P = %s"
			(to_string_raw p)))
	;
    Profile.start "split_in_half";
    let var_set = varSet p in
    let nxt_var = Var.horizon var_set in
    let itvs = List.map
        (fun v ->
            let (itv,_,_) = itvizeSub factory nxt_var p (Vec.mk [Scalar.Rat.u, v]) in
            (v,itv))
        (Var.Set.elements var_set)
    in
    let (max_var, max_itv) = Misc.max
        (fun (_,itv1) (_,itv2) -> match length_itv itv1, length_itv itv2 with
            | None, _ -> -1
            | _, None -> 1
            | Some l1, Some l2 -> Scalar.Rat.cmp l1 l2)
        itvs
    in
    match length_itv max_itv with
    | None -> (Profile.stop "split_in_half"; None)
    | Some length -> begin
        let cste = match get_low max_itv with
        | Infty -> Stdlib.failwith "split_in_half: unexpected unbounded lower bound"
        | Closed r | Open r -> Scalar.Rat.add r (Scalar.Rat.divr length (Scalar.Rat.of_int 2))
        in
        Profile.stop "split_in_half";
        Some (Cs.mk Cstr_type.Le [Scalar.Rat.u, max_var] cste)
        end

let satisfy : 'c t -> Vec.t -> bool
    = fun p point ->
    EqSet.satisfy p.eqs point && IneqSet.satisfy p.ineqs point

let spawn : 'c Factory.t -> 'c t -> Vec.t
    = let rec spawn_rec : 'c t -> (Var.t * float * float) list -> Vec.t
        = fun p itvs ->
        let point = List.map
            (fun (v,low,up) ->
                (Random.float (up -. low) +. low |> Scalar.Rat.of_float, v)
            )
            itvs
            |> Vec.mk
        in
        if IneqSet.satisfy p.ineqs point
        then point
        else spawn_rec p itvs
    in
    fun factory p ->
    let itvs = List.map
        (fun var ->
            let (itv,_,_) = itvize factory p (Vec.mk [Scalar.Rat.u, var]) in
            let low = match itv.low with
            | Infty -> -1. *. max_float
        	| Open r | Closed r -> Scalar.Rat.to_float r
            and up = match itv.up with
            | Infty -> max_float
        	| Open r | Closed r -> Scalar.Rat.to_float r
            in
            (var, low, up))
        (get_cstr_ineqs p |> Cs.getVars |> Var.Set.elements)
    in
    let point = spawn_rec p itvs in
    List.fold_left
        (fun point (var, cons) ->
            let cstr = { (Cons.get_c cons) with
                Cs.v = Vec.add (Cs.get_v (Cons.get_c cons)) (Vec.mk [Scalar.Rat.negU, var]);
            } in
            let coeff = Cs.eval cstr point in
            Vec.set point var coeff
        )
        point p.eqs

let smart_proj_incl : 'c Factory.t -> Var.t -> Var.t list -> 'c t -> 'c t -> 'c t option
    = fun factory nxtVar vars_to_project p1 p2 ->
    let msk = List.fold_left (fun m x ->
        Rtree.set None m x (Some x)
    ) Rtree.Nil vars_to_project in
	let rec findEq eqs ineqs point vars =
		let (opte, eqs1) = EqSet.trySubstM factory msk eqs in
		match opte with
		| Some (e, x) ->
			let ineqs1 = IneqSet.subst factory nxtVar eqs1 x e ineqs in
            let vec = Cons.get_c e |> Cs.get_v |> Vector.Rat.neg |> Vector.Symbolic.ofRat in
            let point1 = Vector.Symbolic.elim x vec point in
            let vars' = try
                let i = Misc.findi (Var.equal x) vars in
                Misc.popi vars i
                with Not_found -> vars
            in
			findEq eqs1 ineqs1 point1 vars'
		| None -> (eqs, ineqs, point, vars)
	in
	let (eqs1, ineqs1, point, vars_to_project') = findEq p1.eqs p1.ineqs (get_point p1) vars_to_project in
    let normalization_point = Vector.Symbolic.toRat point in
    match IneqSet.proj_incl factory normalization_point vars_to_project' eqs1 ineqs1 p2.ineqs with
    | None -> None
    | Some ineqs -> Some {eqs = eqs1; ineqs = ineqs; point = Some point}

let proj_incl : 'c Factory.t -> 'c t -> 'c t -> 'c t option
    = fun factory p1 p2 ->
    let var_set_p1 = varSet p1
    and var_set_p2 = varSet p2 in
    let vars_to_project = Var.Set.diff var_set_p1 var_set_p2
        |> Var.Set.elements
    and nxtVar = Var.horizon var_set_p1 in
    if !Flags.smart_proj_incl
    then smart_proj_incl factory nxtVar vars_to_project p1 p2
    else let p1' = projectSub factory nxtVar p1 vars_to_project in
        match incl factory p2 p1' with
        | NoIncl -> None
        | Incl _ -> Some p1'

let assume_back : 'c Factory.t -> 'c t -> 'c Cons.t -> 'c t option
    = fun factory p cons ->
	match Cons.get_c cons |> Cs.get_typ with
	| Eq -> invalid_arg "assume_back: equations are not handled yet"
	| _ -> match p.point with
        | Some point -> begin
			match IneqSet.assume_back factory p.ineqs cons point with
			| Some (ineqs', point) -> Some { p with
				ineqs = ineqs';
				point = Some point;
			}
			| None -> None
		end
		| None -> invalid_arg "assume_back: input polyhedron has no normalization point set"
