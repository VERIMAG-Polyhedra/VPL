module CP = CstrPoly
module Cs = CP.Cs
module Poly = CP.Poly

let get_vars : CP.t list -> Var.t list
	= fun l ->
    List.fold_left (fun acc cp ->
        Poly.get_vars cp.CP.p
        |> Var.Set.union acc
    ) Var.Set.empty l
    |> Var.Set.elements

(** [add_vars vl l] returns the list of variable appearing in [vl] and in polynomial cosntraints [l]. *)
let add_vars : Var.t list -> CP.t list -> Var.t list
	= fun vl l ->
    let set1 = List.fold_left (fun acc cp ->
        Poly.get_vars cp.CP.p
        |> Var.Set.union acc
    ) Var.Set.empty l
    and set2 = Var.Set.of_list vl in
    Var.Set.union set1 set2
    |> Var.Set.elements

type 'c t = {
    poly_rep : CP.t list;
    vpl_rep : 'c Pol.t option;
    vars : Var.t list;
}

let to_string : 'c t -> string
	= fun p ->
    Misc.list_to_string CP.to_string p.poly_rep " ; "

let get_noneq_poly : 'c t -> CP.t list
	= fun p ->
    List.filter (fun cp ->
        cp.CP.typ <> Cstr_type.Eq
    ) p.poly_rep

let get_cstrs : 'c t -> Cs.t list
	= fun p ->
	match p.vpl_rep with
	| None -> []
	| Some p -> Pol.get_cstr p

let get_ineqs : 'c t -> Cs.t list
	= fun p ->
	match p.vpl_rep with
	| None -> []
	| Some p -> Pol.get_cstr_ineqs p

let horizon : 'c t -> Var.t
	= fun p ->
	Misc.max Var.cmp p.vars
		|> Var.next

let is_empty : 'c t -> bool
	= fun p ->
    match p.vpl_rep with
	| None -> true
	| _ -> false

let empty : 'c t
	= {
        vpl_rep = None;
        poly_rep = [];
		vars = [];
    }

let mk : 'c Pol.t -> CP.t list -> Var.t list -> 'c t
	= fun vpl cl variables -> {
    	vpl_rep = Some vpl;
    	poly_rep = cl;
    	vars = variables;
    }

let mkPol : 'c Pol.t -> 'c t
	= fun vpl_rep ->
	let cstrs = Pol.get_cstr vpl_rep in
    let poly_rep = List.map CP.ofCstr cstrs in
    mk vpl_rep poly_rep (get_vars poly_rep)

let addM : 'c Factory.t -> (CP.t * 'c) list -> 'c t -> 'c t
	= fun factory l p ->
	match p.vpl_rep with
	| None -> empty
	| Some vpl_rep ->
		let variables = add_vars p.vars (List.map Stdlib.fst l) in
		let conss = List.map (fun (cp,cert) -> (CP.toCstr cp, cert)) l in
    	match Pol.addM factory vpl_rep conss with
    		| Pol.Contrad _ -> empty
    		| Pol.Added p' ->
    			let cstrs = Pol.get_cstr p'
    				|> List.map CP.ofCstr in
    			mk p' cstrs variables

let equal : 'c1 Factory.t -> 'c2 Factory.t -> 'c1 t -> 'c2 t -> bool
	= fun factory1 factory2 p1 p2 ->
	match (p1.vpl_rep, p2.vpl_rep) with
	| (None,None) -> true
	| (Some p1, Some p2) ->	(match (Pol.incl factory1 p1 p2, Pol.incl factory2 p2 p1) with
		| (Pol.Incl _, Pol.Incl _) -> true
	| (_,_) -> false)
	| (_,_) -> false

let isInside : Poly.Vec.t -> 'c t -> bool
	= fun point p ->
	List.for_all
		(fun cstr -> CP.Cs.satisfy point cstr)
		(get_ineqs p)

let cstrInPol : Cs.t -> 'c t -> bool
	= fun cstr p ->
	List.exists
		(CP.Cs.equal cstr)
		(get_cstrs p)
