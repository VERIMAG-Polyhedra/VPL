(** Intervalization oracle. *)

open OraclePattern

(** Builds an {!module-type:OracleType} from a*)
module Oracle (T : IOtypes.Type) = struct

    open T

    module P = D.Poly
    module M = D.Poly.Monomial
    module MB = D.Poly.MonomialBasis

    type prophecy = {
        env : env; (** Function from variables to static intervals. *)
        mode : IOtypes.mode; (** Side of approximation. *)
        current_mb : MB.t MapMonomial.t; (** Current state of the monomial *)
        var_to_keep : (Var.t option) MapMonomial.t; (** Variable to keep. *)
        map_itv : (int MapV.t) MapMonomial.t; (** Map from monomials to te number of times they are intervalized in the monomial.*)
        terms: D.BasicTerm.term list; (** List of terms that will replace the monomial. *)
    }

    module type Prayer = sig
        val name : string
        type pneuma
        val kill_when_fail : bool
        val pray : P.t -> prophecy -> pneuma option
        val inhale : P.t -> prophecy -> pneuma -> P.t * prophecy
    end

    let recursive_oracle : (P.t -> prophecy -> (module Prayer) list -> prophecy) ref
        = ref (fun _ pr _ -> pr)

    (** @return true if the sign of the variable is changed by the monomial. *)
    let sign_change : M.t -> Var.t -> env -> bool
        = fun (mb,c) var env ->
        let mb' = MB.remove_var_exp var 1 mb in
        (* All intervals must be either positive or negative. *)
        List.for_all (fun (v,_) ->
            let itv = Itv.of_var env v in
            match Itv.low itv, Itv.up itv with
            | Some l, Some u ->
                P.Coeff.le (P.n_to_coeff u) P.Coeff.z
                || P.Coeff.le P.Coeff.z (P.n_to_coeff l)
            | None, Some u -> P.Coeff.le (P.n_to_coeff u) P.Coeff.z
            | Some l, None -> P.Coeff.le P.Coeff.z (P.n_to_coeff l)
            | _,_ -> false
        ) mb'
        &&
        (* Counting the number of possible negative value: *)
        let sign = List.fold_left (fun sign (v,e) ->
            let itv = Itv.of_var env v in
            match Itv.up itv with
            | Some x when P.Coeff.le (P.n_to_coeff x) P.Coeff.z -> sign + e
            | None -> sign + e
            | _ -> sign
        ) 0 mb'
        in
        (sign + (if P.Coeff.lt c P.Coeff.z then 1 else 0)) mod 2 = 1

    let get_map_itv : MB.t -> prophecy -> int MapV.t
        = fun mb pr ->
        MapMonomial.find mb pr.map_itv

    let get_itv_degree : MB.t -> Var.t -> prophecy -> int
        = fun mb var pr ->
        let mapv = get_map_itv mb pr in
        try MapV.find var mapv
        with Not_found -> 0

    let get_var_to_keep : MB.t -> prophecy -> Var.t option
        = fun mb pr ->
        MapMonomial.find mb pr.var_to_keep

    let get_current_mb : MB.t -> prophecy -> MB.t
        = fun mb pr ->
        MapMonomial.find mb pr.current_mb

    let set_var_to_keep : MB.t -> Var.t -> prophecy -> prophecy
        = fun mb var pr -> {pr with
            var_to_keep = MapMonomial.add mb (Some var) pr.var_to_keep;
        }

    (** @return the current representation of the given monomial, taking into account its intervalized variables. *)
    let update_monomial : MB.t -> prophecy -> MB.t
        = fun mb pr ->
        MapV.fold (fun var i mb ->
            MB.remove_var_exp var i mb
        ) (get_map_itv mb pr) mb

    let set_current_mb : MB.t -> prophecy -> prophecy
        = fun mb pr ->
        let mb' = update_monomial mb pr in {pr with
            current_mb = MapMonomial.add mb mb' pr.current_mb
        }

    (** Adds an intervalization to a variable for a given monomial.
        @param pr the initial prophecy
        @param mb the monomial basis
        @param var the variable th annotate
        @param i the number of time the variable must be intervalized
        @return the prophecy where [var] has been annoted with [annot] in [mb] *)
    let add_itv : prophecy -> MB.t -> Var.t -> int -> prophecy
        = fun pr mb var i ->
        let mapv = get_map_itv mb pr in
        let to_add = try MapV.find var mapv
            with Not_found -> 0
        in
        let mapv' = MapV.add var (to_add + i) mapv
        in {pr with
            map_itv = (MapMonomial.add mb mapv' pr.map_itv)
        }

    module Multiplicity : Prayer = struct
        (* Variable to intervalize. *)
        type pneuma = ((MB.t * int) list * Var.t)

        let name = "Multiplicity"

        let kill_when_fail = false

        let get_monomial_multiplicity var mb =
            let i = MB.get_exponent var mb in
            if i = MB.degree mb
            then i - 1 (* one variable must remain *)
            else i

        let get_multiplicity var p =
            List.fold_left (fun acc (mb,_) ->
                acc + (get_monomial_multiplicity var mb)
            ) 0 p

        let pray p pr =
            try
                let var =
                    let p' = List.map (fun (mb,c) ->
                        (get_current_mb mb pr, c)
                    ) p in
                    List.map
                        (fun v -> (v,get_multiplicity v p'))
                        (P.get_vars p' |> Var.Set.elements)
                    |> List.fast_sort (fun (_,i1) (_,i2) -> Stdlib.compare i1 i2)
                    |> List.rev
                    |> fun l -> match l with
                        | [] | [_] -> Stdlib.raise Not_found
                        | (v1,i1) :: (_,i2) :: _ -> if i1 > i2 then v1 else Stdlib.raise Not_found
                in
                let l = List.map
                    (fun (mb,_) ->
                        (mb, get_monomial_multiplicity var (get_current_mb mb pr))
                    )
                    (P.data p)
                |> List.filter
                    (fun (_, i) -> i > 0)
                in Some (l, var)
            with Not_found -> None

            let inhale p pr (l,var) =
                let pr' = List.fold_left (fun pr (mb, i) ->
                    add_itv pr mb var i
                    |> set_current_mb mb
                ) pr l
                in
                (p,pr')
    end

    (** A monomial is linear. *)
    module LinearMonomial : Prayer = struct
        (** A linear monomial and its single variable. *)
        type pneuma = M.t * Var.t

        let name = "Linear Monomial"

        let kill_when_fail = false

        let rec pray p pr =
            match p with
            | [] -> None
            | (mb,_) as m :: p' ->
                let current_mb = get_current_mb mb pr in
                if get_var_to_keep mb pr = None
                    && MB.degree current_mb = 1
                then let (var,_) = List.hd current_mb in
                    Some (m, var)
                else pray p' pr

        let inhale p pr ((mb,_),var) =
            let pr' = set_var_to_keep mb var pr
            |> set_current_mb mb in
            (p, pr')

    end

    (** Other prophecies can generate constant monomials. *)
    module ConstantMonomial : Prayer = struct

        (** The constant monomial*)
        type pneuma = M.t

        let name = "Constant Monomial"

        let kill_when_fail = false

        let rec pray p pr =
            match p with
            | [] -> None
            | ((mb,_) as m) :: p' ->
                if MB.degree mb = 0
                then Some m
                else pray p' pr

        let inhale p pr ((mb,c) as m) =
            let pr' = {pr with
                terms = Term.of_monomial (mb,c) :: pr.terms
            } in
            let p' = P.sub p [m] in
            (p', pr')

    end

    (** A constant variable can be statically intervalized. *)
    module VarCste : Prayer = struct

        (** Variable to intervalize. *)
        type pneuma = (MB.t * Var.t * int)

        let name = "Constant Variable"

        let kill_when_fail = true

        let rec pray p pr =
            match p with
            | [] -> None
            | (mb,_) :: p' ->
                if get_var_to_keep mb pr = None
                then try
                    let vars = MB.get_vars (get_current_mb mb pr) in
                    let var = Var.Set.find_first (fun var ->
                        (* The variable is constant. *)
                        Itv.of_var pr.env var
                        |> Itv.range
                        |> P.Coeff.equal P.Coeff.z
                    ) vars in
                    if Var.Set.cardinal vars > 1
                    then Some (mb, var, MB.get_exponent var mb)
                    else Some (mb, var, MB.get_exponent var mb - 1)
                with Not_found -> pray p' pr
            else pray p' pr

        let inhale p pr (mb, var, i) =
            let pr' = add_itv pr mb var i
                |> set_current_mb mb in
            (p, pr')
    end

    (** All variables are bounded, and we keep the variable with the greatest interval. *)
    module GreatestInterval : Prayer = struct

        (** Variable to keep. *)
        type pneuma = (MB.t * Var.t)

        let name = "Greatest Interval"

        let kill_when_fail = false

        let rec pray p pr =
            match p with
            | [] -> None
            | (mb,_) :: p' ->
                if get_var_to_keep mb pr = None
                then
                    let current_mb = get_current_mb mb pr in
                    let vars = MB.get_vars current_mb in
                    if not (Var.Set.is_empty vars)
                        && Var.Set.for_all (fun v ->
                        Itv.of_var pr.env v
                        |> Itv.is_bounded
                    ) vars
                    then match Itv.greatest current_mb pr.env with
                        | None -> pray p' pr
                        | Some var -> Some (mb, var)
                    else pray p' pr
                else pray p' pr

        let inhale p pr (mb, var) =
            let pr' = set_var_to_keep mb var pr
                |> set_current_mb mb in
            (p, pr')

    end

    (** Exactly one variable is unbounded in the monomial. *)
    module OneUnboundedVar : Prayer = struct

        (** Variable to keep. *)
        type pneuma = (MB.t * Var.t)

        let name = "One Unbounded Variable"

        let kill_when_fail = false

        let rec pray p pr =
            match p with
            | [] -> None
            | (mb,_) :: p' ->
            if get_var_to_keep mb pr = None
            then
                let vars = MB.get_vars (get_current_mb mb pr) in
                let vars_unbounded = Var.Set.filter (fun var ->
                    not (Itv.is_bounded (Itv.of_var pr.env var))
                ) vars in
                if Var.Set.cardinal vars_unbounded = 1
                then
                    let var = Var.Set.find_first (fun _ -> true) vars_unbounded in
                    Some (mb, var)
                else pray p' pr
            else pray p' pr

        let inhale p pr (mb, var) =
            let pr' = set_var_to_keep mb var pr
                |> set_current_mb mb in
            (p, pr')
    end

    (** Keeps the first unbounded variable.
        Otherwise, take the greatest interval. *)
    module FirstUnbounded : Prayer = struct
        (** Variable to keep. *)
        type pneuma = (MB.t * Var.t)

        let name = "FirstUnbounded"

        let kill_when_fail = false

        let rec pray p pr =
            match p with
            | [] -> None
            | (mb,_) :: p' ->
                if get_var_to_keep mb pr = None
                then
                    let current_mb = get_current_mb mb pr in
                    let vars = MB.get_vars current_mb in
                    try
                        let var = Var.Set.find_first (fun var ->
                            not (Itv.is_bounded (Itv.of_var pr.env var))
                        ) vars in
                        Some (mb, var)
                    with Not_found -> match Itv.greatest current_mb pr.env with
                        | None -> pray p' pr
                        | Some var -> Some (mb, var)
                else pray p' pr

        let inhale p pr (mb, var) =
            let pr' = set_var_to_keep mb var pr
                |> set_current_mb mb in
            (p, pr')
    end

        (** Always applicable.
            Keeps the first variable. *)
        module Default : Prayer = struct
            type pneuma = M.t

            let name = "Default"

            let kill_when_fail = false

            let pray p _ = Some (List.hd p)

            let inhale p pr (mb,c) =
                let var_to_keep = match get_var_to_keep mb pr with
                | Some var -> var
                | None -> update_monomial mb pr
                    |> List.hd
                    |> Stdlib.fst
                in
                let aff = D.BasicTerm.annotAFFINE
                    (Term.of_monomial (M.mk_list [var_to_keep,1] c)) in
                let m = MB.remove_var_exp var_to_keep 1 mb
                    |> Term.of_monomialBasis
                    |> D.BasicTerm.smartAnnot ASTerm.TopLevelAnnot.INTERV
                in
                let value = D.BasicTerm.smartMul m aff in
                let pr' = {pr with
                    terms = value :: pr.terms;
                }
                and p' = P.sub p [mb,c] in
                (p', pr')

        end

    (** Alternative to {!module:Default}.
        Remaining variables are centered onto zero. *)
    module Focusing = struct
        type pneuma = M.t

        let name = "Focusing"

        let kill_when_fail = false

        let pray p _ = Some (List.hd p)

        let inhale p pr (mb,c) =
            let current_mb = get_current_mb mb pr in
            let var_to_keep = match get_var_to_keep mb pr with
                | Some var -> var
                | None -> List.hd current_mb
                    |> Stdlib.fst
            in
            let vars = MB.remove_var_exp var_to_keep 1 current_mb
                |> MB.get_vars
                |> Var.Set.filter (fun var ->
                    let itv = Itv.of_var pr.env var in
                    Itv.is_bounded itv
                  &&
                    not (Itv.contains_zero itv)
                )
                |> Var.Set.elements
            in
            let tlist = List.map (fun var ->
                let bound = Itv.of_var pr.env var
                    |> Itv.get_translation_bound in
                (var,bound)
            ) vars
            |> Term.focusing (mb,c) var_to_keep
            in
            let pr' = {pr with
                terms = tlist @ pr.terms;
            }
            and p' = P.sub p [mb,c] in
            (p', pr')
    end

    (** When a variable is chosen, skip all prophecies. *)
    module VarChosen : Prayer = struct
        type pneuma = Focusing.pneuma

        let name = "VarChosen"

        let kill_when_fail = false

        let rec pray p pr =
            match p with
            | [] -> None
            | (mb,_) as m :: p' ->
                match get_var_to_keep mb pr with
                | Some _ -> Some m
                | None -> pray p' pr

        let inhale = Focusing.inhale

    end

    let prayers : (module Prayer) list = [
        (module VarChosen);
        (module ConstantMonomial);
        (module LinearMonomial);
        (module VarCste);
        (module OneUnboundedVar);
        (module Multiplicity);
        (module GreatestInterval);
        (module FirstUnbounded);
        (module Focusing);
        (module Default); (* Either Focusing or Default is mandatory: they produce terms *)
    ]

end

module Make (T : IOtypes.Type) = struct
    include Make(Oracle(T))

    include T

    let get_mode : linearizeContext -> IOtypes.mode
    	= fun lc ->
    	match lc.cmp with
    	| NumC.Le | NumC.Lt -> DomainInterfaces.UP
    	| _ -> DomainInterfaces.BOTH

    let run_oracle : env -> IOtypes.mode -> P.t -> D.BasicTerm.term list
        = fun env mode p ->
        let init_prophecy : prophecy = {
            env = env;
            mode = mode;
            current_mb = (List.fold_left (fun map (mb,_) ->
                MapMonomial.add mb mb map)
                MapMonomial.empty p);

            var_to_keep = (List.fold_left (fun map (mb,_) ->
                MapMonomial.add mb None map)
                MapMonomial.empty p);

            map_itv = (List.fold_left (fun map (mb,_) ->
                MapMonomial.add mb MapV.empty map)
                MapMonomial.empty p);
            terms = [];
        } in
        let pr' = run init_prophecy p in
        pr'.terms

    (** Factorizes a list of terms.
        Each term must have the form interv(...)*...*interv(...)*affine(cste * variable à guarder + cste) *)
    let rec factorize : D.BasicTerm.term list -> D.BasicTerm.term
    	= function
        | [] -> Term.zero
        | (t :: tl) as terms ->
            match Term.get_interv_part_opt t with
            | None -> D.BasicTerm.smartAdd t (factorize tl)
            | Some t' ->
        		let (l1,l2) = List.partition (fun term ->
                    match Term.get_interv_part_opt term with
                    | None -> false
                    | Some term -> P.equal
                        (Term.to_polynomial term |> P.data |> P.mk)
                        (Term.to_polynomial t' |> P.data |> P.mk)
                    ) terms
                in
                let sum = List.map (fun y ->
                        match Term.get_affine_part_opt y with
                        | Some t -> t
                        | None ->
                            if D.BasicTerm.isCte t
                            then t
                            else Stdlib.failwith (Printf.sprintf "factorisation : %s -> non-constant term with no affine part" (Term.to_string t))
                    ) l1
            		|> List.fold_left D.BasicTerm.smartAdd Term.zero
                in
        		D.BasicTerm.smartAdd
                    (factorize l2)
            		(D.BasicTerm.smartMul
                        (D.BasicTerm.smartAnnot ASTerm.TopLevelAnnot.INTERV t')
                        (D.BasicTerm.annotAFFINE sum))

    let factorize_affine : D.BasicTerm.term -> D.BasicTerm.term
    	= let rec get_terms : D.BasicTerm.term -> (D.BasicTerm.term * D.BasicTerm.term) list
    		= fun t ->
    		match t with
    		| D.BasicTerm.Add (x1,x2) -> (get_terms x1) @ (get_terms x2)
    		| _ -> [(Term.get_affine_part t, Term.get_interv_part t)]
    	in
        let rec factorize_affine_rec : (D.BasicTerm.term * D.BasicTerm.term) list -> D.BasicTerm.term
    		= function
    		| [] -> Term.zero
    		| (t_aff,t_itv) :: tl ->
    			let (l1,l2) = List.partition
    				(fun (t'_aff,_) ->
                        P.equal (Term.to_polynomial t_aff) (Term.to_polynomial t'_aff))
    				tl in
    			D.BasicTerm.smartAdd
    			(D.BasicTerm.smartMul
    				(D.BasicTerm.smartAnnot
    				ASTerm.TopLevelAnnot.INTERV
    				(List.fold_left
    					(fun t1 (_,t'_itv) -> D.BasicTerm.smartAdd t1 t'_itv)
    					t_itv
    					l1))
    				(D.BasicTerm.annotAFFINE t_aff))
    			(factorize_affine_rec l2)
    	in
        fun t ->
    	factorize_affine_rec (get_terms t)

    let rec contains_annot : Term.t -> bool
        = function
        | Var _
        | Cte _ -> false
        | Opp t -> contains_annot t
        | Add (t1,t2)
        | Mul (t1,t2) -> contains_annot t1 || contains_annot t2
        | Annot (_,_) -> true

    let rec simplify : Term.t -> Term.t
        = function
        | Var _ as t -> t
        | Cte _ as t -> t
        | Add (t1,t2) as t ->
            if not (contains_annot t1) && not (contains_annot t2)
            then Term.to_polynomial t |> Term.of_polynomial
            else Add(simplify t1, simplify t2)
        | Mul (t1,t2) as t ->
            if not (contains_annot t1) && not (contains_annot t2)
            then Term.to_polynomial t |> Term.of_polynomial
            else Mul(simplify t1, simplify t2)
        | Opp t' as t ->
            if not (contains_annot t')
            then Term.to_polynomial t |> Term.of_polynomial
            else Opp (simplify t')
        | Annot (a,t') -> Annot(a, simplify t')

    let rec translateAF : Term.t -> env -> Term.t
    	= let translate : Term.t -> Term.t -> D.N.t -> env -> Term.t
    		= fun t_aff t_interv c env->
    		let t_interv' = (D.BasicTerm.smartScalMul c t_interv)
    		|> Term.to_polynomial
    		|> run_oracle env DomainInterfaces.BOTH
    		|> factorize in
    		D.BasicTerm.smartAdd
    			(D.BasicTerm.smartMul
    				t_interv
    				(D.BasicTerm.annotAFFINE
    					(D.BasicTerm.smartAdd
    						t_aff
    						(D.BasicTerm.Opp (D.BasicTerm.Cte c)))))
    			t_interv'
    	in
        let translateAF_mul : Term.t -> env -> Term.t
    		= fun t env ->
    		let t_aff = Term.get_affine_part t
            and t_interv = Term.get_interv_part t in
    		let itv = Itv.of_term t_aff env in
    		if Itv.is_bounded itv && not (Itv.contains_zero itv)
    		then translate t_aff t_interv (Itv.get_translation_bound itv) env
    		else t
    	in fun t env ->
    	match t with
    	| D.BasicTerm.Mul(_,_) -> translateAF_mul t env
    	| D.BasicTerm.Add(t1,t2) -> D.BasicTerm.smartAdd (translateAF t1 env) (translateAF t2 env)
    	| _ -> t

    let oracle: linearizeContext -> D.Term.t ImpureConfig.Core.Base.imp
    	= fun lc ->
    	let p = lc.nonaffine
        and env = lc.env in
    	let p' = (p |> Term.to_polynomial) in
    	Debug.log DebugTypes.Title (lazy "Intervalization Oracle");
    	Debug.log DebugTypes.MInput
    		(lazy (Printf.sprintf "polynomial : %s\nIntervals : %s"
    		(P.to_string p')
    		(Misc.list_to_string
    			(fun v -> Printf.sprintf "%s -> %s"
                    (Var.to_string v)
                    (Itv.of_var env v |> Itv.to_string)
                )
                (P.get_vars p' |> Var.Set.elements) ",")))
        ;
        let terms = run_oracle env (get_mode lc) p' in
    	Debug.log DebugTypes.Normal
    		(lazy (Printf.sprintf "Prayer before factorization : \n%s"
    		(Misc.list_to_string Term.to_string terms ",\n\t")));
    	let term = factorize terms in
    	Debug.log DebugTypes.Normal
    		(lazy (Printf.sprintf "Polynomial after factorization : %s"
    		(Term.to_string term)));
    	let term' = factorize_affine term in
    	Debug.log DebugTypes.Normal
    		(lazy (Printf.sprintf "Polynomial after factorization of affine terms : %s"
    		(Term.to_string term')));
    	let result = translateAF term' env in
    	Debug.log DebugTypes.MOutput
    		(lazy (Printf.sprintf "polynomial : %s"
    		(Term.to_string result)));
        let term_simpl = simplify result in
    	Debug.log DebugTypes.Normal
            (lazy (Printf.sprintf "Polynomial after simplification : %s"
            (Term.to_string term_simpl)));
    	term_simpl

end

module OracleZ = Make(IOtypes.DZ)
module OracleQ = Make(IOtypes.DQ)

let oracleZ : ASTerm.linearizeContext -> ASTerm.ZTerm.t ImpureConfig.Core.Base.imp
    = fun lc ->
    OracleZ.oracle {
        nonaffine = lc.ASTerm.nonaffine;
        env = lc.ASTerm.env;
        affine = lc.ASTerm.affine;
        source = lc.ASTerm.source;
        cmp = lc.ASTerm.cmp;
    }

let oracleQ = OracleQ.oracle
