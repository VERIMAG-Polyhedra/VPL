open IOtypes

module Lift (T : Type) = struct

    include T

    module P = D.Poly
    module M = D.Poly.Monomial
    module MB = D.Poly.MonomialBasis

    module Heuristic = IHeuristic.Lift (T)
    module Pattern = Heuristic.Pattern

    let map_to_string : 'a MapMonomial.t -> ('a -> string) -> string
    	= fun m to_string ->
    	(String.concat " ; " (List.map
    		(fun (k,a) -> String.concat "" ["("; MB.to_string k ;" -> "; to_string a ;")"])
    		(MapMonomial.bindings m)))

    (* choix d'une variable à garder par monome *)
    let choose_var: P.t -> env -> mode -> Heuristic.prophecy
    	= let rec choose_var_rec: P.t -> Heuristic.prophecy
            -> Var.t MapMonomial.t -> (AnnotedVar.t list) MapMonomial.t -> env -> mode
            -> Heuristic.prophecy
    		= fun p pro mapKeep mapNKeep env mode ->
    		if P.isZ p
    		then pro
    		else
    			let (pro', mapKeep',mapNKeep',p') = Pattern.matching p env mode mapKeep mapNKeep
    				|> Heuristic.of_pattern p env mode mapKeep mapNKeep
    			in
    			Debug.log DebugTypes.Normal (lazy "Oracle - recursive call");
    			Debug.log DebugTypes.Detail
    				(lazy (Printf.sprintf "p = %s, \nvariables marked \"to_keep\" = %s, \nvariables marked \"to intervalize\" = %s\nparts already treated : %s\n"
    				(P.to_string p') (map_to_string mapKeep' Var.to_string)
    				(map_to_string mapNKeep' (fun l -> Misc.list_to_string AnnotedVar.to_string l ","))
    				(Misc.list_to_string Term.to_string (pro @ pro') ",")));
    			choose_var_rec p' (pro @ pro') mapKeep' mapNKeep' env mode
    	in
    	fun p env mode ->
    	choose_var_rec p [] MapMonomial.empty MapMonomial.empty env mode

    (* Pour que la factorisation fonctionne :
    	Chaque terme doit être de la forme interv(...)*...*interv(...)*affine(cste * variable à guarder + cste)
    *)
    let rec factorize : Heuristic.prophecy -> D.BasicTerm.term
    	= fun pro ->
    	if List.length pro = 0
    	then Term.zero
    	else let t = (List.hd pro) in
    		let t' = Term.get_interv_part t in
    		let (l1,l2) = pro
    		|> List.partition (fun y -> try P.equal
    			(Term.get_interv_part y |> Term.to_polynomial |> P.data |> P.mk)
    			(t' |> Term.to_polynomial |> P.data |> P.mk)
    			with Not_found -> false)
            in
    		D.BasicTerm.smartAdd
    		(factorize l2)
    		(D.BasicTerm.smartMul
    		(D.BasicTerm.smartAnnot ASTerm.TopLevelAnnot.INTERV t')
    		(D.BasicTerm.annotAFFINE
    		(l1 |> List.map (fun y -> try Term.get_affine_part y with
    			Not_found -> if D.BasicTerm.isCte t then t else Pervasives.failwith (Printf.sprintf "factorisation : %s -> non-constant term with no affine part" (Term.to_string t)))
    		|> List.fold_left
    		D.BasicTerm.smartAdd
    		Term.zero)))

    let rec sum : Heuristic.prophecy -> D.BasicTerm.term
    	= fun pro ->
    	if List.length pro = 0
    	then Term.zero
    	else D.BasicTerm.smartAdd
    	(sum (List.tl pro))
    	(List.hd pro)

    let get_mode : linearizeContext -> mode
    	= fun lc ->
    	match lc.cmp with
    	| NumC.Le | NumC.Lt -> DomainInterfaces.UP
    	| _ -> DomainInterfaces.BOTH

    let rec translateAF : Term.t -> env -> Term.t
    	= let translate : Term.t -> Term.t -> D.N.t -> env -> Term.t
    		= fun t_aff t_interv c env->
    		let t_interv' = (D.BasicTerm.smartScalMul c t_interv)
    		|> Term.to_polynomial
    		|> fun p -> choose_var p env DomainInterfaces.BOTH
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

    let factorize_affine : D.BasicTerm.term -> D.BasicTerm.term
    	= let rec get_terms : D.BasicTerm.term -> (D.BasicTerm.term * D.BasicTerm.term) list
    		= fun t ->
    		match t with
    		| D.BasicTerm.Add (x1,x2) -> (get_terms x1) @ (get_terms x2)
    		| _ -> [(Term.get_affine_part t, Term.get_interv_part t)]
    	in
        let rec factorize_affine_rec : (D.BasicTerm.term * D.BasicTerm.term) list -> D.BasicTerm.term
    		= fun l ->
    		match l with
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

    let (oracle: linearizeContext -> D.Term.t ImpureConfig.Core.Base.imp)
    	= fun lc ->
    	let p = lc.nonaffine
        and env = lc.env
        and mo = get_mode lc in
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
                (P.get_vars p') ",")))
        ;
    	let pro = choose_var p' env mo in
    	Debug.log DebugTypes.Normal
    		(lazy (Printf.sprintf "Prophecy before factorization : \n%s"
    		(Misc.list_to_string Term.to_string pro ",\n\t")));
    	let pro' = factorize pro in
    	Debug.log DebugTypes.Normal
    		(lazy (Printf.sprintf "Polynomial after factorization : %s"
    		(Term.to_string pro')));
    	let t = factorize_affine pro' in
    	Debug.log DebugTypes.Normal
    		(lazy (Printf.sprintf "Polynomial after factorization of affine terms : %s"
    		(Term.to_string t)));
    	let result = translateAF t env in
    	Debug.log DebugTypes.MOutput
    		(lazy (Printf.sprintf "polynomial : %s"
    		(Term.to_string result)));
    	result
end

module OracleZ = Lift(DZ)
module OracleQ = Lift(DQ)

let oracleZ : ASTerm.linearizeContext -> ASTerm.ZTerm.t ImpureConfig.Core.Base.imp
    = fun lc ->
    OracleZ.oracle {
        OracleZ.nonaffine = lc.ASTerm.nonaffine;
        OracleZ.env = lc.ASTerm.env;
        OracleZ.affine = lc.ASTerm.affine;
        OracleZ.source = lc.ASTerm.source;
        OracleZ.cmp = lc.ASTerm.cmp;
    }

let oracleQ = OracleQ.oracle
