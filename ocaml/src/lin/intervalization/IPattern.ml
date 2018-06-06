open IOtypes

module Lift (T : Type) = struct

    open T

    module P = D.Poly
    module M = D.Poly.Monomial
    module MB = D.Poly.MonomialBasis

    module Misc = struct

    	let sign_changing : M.t -> P.V.t -> env -> bool
    		= fun m v env ->
            let (m,c) = M.data m in
    		let l = (Misc.pop P.V.equal (MB.data m) v) in
    		let l' = List.filter (fun v ->
                let itv = Itv.of_var env v in
                match (Itv.low itv, Itv.up itv) with
    			| (_, Some x) when P.Coeff.le (P.n_to_coeff x) P.Coeff.z-> true
    			| _ -> false)
                l
            in
    		(List.length l' + (if P.Coeff.lt c P.Coeff.z then 1 else 0)) mod 2 = 1

    	let is_unbounded : MB.t -> env -> bool
    		= fun m env ->
                let var_list = MB.data m in
        		List.exists
                    (fun v -> let itv = Itv.of_var env v in
                    match (Itv.low itv, Itv.up itv) with
                    | (None, None) -> true
                    | _ -> false)
                    var_list
    		||
        		try
                    let v = List.find
                        (fun v -> let itv = Itv.of_var env v in
                        match (Itv.low itv, Itv.up itv) with
                		| (None, Some _) | (Some _, None)-> true
                		| _ -> false) var_list
                    in
                	List.exists (fun v -> let itv = Itv.of_var env v in
                        match (Itv.low itv, Itv.up itv) with
                		| (Some x, Some y) when
                            P.Coeff.lt (P.n_to_coeff x) P.Coeff.z
                            && P.Coeff.lt P.Coeff.z (P.n_to_coeff y) -> true
                		| (None, Some y) when P.Coeff.lt P.Coeff.z (P.n_to_coeff y) -> true
                		| (Some x, None) when P.Coeff.lt (P.n_to_coeff x) P.Coeff.z -> true
                		| _ -> false)
                        (Misc.pop P.V.equal var_list v)
        		with Not_found -> false
    end

	type t =
	  UnboundedVar of M.t * P.V.t (* une seule variable est non-bornée dans le monôme *)
	| UnboundedVarmode of M.t * P.V.t (* variable non bornée mais du bon côté par rapport au mode *)
	| GreatestItv of M.t * P.V.t (* toutes les variables sont bornées et on garde la variable qui possède le plus grand intervalle *)
	| VarCte of M.t * P.V.t (* cette variable est cste *)
	| MonomialCte of M.t (* the monomial is a constant *)
	| LinearMonomial of M.t * P.V.t (* monôme linéaire *)
	| CenterZero of M.t (* on réécrit le monôme pour centrer les variables non gardées en 0 *)
	| Translation of M.t (* on réécrit le monôme en translatant des variables *)
	| Screwed (* toute intervalization STATIQUE du monôme donne [None, None], on appelle directement Default pour en finir plus rapidement *)
	| FirstUnbounded of M.t * P.V.t (* Garde la première variable non-bornée. S'il n'y en a pas, garde la variable avec le plus grand intervalle*)
	| Multiplicity of (M.t * int)list * P.V.t (* la variable a une grande multiplicité *)
	| Default of P.t (* On garde la première variable du monôme *)

	type matcher = (P.t -> env -> mode -> P.V.t MapMonomial.t -> (AnnotedVar.t list) MapMonomial.t -> t option)

	(* Marque une variable 'to_keep' *)
	let unboundedVar : matcher
		= fun p env _ mapKeep mapNKeep ->
		try let m = List.find
			(fun mono ->
                let (m',_) = M.data mono in
				not (MapMonomial.mem m' mapKeep) (* si le monôme n'a pas déjà de variable gardée *)
				&&
				(let l = List.find_all
					(fun v -> (* MAJ:not (Var.equal v Var.null) &&*) Itv.of_var env v |> Itv.is_bounded |> not)
					(AnnotedVar.update_monomial m' mapNKeep |> MB.data)
				 in
				 List.length l >= 1)) (* si le nombre de variable non bornée est 1*)
			(P.data p)
			in
            let var =
                let l = AnnotedVar.update_monomial (M.data m |> fst) mapNKeep
                    |> MB.data
                in
    			try
                    List.find
                    (fun v -> Itv.of_var env v |> Itv.is_fully_unbounded)
                    l
    			with Not_found ->
                    List.find
                    (fun v -> Itv.of_var env v |> Itv.is_bounded |> not)
                    l
            in
            Some (UnboundedVar (m,var))
		with Not_found -> None


	(* Marque une variable 'to_interv' *)
    let unboundedVarmode : matcher
		= fun p env mode _ mapNKeep ->
		match mode with
		| DomainInterfaces.BOTH -> None
		| _ -> begin try
				let mono = List.find (* premier cas *)
    				(fun mono ->
                        let (m',c') = M.data mono in
    					let m'' = (AnnotedVar.update_monomial m' mapNKeep) in
    					List.length (MB.data m'') > 0 (* il reste plus d'une variable *)
    				  &&
    					not (Misc.is_unbounded m'' env)
    				  &&
                        let v = List.find (fun v ->
                            let itv = Itv.of_var env v in
                            match (Itv.low itv, Itv.up itv) with
        					| (None, _) -> true
        					| _ -> false)
                            (MB.data m'')
                        in
                        let mono' = M.mk m'' c' in
    					match mode with
    					| DomainInterfaces.UP -> not (Misc.sign_changing mono' v env)
    					| DomainInterfaces.LOW -> Misc.sign_changing mono' v env
    					| DomainInterfaces.BOTH -> Pervasives.failwith "IPattern.unboundedVarmode"
                    )
    				(P.data p)
				in
                let m = M.data mono |> fst in
                let var = List.find
                    (fun v -> let itv = Itv.of_var env v in
                        match (Itv.low itv, Itv.up itv) with
    					| (None, _) -> true
    					| _ -> false)
                    (AnnotedVar.update_monomial m mapNKeep |> MB.data)
                in
                Some (UnboundedVarmode (mono, var))
			with Not_found ->
			try
				let mono = List.find (* second cas *)
				(fun mono ->
                    let (m',c') = M.data mono in
					let m'' = (AnnotedVar.update_monomial m' mapNKeep) in
                    List.length (MB.data m'') > 0
				  &&
					not (Misc.is_unbounded m'' env)
				  &&
                    let v = List.find
                        (fun v -> let itv = Itv.of_var env v in
                            match (Itv.low itv, Itv.up itv) with
                            | (_,None) -> true
                            | _ -> false)
                            (MB.data m'')
                    in
                    let mono' = M.mk m'' c' in
					match mode with
					| DomainInterfaces.UP -> Misc.sign_changing mono' v env
					| DomainInterfaces.LOW -> not (Misc.sign_changing mono' v env)
					| DomainInterfaces.BOTH -> Pervasives.failwith "IPattern.unboundedVarmode"
                )
				(P.data p)
				in
                let m = M.data mono |> fst in
                let var = List.find
                    (fun v -> let itv = Itv.of_var env v in
                        match (Itv.low itv, Itv.up itv) with
                        | (_, None) -> true
                        | _ -> false)
                    (AnnotedVar.update_monomial m mapNKeep |> MB.data)
                in
                Some (UnboundedVarmode (mono, var))
			with Not_found -> None
		end


	(* Marque une variable 'to_keep' *)
	let greatestItv : matcher
		= fun p env _ mapKeep mapNKeep ->
		try
            let mono = List.find
			(fun mono ->
                let (m',_) = M.data mono in
				not (MapMonomial.mem m' mapKeep)
			  && (* si le monôme n'a pas déjà de variable gardée *)
				List.for_all
                    (fun v -> (* Var.equal v Var.null ||*) Itv.of_var env v |> Itv.is_bounded)
				(AnnotedVar.update_monomial m' mapNKeep |> MB.data)
			  && (* il faut qu'il y ait au moins une constante *)
				not (MB.equal
                    (AnnotedVar.update_monomial m' mapNKeep)
                    MB.null)
            )
			(P.data p)
			in
            let (m,_) = M.data mono in
            Some (GreatestItv (mono, Itv.greatest (AnnotedVar.update_monomial m mapNKeep) env))
		with Not_found -> None

	(* Marque une variable 'to_interv' *)
	let varCte : matcher
		= fun p env _ _ mapNKeep ->
		try
            let mono = List.find
                (fun mono ->
                    not (M.isConstant mono)
                  &&
                    let (m',_) = M.data mono in
                    let l = AnnotedVar.update_monomial m' mapNKeep
                        |> MB.data
                    in
                    List.exists
                        (fun v -> P.Coeff.equal P.Coeff.z (Itv.of_var env v |> Itv.range))
                        l
                )
                (P.data p)
			in
            let (m,_) = M.data mono in
            let var = List.find
				(fun v ->  P.Coeff.equal P.Coeff.z (Itv.of_var env v |> Itv.range))
				(AnnotedVar.update_monomial m mapNKeep |> MB.data)
            in
            Some (VarCte (mono, var))
		with Not_found -> None

	(* Marque une variable 'to_interv' *)
	let multiplicity : matcher
		= let get_monomial_multiplicity : MB.t -> P.V.t -> int
				= fun m v ->
				let (l1,l2) = List.partition (fun v' -> P.V.equal v v') (MB.data m) in
				if List.length l2 = 0
				then (List.length l1) - 1
				else List.length l1
		in
        let get_multiplicity : P.t -> P.V.t -> int
			= fun p v ->
			List.fold_left
                (fun i mono -> i + (get_monomial_multiplicity (M.data mono |> fst) v))
                0 (P.data p)
		in
        fun p _ _ _ mapNKeep ->
		try
            let v =
    			let p' = List.map
                    (fun mono ->
                        let (m,c) = M.data mono in
                        M.mk (AnnotedVar.update_monomial m mapNKeep) c)
                    (P.data p)
                    |> P.mk
                in
    			List.map
                    (fun v -> (v,get_multiplicity p' v))
                    (p' |> P.get_vars)
    			|> List.fast_sort (fun (_,i1) (_,i2) -> Pervasives.compare i1 i2)
    			|> List.rev
    			|> fun l -> match l with
    				| [] | [_] -> Pervasives.raise Not_found
    				| (v1,i1) :: (_,i2) :: _ -> if i1 > i2 then v1 else Pervasives.raise Not_found
            in
            let l = List.map
                (fun mono ->
                    let (m,_) = M.data mono in
                    (mono, get_monomial_multiplicity (AnnotedVar.update_monomial m mapNKeep) v)
                )
                (P.data p)
            |> List.filter
                (fun (_, i) -> i > 0)
			in Some (Multiplicity (l,v))
		with Not_found -> None

	(* Marque une variable 'to_keep' *)
	let linearMonomial : matcher
		= fun p _ _ mapKeep mapNKeep ->
		try
            let mono = List.find
                (fun mono ->
                    let (m',_) = M.data mono in
    				not (MapMonomial.mem m' mapKeep)
    			  && (* si le monôme n'a pas déjà de variable gardée *)
    				MB.isLinear (AnnotedVar.update_monomial m' mapNKeep)
                )
			(P.data p)
			in
            let var = List.hd (AnnotedVar.update_monomial (M.data mono |> fst) mapNKeep |> MB.data)
            in
            Some (LinearMonomial(mono,var))
		with Not_found -> None

	(* Supprime un monôme qui est constant
		A utiliser de paire avec centerZero qui risque de générer des monômes constants *)
	let monomialCte : matcher
		= fun p _ _ _ _ ->
		try
            let mono = (List.find M.isConstant (P.data p))
			in Some (MonomialCte mono)
		with Not_found -> None

	(* Réécrit le polynôme *)
	let centerZero : matcher
		= fun p _ _ mapKeep _ ->
		try
            let mono =
                List.find
                    (fun mono -> (MapMonomial.mem (M.data mono |> fst) mapKeep)) (* si le monôme a déjà une variable gardée *)
                (P.data p)
			in Some (CenterZero mono)
		with Not_found -> None

	(* Réécrit le polynôme *)
	let translation : matcher
		= fun p env _ mapKeep _ ->
		try
            let mono = List.find
			(fun mono ->
                let (m',_) = M.data mono in
				not (MB.isLinear m')
			  &&
				try
                    MapMonomial.find m' mapKeep
                    |> Itv.of_var env
                    |> fun itv -> Itv.is_bounded itv && not (Itv.contains_zero itv)
				with Not_found -> false
            )
			(P.data p)
			in Some (Translation mono)
		with Not_found -> None

	(* What's the point of even being alive?
		KILL THEM ALL!!
		Utile pour gagner du temps *)
	let screwed : matcher
		= fun p env _ mapKeep mapNKeep ->
		if List.exists
			(fun mono ->
                let (m',_) = M.data mono in
                not (MapMonomial.mem m' mapKeep)
              && (* si le monôme n'a pas déjà de variable gardée *)
                Misc.is_unbounded (AnnotedVar.update_monomial m' mapNKeep) env
            )
			(P.data p)
		then Some Screwed
		else None

	(* Marque une variable 'to_keep' *)
	let firstUnbounded : matcher
		= fun p env _ mapKeep mapNKeep ->
		try
            let mono = List.find
                (fun mono ->
                    not (MapMonomial.mem (M.data mono |> fst) mapKeep)) (* si le monôme n'a pas déjà de variable gardée *)
                (P.data p)
			in
            let var =
                let m' = (AnnotedVar.update_monomial (M.data mono |> fst) mapNKeep) in
                try List.find (fun v -> Itv.of_var env v |> Itv.is_bounded |> not) (MB.data m')
				with Not_found -> Itv.greatest m' env
            in
            Some (FirstUnbounded(mono, var))
		with Not_found -> None

	(* Ordre de matching!! ne doit pas contenir Default qui est par défaut dans matching*)
	let matching_order = [monomialCte ; linearMonomial ; varCte ; unboundedVar ; unboundedVarmode ; multiplicity ; greatestItv ; firstUnbounded ;
	centerZero]

	let (matching :  P.t -> env -> mode -> P.V.t MapMonomial.t -> (AnnotedVar.t list) MapMonomial.t -> t)
		= let rec(find_first : P.t -> env -> mode -> matcher list -> P.V.t MapMonomial.t -> (AnnotedVar.t list) MapMonomial.t -> t)
			= fun p env mode l mapKeep mapNKeep->
			match l with
			| [] -> Default p
			| matcher :: tl -> match matcher p env mode mapKeep mapNKeep with
				| Some pat -> pat
				| None -> find_first p env mode tl mapKeep mapNKeep in
		fun p env mode mapKeep mapNKeep ->
		find_first p env mode matching_order mapKeep mapNKeep
end
