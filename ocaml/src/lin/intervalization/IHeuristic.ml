open IOtypes

module Lift (T : Type) = struct

    open T

    module Pattern = IPattern.Lift(T)

    module P = D.Poly
    module M = D.Poly.Monomial
    module MB = D.Poly.MonomialBasis

	type prophecy = D.BasicTerm.term list

	type t = P.t -> env -> mode -> P.V.t MapMonomial.t
        -> (AnnotedVar.t list) MapMonomial.t -> Pattern.t
        -> (prophecy * P.V.t MapMonomial.t * (AnnotedVar.t list) MapMonomial.t * P.t)

	let rec default : t
		= fun p env mode mapKeep mapNKeep pat ->
		match pat with
		| Pattern.Default p -> begin
            match P.data p with
			| [] -> ([], mapKeep, mapNKeep, P.z)
			| mono :: tl ->
                let (m,c) = M.data mono in
                let (pro', mapKeep', mapNKeep', poly') = default p env mode mapKeep mapNKeep (Pattern.Default (tl |> P.mk)) in
    			let var_to_keep = try MapMonomial.find m mapKeep'
    				with Not_found -> AnnotedVar.update_monomial m mapNKeep'
                        |> MB.data
                        |> List.hd
                in
    			let aVarl = try MapMonomial.find m mapNKeep'
    				with Not_found -> []
                in
    			let aff = D.BasicTerm.annotAFFINE (Term.of_monomial (M.mk2 [var_to_keep] c)) in
    			let mon = Misc.pop P.V.equal (MB.data m) var_to_keep
                    |> MB.mk
                    |> Term.of_monomialBasis
                    |> D.BasicTerm.smartAnnot ASTerm.TopLevelAnnot.INTERV
    				|> fun t -> AnnotedVar.apply t aVarl in
    			let value = D.BasicTerm.smartMul mon aff in
    			( value :: pro', mapKeep', mapNKeep', poly')
        end
		| _ -> Pervasives.failwith "Heuristic.default"

	let varCte : t
		= fun p env mode mapKeep mapNKeep pat ->
		match pat with
		| Pattern.VarCte (mono,v) ->
            let (m,_) = M.data mono in
            let l = try MapMonomial.find m mapNKeep
			with Not_found -> []
            in
			([],
             mapKeep,
             MapMonomial.add m ((AnnotedVar.of_var v ASTerm.TopLevelAnnot.STATIC) :: l) mapNKeep,
             p)
		| _ -> Pervasives.failwith "Heuristic.varCte"

	let multiplicity : t
		= fun p env mode mapKeep mapNKeep pat ->
		match pat with
		| Pattern.Multiplicity(l,v) ->
    		let mapNKeep' = List.fold_left
                (fun map (mono,i) ->
                    let (m,_) = M.data mono in
                    let l' = try MapMonomial.find m map with Not_found -> [] in
                    let l'' =  List.map (fun j -> AnnotedVar.Var v) (Misc.range 0 i) in
                MapMonomial.add m (l'' @ l') map)
                mapNKeep
    		l
    		in ([], mapKeep, mapNKeep', p)
		| _ -> Pervasives.failwith "Heuristic.multiplicity"

	let unboundedVarmode : t
		= fun p env mode mapKeep mapNKeep pat ->
		match pat with
		| Pattern.UnboundedVarmode(mono,v) ->
            let (m,_) = M.data mono in
            let l = try MapMonomial.find m mapNKeep with Not_found -> [] in
			([], mapKeep, MapMonomial.add m (AnnotedVar.Var v :: l) mapNKeep, p)
		| _ -> Pervasives.failwith "Heuristic.unboundedVarmode"

	let greatestItv : t
		= fun p env mode mapKeep mapNKeep pat ->
		match pat with
		| Pattern.GreatestItv(mono,v) ->
            ([], MapMonomial.add (M.data mono |> fst) v mapKeep, mapNKeep, p)
		| _ -> Pervasives.failwith "Heuristic.greatestItv"

	let unboundedVar : t
		= fun p env mode mapKeep mapNKeep pat ->
		match pat with
		| Pattern.UnboundedVar(mono,v) ->
            ([], MapMonomial.add (M.data mono |> fst) v mapKeep, mapNKeep, p)
		| _ -> Pervasives.failwith "Heuristic.unboundedVar"

	let linearMonomial : t
		= fun p env mode mapKeep mapNKeep pat ->
		match pat with
		| Pattern.LinearMonomial (mono,v) ->
            ([], MapMonomial.add (M.data mono |> fst) v mapKeep, mapNKeep, p)
		| _ -> Pervasives.failwith "Heuristic.linearMonomial"

	let centerZero : t
		= fun p env mode mapKeep mapNKeep pat ->
		match pat with
		| Pattern.CenterZero mono ->
            let (m,_) = M.data mono in
            let vToKeep = MapMonomial.find m mapKeep in
    		let aVarl = try MapMonomial.find m mapNKeep with Not_found -> [] in
    		let l = Misc.pop P.V.equal (MB.data m) vToKeep
                |> List.filter
                    (fun v ->
                        let itv = Itv.of_var env v in
                        Itv.is_bounded itv
                      &&
                        not (Itv.contains_zero itv)
                      &&
                        not (AnnotedVar.mem v aVarl)
                    )
            in
    		let tlist = List.map
                (fun v -> Itv.of_var env v|> Itv.get_translation_bound) l
            |> Term.center_zero_var mono vToKeep l
    		|> List.map (fun t -> AnnotedVar.apply t aVarl)
            in
    		(tlist, mapKeep, mapNKeep, P.sub_monomial p m)
		| _ -> Pervasives.failwith "Heuristic.centerZero"

	let translation : t
		= fun p env mode mapKeep mapNKeep pat ->
		match pat with
		| Pattern.Translation mono ->
            let (m,_) = M.data mono in
    		let aVarl = try MapMonomial.find m mapNKeep with Not_found -> [] in
    		let vToKeep = MapMonomial.find m mapKeep in
    		let (t, m') = Itv.of_var env vToKeep |> Itv.get_translation_bound
                |> Term.translate mono vToKeep
            in
    		let t' = AnnotedVar.apply t aVarl in
            let p' = P.add (P.sub_monomial p m) (P.mk [m']) in
    		([t'], mapKeep, mapNKeep, p')
		| _ -> Pervasives.failwith "Heuristic.translation"

	let screwed : t
		= fun p env mode mapKeep mapNKeep pat ->
		match pat with
		| Pattern.Screwed -> default p env mode mapKeep mapNKeep (Pattern.Default p)
		| _ -> Pervasives.failwith "Heuristic.screwed"

	let monomialCte : t
		= fun p env mode mapKeep mapNKeep pat ->
		match pat with
		| Pattern.MonomialCte mono ->
			([Term.of_monomial mono], mapKeep, mapNKeep, P.sub_monomial p (M.data mono |> fst))
		| _ -> Pervasives.failwith "Heuristic.monomialCte"

	let firstUnbounded : t
		= fun p env mode mapKeep mapNKeep pat ->
		match pat with
		| Pattern.FirstUnbounded (mono,v) ->
            ([], MapMonomial.add (M.data mono |> fst) v mapKeep, mapNKeep, p)
		| _ -> Pervasives.failwith "Heuristic.firstUnbounded"

	let of_pattern : t
		= fun p env mode mapKeep mapNKeep pat ->
		Debug.log DebugTypes.Normal (lazy "Heuristic used : ");
		match pat with
		| Pattern.Multiplicity _ -> Debug.log DebugTypes.Normal (lazy "Multiplicity\n");
			multiplicity p env mode mapKeep mapNKeep pat
		| Pattern.MonomialCte _ -> Debug.log DebugTypes.Normal (lazy "MonomialCte\n");
			monomialCte p env mode mapKeep mapNKeep pat
		| Pattern.CenterZero _ -> Debug.log DebugTypes.Normal (lazy "CenterZero\n");
			centerZero p env mode mapKeep mapNKeep pat
		| Pattern.Translation _ -> Debug.log DebugTypes.Normal (lazy "Translation\n");
			translation p env mode mapKeep mapNKeep pat
		| Pattern.LinearMonomial _ -> Debug.log DebugTypes.Normal (lazy "LinearMonomial\n");
			linearMonomial p env mode mapKeep mapNKeep pat
		| Pattern.Screwed -> Debug.log DebugTypes.Normal (lazy "Screwed\n");
			screwed p env mode mapKeep mapNKeep pat
		| Pattern.VarCte _ -> Debug.log DebugTypes.Normal (lazy "VarCte\n");
			varCte p env mode mapKeep mapNKeep pat
		| Pattern.GreatestItv _ -> Debug.log DebugTypes.Normal (lazy "GreatestItv\n");
			greatestItv p env mode mapKeep mapNKeep pat
		| Pattern.UnboundedVar _ -> Debug.log DebugTypes.Normal (lazy "UnboundedVar\n");
			unboundedVar p env mode mapKeep mapNKeep pat
		| Pattern.UnboundedVarmode _ -> Debug.log DebugTypes.Normal (lazy "UnboundedVarmode\n");
			unboundedVarmode p env mode mapKeep mapNKeep pat
		| Pattern.FirstUnbounded _ -> Debug.log DebugTypes.Normal (lazy "FirstUnbounded\n");
			firstUnbounded p env mode mapKeep mapNKeep pat
		| Pattern.Default _ -> Debug.log DebugTypes.Normal (lazy "Default\n");
			default p env mode mapKeep mapNKeep pat

end
