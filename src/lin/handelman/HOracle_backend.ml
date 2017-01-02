module Poly = Poly.Rat.Poly(Vector.Rat.Positive)
module Coeff = Poly.Coeff
module Var = Poly.V
module CP = CstrPoly.Positive

module MapMonomial = Map.Make(Poly.MonomialBasis)
	

module Translation = struct
	
	let rec term_to_poly: ASTerm.BasicQTerm.term -> Poly.t
		= fun t -> 
		ASTerm.BasicQTerm.(match t with 
		| Var x -> PedraQOracles.coqPosToZ x |> Scalar.RelInt.toInt |> Var.fromInt |> Poly.fromVar
		| Cte x -> PedraQOracles.coq_QToNb x |> Poly.cste
		| Add (x1,x2) -> Poly.add (term_to_poly x1) (term_to_poly x2)
  		| Opp x -> Poly.mul (Poly.cste Coeff.negU) (term_to_poly x)
  		| Mul (x1,x2) -> Poly.mul (term_to_poly x1) (term_to_poly x2)
  		| Annot (annot, x) -> term_to_poly x)
  		
  	let poly_to_term : Poly.t -> ASTerm.BasicQTerm.term
  		= fun pol ->
  		ASTerm.BasicQTerm.(List.fold_left
  			(fun res (vars,coeff) ->
  				let vars_term = List.fold_left
  					(fun res v -> 
  						let v' = Var.toInt v |> Scalar.Rat.Z.mk |> PedraQOracles.zToCoqPos in
  						Mul (res, Var v'))
  					(Cte NumC.QNum.u) vars
  				in
  				Add (res, Mul (vars_term, Cte (PedraQOracles.nToNumC coeff))))
  			(Cte NumC.QNum.z) (Poly.data2 pol))
  			
  		
  	let var_to_PExpr : Var.t -> BinNums.positive
  		= fun v ->
  		Var.toInt v |> Scalar.Rat.Z.mk |> PedraQOracles.zToCoqPos
  	
  	let int_to_coq_N : int -> BinNums.coq_N
  		= fun i ->
  		BinNums.Npos (Scalar.Rat.Z.mk i |> PedraQOracles.zToCoqPos) 
  	
	let squares_trans : Hi.Cert.squares -> Map_poly.squares
		= fun l ->
		List.map 
			(fun (v,k) -> (Ring_polynom.PEX (var_to_PExpr v), int_to_coq_N k)) 
			l
	
	(* Input integer must be nonnegative! *)
	let rec int_to_nat : int -> Datatypes.nat
		= function 
		| 0 -> Datatypes.O
		| n -> Datatypes.S (int_to_nat (n-1))
		
	let cIndex_trans : Hi.cIndex -> CIndex.NatIndex.t
		= fun id ->
		List.map int_to_nat (Index.Int.data id)
		
	let boundIndex_trans : Hi.boundIndex -> CIndex.QcIndex.t
		= fun id ->
		List.map PedraQOracles.nToNumC (Index.Rat.data id)
	
	let sch_trans : Hi.Cert.schweighofer -> QArith_base.coq_Q * Map_poly.MapPoly.schweighofer
		= fun (coeff, (cId, squares, bIds)) ->
		(PedraQOracles.nToCoq_Q coeff, 
			((cIndex_trans cId, squares_trans squares), 
		 	(List.map boundIndex_trans bIds)))
		
	let trans_map : IndexBuild.Map.t -> Map_poly.MapPoly.buildOrder
		= fun map ->
		IndexBuild.MapI.bindings map
		|> List.map
			(fun (id, l) -> Map_poly.MapPoly.({
				ind = cIndex_trans id;
				cons = List.map cIndex_trans l;
				}))
		
	
	(* TODO : la traduction de cp terme est elle bonne? *)
	let cert_trans : IndexBuild.Map.t -> (CP.t * Hi.Cert.schweighofer list) list -> Map_poly.Handelman_compute.certif list
		= fun map l ->
		List.map
			(fun (cp, schs) -> Map_poly.Handelman_compute.({
				aff = poly_to_term cp.CP.p;
				sch = List.map sch_trans schs;
				bo = trans_map map
				}))
			l
end

let get_indexes : (CP.t * Hi.Cert.schweighofer list) list -> IndexBuild.Liste.t
	= fun certs ->
	List.map
		(fun (_,l) -> List.map
			(fun (_,(id,_,_)) -> id) l)
		certs
	|> List.concat

(* TODO : la traduction de poly à cp est elle bonne? *)
let oracle : PedraQBackend.t -> NumC.cmpG -> ASTerm.BasicQTerm.t -> Map_poly.Handelman_compute.certif list
	= fun pol cmp qt -> 
	let poly = Translation.term_to_poly qt in
	let cp = match cmp with
	| NumC.Le -> CP.mk Cstr.Le poly
	| NumC.Lt -> CP.mk Cstr.Lt poly
	| _ -> Pervasives.invalid_arg "HOracle_backend.oracle"
	in
	let res = match !Flags.proj with
		| Flags.Proj_PLP(Flags.Rat) -> Handelman.Rat.Pb.run_oracle pol cp
		| Flags.Proj_PLP(Flags.Symbolic)-> Handelman.Symbolic.Pb.run_oracle pol cp
		| Flags.Proj_PLP(Flags.Float) -> Handelman.Float.Pb.run_oracle pol cp
		| _ -> Handelman.Symbolic.Pb.run_oracle pol cp
	in
	match res with
	| None -> Pervasives.failwith "HOracle_backend"
	| Some res ->
		let build_order = get_indexes res 
			|> IndexBuild.Map.compute in
		Translation.cert_trans build_order res
