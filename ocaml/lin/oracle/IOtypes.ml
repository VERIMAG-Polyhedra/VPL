(** Types and conversions for intervalization oracle. *)

open ProgVar
open FMapPositive

type mode = DomainInterfaces.mode

module Debug = OraclePattern.Debug

(** Type of numerical domains providing linear terms, intervals and polynomials.
    The intervalization oracle can be run with two domains (in Z when called from Coq, or in Q). *)
module type Domain_T = sig

    (** Type of scalar values. *)
    module N : NumC.NumSig

    (** Basic AST Terms, copied from ASTerm.ModalTerm *)
    module BasicTerm : sig

        module Annot : sig
            type topLevelAnnot = ASTerm.TopLevelAnnot.topLevelAnnot =
            | OLD
            | AFFINE
            | INTERV
            | STATIC
            | SKIP_ORACLE

            val topLevelAnnot_rect :
            'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> topLevelAnnot -> 'a1

            val topLevelAnnot_rec :
            'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> topLevelAnnot -> 'a1

            type t = topLevelAnnot

            val pr : topLevelAnnot -> char list
        end

        type term =
        | Var of PVar.t
        | Cte of N.t
        | Add of term * term
        | Opp of term
        | Mul of term * term
        | Annot of Annot.topLevelAnnot * term

        val term_rect :
          (PVar.t -> 'a1) -> (N.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
          (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
          (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

        val term_rec :
          (PVar.t -> 'a1) -> (N.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
          (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
          (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

        type t = term

        val eval : term -> N.t Mem.t -> N.t
        val mdBound : term -> PVar.t -> PVar.t
        val fold_variables : term -> (PVar.t -> 'a1 -> 'a1) -> 'a1 -> 'a1
        val map : term -> PVar.t Mem.t -> term
        val pseudoIsZero : term -> bool
        val smartScalAdd1 : N.t -> term -> term
        val smartScalAdd : N.t -> term -> term
        val smartAdd : term -> term -> term
        val smartOpp : term -> term
        val smartScalMul1 : N.t -> term -> term
        val smartScalMul : N.t -> term -> term
        val smartMul : term -> term -> term
        val smartAnnot : Annot.topLevelAnnot -> term -> term
        val import_acc : (PVar.t*N.t) list -> term -> term
        val import : (PVar.t*N.t) list -> term
        val coq_Old : term -> term
        val xeval : term -> N.t Mem.t -> N.t Mem.t -> N.t
        val xmap : term -> PVar.t Mem.t -> PVar.t Mem.t -> term
        val isCte : term -> bool
        val annotAFFINEx : term -> term
        val annotAFFINE_rec : term -> term option
        val annotAFFINE : term -> term
        val matchCte : term -> N.t option
        val pr : term -> char list
    end

    (** Linear terms represented as maps from variables to scalars. *)
    module LinTerm : sig
        type t = N.t PositiveMap.t

        val absEval : (PVar.t*N.t) list -> N.t Mem.t -> N.t
        val eval : t -> N.t Mem.t -> N.t
        type exportT = (PVar.t*N.t) list
        val export : t -> exportT
        val nil : t
        val isNil : t -> bool
        val single : PVar.t -> N.t -> t
        val opp : t -> N.t PositiveMap.t
        val mul : N.t -> t -> t
        val coq_N_eqb : N.t -> N.t -> bool
        val isEq : t -> t -> bool
        val add : t -> t -> t
        val isFree : PVar.t -> t -> bool
        val rename : PVar.t -> PVar.t -> t -> t
        val fmtAux : char list list -> char list -> char list
        val fmt : char list list -> char list
        val pairPr : (PVar.t*N.t) -> char list
        val pr : t -> char list
        val pair_to_string : (PVar.t -> char list) -> (PVar.t*N.t) -> char list
        val to_string : (PVar.t -> char list) -> t -> char list
    end

    (** Affine terms. *)
    module AffTerm : sig

        type affTerm = { lin : LinTerm.t; cte : N.t }

        val lin : affTerm -> LinTerm.t
        val cte : affTerm -> N.t
        type t = affTerm
        val eval : affTerm -> N.t Mem.t -> N.t
        val nil : affTerm
        val opp : affTerm -> affTerm
        val mul : N.t -> affTerm -> affTerm
        val add : affTerm -> affTerm -> affTerm
        val addc : N.t -> affTerm -> affTerm
        val addx : PVar.t -> affTerm -> affTerm
        val addnx : PVar.t -> affTerm -> affTerm
        val isZero : affTerm -> bool
    end

    (** Terms with annotations. *)
    module Term : sig
        module Annot :
        sig
        type topLevelAnnot =
        | OLD
        | AFFINE
        | INTERV
        | STATIC
        | SKIP_ORACLE

        val topLevelAnnot_rect :
         'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> topLevelAnnot -> 'a1

        val topLevelAnnot_rec :
         'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> topLevelAnnot -> 'a1

        type t = topLevelAnnot

        val pr : topLevelAnnot -> char list
        end

        type term = BasicTerm.term

        val term_rect :
        (PVar.t -> 'a1) -> (N.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
        -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
        (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

        val term_rec :
        (PVar.t -> 'a1) -> (N.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
        -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
        (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

        type t = term

        val eval : term -> N.t Mem.t -> N.t
        val mdBound : term -> PVar.t -> PVar.t
        val fold_variables : term -> (PVar.t -> 'a1 -> 'a1) -> 'a1 -> 'a1
        val map : term -> PVar.t Mem.t -> term
        val pseudoIsZero : term -> bool
        val smartScalAdd1 : N.t -> term -> term
        val smartScalAdd : N.t -> term -> term
        val smartAdd : term -> term -> term
        val smartOpp : term -> term
        val smartScalMul1 : N.t -> term -> term
        val smartScalMul : N.t -> term -> term
        val smartMul : term -> term -> term
        val smartAnnot : Annot.topLevelAnnot -> term -> term
        val import_acc : (PVar.t*N.t) list -> term -> term
        val import : (PVar.t*N.t) list -> term
        val coq_Old : term -> term
        val xeval : term -> N.t Mem.t -> N.t Mem.t -> N.t
        val xmap : term -> PVar.t Mem.t -> PVar.t Mem.t -> term
        val isCte : term -> bool
        val annotAFFINEx : term -> term
        val annotAFFINE_rec : term -> term option
        val annotAFFINE : term -> term
        val matchCte : term -> N.t option
        val pr : term -> char list
        val fromLin : LinTerm.t -> BasicTerm.term
        val fromAff : AffTerm.affTerm -> BasicTerm.term
        val affineDecompose : BasicTerm.term -> BasicTerm.term*AffTerm.t
    end

    (** Not necesarily bounded intervals. *)
    module NoneItv : sig
        type t

        val mk : N.t option -> N.t option -> t
        val low: t -> N.t option
        val up: t -> N.t option
        val add : DomainInterfaces.mode -> t -> t -> t
        val mul : DomainInterfaces.mode -> t -> t -> t
        val opp : t -> t
        val oppMode : mode -> mode
    end

    module NA : sig
        type t = AffTerm.t option

        val cte : N.t option -> t
        val add : t -> t -> t
        val opp : t -> t
        val mul : N.t option -> AffTerm.t -> t
        val mulZ1 : N.t -> t -> t
        val mulZ : N.t -> t -> t
     end

     (** Type of intervals where bounds are affine terms.
        Bounds can be unbounded as well. *)
     module NAItv :sig
        type itv = { low : NA.t; up : NA.t }

        val low : itv -> NA.t
        val up : itv -> NA.t
        val cte : NoneItv.t -> itv
        val single : AffTerm.t -> itv
        val select : mode -> NA.t -> NA.t -> itv
        val add : mode -> itv -> itv -> itv
        val opp : mode -> itv -> itv
        val mulZ : mode -> N.t -> itv -> itv
        val mulN : mode -> NoneItv.t -> AffTerm.t -> itv
        val mulP1 : mode -> NoneItv.t -> AffTerm.t -> itv
    end

    (** Type fo polynomials. *)
    module Poly : sig
        include Poly.Type

        val coqvar_to_var : PVar.t -> Var.t
        val var_to_coqvar : Var.t -> PVar.t
        val n_to_coeff : N.t -> Vec.Coeff.t
        val coeff_to_n : Vec.Coeff.t -> N.t
    end
end

(** Type of domains for intervalization. *)
module type Type = sig

    (** The input domain. *)
    module D : Domain_T

    (** Environment from variables to intervals. *)
    type env = PVar.t -> D.NoneItv.t

    (** Input type for intervalization. *)
    type linearizeContext = {
        nonaffine : D.Term.t; (** nonlinear part *)
        env : (PVar.t -> D.NoneItv.t); (** static environment *)
        affine : D.AffTerm.t; (** affine part *)
        source : D.Term.t; (** whole expression *)
        cmp : NumC.cmpG; (** comparison sign *)
    }

    (** Type of maps indexed by monomials. *)
    module MapMonomial : Map.S with type key = D.Poly.MonomialBasis.t

    (** Type of maps indexed by variables. *)
    module MapV : Map.S with type key = Var.t

    (** Extension of the type of terms.
        Implements several strategies for intervalization. *)
    module Term : sig

    	type t = D.BasicTerm.term

    	val zero : t
    	val one : t

    	val to_polynomial: t -> D.Poly.t
    	val to_string : t -> string
    	val of_cte : D.Poly.Coeff.t -> t
    	val of_var : Var.t -> t
    	val of_monomialBasis : D.Poly.MonomialBasis.t -> t
    	val of_monomial : D.Poly.Monomial.t -> t
    	val of_polynomial : D.Poly.t -> t
        val focusing : D.Poly.Monomial.t -> Var.t -> (Var.t * D.N.t) list -> D.BasicTerm.term list
    	val get_affine_part : t -> t
    	val get_affine_part_opt : t -> t option
    	val get_interv_part : t -> t
        val get_interv_part_opt :  t -> t option
    end

    (** Extension of the type of intervals.
        Contains useful functions for intervalization. *)
    module Itv : sig
    	type t = D.NoneItv.t

    	val of_var : env -> Var.t -> t
    	val of_term : Term.t -> env -> t
    	val low : t -> D.N.t option
    	val up : t -> D.N.t option
    	val to_string : t -> string
    	val range : t -> D.Poly.Coeff.t
    	val is_bounded : t -> bool
    	val is_fully_unbounded : t -> bool
    	val greatest : D.Poly.MonomialBasis.t -> env -> Var.t option
    	val contains_zero : t -> bool
    	val get_translation_bound : t -> D.N.t
    end
end

(** Lifts a numerical domain into an intervalization domain. *)
module Lift (D : Domain_T) = struct

    module D = D

    type env = PVar.t -> D.NoneItv.t

    type linearizeContext = {
        nonaffine : D.Term.t;
        env : (PVar.t -> D.NoneItv.t);
        affine : D.AffTerm.t;
        source : D.Term.t;
        cmp : NumC.cmpG;
    }

    module MapMonomial = Map.Make(D.Poly.MonomialBasis)
    module MapV = Map.Make(struct type t = Var.t let compare = Var.cmp end)

    module Term = struct

    	type t = D.BasicTerm.term

    	let zero = D.BasicTerm.Cte (D.Poly.coeff_to_n D.Poly.Coeff.z)

    	let one = D.BasicTerm.Cte (D.Poly.coeff_to_n D.Poly.Coeff.u)

    	(* Expanding polynomial *)
    	let rec to_polynomial: t -> D.Poly.t
    		= D.BasicTerm.(function
    		| Var x -> D.Poly.coqvar_to_var x |> D.Poly.fromVar
    		| Cte x -> D.Poly.n_to_coeff x |> D.Poly.cste
    		| Add (x1,x2) -> D.Poly.add (to_polynomial x1) (to_polynomial x2)
      		| Opp x -> D.Poly.neg (to_polynomial x)
      		| Mul (x1,x2) -> D.Poly.mul (to_polynomial x1) (to_polynomial x2)
      		| Annot (_, x) -> to_polynomial x
            )

    	let rec to_string : t -> string
    		= D.BasicTerm.(function
    		| Var x -> "v" ^ (D.Poly.coqvar_to_var x |> Var.to_string)
    		| Cte x -> (D.Poly.n_to_coeff x |> D.Poly.Coeff.to_string)
    		| Add (x1,x2) ->  Printf.sprintf "%s + %s" (to_string x1) (to_string x2)
      		| Opp x -> Printf.sprintf "-1*(%s)" (to_string x)
      		| Mul (x1,x2) -> (match (x1,x2) with
      			| (Add (_,_),Add(_,_)) -> Printf.sprintf "(%s)*(%s)" (to_string x1) (to_string x2)
      			| (Add (_,_),_) -> Printf.sprintf "(%s)*%s" (to_string x1) (to_string x2)
      			| (_,Add(_,_)) -> Printf.sprintf "%s*(%s)" (to_string x1) (to_string x2)
      			| _ -> Printf.sprintf "%s*%s" (to_string x1) (to_string x2))
      		| Annot (ASTerm.TopLevelAnnot.STATIC, x) -> Printf.sprintf "STATIC(%s)" (to_string x)
      		| Annot (ASTerm.TopLevelAnnot.AFFINE, x) -> Printf.sprintf "AFFINE(%s)" (to_string x)
      		| Annot (ASTerm.TopLevelAnnot.INTERV, x) -> Printf.sprintf "INTERV(%s)" (to_string x)
    		| _ -> Stdlib.invalid_arg "IOtypes.to_string"
            )

    	let of_cte : D.Poly.Coeff.t -> t
    		= fun i -> D.BasicTerm.Cte (D.Poly.coeff_to_n i)

    	let of_var : Var.t -> t
    		= fun v -> D.BasicTerm.Var (D.Poly.var_to_coqvar v)

    	let of_monomialBasis : D.Poly.MonomialBasis.t -> t
    		= fun m ->
            D.Poly.MonomialBasis.to_list_expanded m
            |> List.map of_var
            |> List.fold_left D.BasicTerm.smartMul one

    	let of_monomial : D.Poly.Monomial.t -> t
    		= fun m ->
            let (m,c) = D.Poly.Monomial.data m in
    		D.BasicTerm.smartScalMul
    		(D.Poly.coeff_to_n c)
    		(of_monomialBasis m)

    	let of_polynomial : D.Poly.t -> t
    		= fun p ->
    		List.fold_left
        		(fun t m -> D.BasicTerm.smartAdd t (of_monomial m))
        		zero
        		(D.Poly.data p)

        let rec focusing : D.Poly.Monomial.t -> Var.t -> (Var.t * D.N.t) list -> D.BasicTerm.term list
    		= fun (mb,c) vToKeep -> function
    		| [] ->
                let basis = D.Poly.MonomialBasis.remove_var_exp vToKeep 1 mb
                    |> of_monomialBasis
                in
                [D.BasicTerm.smartMul
        			(D.BasicTerm.smartAnnot
        				ASTerm.TopLevelAnnot.INTERV
                        basis)
        			(D.BasicTerm.annotAFFINE (of_monomial (D.Poly.Monomial.mk_list [vToKeep,1] c)))]
    		| (v,x) :: l ->
                let mb' = D.Poly.MonomialBasis.remove_var_exp v 1 mb in
        		let tlist1 = focusing (mb',c) vToKeep l in
        		let x' = D.Poly.n_to_coeff x
                    |> D.Poly.Coeff.mul c
                in
        		let tlist2 = focusing (mb', x') vToKeep l in
                List.map2 (fun t1 t2 ->
                    let t1' = D.BasicTerm.smartMul
            			(D.BasicTerm.smartAnnot
            				ASTerm.TopLevelAnnot.INTERV
            					(D.BasicTerm.smartAdd
            						(of_var v)
            						(D.BasicTerm.Opp (D.BasicTerm.Cte x))))
            			t1
                    in
            		[t1' ; t2]
                ) tlist1 tlist2
                |> List.concat

        let rec get_affine_part_opt : t -> t option
			= D.BasicTerm.(function
			| Var _ -> None
			| Cte _ -> None
			| Add (x1,x2) ->  (match (get_affine_part_opt x1, get_affine_part_opt x2) with
				| (None, Some x) -> Some x
				| (Some x, None) -> Some x
				| (Some x1, Some x2) -> Some (smartAdd x1 x2)
				| _ -> None (* a modifier *))
  			| Opp x -> (match get_affine_part_opt x with
  				| Some y -> Some(Opp y)
  				| None -> None)
			| Mul (x1,x2) ->  (match (get_affine_part_opt x1, get_affine_part_opt x2) with
				| (None, Some x) -> Some x
				| (Some x, None) -> Some x
				| _ -> None (* a modifier *))
  			| Annot (ASTerm.TopLevelAnnot.AFFINE, x) -> Some x
  			| Annot (a, x) -> (match get_affine_part_opt x with
  				| Some y -> Some(Annot (a, y))
  				| None -> None)
            )

    	(* Renvoie une partie affine en enlevant les annotations affines *)
    	let get_affine_part : t -> t
    		= fun t ->
         	match get_affine_part_opt t with
      		| Some x -> x
      		| None -> Stdlib.raise Not_found

        (* remarque : retire les annotations interv*)
    	let rec get_interv_part_opt : t -> t option
			= D.BasicTerm.(function
			| Var _ -> None
			| Cte _ -> None
			| Add (x1,x2) ->  (match (get_interv_part_opt x1, get_interv_part_opt x2) with
				| (None, Some x) -> Some x
				| (Some x, None) -> Some x
				| (Some x1, Some x2) -> Some (Add (x1,x2))
				| _ -> None)
  			| Opp x -> (match get_interv_part_opt x with
  				| Some y -> Some(Opp y)
  				| None -> None)
			| Mul (x1,x2) ->  (match (get_interv_part_opt x1, get_interv_part_opt x2) with
				| (None, Some x) -> Some x
				| (Some x, None) -> Some x
				| (Some x1, Some x2) -> Some(Mul (x1,x2))
				| _ -> None)
  			| Annot (ASTerm.TopLevelAnnot.INTERV, x) -> Some x
  			| Annot (a, x) -> (match get_interv_part_opt x with
  				| Some y -> Some(Annot (a, y))
  				| None -> None)
            )

      	(* remarque : retire les annotations interv*)
    	let get_interv_part : t -> t
    		= fun t ->
          	match get_interv_part_opt t with
      		| Some x -> x
     	 	| None -> one
    end

    module Itv = struct
    	type t = D.NoneItv.t

    	let of_var : env -> Var.t -> t
    		= fun env v ->
            D.Poly.var_to_coqvar v |> env

    	let rec of_term : Term.t -> env -> t
    		= fun t env ->
    		D.BasicTerm.(match t with
    		| Var v -> env v
    		| Cte x -> D.NoneItv.mk (Some x) (Some x)
    		| Add (x1,x2) -> D.NoneItv.add DomainInterfaces.BOTH (of_term x1 env) (of_term x2 env)
      		| Opp x -> D.NoneItv.opp (of_term x env)
      		| Mul (x1,x2) -> D.NoneItv.mul DomainInterfaces.BOTH (of_term x1 env) (of_term x2 env)
      		| Annot (_, x) -> of_term x env
            )

    	let low : t -> D.N.t option
    		= fun itv -> D.NoneItv.low itv

    	let up : t -> D.N.t option
    		= fun itv -> D.NoneItv.up itv

    	let to_string : t -> string
    		= fun itv ->
            let str x = D.Poly.n_to_coeff x |> D.Poly.Coeff.to_string in
    		match (D.NoneItv.low itv, D.NoneItv.up itv) with
    		| (None, Some b) -> Printf.sprintf "]-inf,%s]" (str b)
    		| (Some a,None) -> Printf.sprintf "[%s,+inf[" (str a)
    		| (Some a, Some b) -> Printf.sprintf "[%s,%s]" (str a) (str b)
    		| (None, None) -> Printf.sprintf "]-inf,+inf["

    	let range : t -> D.Poly.Coeff.t
    		= fun itv ->
    		match (low itv, up itv) with
    		| (None, _) | (_,None) -> D.Poly.Coeff.negU
    		| (Some a, Some b) -> D.N.sub b a
                |> D.Poly.n_to_coeff

    	let is_bounded : t -> bool
    		= fun itv ->
    		match (D.NoneItv.low itv, D.NoneItv.up itv) with
    			| (Some _, Some _) -> true
    			| _ -> false

    	let is_fully_unbounded : t -> bool
    		= fun itv ->
    		match (D.NoneItv.low itv, D.NoneItv.up itv) with
    			| (None,None) -> true
    			| _ -> false

    	let greatest : D.Poly.MonomialBasis.t -> env -> Var.t option
    		= fun mb env ->
            let ranges = List.map (fun (var,_) ->
                (var, of_var env var |> range)
                ) mb
            (* Negative ranges correspond to unbounded intervals *)
    		|> List.filter (fun (_, range) ->
                D.Poly.Coeff.le D.Poly.Coeff.z range)
            in
            if ranges = []
            then None
            else
                let max = Misc.max (fun (_,r1) (_,r2) ->
                    D.Poly.Coeff.cmp r1 r2
                ) ranges
        		|> Stdlib.fst
                in Some max

        (* TODO: le -> lt ?*)
    	let contains_zero : t -> bool
    		= fun itv ->
    			match (D.NoneItv.low itv, D.NoneItv.up itv) with
    			| (Some x1, Some x2) ->
                    let a = D.Poly.n_to_coeff x1
                    and b = D.Poly.n_to_coeff x2 in
                    D.Poly.Coeff.le a D.Poly.Coeff.z && D.Poly.Coeff.le D.Poly.Coeff.z b
    			| (Some x1, None) ->
                    let a = D.Poly.n_to_coeff x1 in
                    D.Poly.Coeff.le a D.Poly.Coeff.z
            	| (None, Some x2) ->
                    let b = D.Poly.n_to_coeff x2 in
                    D.Poly.Coeff.le D.Poly.Coeff.z b
                | (None, None) -> false

    	let get_translation_bound : t -> D.N.t
    		= fun itv ->
    			match (D.NoneItv.low itv, D.NoneItv.up itv) with
    			| (Some x1, Some x2) ->
    				let b = D.Poly.n_to_coeff x2 in
    				if D.Poly.Coeff.lt b (D.Poly.Coeff.z) then x2 else x1
    			| _ -> Stdlib.failwith "Itv.get_translation_bound"
    end
end

(** Numerical domain for integers. *)
module DomainZ = struct

    module N = NumC.ZNum

    module BasicTerm = ASTerm.BasicZTerm

    module AffTerm = LinTerm.ZAffTerm

    module LinTerm = LinTerm.LinZ

    module Term = ASTerm.ZTerm

    module NoneItv = struct
        include ZNoneItv.ZNItv

        let mk a b = {low = a ; up = b}
    end

    module NA = ZNoneItv.NA

    module NAItv = ZNoneItv.NAItv

    module Poly = struct
        include Poly.Make(Vector.Int)

        let coqvar_to_var : PVar.t -> Var.t
            = fun var ->
            PedraQOracles.coqPosToZ var
            |> Z.to_int
            |> Var.fromInt

        let var_to_coqvar : Var.t -> PVar.t
            = fun var ->
            Var.toInt var
            |> Z.of_int
            |> PedraQOracles.zToCoqPos

        let n_to_coeff : N.t -> Coeff.t
            = fun c ->
            PedraQOracles.coqZToZ c
            |> Z.to_int
            |> Coeff.of_int

        let coeff_to_n : Coeff.t -> N.t
            = fun c ->
            PedraQOracles.zToCoqZ c
    end
end

(** Numerical domain for rationals. *)
module DomainQ = struct

    module N = NumC.QNum

    module BasicTerm = ASTerm.BasicQTerm

    module AffTerm = LinTerm.QAffTerm

    module LinTerm = LinTerm.LinQ

    module Term = ASTerm.QTerm

    module NoneItv = struct
        include QNoneItv.QNItv

        let mk a b = {low = a ; up = b}
    end

    module NA = struct
        type t = AffTerm.t option

        (** val cte : ZN.t -> t **)

        let cte = function
        | Some z -> Some { AffTerm.lin = AffTerm.Lin.nil; AffTerm.cte = z }
        | None -> None

        (** val add : t -> t -> t **)

        let add aft1 aft2 =
        match aft1 with
        | Some aft3 ->
          (match aft2 with
           | Some aft4 -> Some (AffTerm.add aft3 aft4)
           | None -> None)
        | None -> None

        (** val opp : t -> t **)

        let opp = function
        | Some aft0 -> Some (AffTerm.opp aft0)
        | None -> None

        (** val mul : ZN.t -> AffTerm.t -> t **)

        let mul zn aft =
        if AffTerm.isZero aft
        then Some AffTerm.nil
        else (match zn with
              | Some z -> Some (AffTerm.mul z aft)
              | None -> None)

        (** val mulZ1 : ZNum.t -> t -> t **)

        let mulZ1 z = function
        | Some aft0 -> Some (AffTerm.mul z aft0)
        | None -> None

        (** val mulZ : ZNum.t -> t -> t **)

        let mulZ z = function
        | Some aft0 -> Some (AffTerm.mul z aft0)
        | None -> if N.isZero z then Some AffTerm.nil else None
    end

    module NAItv = struct
        type itv = { low : NA.t; up : NA.t }

        let low x = x.low

        let up x = x.up

        let cte i =
        { low = (NA.cte i.NoneItv.low); up = (NA.cte i.NoneItv.up) }

        let single aft =
        let bnd = Some aft in { low = bnd; up = bnd }

        let select mo l u =
        DomainInterfaces.(match mo with
        | BOTH -> { low = l; up = u }
        | UP -> { low = None; up = u }
        | LOW -> { low = l; up = None }
        )

        let add mo i1 i2 =
        DomainInterfaces.(match mo with
        | BOTH -> { low = (NA.add i1.low i2.low); up = (NA.add i1.up i2.up) }
        | UP -> { low = None; up = (NA.add i1.up i2.up) }
        | LOW -> { low = (NA.add i1.low i2.low); up = None }
        )

        let opp mo i =
        DomainInterfaces.(match mo with
        | BOTH -> { low = (NA.opp i.up); up = (NA.opp i.low) }
        | UP -> { low = None; up = (NA.opp i.low) }
        | LOW -> { low = (NA.opp i.up); up = None }
        )

        let mulZ mo c i =
        if N.ltLeDec N.z c
        then DomainInterfaces.(match mo with
              | BOTH -> { low = (NA.mulZ c i.low); up = (NA.mulZ c i.up) }
              | UP -> { low = None; up = (NA.mulZ c i.up) }
              | LOW -> { low = (NA.mulZ c i.low); up = None })
        else DomainInterfaces.(match mo with
              | BOTH -> { low = (NA.mulZ1 c i.up); up = (NA.mulZ1 c i.low) }
              | UP -> { low = None; up = (NA.mulZ1 c i.low) }
              | LOW -> { low = (NA.mulZ1 c i.up); up = None })

        let mulN mo i aft =
        DomainInterfaces.(match mo with
        | BOTH -> { low = (NA.mul i.NoneItv.up aft); up = (NA.mul i.NoneItv.low aft) }
        | UP -> { low = None; up = (NA.mul i.NoneItv.low aft) }
        | LOW -> { low = (NA.mul i.NoneItv.up aft); up = None }
        )

        let mulP1 mo i aft =
        DomainInterfaces.(match mo with
        | BOTH -> { low = (NA.mul i.NoneItv.low aft); up = (NA.mul i.NoneItv.up aft) }
        | UP -> { low = None; up = (NA.mul i.NoneItv.up aft) }
        | LOW -> { low = (NA.mul i.NoneItv.low aft); up = None }
        )
    end

    module Poly = struct
        (** Polynomials over integers ?*)
        include Poly.Make(Vector.Rat)

        let coqvar_to_var : PVar.t -> Var.t
            = fun var ->
            PedraQOracles.coqPosToZ var
            |> Z.to_int
            |> Var.fromInt

        let var_to_coqvar : Var.t -> PVar.t
            = fun var ->
            Var.toInt var
            |> Z.of_int
            |> PedraQOracles.zToCoqPos

        let n_to_coeff : N.t -> Coeff.t
            = fun c ->
            PedraQOracles.coq_QToNb c

        let coeff_to_n : Coeff.t -> N.t
            = fun c ->
            PedraQOracles.nToCoq_Q c
    end
end

module DZ = Lift(DomainZ)

module DQ = Lift(DomainQ)
