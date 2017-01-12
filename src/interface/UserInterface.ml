open CWrappers

module Cs = Cstr.Rat.Positive
module Vec = Cs.Vec
module Var = Vec.V
module CP = CstrPoly.Positive
module Polynomial = CP.Poly
(*
module type Type = sig
	module Coeff : Scalar.Type
	
	module type Term_T = sig
		
		type t =
		| Var of Var.t
		| Cte of Coeff.t
		| Add of t * t
		| Sum of t list
		| Opp of t
		| Mul of t * t
		| Prod of t list
		(* | Poly of Polynomial.t *)
		| Annot of Annot.t * t
		
		val to_poly : t -> Polynomial.t
		
		val to_cp : cmpT * t -> CP.t
	
		val to_string : (Var.t -> string) -> t -> string
	end
	
	module Term : Term_T
	
	module Cond : sig
        
		type t = 
		| Basic of bool
		| Atom of Term.t * cmpT * Term.t
		| BinL of t * binl * t
		| Not of t

		val to_string : (Var.t -> string) -> t -> string
	end
	
	module type Ident_t = sig
		type t

		val compare: t -> t -> int
		val toVar: t -> Var.t
		val to_string: t -> string
	end
	
	module type Expr_t = sig

		module Ident : Ident_t
		module Term_ : Term_T
		type t 

		(** {!val:to_term} may raise this exception. *)
		exception Out_of_Scope

		val to_term: t -> Term_.t
	end
	
	module Interface : functor (I : HighLevelDomain)(Expr : Expr_t with module Term_ = Term) -> sig
	end
	
	
end
*)
module Interface (Coeff : Scalar.Type) = struct
	
	include Interface(Coeff)
	
	module type Ident_t = sig
		type t

		val compare: t -> t -> int
		val toVar: t -> Var.t
		val to_string: t -> string
	end
	
	module type Term_T = sig
		type t
	end
	
	module type Expr_t = sig

		module Ident : Ident_t
		type t 

		(** {!val:to_term} may raise this exception. *)
		exception Out_of_Scope

		val to_term: t -> Term.t
	end
	
	(** If you want to use directly the VPL datatypes, instanciate the functor Interface with this module Expr.*)
	module VPL_Expr = struct
		module Ident = struct
			include Var
			let compare x y = cmp x y
			
			let toVar x = x
		end
		
		type t = Term.t
		
		exception Out_of_Scope
		let to_term x = x
	end
		
	module Interface (I : HighLevelDomain)(Expr : Expr_t) = struct
		
		include I
		
		module Expr = Expr
		
		module UserCond = struct

			type t = 
				| Basic of bool
				| Atom of Expr.t * cmpT * Expr.t
				| BinL of t * binl * t
				| Not of t
			
			let rec to_cond : t -> Cond.t
				= function
				| Basic b -> Cond.Basic b
				| Atom (e1,cmp,e2) -> Cond.Atom (Expr.to_term e1, cmp, Expr.to_term e2)
				| BinL (c1,bl,c2) -> Cond.BinL(to_cond c1, bl, to_cond c2)
				| Not c -> Cond.Not (to_cond c)
		end
		
		module User = struct
			
			let rename : Expr.Ident.t -> Expr.Ident.t -> t -> t
				= fun fromX toY p ->
				rename (Expr.Ident.toVar fromX) (Expr.Ident.toVar toY) p
			
			let assume: UserCond.t -> t -> t
				= fun c p ->
				assume (UserCond.to_cond c) p

			let asserts: UserCond.t -> t -> bool
				= fun c p ->
				asserts (UserCond.to_cond c) p

			let assign: (Expr.Ident.t * Expr.t) list -> t -> t
				= fun l p ->
				assign 
					(List.map (fun (v,e) -> Expr.Ident.toVar v, Expr.to_term e) l) 
					p
			
			let project: Expr.Ident.t list -> t -> t
				= fun vars p ->
				let vars' = List.map Expr.Ident.toVar vars in
				project vars' p
				
			let guassign: (Expr.Ident.t list) -> UserCond.t -> t -> t
				= fun vl c p ->
				guassign (List.map Expr.Ident.toVar vl) (UserCond.to_cond c) p
			
			let getUpperBound : t -> Expr.t -> Pol.bndT option
				= fun p expr ->
				getUpperBound p (Expr.to_term expr)
			
			let getLowerBound : t -> Expr.t -> Pol.bndT option
				= fun p expr ->
				getLowerBound p (Expr.to_term expr)
			
	  		let itvize : t -> Expr.t -> Pol.itvT
	  			= fun p expr ->
				itvize p (Expr.to_term expr)
		end
	end
end


