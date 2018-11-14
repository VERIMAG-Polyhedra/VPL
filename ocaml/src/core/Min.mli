module Debug : DebugTypes.Type

module type Type = sig
	module VecInput : Vector.Type with module V = Var.Positive and module M = Rtree

	(** [minimize x cstrs] removes the redundancies in the list of constraints [cstrs].
        @param cstrs is the list of constraints to minimize. Syntactic redundancies should have been removed previously.
		@param x is a point that should lie in the interior of the polyhedron defined by [cstrs].
		This function attach to each returned constraint [cstr] a point that violates only [cstr].
		When possible, this point does not saturate the completentary of [cstr]. *)
	val minimize : VecInput.t -> Cstr.Rat.Positive.t list -> (Cstr.Rat.Positive.t * VecInput.t) list

	val minimize_cone : VecInput.t -> Cstr.Rat.Positive.t list -> (Cstr.Rat.Positive.t * VecInput.t) list

	val name : string
end

module Make : functor
    (VecInput : Vector.Type with module V = Var.Positive and module M = Rtree)
    (Vec : Vector.Type with module V = Var.Positive and module M = Rtree)
    (CsInput : Cstr.Type)
    (LP : MinLP.Type) -> sig

    include Type with module VecInput = VecInput

    type conversion = {
		vec_CsVec : Vec.t -> LP.CsUser.Vec.t;
		csVec_Vec : LP.CsUser.Vec.t -> Vec.t;
		csCoeff_Coeff : LP.CsUser.Vec.Coeff.t -> Vec.Coeff.t;
		csInput_Cs : CsInput.t -> LP.CsUser.t;
		vecInput_Vec : VecInput.t -> Vec.t;
		vec_VecInput : Vec.t -> VecInput.t
	}

    val conv : conversion ref

    type direction_type  =
	| Normal of Vec.t * LP.CsUser.Vec.t (** x' = x0 + t * normal *)
	| TwoPoints of Vec.t * Vec.t (** x' = x0 + t * (x1 - x0) *)

	type direction = direction_type * LP.CsUser.Vec.Coeff.t

    module Sort : sig
        val value : direction_type -> LP.CsUser.t -> LP.CsUser.Vec.Coeff.t
        val sort : ('a * direction) list -> ('a * direction) list
		val stack : ('a * direction) list -> ('a * direction) list list
        val getPoint' : direction -> VecInput.t
        val getPoint : LP.CsUser.t -> direction -> VecInput.t
    end
end

module Classic : functor (Vec : Vector.Type with module V = Var.Positive and module M = Rtree) -> Type with module VecInput = Vec

module Glpk : functor (Vec : Vector.Type with module V = Var.Positive and module M = Rtree) -> Type with module VecInput = Vec

module Rat_Glpk : Type with module VecInput = Vector.Rat.Positive

module Heuristic : functor (Vec : Vector.Type with module V = Var.Positive and module M = Rtree) -> Type with module VecInput = Vec
