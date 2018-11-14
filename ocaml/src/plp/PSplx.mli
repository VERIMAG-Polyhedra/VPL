(** This module implements a minimizing simplex, with parametric objective function.
It means that coefficients of the objective function are affine functions of parameters.
The given simplex tableau must respect these conditions:
{ul
	{- Variables are nonnegative. No need to provide nonnegativity constraints, they are implicitly known. }
	{- Constraints are nonstrict. }
}*)


open PSplx_type

module type Type = Type
module Debug : DebugTypes.Type

module Make : functor (Vec2: Vector.Type with module M = Rtree and module V = Var.Positive) -> Type with module Vec = Vec2
