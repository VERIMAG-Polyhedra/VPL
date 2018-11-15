open Poly_type

(** Exception raised when trying to divide a polynomial by a non-constant value *)
exception Div_by_non_constant

module type Type = Type

module Make : functor (Vec : Vector.Type with module M = Rtree and module V = Var.Positive) -> Type with module Vec = Vec