(** Type of polynomials. *)

open Poly_type

(** Exception raised when trying to divide a polynomial by a non-constant value *)
exception Div_by_non_constant

(** Interface of polynomials. *)
module type Type = Type

(** Functor to build a polynomial type from a type of vectors.
    The type of vector coefficients is used for polynomial coefficients.
    The type of vectors is used to build polynomials from constraints. *)
module Make : functor (Vec : Vector.Type) -> Type with module Vec = Vec
