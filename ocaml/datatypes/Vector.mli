(** Type fo vector values.

A vector associates variables from {!module-type:Var.Type} to values from {!module-type:Scalar.Type}.
A vector is a Radix tree (of type {!type:Rtree.t}), indexed by variables (of type {!type:Var.t}).
*)

open Vector_type

(** Interface of vectors. *)
module type Type = Type

(** Module of vectors with rational coefficients. *)
module Rat : Type with module Coeff = Scalar.Rat

(** Module of vectors with float coefficients. *)
module Float : Type with module Coeff = Scalar.Float

(** Module of vectors with symbolic error rational coefficients. *)
module Symbolic : Type with module Coeff = Scalar.Symbolic

(** Module of vectors with integer coefficients. *)
module Int : Type with module Coeff = Scalar.Int
