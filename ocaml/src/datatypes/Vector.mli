(** Type fo vector values.

A vector associates variables from {!modtype:Var.Type} to values from {!modtype:Scalar.Type}.
A vector is either a {!type:Rtree.t} with variables {!type:Var.Positive.t} or a {!type:VarMap.t} with variables {!type:Var.Int.t}.
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
