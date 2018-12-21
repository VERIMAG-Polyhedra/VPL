(** Type fo vector values.

A vector associates variables from {!modtype:Var.Type} to values from {!modtype:Scalar.Type}.
A vector is either a {!type:Rtree.t} with variables {!type:Var.Positive.t} or a {!type:VarMap.t} with variables {!type:Var.Int.t}.
*)

open Vector_type

(** Interface of vectors. *)
module type Type = Type

(** Module of vectors with rational coefficients. *)
module Rat : sig

    (** Module of vectors with rational coefficients and mapped with {!module:Rtree}. *)
    module Positive : Type with module Coeff = Scalar.Rat
end

(** Module of vectors with float coefficients. *)
module Float : sig

    (** Module of vectors with float coefficients and mapped with {!module:Rtree}. *)
    module Positive : Type with module Coeff = Scalar.Float

end

(** Module of vectors with symbolic error rational coefficients. *)
module Symbolic : sig

    (** Module of vectors with symbolic error rational coefficients and mapped with {!module:Rtree}. *)
    module Positive : Type with module Coeff = Scalar.Symbolic
end

(** Module of vectors with integer coefficients. *)
module Int : sig

    (** Module of vectors with integer coefficients and mapped with {!module:Rtree}. *)
    module Positive : Type with module Coeff = Scalar.Int
end
