(** Type of constraints. *)

open Cstr_type

(** Interface of contraints. *)
module type Type = Type

(** Constaints with rational coefficients. *)
module Rat : sig

    (** Constraints with rational coefficients and positive variables. *)
    module Positive : sig
        include Type with module Vec = Vector.Rat.Positive and module Coeff = Scalar.Rat and module V = Var.Positive

        (** [elim c1 c2 x] linearly combines [t1] and [t2] to eliminate [x] from the result.
		The coefficients applied to [c1] and [c2] are returned, in order.
		@raise NoElim if [x] does not appear in any of the two constraints
		@raise CannotElim if elimination is impossible, due to [x] not appearing in one of the two constraints
		or appearing with coefficients of the same sign in two inequalities *)
		val elim : t -> t -> Vec.V.t -> (t * Vec.Coeff.t * Vec.Coeff.t)

        (** @return the canonical form of a constraint.
            Each coefficient is multiplied by the gcd of all coefficients. *)
        val canon : t -> t
    end

    (** Constraints with rational coefficients and integer variables. *)
    module Int : sig
        include Type with module Vec = Vector.Rat.Int and module Coeff = Scalar.Rat and module V = Var.Int

        (** [elim c1 c2 x] linearly combines [t1] and [t2] to eliminate [x] from the result.
		The coefficients applied to [c1] and [c2] are returned, in order.
		@raise NoElim if [x] does not appear in any of the two constraints
		@raise CannotElim if elimination is impossible, due to [x] not appearing in one of the two constraints
		or appearing with coefficients of the same sign in two inequalities *)
		val elim : t -> t -> Vec.V.t -> (t * Vec.Coeff.t * Vec.Coeff.t)

        (** @return the canonical form of a constraint.
            Each coefficient is multiplied by the gcd of all coefficients. *)
        val canon : t -> t
    end
end

(** Constaints with float coefficients. *)
module Float : sig

    (** Constraints with float coefficients and integer variables. *)
    module Positive : Type with module Vec = Vector.Float.Positive and module Coeff = Scalar.Float and module V = Var.Positive

    (** Constraints with float coefficients and integer variables. *)
    module Int : Type with module Vec = Vector.Float.Int  and module Coeff = Scalar.Float and module V = Var.Int
end

(** Pretty-printer for comparators. *)
val cmpT_to_string : cmpT -> string

(** Pretty-printer for extended comparators.*)
val cmpT_extended_to_string : cmpT_extended -> string
