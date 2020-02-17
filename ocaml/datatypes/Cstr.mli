(** Type of constraints. *)

open Cstr_type

(** Interface of contraints. *)
module type Type = Type

(** Constaints with rational coefficients. *)
module Rat : sig

    include Type with module Vec = Vector.Rat and module Coeff = Scalar.Rat

    (** [elim c1 c2 x] linearly combines [c1] and [c2] to eliminate [x] from the result.
	The coefficients applied to [c1] and [c2] are returned, in order.
	@raise NoElim if [x] does not appear in any of the two constraints
	@raise CannotElim if elimination is impossible, due to [x] not appearing in one of the two constraints
	or appearing with coefficients of the same sign in two inequalities *)
	val elim : t -> t -> Var.t -> (t * Vec.Coeff.t * Vec.Coeff.t)

    (** @return the canonical form of a constraint.
        Each coefficient is multiplied by the gcd of all coefficients. *)
    val canon : t -> t
end

(** Pretty-printer for comparators. *)
val cmpT_to_string : cmpT -> string

(** Pretty-printer for extended comparators.*)
val cmpT_extended_to_string : cmpT_extended -> string
