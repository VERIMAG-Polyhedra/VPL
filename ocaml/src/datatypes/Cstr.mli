open Cstr_type

module type Type = Type

module Rat : sig
    module Positive : sig
        include Type with module Vec = Vector.Rat.Positive and module Coeff = Scalar.Rat and module V = Var.Positive

        (** [elim c1 c2 x] linearly combines [t1] and [t2] so as to eliminate [x] from the result.
		The coefficients applied to [c1] and [c2] are returned, in order.
		If [x] does not appear in any of the two, then exception [NoElim] is raised.
		If elimination is impossible, due to [x] not appearing in one of the two constraints
		or appearing with coefficients of the same sign in two inequalities, [CannotElim] is raised. *)
		val elim : t -> t -> Vec.V.t -> (t * Vec.Coeff.t * Vec.Coeff.t)

        val canon : t -> t
    end

    module Int : sig
        include Type with module Vec = Vector.Rat.Int and module Coeff = Scalar.Rat and module V = Var.Int

		val elim : t -> t -> Vec.V.t -> (t * Vec.Coeff.t * Vec.Coeff.t)

        val canon : t -> t
    end
end

module Float : sig
    module Positive : Type with module Vec = Vector.Float.Positive and module Coeff = Scalar.Float and module V = Var.Positive

    module Int : Type with module Vec = Vector.Float.Int  and module Coeff = Scalar.Float and module V = Var.Int
end

val cmpT_extended_to_string : cmpT_extended -> string
val cmpT_to_string : cmpT -> string
