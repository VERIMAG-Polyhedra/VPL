(** Type of factories.

A factory defines a set of operators on certificates.
The type of factory is polymorphic to ensure soundness.
This polymorphic type may be instantiated with certified constraints extracted from Coq.
For efficiency, it may also be instantiated with unit or with anything else.
*)

(** Type of polymorphic factory *)
type 'c t = {
    name : string; (** Factorty name *)
    top : 'c; (** trivial constraint 0 = 0. Equivalent to triv {!val:Cstr_type.Eq} 0. *)
    triv: Cstr_type.cmpT -> Scalar.Rat.t -> 'c; (** Trivial constraint 0 cmp n. *)
    add : 'c -> 'c -> 'c; (** Adding two constraints. *)
    mul : Scalar.Rat.t -> 'c -> 'c;  (** Multiplication of constraint with a nonnegative rational.
        The constant may be negative if the constraint is an equality. *)
    merge : 'c -> 'c -> 'c;  (** Merges two complementary inequalities to build an equality. *)
    to_le : 'c -> 'c; (** Relaxes a constraint into a large inequality. *)
    to_string : 'c -> string; (** Pretty-printer of constraints. *)
    rename : Cstr.Rat.Positive.Vec.V.t -> Cstr.Rat.Positive.Vec.V.t -> 'c -> 'c; (** Renaming a variable in a constraint. *)
}

(** Computes a linear combinations of a list of certificates. *)
val linear_combination : 'c t -> ('c * Scalar.Rat.t) list -> 'c
