(** Interface of vector types.*)

(** Interface of vector types.

A vector is a map associating variables to scalar coefficients.
They are represented as sparse structures: only non-zero coefficients are stored
in the map. *)
module type Type = sig

    (** A type of scalars. *)
	module Coeff : Scalar.Type

    (** Type of vectors associating variables to coefficients. *)
	type t = Coeff.t Rtree.t

    (** Name of the module. *)
	val name : string

	(** A vector with coefficients all set to zero. *)
	val nil: t

    (** Conversion into string.
        @param var_to_string a pretty-printer for variables. *)
	val to_string : (Var.t -> string) -> t -> string

    (** Builds a vector from a list of pairs coefficient * variable.*)
	val mk : (Coeff.t * Var.t) list -> t

    (** Retrieve coefficients and variables of a vector. *)
	val toList : t -> (Var.t * Coeff.t) list

	(** [get vec v] returns the coefficient of variable [v] in vec. Returns zero if [v] is not present in [vec]. *)
	val get: t -> Var.t -> Coeff.t

	(** [set vec v n] sets the coefficient of variable [v] to [n] in vector [vec]. *)
	val set: t -> Var.t -> Coeff.t -> t

	(** [getvars l] returns the set of variables that appear in the list of vectors [l]. *)
	val getVars: t list -> Var.Set.t

    (** Evaluates a vector on a point.
        @param vec vector to evaluate
        @param point evaluation point *)
	val eval: t -> t -> Coeff.t

	(** [neg vec] multiplies each coefficient in [vec] by -1. *)
	val neg : t -> t

    (** Adding two vectors. *)
	val add : t -> t -> t

    (** Substracting two vectors. *)
	val sub : t -> t -> t

	(** Computes the multiplication componentwise.*)
	val mul_t : t -> t -> t

	(** Multiplication by a scalar. *)
	val mulc : Coeff.t -> t -> t

	(** Division by a scalar. *)
	val divc : t -> Coeff.t -> t

	(** Multiplication by a Rational. *)
	val mulr : Scalar.Rat.t -> t -> t

	(** Division by a Rational. *)
	val divr : t -> Scalar.Rat.t -> t

	(** [middle x y] returns the middle point of the segment [xy]. *)
	val middle : t -> t -> t

	(** Syntaxic comparison *)
	val cmp : t -> t -> int

	(** Syntaxic equality *)
	val equal : t -> t -> bool

    (** Dot product of two vectors. *)
	val dot_product : t -> t -> Coeff.t

	(** Same as {!val:dot_product}, where the first vector has rational coefficients. *)
	val dot_productr : Scalar.Rat.t Rtree.t -> t -> Coeff.t

	(** [isomorph v1 v2] returns [Some r] if [v1] is equal to [mult r v2].
	If there is no such [r], [None] is returned. *)
	val isomorph: t -> t -> Coeff.t option

	(** [elim x v1 v2] eliminates the [x] from [v2] using [v1].
	If [v2] has coefficient zero for variable [x], it is returned untouched.
	Otherwise, if variable [x] has coefficient zero in [v1], [Invalid_argument "Vec.elim"] is raised. *)
	val elim: Var.t -> t -> t -> t

	(**/**)
	val shift: Var.t -> t -> Var.t option Rtree.t -> Var.t * t * Var.t option Rtree.t
	(**/**)

    (** Conversion from a vector with symbolic error.
        @raise Failure if called with integer coefficients. *)
	val ofSymbolic : Scalar.Symbolic.t -> Coeff.t

    (** Conversion into a rational vector. *)
	val toRat : t -> Scalar.Rat.t Rtree.t

    (** Conversion from a rational vector.*)
	val ofRat : Scalar.Rat.t Rtree.t -> t

    (** Eliminates a list of variables from a vector, by setting their value to 0.
        @param vars the variables to eliminate
        @param vec the vector *)
    val project : Var.t list -> t -> t

    (** Renames a variable.
        @param fromX the variable to rename
        @param toY the new variable name
        @param vec the vector *)
    val rename : Var.t -> Var.t -> t -> t

    (** Renames all variables of a vector according to a renaming function
        @param f the renaming function
        @param vec the vector to rename *)
	val rename_f : (Var.t -> Var.t) -> t -> t

    (** @return the gcd of the coefficients in the vector. *)
    val gcd : t -> Q.t
end
