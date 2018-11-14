module type Type = sig
	module M : VarMap.Type
	module V : Var.Type
	module Coeff : Scalar.Type
	type t = Coeff.t M.t

	val name : string

	(** A vector with coefficients all set to zero. *)
	val nil: t

	val to_string : (V.t -> string) -> t -> string

	val mk : (Coeff.t * V.t) list -> t

	val toList : t -> (V.t * Coeff.t) list

	(** [get vec v] returns the coefficient of variable [v] in vec. Returns {!val:Coeff.z} if [v] is not present in [vec]. *)
	val get: t -> V.t -> Coeff.t

	(** [set vec v n] sets the coefficient of variable [v] to [n] in vector [vec]. *)
	val set: t -> V.t -> Coeff.t -> t

	(** [getvars l] returns the set of variables that appear in the list of vectors [l]. *)
	val getVars: t list -> V.Set.t

	(** [neg vec] multiplies each coefficient in [vec] by -1. *)
	val neg : t -> t


	val add : t -> t -> t
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

	val dot_product : t -> t -> Coeff.t

	(** Same as {!val:dot_product}, where the first vector has rational coefficients. *)
	val dot_productr : Scalar.Rat.t M.t -> t -> Coeff.t

	(** [isomorph v1 v2] returns [Some r] if [v1] is equal to [mult r v2].
	If there is no such [r], [None] is returned. *)
	val isomorph: t -> t -> Coeff.t option

	(** [elim x v1 v2] eliminates the [x] from [v2] using [v1].
	If [v2] has coefficient zero for variable [x], it is returned untouched.
	Otherwise, if variable [x] has coefficient zero in [v1], [Invalid_argument "Vec.elim"] is raised. *)
	val elim: V.t -> t -> t -> t

	(**/**)
	val shift: V.t -> t -> V.t option M.t -> V.t * t * V.t option M.t
	(**/**)

	val ofSymbolic : Scalar.Symbolic.t -> Coeff.t

	val toRat : t -> Scalar.Rat.t M.t
	val ofRat : Scalar.Rat.t M.t -> t

    (** [projects vars v] eliminates all variables from [vars] into [v], by setting their value to 0. *)
    val project : V.t list -> t -> t

    (** [rename fromX toY vec] *)
    val rename : V.t -> V.t -> t -> t

	val rename_f : (V.t -> V.t) -> t -> t

    val gcd : t -> Q.t
end
