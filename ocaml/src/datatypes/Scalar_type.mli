(** Interface of scalar types. *)

(** Rouding type when converting into integers. *)
type roundT = Up | Down

(** Interface of scalar types. *)
module type Type = sig

    (** Type of scalar value *)
	type t

	(** Name of the scalar type. *)
	val name : string

	(** Zero value. *)
	val z: t

	(** Value 1. *)
	val u: t

	(** Value -1. *)
	val negU: t

	(** Small value used for conversions from {!type:Scalar.symbolic}. *)
	val delta : t

	(** [cmp x y] returns 0 if x = y, a negative integer if x < y, and a positive one otherwise. *)
	val cmp : t -> t -> int

	(** [le x y] returns true if x <= y, false otherwise. *)
	val le: t -> t -> bool

	(** [lt x y] returns true if x < y, false otherwise. *)
	val lt: t -> t -> bool

	(** [cmpz n] is equivalent to [cmp z n]. *)
	val cmpz: t -> int

	(** [equal x y] returns true if x = y, false otherwise. *)
	val equal : t -> t -> bool

	(** [equalApprox x y] returns true if [abs (x - y) <= delta]. *)
	val equalApprox : t -> t -> bool

	(** [isZ n] returns true is [n] is zero ([z]) and false otherwise. *)
	val isZ : t -> bool

	(** Compute the opposite of a number. *)
	val neg: t -> t

	(** Compute the inverse of a number. *)
	val inv: t -> t

	(** Compute the absolute value. *)
	val abs: t -> t

    (** Addition of two constants. *)
	val add : t -> t -> t

    (** Substraction of a constant by another. *)
	val sub : t -> t -> t

    (** Multiplication of two constants. *)
	val mul : t -> t -> t

    (** Power of a constant. *)
	val pow : t -> int -> t

	(** [div num den] *)
	val div : t -> t -> t

	(** Multiplication by a rational. *)
	val mulr : Q.t -> t -> t

	(** Division by a rational. *)
	val divr : t -> Q.t -> t

    (** [mk ~den:i1 i2] builds an element of type {!type:t} and of value [i2]/[i1]. *)
	val mk: int -> int -> t

    (** Builds an element of type {!type:t} from an integer. *)
	val of_int : int -> t

    (** [to_int x Up] returns the smallest [i] such that [i >= x].
		[to_int x Down] returns the greatest [i] such that [i <= x].
		Returns None if x does not fit within a machine integer. *)
	val to_int : t -> roundT -> int option

    (** Conversion from float. *)
	val of_float : float -> t

    (** Conversion to float. *)
	val to_float : t -> float

    (** Conversion into integer. *)
    val toZ : t -> Z.t * Z.t

    (** Conversion from integer. *)
    val ofZ : Z.t -> Z.t -> t

    (** Conversion from rational. *)
	val ofQ : Q.t -> t

    (** Conversion into rational. *)
	val toQ : t -> Q.t

    (** Conversion into string. *)
	val to_string : t -> string

    (** Conversion from string. *)
	val of_string : string -> t

	(** [well_formed x] returns true if x is not +/- infinity *)
	val well_formed : t -> bool

	(** [well_formed_nonnull x] returns true if x is neither 0 nor +/- infinity *)
	val well_formed_nonnull : t -> bool

	(**/**)
	(** Used in the simplex tableau sharing during the distributed PLP.*)
	val plp_print : t -> string

    (** [gcd nGcd dGcd n] returns the gcds of nGcd and the numerator of [n] and that of [dGcd] and the denominator of [n]. *)
    val gcd : Z.t * Z.t -> t -> Z.t * Z.t

end
