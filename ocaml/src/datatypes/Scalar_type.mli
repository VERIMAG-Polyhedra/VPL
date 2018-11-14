type roundT = Up | Down

module type Type = sig
	type t

	(** Name of the scalar type. *)
	val name : string

	(** Zero value. *)
	val z: t

	(** Value 1. *)
	val u: t

	(** Value -1. *)
	val negU: t

	(** Small value *)
	val delta : t

	val to_string : t -> string
	val of_string : string -> t

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

	val add : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> t -> t
	val pow : t -> int -> t

	(** [div num den] *)
	val div : t -> t -> t

	(** Multiplication by a rational. *)
	val mulr : Q.t -> t -> t

	(** Division by a rational. *)
	val divr : t -> Q.t -> t

	val of_float : float -> t
	val to_float : t -> float

	val ofQ : Q.t -> t
	val toQ : t -> Q.t

	(** [toInt_round x Up] returns the smallest [i] such that [i >= x].
		[toInt_round x Down] returns the greatest [i] such that [i <= x].
		Returns None if x does not fit within a machine integer. *)
	val toInt_round : t -> roundT -> int option

	(** [mk ~den:i1 i2] builds an element of type {!type:t} and of value [i2]/[i1]. *)
	val mk: int -> int -> t

	(** [mk1 i] builds an element of type {!type:t} from integer [i]. *)
	val mk1 : int -> t

	(** [well_formed x] returns true if x is not +/- infinity *)
	val well_formed : t -> bool

	(** [well_formed_nonnull x] returns true if x is neither 0 nor +/- infinity *)
	val well_formed_nonnull : t -> bool

	(**/**)
	(** Used in the simplex tableau sharing during the distributed PLP.*)
	val plp_print : t -> string

    val gcd : Z.t * Z.t -> t -> Z.t * Z.t
    val toZ : t -> Z.t * Z.t
    val ofZ : Z.t -> Z.t -> t
end
