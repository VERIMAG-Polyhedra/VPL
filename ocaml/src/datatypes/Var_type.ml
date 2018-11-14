type positive =
    | XH
    | XO of positive
    | XI of positive

module type Type = sig
	type t

	(** Name of the module. *)
	val name : string

	(** [cmp x1 x2] compares [x1] and [x2] with "less" meaning "comes before in a breadth-first search".
	The result follows the same convention as [Pervasives.compare]. *)
	val cmp: t -> t -> int

	val equal : t -> t -> bool

	(** [toInt p] returns the integer which [p] represents, according to the Coq definition of [positive].
	If the conversion of [p] overflows the [int] representation, [Invalid_argument "Var.toInt"] is raised. *)
	val toInt: t -> int

	(** [fromInt i] converts a strictly positive integer into its bit string representation.
	If [i] is negative or nil, [Invalid_argument "Var.fromInt"] is raised.*)
	val fromInt: int -> t

	val toPos: t -> positive
	val fromPos: positive -> t

	(** Pretty-print a value [v] of type [t] as "vN", where N is the number
	denoted by [v]. *)
	val to_string: t -> string

	(** [to_string' s v] pretty-prints [v] as "sN", where N is the number denoted by [v]. *)
	val to_string': string -> t -> string

	val plp_print : t -> string

	(** A module for manipulation sets of values of type [t]. *)
	module Set : Set.S with type elt = t

	(** [horizon s] returns the path which immediately follows the maximum path found in [s]. *)
	val horizon: Set.t -> t

	val max : t list -> t

	(** Depend on the kind of variables. See {!val:Positive.next} and {!val:Int.next}. *)
	val next: t -> t

	(**/**)
	(* utilisé dans Splx.ml *)
	val fromLeft : t -> t

	val fromRight : t -> t
	(**/**)

	(** First variable : equal to one. *)
	val u : t

	(** Build a value of type [t] from a string of the form ["xn"],
	where [x] is a single letter and [n] is an integer.  The resulting
	[t] has the value [n]. *)
	val of_prefixed_string : string -> t
end
