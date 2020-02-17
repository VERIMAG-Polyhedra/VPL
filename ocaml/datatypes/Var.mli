(** Types of variables, which are positive integers. *)

(** Type of variable *)
type t =
    | XH (** Last bit: represents value 1. The end of a path. *)
    | XO of t (** Bit 0. Take the left branch. *)
    | XI of t (** Bit 1. Take the right branch. *)

(** First variable : equal to one. *)
val u : t

(** [cmp x1 x2] compares [x1] and [x2] with "less" meaning "comes before in a breadth-first search".
The result follows the same convention as [Stdlib.compare]. *)
val cmp: t -> t -> int

(** Equality test between two variables. *)
val equal : t -> t -> bool

(** [toInt p] returns the integer which [p] represents, according to the Coq definition of [positive].
If the conversion of [p] overflows the [int] representation, [Invalid_argument "Var.toInt"] is raised. *)
val toInt: t -> int

(** [fromInt i] converts a strictly positive integer into its bit string representation.
If [i] is negative or nil, [Invalid_argument "Var.fromInt"] is raised.*)
val fromInt: int -> t

(** Pretty-print a value [v] of type [t] as "vN", where N is the number
denoted by [v]. *)
val to_string: t -> string

(** [to_string' s v] pretty-prints [v] as "sN", where N is the number denoted by [v]. *)
val to_string': string -> t -> string

(** A module for manipulation sets of values of type [t]. *)
module Set : Set.S with type elt = t

(** [horizon s] returns the path which immediately follows the maximum path found in [s]. *)
val horizon: Set.t -> t

(** @return the maximum of a given list of variables, w.r.t. {!val: cmp}. *)
val max : t list -> t

(** [next v] gives the path to the next variable in a breadth-first search in a tree.
next of [... XO XH] is [... XI XH],
next of [... XO (XI XH)] is [... XI (XO XH)] and
next of [XI (XI XH)] is [XO (XO (XO XH))].
*)
val next: t -> t

(**/**)
(* Used in {!module:Splx} *)
val fromLeft : t -> t

val fromRight : t -> t

val plp_print : t -> string
(**/**)

(** Build a value of type [t] from a string of the form ["xn"],
where [x] is a single letter and [n] is an integer.  The resulting
[t] has the value [n]. *)
val of_prefixed_string : string -> t
