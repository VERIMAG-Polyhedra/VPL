(** Type of polynomial constraints. *)

module Cs = Cstr.Rat
module Poly : Poly.Type with module Vec = Vector.Rat

(** A polynomial constraint of the form [p typ 0]. *)
type t = {
	typ : Cstr_type.cmpT; (** the comparison operator *)
	p: Poly.t; (** the polynomial *)
}

val to_string : t -> string

(** [mk typ poly] builds the linear constraint [p typ 0]. *)
val mk: Cstr_type.cmpT -> Poly.t -> t

(** [eq p] builds the equality [p = 0] *)
val eq: Poly.t -> t

(** [le p] builds the equality [p <= 0] *)
val le: Poly.t -> t

(** [lt p] builds the equality [p < 0] *)
val lt: Poly.t -> t

val compl : t -> t

(** [empty] represents 0 = 0*)
val empty : t

(** [equal c1 c2] checks that the two polynomials within constraints are syntactically equal. *)
val equal: t -> t -> bool

(** syntaxic comparison *)
val cmp : t -> t -> int
val compare : t -> t -> int

val toCstr : t -> Cs.t

val ofCstr : Cs.t -> t

(** Separates the affine constraints from the given list of polynomial constraints. *)
val partition_affine : t list -> (Cs.t list * t list)
