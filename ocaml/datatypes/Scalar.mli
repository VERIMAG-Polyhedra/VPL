(** Types for scalar values.
Most scalar used in the VPL are rationals.
But some applications (e.g. finding a point in the interior of a polyhedron) require the handling of a symbolic error.

{ul
	{- {!module:Rat}: zarith rationals}
    {- {!module:Int}: zarith integers}
	{- {!module:Symbolic}: rationals of the form [a + b.delta] where [delta] represents a symbolic error}
	{- {!module:Float}: floating points}
}*)

open Scalar_type

(** Interface of scalar modules. *)
module type Type = Type

(** Type of rational constants. *)
module Rat : Type with type t = Q.t

(** Type of float constants. *)
module Float : Type with type t = float

(** Type of integer constants. *)
module Int : Type with type t = Z.t

(** Type of rationals with symbolic error. *)
type symbolic = {
    v: Q.t;
    d: Q.t;
}

(** Type of rationals with symbolic error. *)
module Symbolic : sig
    include Type with type t = symbolic
    val hasDelta : t -> bool

    val get_d : t -> Q.t

    val get_v : t -> Q.t

    val pdelta : Q.t -> t

	val ndelta : Q.t -> t

    val subdelta : t -> t
    val adddelta : t -> t
end
