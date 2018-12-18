(** Types for scalar values.

{ul
	{- {!module:Rat}: rationals}
    {- {!module:Int}: mathematical integers}
	{- {!module:Symbolic}: rationals of the form [a + b.delta] where [delta] represents a symbolic error}
	{- {!module:Float}: floating points}
    {- {!module:MachineInt}: machine integers}
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
type symbolic = { v: Q.t; d: Q.t }

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

(** Type of machine integer constants. *)
module MachineInt : Type with type t = int
