open Scalar_type

module type Type = Type

module Rat : Type with type t = Q.t

module Float : Type with type t = float

module RelInt : Type with type t = Z.t

type symbolic = { v: Q.t; d: Q.t }

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

module MachineInt : Type with type t = int
