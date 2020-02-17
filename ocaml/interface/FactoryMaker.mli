(* Module of factory makers. *)

(** Interface of factory makers.
    Provides operators for creating certificates. *)
module type Type = sig

    (** Type of certificates. *)
	type cert

    (** The factory itself. *)
	val factory : cert Factory.t

    (** Builds a certificate from a contraint.*)
	val mk : Cstr.Rat.t -> cert

    (** Checks if a constraint and a certificate represent the same space. *)
	val equal : Cstr.Rat.t -> cert -> bool
end

(** Functor that builds a factory from a factory maker. *)
module Make : functor (F : Type) -> sig
    include Type

    (** Builds a cons from a constraint. *)
    val mkCons : Cstr.Rat.t -> cert Cons.t

    (** Converts polymorphic certificates of a polyhedron into type {!type:F.cert}. *)
    val convert : 'c Pol.t -> cert Pol.t

    (** Checks if all certificates of a polyhedron correspond to their associated
        contraints. *)
    val check : cert Pol.t -> bool

    (** Pretty-printer for certificates. *)
    val to_string : cert -> string
end

module Cstr : Type with type cert = Cstr.Rat.t

module Unit : Type with type cert = unit
