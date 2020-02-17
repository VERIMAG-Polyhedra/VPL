(** This module defines the type of Handelman products.
    More precisely, tThey are Schweighofer products. *)

module Debug : DebugTypes.Type

module CP = CstrPoly
module Poly = CP.Poly

(** Type of Handelman indices.
    For instance, [cIndex (1, 0, 3)] represents C_1^1 * C_2^0 * C_3^3. *)
type cIndex = Index.Int.t

(** Type of indices that represent a product of variables.
    For instance, [varIndex (1, 0, 3)] represents x_1^1 * x_2^0 * x_3^3. *)
type varIndex = Index.Int.t

(** Type defining the Farkas decomposition of a constraint.
    For instance, [boundIndex (1, 0, 3)] represents 1*C_1 + 0*C_2 + 3*C_3. *)
type boundIndex = Index.Rat.t

(** Defines the type of Schweighofer products as either :
{ul
    {- A Handelman product using type {!type:cIndex}}
	{- A product of variables bounds (each of them is represented by its Farkas decomposition using type {!type:boundIndex}) multiplied by a monomial of type {!type:varIndex}}
	{- a Handelman product (type {!type:cIndex}) multiplied by a monomial of type {!type:varIndex}}
} *)
type t =
| Ci of cIndex
| VarBounds of varIndex * (boundIndex list)
| VarCi of varIndex * cIndex

(** Conversion into string. *)
val to_string : t -> string

(** Equality test. *)
val eq : t -> t -> bool

(** Is a Handelman product linear.*)
val is_linear : t -> bool

(** Computes the polynomial corresponding to a variable index.
    @param id the variable index
    @param the list of variables *)
val computeVarIndex : Index.Int.t -> Var.t list -> Poly.t

(** Computes the given Farkas combination.
    @param the Farkas coefficients
    @param the constraints *)
val computeBoundIndex : Index.Rat.t -> Poly.t list -> Poly.t

(** Computes a product of Farkas combinations.
    @param the list of Farkas coefficients
    @param the constraints *)
val computeBoundIndexList : Index.Rat.t list -> Poly.t list -> Poly.t

(** Module for building certificates out of Handelman products. *)
module Cert : sig

	type squares = (Var.t * int) list

	type schweighofer = Scalar.Rat.t * (cIndex * squares * boundIndex list)

	(** [hi_to_cert n_cstrs vars coeff hi] *)
	val hi_to_cert : int -> Var.t list -> Scalar.Rat.t -> t -> schweighofer

    val to_string : schweighofer list -> string
end
