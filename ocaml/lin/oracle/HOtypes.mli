(** This module defines datatypes used by the oracle that generates Schweighofer products used in Handelman linearization. *)

module CP = CstrPoly
module Poly = CP.Poly

module Debug : DebugTypes.Type

(** This map binds things to elements of type {!type:Poly.t}. *)
module MapP : Map.S with type key = Poly.t

val neg_poly : CstrPoly.t -> Poly.t list

(** Module associating in a map a list of {!type:Hi.t} to a polynomial.
    It is used to remember what products were used to cancel a given polynomial. *)
module MapPolyHi : sig

    (** Map from polynomials to the list of Handelman products that cancel it. *)
	type t = Hi.t list MapP.t

    (** Conversion into string. *)
	val to_string : t -> string

	(** @return [true] if [map] contains a binding to a polynomial that contains monomial [m].
        @param m a monomial
        @param map a cancellation map *)
	val memMonom : Poly.MonomialBasis.t -> t -> bool

	(** @return [true] if [map] contains a binding to a polynomial that contains monomial [m] with a coefficient of the same sign. *)
	val memMonomSigned : Poly.Monomial.t -> t -> bool

    (** Merges two maps. *)
	val merge : t -> t -> t
end

(** This map binds things to elements of type {!type:Index.Int.t}. *)
module MapI = IndexBuild.MapI

(** This module associates in Handelman indexes to polynomials. *)
module MapIndexP : sig

	type t = Poly.t MapI.t

	(** Initializes a map from a list of constraints. *)
	val of_polyhedron : Poly.t list -> t

    (** Conversion into string. *)
	val to_string : t -> string

	(** @return an index [ind] whose [i]th component is the maximum power in [p] of the [i]th variable in [l].
        For instance,
		{ul
            {- [poly_to_deg_max (x^2y - 2x^3 + y^2 + xz^2) [x;z] = (3,2)]}
            {- [poly_to_dex_max (x^2z - z^3) [x;y;z] = (2,0,3)]}
        }
        @param p the polynomial
        @param vl the list of variables *)
	val poly_to_deg_max : Poly.t -> Var.t list -> Index.Int.t

	(** [get ind mapIP mapI] returns the polynomial corresponding to index [ind] in map [mapIP].
	If [ind] has no binding in [mapIP], maps are updated. *)
	val get : Index.Int.t -> t -> IndexBuild.Map.t -> (Poly.t * t * IndexBuild.Map.t)
end

(** This map binds things to elements of type {!type:Var.t}. *)
module MapV : Map.S with type key = Var.t

(** This module defines the maps used in {!module:HLP}. They gather information about variable bounds. *)
module LPMaps : sig

	type t = Upper | Lower

	(** It associates a variable [v] to a pair of option booleans [(b1,b2)].
	[b1] = [Some true] (resp. [b2] = [Some true]) means that [v] has a lower (resp. upper) bound.
	[b1] = [Some false] (resp. [b2] = [Some false]) means that [v] has no lower (resp. upper) bound.
	[b1] = [None] (resp. [b2] = [None]) means that we don't have information on [v]'s lower (resp. upper) bound. *)
	type mapDetBound = (bool option * bool option) MapV.t

	(** It associates a variable to a pair of option {!type:Hi.boundIndex}. *)
	type mapBound = (Hi.boundIndex option * Hi.boundIndex option)  MapV.t

	type bounds = {mapDB : mapDetBound ; mapB : mapBound}

	(** [init pl vl] initializes maps with bounds that appear syntactically in [pl]. *)
	val init : Poly.t list -> Var.t list -> bounds

	(** [hasSup v mapDB] returns 0 if [v] has no upper bound in [mapDB], 1 otherwise. *)
	val hasSup : Var.t -> mapDetBound -> Q.t

	(** [hasInf v mapDB] returns 0 if [v] has no lower bound in [mapDB], 1 otherwise. *)
	val hasInf : Var.t -> mapDetBound -> Q.t

	(** [detSup v mapDB] returns 1 if [v] has an upper bound in [mapDB], 0 otherwise. *)
	val detSup : Var.t -> mapDetBound -> Q.t

	(** [detInf v mapDB] returns 1 if [v] has a lower bound in [mapDB], 0 otherwise. *)
	val detInf : Var.t -> mapDetBound -> Q.t

	val mapDB_to_string : mapDetBound -> string

	val mapB_to_string : mapBound -> string

	(** [updateMapDB mapDB v value bound] binds in [mapDB] the upper bound of variable [v] to [value] if [upper] = [true].
	Otherwise, it binds the lower bound of variable [v] to [value]. *)
	val updateMapDB : mapDetBound -> Var.t -> bool -> t -> mapDetBound

	(** [updateMapB mapB v value bound] binds in [mapB] the upper bound of variable [v] to [value] if [upper] = [true].
	Otherwise, it binds the lower bound of variable [v] to [value]. *)
	val updateMapB : mapBound -> Var.t -> Hi.boundIndex -> t -> mapBound

end
