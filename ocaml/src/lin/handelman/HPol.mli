module CP = CstrPoly
module Poly = CP.Poly

class ['c] t : object
	val mutable poly_rep : CP.t list
	val mutable vars : Var.t list
	val mutable vpl_rep : 'c Pol.t option
	method addM : 'c Factory.t -> (CP.t * 'c) list -> 'c t
	method cstrInPol : CP.Cs.t -> bool
	method equal : 'c Factory.t -> 'c Factory.t -> 'c t -> bool
	method get_cstr : unit -> CP.Cs.t list
	method get_ineqs : unit -> CP.Cs.t list
	method get_noneq_poly : CP.t list
	method get_poly_rep : CP.t list
	method get_vars : Var.t list
	method get_vpl_rep : 'c Pol.t option
	method horizon : unit -> Var.t
	method init : unit -> unit
	method isInside : Poly.Vec.t -> bool
	method is_empty : bool
	method mk : 'c Pol.t -> CP.t list -> Var.t list -> unit
	method mkPol : 'c Pol.t -> unit
	method to_string : string
	method private update : unit -> unit
end
