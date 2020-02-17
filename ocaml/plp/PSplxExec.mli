(** This modules contains functions for executing the simplex algorithm on a parametric sumplex tableau. *)

open PSplxBuild

module Cs = Cstr.Rat

(** Exception raised when a problem is unbounded. *)
exception Unbounded_problem

(** Exception raised when a problem is infeasible. *)
exception Infeasible_problem

module Explore : sig

    val pivot : bool -> 'c PSplx.t -> int -> int -> unit

    (** Run the optimization process of the simplex algorithm.
        @param init_phase is true if this is the initialization phase
        @param point the parametric point on which the objective function is instantiated
        @param sx the simplex tableau *)
    val push : bool -> Cs.Vec.t -> 'c PSplx.t -> unit

end

module Init : sig
    val chooseBasicVar : int -> 'c PSplx.t -> bool

    (** Tries to find a feasible basis.
        @param factory the certified factory
        @param sx the simplex tableau to initialize
        @param point the instantiation parametric point
        @return [false] if the problem is infeasible, [true] otherwise *)
    val findFeasibleBasis : 'c Factory.t -> 'c PSplx.t -> Cs.Vec.t -> bool
end

(** Initializes the simplex tableau and launch the optimization.
    @param factory the certified factory
    @param point the parametric point on which the objective function is instantiated
    @param sx the simplex tableau
    @return [false] if the problem is infeasible, [true] otherwise *)
val init_and_push : 'c Factory.t -> Cs.Vec.t -> 'c PSplx.t -> bool
