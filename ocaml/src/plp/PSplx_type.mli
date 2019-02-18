(** This module defines the type of parametric simplex tableau.
    In VPL, the PLP is always use to express a combination of some input linear constraints. This specificity is thus put  directly in the simplex tableau.
    Each column of the tableau is associated with an input constraint (with a certificate).
    Constraints of the problem are expressed as a function of these constraints. *)

module Cs = Cstr.Rat

(** Each column of the simplex tableau is associated with a decision variable.
Therefore, decision variables are identified by their column position in the simplex tableau. *)
type decision_variable = int

(** Decision variables can be given a set of variables, with which they are allowed to pivot.
    By default, variables are given the set [1]. *)
type var_set = int

module VarMap : Map.S with type key = decision_variable

(** Type of simplex tableau. *)
type 'c t = {
    obj : Objective.t; (** Objective function *)
    mat : Tableau.Matrix.t; (** Constraint matrix *)
    get_set : var_set VarMap.t; (** map associating decision variables to their set *)
    basis : int list;
    sets : var_set list; (** Associates a var_set to each row *)
    cstrs : 'c Cons.t list; (** The constraint associated to each variable. *)
    add_col : 'c Cons.t -> Tableau.Vector.t; (* Computes the the column from a constraint. *)
}

(** Empty simplex tableau. *)
val empty : t

(** Conversion intro string. *)
val to_string : t -> string

(** Prints the simplex tableau in the standard output. *)
val print : t -> unit

(** Adds a row to the simplex tableau.
    @param row the row
    @param set the variable set associated to the row
    @param sx the simplex tableau *)
val addRow : Tableau.Vector.t -> var_set -> 'c t -> 'c t

(** @return the number of decision variables of the problem. *)
val nVars : 'c t -> int

(** @return the number of parameters of the problem. *)
val nParams : 'c t -> int

(** @return the number of rows of the problem. *)
val nRows : 'c t -> int

(** @return the set of parameters of the problem. *)
val getParams : 'c t -> Var.Set.t

(** @return the current value of the objective function, expressed as a constraint over the parameters. *)
val objValue : 'c t -> Cs.t

(** [getCurVal sx] returns the current value of the basic variables in [sx].
The value of the other variables is implicitly zero.  Each basic variable
is given its value as a pair (column number, value) *)
val getCurVal : t -> (int * Q.t) list

    (** Syntactic equality test between two simplex tableaus. *)
    val equal : t -> t -> bool

    (** [isFeasible sx] returns true is the basic solution is feasible, i.e. all variables have a non-negative value.
    The basic solution consists in setting all non-basic variables to zero and setting each basic variable to the right-hand side of the constraint it appears on. *)
    val isFeasible : t -> bool

    (** [addSlackAt i sx] adds a slack variable at row [i] of [sx].
    The slack variable is added as an extra column at the right of the tableau. *)
    val addSlackAt : int -> t -> t

    (** [addSlacks n sx] adds slack variables for the first [n] rows of the tableau. *)
    val addSlacks : int -> t -> t

    (** Adds a column to the simplex tableau, at the right side.
        All pivots previously applied on the tableau are also applied on the column.
        @param pcoeff the parametric coefficient of the column
        @param column the column
        @param sx the simplex tableau
        @raise Invalid_argument if the column has not the right length. *)
    val add_col : ParamCoeff.t -> Tableau.Vector.t -> t -> t

    (** Exception raised when a problem is unbounded. *)
    exception Unbounded_problem

    (** Returns the row at which the pivot must occur.
        @param str the pivot strategy
        @param basis the LP basis
        @param m the matrix of constraints
        @param col the column of the entering variable.
        @raise Unbounded_problem if the problem is detected unbounded by this pivot.
        @return None if no suitable row has been found*)
    val get_row_pivot : rowPivotStrgyT -> int list -> Tableau.Matrix.t -> int -> int option

    (** [pivot sx row col] performs a pivot on the element at [row] and [col].
        @param init_phase true if the pivot occurs during initialization phase *)
    val pivot : bool -> t -> int -> int -> t

    (** [isCanon sx] returns true if the t tableau [sx] is in canonical form.
        It checks that each row of the matrix of constraints has a designated
        basic variable which appears only on that row. *)
    val isCanon : t -> bool

    (** Module performing the simplex algorithm. *)
    module Explore : sig

        (** Module [Init] gathers all the functions necessary to find an initial feasible basis for a parametric linear problem.
        XXX: it uses an internal variable [a] which [VariablesInt.t] identifier is [-1].
        *)
        module Init : sig

            val getReplacementForA : t -> int -> int
            val buildInitFeasibilityPb : t -> t
            val buildFeasibleTab : Objective.t -> t -> t

            (** Tries to find a feasible basis.
                @param st the pivoting strategy
                @param st_row the pivoting strategy for the leaving variable
                @param sx the simplex tableau to initialize
                @pararam the instantiation parametric point
                @return None if the problem is infeasible *)
            val findFeasibleBasis : Objective.pivotStrgyT -> rowPivotStrgyT -> t -> Vec.t -> t option
        end

        (** Run the optimization process of the simplex algorithm.
            @param init_phase true if the push occurs at during initialization phase
            @param str pivoting strategy
            @param str_row pivoting strategy for the leaving variable
            @param point the parametric point on which the objective function is instantiated
            @param sx the simplex tableau *)
        val push : bool -> Objective.pivotStrgyT -> rowPivotStrgyT -> Vec.t -> t -> t

        (** Initializes the simplex tableau and launch the optimization.
            @param str pivoting strategy
            @param str_row pivoting strategy for the leaving variable
            @param point the parametric point on which the objective function is instantiated
            @param sx the simplex tableau
            @return None if the problem is infeasible *)
        val init_and_push : Objective.pivotStrgyT -> rowPivotStrgyT -> Vec.t -> t -> t option
    end

    (** Module for building a simplex tableau from polynomials. *)
    module Build : sig
        module Poly = ParamCoeff.Poly

        (** [from_poly vars ineqs eqs obj] builds the parametric simplex tableau with constraints [ineqs @ eqs], objective [obj] and where simplex variables are in [vars].
        @param ineqs represent polynomials of the form [p <= 0].*)
        val from_poly : Var.t list -> Poly.t list -> Poly.t list -> Poly.t -> t
    end
end
