module Cs = Cstr.Rat

(** Type of pivoting strategy for the choice of the leaving variable. *)
type rowPivotStrgyT =
    | Standard
    | LexPositive

(** Type of simplex tableau. It is parametrized by a type of vectors, which defines the type of parametric points. *)
module type Type = sig

    (** Type of parametric points, used to instantiate the objective function. *)
    module Vec : Vector.Type

    (** Type of Pivot, that instantiates objective functions with parametric points. *)
    module Pivot : Objective.PivotType with module Vec = Vec

    (** Naming module for decision variables and parameters. *)
    module Naming = Pivot.Naming

    type pivotT = ParamCoeff.t * Tableau.Vector.t -> ParamCoeff.t * Tableau.Vector.t

    (** Type of simple tableau. *)
    type t = {
        obj : Objective.t; (** Objective function *)
        mat : Tableau.Matrix.t; (** Matrix of constraints *)
        basis : int list; (** Current basis *)
        names : Naming.t; (** Naming module *)
        (** Function that applies pivots to columns and objective *)
        pivot : pivotT;
    }

    (** Build a simplex tableau.
        @param obj the objective function
        @param mat the matrix of constraints
        @param basis the basis
        @param names the naming module*)
    val mk : Objective.t -> Tableau.Matrix.t -> int list -> Naming.t -> t

    (** An empty simplex tableau.*)
    val empty : t

    (** Returns the objective function. *)
    val get_obj : t -> Objective.t

    (** Returns the matrix of constraints. *)
    val get_mat : t -> Tableau.Matrix.t

    (** Current basis. *)
    val get_basis : t -> int list

    (** Conversion intro string. *)
    val to_string : t -> string

    (** Returns the number of rows of the simplex tableau. *)
    val nRows : t -> int

    (** Returns the number of columns of the simplex tableau. *)
    val nCols : t -> int

    (** Returns the number of decision variables of the simplex tableau. *)
    val nVars : t -> int

    (** Returns the number of parameters in the objective function. *)
    val nParams : t -> int

    (** Returns the decision variables of the simplex tableau. *)
    val getVars : t -> Var.t list

    (** Returns the parameters of the simplex tableau. *)
    val getParams : t -> Var.t list

    (** Returns the current (parametric) value of the objective function. *)
    val obj_value : t -> ParamCoeff.t

    (** Returns the current value of the objective function, expressed as a constraint of the parameters. *)
    val obj_value' : t -> Cs.t

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
