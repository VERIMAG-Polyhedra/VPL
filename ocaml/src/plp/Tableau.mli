(** Type of simplex tableau *)

(** A list of rationals. Used as rows in the siplex tableau.
    The last element is the current value of the associated basic variable. *)
module Vector : sig

    type t = Scalar.Rat.t list

    (** Conversion into string. *)
    val to_string : t -> string

    (** Length of the vector. *)
    val size : t -> int

    (** Equality test between two vectors. *)
    val equal : t -> t -> bool

    (** Returns the value at index [i] in the vector. *)
    val get : int -> t -> Q.t

    (** Returns the last element of the vector. *)
    val last : t -> Q.t

    (** Builds a vector from a size and an initialization function.
        @param len the vector wanted size
        @param f an initialization function: cell [i] is initialized as [f i]
        @param raise Invalid_argument if [len] < 1 *)
    val init : int -> (int -> Q.t) -> t

    (** [consAppend a [a0; ...; aN]] builds the vector [[a0; ...; a; aN]].
        @raise Invalid_argument if the vector has size 0 *)
    val consAppend : Q.t -> t -> t

    (** Pointwise addition between two vectors.
        @raise Invalid_argument is the two vectors have different lengths. *)
    val add : t -> t -> t

    (** Pointwise multiplication between two vectors. *)
    val mul : t -> t -> t

    (** Multiply the vector by a rational scalar. *)
    val mul_scalar : t -> Q.t -> t

	(** [pretty_print v l] returns a string corresponding to a row of a matrix. Each column has a fixed width.
        @param v the vector to print
        @param l the list of column width*)
	val pretty_print : t -> int list -> string

end

(** The simplex tableau. *)
module Matrix : sig

    (** A simplex tableau is a list of vectors, representing constraints of the problem.*)
    type t = Vector.t list

    (** Returns the number of rows in the matrix. *)
    val nRows : t -> int

    (** Returns the number of columns in the matrix.
        @raise Invalid_argument if the matrix has no row
        @raise Invalid_argument if the rows have different lengths*)
    val nCols : t -> int

    (** Equality test between two matrices. *)
    val equal : t -> t -> bool

    (** Apply a function to each row. *)
    val mapi : (int -> Vector.t -> Vector.t) -> t -> t

    (** Returns the row at index [i].
        @raise Invalid_argument if [i] is not a valid index. *)
    val getRow : int -> t -> Vector.t

    (** Returns the [i]th column of the matrix m as a vector
        @raise Invalid_argument if [i] is not a valid index. *)
    val getCol : int -> t -> Vector.t

    (** Returns the constant at row [i] and column [j]. *)
    val get : int -> int -> t -> Q.t

    (** Applies a pivot on the matrix, centered on the given element.
        @param m matrix
        @param i the pivot row
        @param j the pivot column
        @raise Invalid_argument if the pivot element is 0*)
    val pivot : t -> int -> int -> t

	(** [add_row m v i] adds the row vector [v] at index [i] in matrix [m]. *)
	val add_row : t -> Vector.t -> int -> t

	(** [add_col m v i] adds the column vector [v] at index [i] in matrix [m]. *)
	val add_col : t -> Vector.t -> int -> t

	(** [get_width_column m i] returns the width (in number of characters) of the [i]th column of matrix [m] *)
	val get_width_column : t -> int -> int

	(** Returns the list [width of col 1 ; width of col 2 ; ...]. *)
	val get_width_column_vector : t -> int list

    (** Conversion into string.
        @param m the matrix to convert
        @param l the width of each column *)
	val to_string : t -> int list -> string

	(** [rescale m i coeff] returns the matrix [m] where the row [i] has been multiplied by [coeff]. *)
	val rescale : t -> int -> Scalar.Rat.t -> t

	(** [add_multiple_of_row m row1 row2 coeff] returns the matrix [m] where [row1] has been added [row2] multiplied by [coeff]. *)
	val add_multiple_of_row : t -> int -> int -> Scalar.Rat.t -> t
end
