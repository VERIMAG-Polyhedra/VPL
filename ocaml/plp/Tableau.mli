(** Type of simplex tableau *)

(** Type of row *)
type row = Scalar.Rat.t array

(** Type of column *)
type col = Scalar.Rat.t array

(** Type of simplex tableau. *)
type t = row array

(** Empty simplex tableau. *)
val empty : t

(** Number of rows. *)
val nRows : t -> int

(** Number of columns. *)
val nCols : t -> int

(** Initializes a simplex tableau.
	@param n_rows the number of rows
	@param n_cols the number of columns *)
val init : int -> int -> t

(** @return a copy of the given simplex tableau. *)
val copy : t -> t

(** @return a tableau's coefficient.
	@param i_row the row index
	@param i_col the column index *)
val get : int -> int -> t -> Scalar.Rat.t

(** @return a row.
	@param i_row the row index *)
val getRow : int -> t -> row

(** @return a column.
	@param i_col the column index *)
val getCol : int -> t -> col

(** Performs a pivot on the given simplex tableau.
	@param i_row the row index of the pivot
	@param i_col the column index of the pivot  *)
val pivot : int -> int -> t -> unit

val iteri_col : (int -> Q.t -> Q.t) -> int -> t -> unit


(** [fold_left_cols f a0 i_col1 i_col2 tab] computes
	[ f (... (f (f a0 tab[0][i_col1] tab[0][i_col2]) tab[1][i_col1] tab[1][i_col2])) tab[n_rows][i_col1] tab[n_rows][i_col2] ]
	*)
val fold_left_cols : ('a -> Q.t -> Q.t -> 'a) -> 'a -> int -> int -> t -> 'a

(** @return a list [l] of integers such that [l[i]] is the width of column [i]. *)
val get_width_column_vector : t -> int list

(** Pretty-printer for rows.
 	@param i_row the row index
	@param tab the simplex tableau
	@param l the list of widths (see {!val:get_width_column_vector}) *)
val pretty_print_row : int -> t -> int list -> string

(** Adds a column in a simplex tableau.
	The row is added at index (nCols -1) (constant part of each row remains unchanged).
	@param init_f the initialization function for the new column
	@param tab the initial simplex tableau *)
val addCol : (int -> Q.t) -> t -> t

(** *)
val remCol : int -> t -> t
