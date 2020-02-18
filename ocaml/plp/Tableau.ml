type row = Scalar.Rat.t array
type col = Scalar.Rat.t array
type t = row array

let empty = [| |]

let nRows = Array.length

let nCols tab =
	if nRows tab > 0
	then Array.length tab.(0)
	else 0

let init : int -> int -> t
	= fun n_rows n_cols ->
	Array.make_matrix n_rows n_cols Scalar.Rat.z

let copy : t -> t
	= fun tab ->
	let n_rows = nRows tab in
	if n_rows = 0
	then tab
	else let result = Array.make n_rows tab.(0) in
		for i = 0 to n_rows - 1 do
			result.(i) <- Array.copy tab.(i)
		done;
		result

let get : int -> int -> t -> Scalar.Rat.t
    = fun i_row i_col tab ->
    tab.(i_row).(i_col)

let getRow : int -> t -> row
	= fun i_row tab ->
	tab.(i_row)

let getCol : int -> t -> col
	= fun i_col tab ->
	Array.map (fun row ->
		row.(i_col)
	) tab

exception Invalid_Pivot

let pivot : int -> int -> t -> unit
	= fun i_row i_col tab ->
	let pivot_coeff = tab.(i_row).(i_col) in
	if Scalar.Rat.isZ pivot_coeff
	then Stdlib.raise Invalid_Pivot
	else
		Array.iteri (fun i_col coeff ->
			Scalar.Rat.div coeff pivot_coeff
			|> Array.set tab.(i_row) i_col
		) tab.(i_row);
		Array.iteri (fun i_row' row ->
			if i_row != i_row'
			then
				let coeff_col = tab.(i_row').(i_col) in
				Array.iteri (fun i_col' coeff ->
					Scalar.Rat.mul coeff_col tab.(i_row).(i_col')
					|> Scalar.Rat.sub coeff
					|> Array.set row i_col'
				) tab.(i_row')
		) tab

let iteri_col : (int -> Q.t -> Q.t) -> int -> t -> unit
    = fun f i_col tab ->
    for i_row = 0 to (nRows tab) - 1 do
        Array.set tab.(i_row) i_col (f i_row tab.(i_row).(i_col))
    done

let fold_left_cols : ('a -> Q.t -> Q.t -> 'a) -> 'a -> int -> int -> t -> 'a
	= fun f a0 i_col1 i_col2 tab ->
	let a = ref a0 in
	for i_row = 0 to (nRows tab) - 1 do
		a := f !a tab.(i_row).(i_col1) tab.(i_row).(i_col2)
	done;
	!a

let get_width_q : Scalar.Rat.t -> int
	= fun c ->
	String.length (Scalar.Rat.to_string c)

let get_width_column : t -> int -> int
	= fun m col ->
    if nCols m = 0
    then 0
    else getCol col m
		|> Array.map get_width_q
		|> Array.to_list
		|> Misc.max compare

let get_width_column_vector : t -> int list
	= fun m ->
	Misc.init_list (nCols m) (get_width_column m)

let pretty_print_row : int -> t -> int list -> string
	= fun i_row tab l ->
	List.mapi (fun i_col width ->
		let coeff = tab.(i_row).(i_col) in
		let nb_spaces = width - (get_width_q coeff) in
		Printf.sprintf "%s%s%s | "
			(Misc.string_repeat " " (nb_spaces/2))
			(Q.to_string coeff)
			(Misc.string_repeat " " (nb_spaces/2 + (nb_spaces mod 2)))
	) l
	|> String.concat ""


(** Adds a column in the (n-1) position *)
let addCol : (int -> Q.t) -> t -> t
	= fun init_f tab ->
	let n_cols = nCols tab
	and n_rows = nRows tab in
	let tab' = Array.make_matrix n_rows (n_cols + 1) Scalar.Rat.z in
	Array.iteri (fun i_row row ->
		Array.blit row 0 tab'.(i_row) 0 (n_cols-1);
		Array.set tab'.(i_row) (n_cols - 1) (init_f i_row);
		Array.set tab'.(i_row) (n_cols) row.(n_cols - 1)
	) tab;
	tab'

let remCol : int -> t -> t
	= fun i_col tab ->
	let len = (nCols tab) - 1 in
	Array.map (fun row ->
		Array.init len (fun i_col' ->
			if i_col' >= i_col
			then row.(i_col' + 1)
			else row.(i_col')
		)
	) tab
