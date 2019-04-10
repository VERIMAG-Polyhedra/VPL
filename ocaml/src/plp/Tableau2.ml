type row = Scalar.Rat.t array
type col = Scalar.Rat.t array

type t = {
	mat : row array;
	inv : row array;
}

let empty = {
	mat = [| |];
	inv = [| |];
}

let nRows tab = Array.length tab.mat

let nCols tab =
	if nRows tab > 0
	then Array.length tab.mat.(0)
	else 0

let init_inv : t -> unit
	= fun tab ->
	for i = 0 to (nRows tab) - 1 do
		tab.inv.(i).(i) <- Scalar.Rat.u
	done

let init : int -> int -> t
	= fun n_rows n_cols ->
	let res = {
		mat = Array.make_matrix n_rows n_cols Scalar.Rat.z;
		inv = Array.make_matrix n_rows n_rows Scalar.Rat.z;
	} in
	init_inv res;
	res

let copy : t -> t
	= fun tab ->
	let n_rows = nRows tab in
	if n_rows = 0
	then tab
	else
		let result = {
			mat = Array.make n_rows tab.mat.(0);
			inv = Array.make n_rows tab.inv.(0);
		} in
		for i = 0 to n_rows - 1 do
			result.mat.(i) <- Array.copy tab.mat.(i);
			result.inv.(i) <- Array.copy tab.inv.(i)
		done;
		result

let copy_matrix : row array -> row array
 	= fun mat ->
	let n_rows = Array.length mat in
	if n_rows = 0
	then [| |]
	else
		let result = Array.make n_rows mat.(0) in
		for i = 0 to n_rows - 1 do
			result.(i) <- Array.copy mat.(i)
		done;
		result

let get : int -> int -> t -> Scalar.Rat.t
    = fun i_row i_col tab ->
    tab.mat.(i_row).(i_col)

let getRow : int -> t -> row
	= fun i_row tab ->
	tab.mat.(i_row)

let getCol : int -> t -> col
	= fun i_col tab ->
	Array.map (fun row ->
		row.(i_col)
	) tab.mat

let getCol_inv : int -> t -> col
	= fun i_col tab ->
	Array.map (fun row ->
		row.(i_col)
	) tab.inv

exception Invalid_Pivot

let pivot : int -> int -> t -> unit
	= fun i_row i_col tab ->
	let pivot_coeff = get i_row i_col tab in
	if Scalar.Rat.isZ pivot_coeff
	then Pervasives.raise Invalid_Pivot
	else begin
		(* Scaling matrix pivot row *)
		Array.iteri (fun i_col' coeff ->
			Scalar.Rat.div coeff pivot_coeff
			|> Array.set tab.mat.(i_row) i_col'
		) tab.mat.(i_row);
		(* Scaling inverse pivot row *)
		Array.iteri (fun i_col' coeff ->
			Scalar.Rat.div coeff pivot_coeff
			|> Array.set tab.inv.(i_row) i_col'
		) tab.inv.(i_row);
		(* Pivoting other rows on matrix *)
		Array.iteri (fun i_row' row ->
			if i_row != i_row'
			then
				let coeff_col = tab.mat.(i_row').(i_col) in
				Array.iteri (fun i_col' coeff ->
					Scalar.Rat.mul coeff_col tab.mat.(i_row).(i_col')
					|> Scalar.Rat.sub coeff
					|> Array.set row i_col'
				) tab.mat.(i_row')
		) tab.mat;
		(* Pivoting other rows on inverse *)
		Array.iteri (fun i_row' row ->
			if i_row != i_row'
			then
				let coeff_col = tab.mat.(i_row').(i_col) in
				Array.iteri (fun i_col' coeff ->
					Scalar.Rat.mul coeff_col tab.inv.(i_row).(i_col')
					|> Scalar.Rat.sub coeff
					|> Array.set row i_col'
				) tab.inv.(i_row')
		) tab.inv
		end

let iteri_col : (int -> Q.t -> Q.t) -> int -> t -> unit
    = fun f i_col tab ->
    for i_row = 0 to (nRows tab) - 1 do
        Array.set tab.mat.(i_row) i_col (f i_row tab.mat.(i_row).(i_col))
    done

let fold_left_cols : ('a -> Q.t -> Q.t -> 'a) -> 'a -> int -> int -> t -> 'a
	= fun f a0 i_col1 i_col2 tab ->
	let a = ref a0 in
	for i_row = 0 to (nRows tab) - 1 do
		a := f !a tab.mat.(i_row).(i_col1) tab.mat.(i_row).(i_col2)
	done;
	!a

let get_width_q : Scalar.Rat.t -> int
	= fun c ->
	String.length (Scalar.Rat.to_string c)

let get_width_column : t -> int -> int
	= fun tab i_col ->
    if nCols tab = 0
    then 0
    else let width = getCol i_col tab
		|> Array.map get_width_q
		|> Array.to_list
		|> Misc.max compare
		in
		if i_col > (nRows tab) - 1
		then width
		else let width' = getCol_inv i_col tab
			|> Array.map get_width_q
			|> Array.to_list
			|> Misc.max compare
			in
			max width width'

let get_width_column_vector : t -> int list
	= fun tab ->
	List.map (fun i ->
		get_width_column tab i
	) (Misc.range 0 (nCols tab));;

let rec pretty_print_row : int -> t -> int list -> string
	= fun i_row tab l ->
	List.mapi (fun i_col width ->
		let coeff = get i_row i_col tab in
		let nb_spaces = width - (get_width_q coeff) in
		Printf.sprintf "%s%s%s | "
			(Misc.string_repeat " " (nb_spaces/2))
			(Q.to_string coeff)
			(Misc.string_repeat " " (nb_spaces/2 + (nb_spaces mod 2)))
	) l
	|> String.concat ""

let rec pretty_print_row_inv : int -> t -> int list -> string
	= fun i_row tab l ->
	List.mapi (fun i_col width ->
		let coeff = tab.inv.(i_row).(i_col) in
		let nb_spaces = max 0 (width - (get_width_q coeff)) in
		Printf.sprintf "%s%s%s | "
			(Misc.string_repeat " " (nb_spaces/2))
			(Q.to_string coeff)
			(Misc.string_repeat " " (nb_spaces/2 + (nb_spaces mod 2)))
	) (Misc.sublist l 0 (nRows tab))
	|> String.concat ""

(** Adds a column in the (n-1) position *)
let addCol : (int -> Q.t) -> t -> t
	= fun init_f tab ->
	let n_cols = nCols tab
	and n_rows = nRows tab in
	let mat' = Array.make_matrix n_rows (n_cols + 1) Scalar.Rat.z in
	Array.iteri (fun i_row row ->
		Array.blit row 0 mat'.(i_row) 0 (n_cols-1);
		Array.set mat'.(i_row) (n_cols - 1) (init_f i_row);
		Array.set mat'.(i_row) (n_cols) row.(n_cols - 1)
	) tab.mat;
	{
		mat = mat';
		inv = copy_matrix tab.inv;
	}

let remCol : int -> t -> t
	= fun i_col tab ->
	let len = (nCols tab) - 1 in
	let mat' = Array.map (fun row ->
		Array.init len (fun i_col' ->
			if i_col' >= i_col
			then row.(i_col' + 1)
			else row.(i_col')
		)
	) tab.mat in {
		mat = mat';
		inv = copy_matrix tab.inv;
	}
