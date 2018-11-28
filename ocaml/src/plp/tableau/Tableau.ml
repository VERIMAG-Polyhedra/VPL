module Print = struct

	let print_list : ('a -> string) -> 'a list -> string -> unit
		= fun pr l sep-> Misc.list_to_string pr l sep|> Pervasives.print_endline

	(** get_width c returns the length of the string to_string c *)
	let get_width : Scalar.Rat.t -> int
	= fun c ->
	String.length (Scalar.Rat.to_string c);;

end

module Vector = struct

    type t = Scalar.Rat.t list

    let to_string : t -> string
        = fun v ->
        "[" ^ (List.map Scalar.Rat.to_string v |> String.concat "; ") ^ "]"

    let size : t -> int = List.length

    let equal : t -> t -> bool
        = fun v v' ->
        if size v <> size v' then false
        else List.for_all2 Q.equal v v'

    let get : int -> t-> Q.t
        = fun i v ->
        if 0 <= i && i < size v
        then List.nth v i
        else Printf.sprintf "Matrix.Vector.get (%i) %s: out-of-bounds" i (to_string v)
            |> Pervasives.invalid_arg

    let last : t -> Q.t
        = fun v -> get (size v - 1) v

    let init : int -> (int -> Q.t) -> t
        = fun len f ->
        Misc.init_list len f

    let consAppend : Q.t -> t -> t
        = fun a v ->
        let v' = Misc.sublist v 0 (size v - 1) in
        v' @ [a; last v]

    let add : t -> t -> t
        = fun v v' ->
        if size v = size v'
        then List.map2 Scalar.Rat.add v v'
        else Pervasives.invalid_arg "Matrix.Vector.add"

    let sub : t -> t -> t
        = fun v v' ->
        if size v = size v'
        then List.map2 Scalar.Rat.sub v v'
        else Pervasives.invalid_arg "Matrix.Vector.add"

    let mul : t -> t -> t
        = fun v1 v2 ->
        List.map2 Scalar.Rat.mul v1 v2;;

    let mul_scalar : t -> Q.t -> t
        = fun v a ->
        List.map (Q.mul a) v

	(** [pretty_print v l] returns a string corresponding to a row of a matrix. Each column has a fixed width *)
	let rec pretty_print : t -> int list -> string
		= fun v l ->
		match (v,l) with
		| ([],_) | (_,[]) -> ""
		| (p :: tail1 , i :: tail2) -> let nb_spaces = i - (Print.get_width p) in
		if nb_spaces mod 2 = 1
			then String.concat "" [Misc.string_repeat " " (nb_spaces/2) ; Q.to_string p ; Misc.string_repeat " " (nb_spaces/2 + 1) ; " | " ; pretty_print tail1 tail2]
			else String.concat "" [Misc.string_repeat " " (nb_spaces/2) ; Q.to_string p ; Misc.string_repeat " " (nb_spaces/2) ; " | " ; pretty_print tail1 tail2]

    let rec is_lexpositive : t -> bool
        = function
        | [] -> false
        | x :: tl ->
            if Scalar.Rat.isZ x
            then is_lexpositive tl
            else Scalar.Rat.lt Scalar.Rat.z x
end

module Matrix = struct

    type t = Vector.t list

    let empty = [[]]

    let nRows : t -> int = List.length

    let nCols : t -> int
        = function
        | [] -> Pervasives.invalid_arg "Matrix.Matrix.nCols: empty"
        | r :: t ->
           let n = Vector.size r in
           if List.for_all (fun r' -> Vector.size r' = n) t then n
           else Pervasives.invalid_arg "Matrix.Matrix.nCols: different sizes"

    let equal : t -> t -> bool
        = fun m m' ->
        if nRows m <> nRows m' || nCols m <> nCols m' then false
        else List.for_all2 Vector.equal m m'

    let mapi : (int -> Vector.t -> Vector.t) -> t -> t
        = fun f m -> List.mapi f m

    let getRow : int -> t -> Vector.t
        = fun i m ->
        if 0 <= i && i < nRows m
        then List.nth m i
        else Pervasives.invalid_arg "Matrix.Matrix.getRow"

    let rec getCol : int -> t -> Vector.t
        = fun col m ->
        match m with
        | [] -> []
        | v :: tail -> (Vector.get col v) :: (getCol col tail)

    let get : int -> int -> t -> Q.t
        = fun row col m ->
        Vector.get col (getRow row m)

    let pivot : t -> int -> int -> t
        = fun m row col ->
        let pivRow = getRow row m in
        let pivElt = Vector.get col pivRow in
        if Q.sign pivElt <> 0
        then let normRow = Vector.mul_scalar pivRow (Q.inv pivElt) in
            mapi (fun i r ->
                if i = row then normRow
                else Vector.add
                    r
                    (Vector.mul_scalar normRow (Q.neg (Vector.get col r)))
            ) m
        else Pervasives.invalid_arg "Matrix.Matrix.pivot"

	let add_row : t -> Vector.t -> int -> t
		= let rec add_row_rec : t -> Vector.t -> int -> int -> t
    		= fun m v i k ->
    		if i = k then v :: (add_row_rec m v i (k+1))
    		else match m with
        		| [] -> []
        		| vec :: tail -> vec :: (add_row_rec tail v i (k+1))
		in
        fun m v i ->
		add_row_rec m v i 0

	(** add_col m v i adds the column vector v at the index i in m *)
	let add_col : t -> Vector.t -> int -> t
		= fun m v col ->
		let len = List.length (List.nth m 0) in
		if len = 0
		then List.map (fun coeff -> [coeff]) v
		else List.map2 (fun vec coeff -> (Misc.sublist vec 0 col) @ [coeff] @ (Misc.sublist vec col len)) m v

	let get_width_column : t -> int -> int
		= fun m col->
		let l = List.map (fun p -> Print.get_width p) (getCol col m) in
		Misc.max compare l

	let get_width_column_vector : t -> int list
		= fun m ->
		List.map
            (fun i -> get_width_column m i)
            (Misc.range 0 (nCols m));;

	let rec to_string : t -> int list -> string
		= fun m l ->
		match m with
		| [] -> ""
		| v :: tail -> String.concat "" [Vector.pretty_print v l ; "\n" ; to_string tail l];;

	let rescale : t -> int -> Scalar.Rat.t -> t
		= let rec rescale_rec : t -> int -> Scalar.Rat.t -> int -> t
			= fun m row coeff k ->
			match m with
			| [] -> []
			| v :: tail -> if row = k
                then (Vector.mul_scalar v coeff) :: tail
                else v::(rescale_rec tail row coeff (k+1))
		in
        fun m row coeff ->
		rescale_rec m row coeff 0

	let add_multiple_of_row : t -> int -> int -> Scalar.Rat.t -> t
		= let rec add_multiple_of_row_rec : t -> int -> Vector.t -> Scalar.Rat.t -> int -> t
			= fun m row v coeff k ->
			match m with
			| [] -> []
			| vec :: tail -> if row = k then (Vector.add vec (Vector.mul_scalar v coeff)) :: tail
			else vec :: (add_multiple_of_row_rec tail row v coeff (k+1))
		in fun m row1 row2 coeff ->
		add_multiple_of_row_rec m row1 (getRow row2 m) coeff 0

    let mul : t -> Q.t -> t
        = fun m c ->
        List.map (fun row -> Vector.mul_scalar row c) m

    let transpose : t -> t
        = fun m ->
        Misc.fold_left_i (fun i m' row ->
            add_col m' row i)
        empty m
end
