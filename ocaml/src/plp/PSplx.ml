open PSplx_type

module type Type = Type

module Debug = DebugTypes.Debug(struct let name = "PSplx" end)

module Cs = Cstr.Rat.Positive

module Make(Vec : Vector.Type with module M = Rtree and module V = Var.Positive) = struct
    module Vec = Vec
	module Pivot = Objective.Pivot(Vec)
	module Naming = Pivot.Naming

    type pivotT = ParamCoeff.t * Tableau.Vector.t -> ParamCoeff.t * Tableau.Vector.t

	type t = {
        obj : Objective.t;
        mat : Tableau.Matrix.t;
        basis : int list;
        names : Naming.t;
        pivot : pivotT;
	}

	let empty = {
		obj = Objective.empty;
		mat = [];
		basis = [];
		names = Naming.empty;
        pivot = (fun x -> x);
	}

    let mk : Objective.t -> Tableau.Matrix.t -> int list -> Naming.t -> t
        = fun obj mat basis names -> { empty with
            obj = obj;
            mat = mat;
            basis = basis;
            names = names;
        }

	let get_obj : t -> Objective.t
        = fun sx -> sx.obj

	let get_mat : t -> Tableau.Matrix.t
        = fun sx -> sx.mat

	let get_basis : t -> int list
	   = fun sx -> sx.basis

	let nRows : t -> int
        = fun sx -> Tableau.Matrix.nRows sx.mat

	let nCols : t -> int
	  = fun sx ->
	  let n = Tableau.Matrix.nCols sx.mat in
	  if n = Objective.nVars sx.obj + 1 then n
	  else Pervasives.invalid_arg "t.nCols: different sizes"

	let nVars : t -> int
	  = fun sx -> nCols sx - 1

	let nParams : t -> int
	  = fun sx -> Objective.nParams sx.obj

	let getParams : t -> Vec.V.t list
		= fun sx ->
		List.map (fun i ->
            Naming.to_user sx.names Naming.Param i |> fst
        ) (Misc.range 0 (nParams sx))

	let getVars : t -> Vec.V.t list
		= fun sx ->
		List.map (fun i ->
            Naming.to_user sx.names Naming.Var i |> fst
        ) (Misc.range 0 (nVars sx))

    let obj_value : t -> ParamCoeff.t
        = fun sx -> Objective.value sx.obj

	let obj_value' : t -> Cs.t
		= fun sx ->
		let pCoeff = sx |> get_obj |> Objective.value in
		ParamCoeff.to_cstr
		(fun i -> Naming.to_user sx.names Naming.Param i
			|> Pervasives.fst)
		Cstr_type.LE pCoeff

	let equal : t -> t -> bool
        = fun sx sx' ->
        Objective.equal sx.obj sx'.obj
        && Tableau.Matrix.equal sx.mat sx'.mat
        && List.length sx.basis = List.length sx'.basis
        && List.for_all2 (=) sx.basis sx'.basis
        && Naming.equal sx.names sx'.names

	let isFeasible : t -> bool
        = fun sx ->
        List.fold_left (fun r v ->
            r && Q.geq (Tableau.Vector.last v) Q.zero
        ) true sx.mat

	let getCurVal : t -> (int * Q.t) list
        = fun sx ->
        List.map2 (fun xb v ->
            (xb, Tableau.Vector.last v)
        ) sx.basis sx.mat

    let add_pivots: bool -> int -> int -> t -> pivotT
        = fun init_phase row col sx (pcoeff, column) ->
        let column_pivot = Tableau.Matrix.getCol col sx.mat in
        let ak = Tableau.Vector.get row column_pivot in
        let (pcoeff', column') = sx.pivot (pcoeff,column) in
        let bk' = Q.div (Tableau.Vector.get row column') ak in
        let new_pcoeff = if init_phase
            then pcoeff (* objective must not be affected by the initialization phase*)
            else ParamCoeff.mul bk' (Objective.get col sx.obj)
            |> ParamCoeff.sub pcoeff'
        and new_column = List.mapi (fun row' coeff ->
            if row' = row then bk'
            else Q.sub coeff
                (Q.mul bk' (Tableau.Vector.get row' column_pivot))
        ) column'
        in
        (new_pcoeff, new_column)

	let pivot : bool -> t -> int -> int -> t
        = fun init_phase sx row col -> { sx with
            obj = Objective.elim sx.obj (Tableau.Matrix.getRow row sx.mat) col;
            mat = Tableau.Matrix.pivot sx.mat row col;
            basis = List.mapi (fun i j -> if i = row then col else j) sx.basis;
            pivot = (add_pivots init_phase row col sx)
        }

	let addSlackAt : int -> t -> t
        = fun i sx ->
        let idx = nVars sx in { sx with
            obj = Objective.add_col sx.obj
                (ParamCoeff.mkSparse (Objective.nParams sx.obj) [] Scalar.Rat.z) idx;
            mat = Tableau.Matrix.add_col sx.mat
            	 (Tableau.Vector.init (nRows sx) (fun i' ->
                    if i' = i then Scalar.Rat.u else Scalar.Rat.z)
            	 ) idx;
            basis = sx.basis @ [idx];
            names = Naming.allocAt Naming.Slack (Vec.V.fromInt (i+1)) idx sx.names; (* +1 car on compte les slack à partir de 1*)
        }

	let addSlacks : int -> t -> t
        = fun n sx ->
        List.fold_left (fun sx' i ->
            addSlackAt i sx'
        ) sx (Misc.range 0 n)

	(* PRETTY PRINTERS *)
	let (t_get_width_column_vector : t -> int list)
		= fun sx ->
		let l =
		  List.map2
			 Pervasives.max
			 (Tableau.Matrix.get_width_column_vector sx.mat)
			 (Objective.getColumnsWidth (Naming.to_string sx.names Naming.Param) sx.obj)
		in
		List.mapi
		  (fun i n ->
			let n' = Naming.to_string sx.names Naming.Var i |> String.length in
			Pervasives.max n n')
		  (Misc.sublist l 0 (List.length l - 1))
		@ [List.nth l (List.length l - 1)]

	let to_string : t -> string
		= fun sx0 ->
		let sx = (* XXX: The output of first_t does not have a complete basis. *)
		  let l = (nRows sx0) - (get_basis sx0 |> List.length) in
		  if l < 0 then Pervasives.failwith "t.print_t"
		  else
			 let rec gen : int -> int list
				= fun i -> if i = 0 then [] else -1 :: gen (i - 1)
			 in
			 {sx0 with basis = get_basis sx0 @ gen l}
		in
		let width_columns = t_get_width_column_vector sx in
		(["\n"]
		@ (List.mapi
			(fun i width_col->
			 let s = Naming.to_string sx.names Naming.Var i in
			 let nb_spaces = width_col - (String.length s) in
			if nb_spaces mod 2 = 1
				then String.concat "" [Misc.string_repeat " " (nb_spaces/2) ; s ; Misc.string_repeat " " (nb_spaces/2 + 1) ; " | "]
				else String.concat "" [Misc.string_repeat " " (nb_spaces/2) ; s ; Misc.string_repeat " " (nb_spaces/2) ; " | " ])
			(Misc.sublist width_columns 0 ((List.length width_columns) - 1)))
		@ ["\n"]
		@ [Objective.pretty_print (Naming.to_string sx.names Naming.Param) sx.obj width_columns ; "\n"]
		@ (List.map (fun i -> String.concat "" [Misc.string_repeat "-" i ; " | "]) width_columns)
		@ ["\n"]
		@ (List.map2
			(fun vec var -> String.concat "" [(Tableau.Vector.pretty_print vec width_columns) ; Naming.to_string sx.names Naming.Var var; "\n"])
				  sx.mat (get_basis sx))
        )
        |> String.concat ""

	let print : t -> unit
        = fun sx ->
        to_string sx
        |> Pervasives.print_endline

    let add_col : ParamCoeff.t -> Tableau.Vector.t -> t -> t
        = fun pcoeff column sx ->
        if Tableau.Vector.size column != nRows sx
        then Pervasives.invalid_arg "PSplx.addCol: invalid column size"
        else
            let (pcoeff', column') = sx.pivot (pcoeff,column) in
            let ncols = nCols sx in
            { sx with
                obj = Objective.add_col sx.obj pcoeff' ncols;
                mat = Tableau.Matrix.add_col sx.mat column' ncols;
            }



    exception Unbounded_problem

    let get_row_pivot_standard : int list -> Tableau.Matrix.t -> int -> int option
        = let bounds : Tableau.Matrix.t -> int -> Q.t option list
           = fun m col ->
           List.map2
               (fun a b -> if Q.sign a > 0 then Some (Q.div b a) else None)
               (List.map (fun r -> Tableau.Vector.get col r) m)
               (List.map (fun r -> Tableau.Vector.get (Tableau.Matrix.nCols m - 1) r) m)
           in
        let min : (Q.t * int) option * int -> Q.t option -> (Q.t * int) option * int
           = fun (mcur, idx) ma ->
           let mcur' =
           match ma with
           | None -> mcur
           | Some a ->
               match mcur with
               | None -> Some (a, idx)
               | Some cur -> if Q.lt a (Pervasives.fst cur) then Some (a, idx) else mcur
               in
               (mcur', idx + 1)
        in
        fun _ m col ->
        bounds m col |> List.fold_left min (None, 0)
        |> function
           | None, _ -> Pervasives.raise Unbounded_problem
           | Some (a, i), _ ->
        if Q.sign a < 0
        then Pervasives.failwith "t.get_row_pivot: negative right-hand side"
        else Some i

    let get_row_pivot_lexpositive : int list -> Tableau.Matrix.t -> int -> int option
        = fun basis m col ->
        Debug.log DebugTypes.Detail (lazy (Printf.sprintf "lex_positive on column %i" col));
        let n_cols = List.length basis in
        let transpose_matrix = List.fold_right (fun i m ->
                Tableau.Vector.init n_cols
                    (fun j -> if List.nth basis j == i then Q.one else Q.zero)
                :: m
            ) (List.fast_sort Pervasives.compare basis) []
        in
        let matrix = Tableau.Matrix.add_col
            transpose_matrix
            (Tableau.Matrix.getCol ((Tableau.Matrix.nCols m) - 1) m)
            0
        |> Tableau.Matrix.mapi (fun i_row row ->
            Tableau.Matrix.get i_row col m
            |> Scalar.Rat.inv
            |> Tableau.Vector.mul_scalar row)
        in
        Debug.log DebugTypes.Detail (lazy (Printf.sprintf "matrix : %s"
            (Tableau.Matrix.to_string matrix
                (Tableau.Matrix.get_width_column_vector matrix))));
        let range = Misc.range 0 n_cols in
        let rec find_min : int ->  Tableau.Matrix.t -> Tableau.Matrix.t -> Tableau.Vector.t -> int option
            = fun row_number rest_matrix matrix column ->
            match rest_matrix with
            | [] -> None
            | row :: tl ->
                if Scalar.Rat.lt Scalar.Rat.z (Tableau.Vector.get row_number column)
                    && List.for_all2 (fun row' i ->
                        i = row_number
                        (* if row' has a negative coefficient in column, it must not be tested*)
                        || Scalar.Rat.lt (Tableau.Vector.get i column) Scalar.Rat.z
                        || (Tableau.Vector.sub row' row |> Tableau.Vector.is_lexpositive)
                    ) matrix range
                then Some (row_number)
                else find_min (row_number + 1) tl matrix column
        in
        find_min 0 matrix matrix (Tableau.Matrix.getCol col m)

	let get_row_pivot : rowPivotStrgyT -> int list -> Tableau.Matrix.t -> int -> int option
        = function
        | Standard -> get_row_pivot_standard
        | LexPositive ->  get_row_pivot_lexpositive

	module Explore = struct

		module Obj = Objective.Pivot (Vec)

	 	let rec push' : bool -> Objective.pivotStrgyT -> rowPivotStrgyT -> Vec.t -> t -> t
			= fun init_phase st st_row point sx ->
			Debug.log DebugTypes.Detail
				(lazy (Printf.sprintf "push' : \n%s" (to_string sx)));
		  	match Pivot.getPivotCol
			  	(Naming.to_vpl sx.names)
			  	(Naming.vpl_max sx.names)
			  	st sx.names point sx.obj
		  	with
		  	| Objective.OptReached -> sx
		  	| Objective.PivotOn i_col -> begin
                match get_row_pivot st_row sx.basis sx.mat i_col with
                | Some i_row ->
                    let sx' = pivot init_phase sx i_row i_col in
			        push' init_phase st st_row point sx'
                | None -> failwith "LexPositive"
            end

		let push : bool -> Objective.pivotStrgyT -> rowPivotStrgyT -> Vec.t -> t -> t
			= fun init_phase st str_row point sx ->
			Debug.log DebugTypes.Title
				(lazy "Parametric simplex");
		  	Debug.log DebugTypes.MInput
		  		(lazy (Printf.sprintf "Point %s\n%s"
		  			(Vec.to_string Vec.V.to_string point)
		  			(to_string sx)));
		  	let res = push' init_phase st str_row point sx in
		  	Debug.exec res DebugTypes.MOutput (lazy (to_string res))

		module Init = struct

			let a_value : Vec.V.t ref = ref Vec.V.u

			let getReplacementForA : t -> int -> int
				= fun sx row ->
				let r = Tableau.Matrix.getRow row sx.mat in
				let rowCoeffs = Misc.sublist r 0 (List.length r - 1) in
				try
					let col = Misc.findi
						(fun a -> not (Scalar.Rat.equal Scalar.Rat.z a))
						(List.tl rowCoeffs) in
					col + 1
				with Not_found -> Pervasives.failwith "t.a_still_in_basis"

		 	let buildInitFeasibilityPb : t -> t
				= let getMinRhs : Tableau.Matrix.t -> int
					= fun m ->
					let (_, i, a) = List.fold_left
						(fun (i, j, a) v ->
				 		let b = Tableau.Vector.last v in
				 		if Q.lt b a then (i + 1, i, b) else (i + 1, j, a))
					(0, -1, Q.zero) m
					in
					if Q.lt a Q.zero then i
					else Pervasives.failwith "t.buildInitFeasibilityPb"
				in
				fun sx ->
				a_value := Naming.slack_max sx.names;
				let sx' = mk
					(Objective.mkSparse
                        (nVars sx + 1)
                        [0, ParamCoeff.mkCst Scalar.Rat.u]
                        (ParamCoeff.mkCst Scalar.Rat.z))
					(List.map (fun v ->
                        (if Q.lt (Tableau.Vector.last v) Q.zero
                         then Q.minus_one
                         else Q.zero) :: v
                    ) sx.mat)
					(List.map ((+) 1) sx.basis)
					(Naming.allocSlackShift !a_value sx.names)
				in
				pivot true sx' (getMinRhs sx.mat) 0

			let buildFeasibleTab : Objective.t -> t -> t
				= let syncObjWithBasis : Tableau.Matrix.t -> int list -> Objective.t -> Objective.t
					= fun m b o ->
					List.fold_left2 (fun o i v -> Objective.elim o v i) o b m
				in
                let fix_pivot : int list -> Objective.t -> pivotT -> pivotT
                    = fun basis obj pivot_f ->
                    Misc.fold_left_i (fun row pivot_f col ->
                        fun (pcoeff, column) ->
                        let (pcoeff', column') = pivot_f (pcoeff,column) in
                        let new_pcoeff = ParamCoeff.sub pcoeff'
                            (ParamCoeff.mul
                                (Tableau.Vector.get row column')
                                (Objective.get col obj))
                        in (new_pcoeff, column')
                    ) pivot_f basis
                in
				fun o sx ->
				let newMat = List.map List.tl sx.mat in
				let b = List.map (fun i -> i - 1) sx.basis in {
                    obj = syncObjWithBasis newMat b o;
                    mat = newMat;
                    basis = b;
                    names = (Naming.freeSlackShift !a_value sx.names);
                    pivot = fix_pivot b o sx.pivot;
                }

			let correction : t -> t option
				= let removeRow : int -> t -> t
					= let rm : int -> int * Tableau.Vector.t list -> Tableau.Vector.t -> int * Tableau.Vector.t list
				 		= fun r (i, l) v -> if i = r then (i + 1, l) else (i + 1, v :: l)
			  		in
					fun r sx ->
					let m = sx.mat
					|> List.fold_left (rm r) (0, [])
					|> Pervasives.snd
					|> List.rev
			  		in
			  		{sx with mat = m}
				in
				let rec chooseBasicVar : int -> t -> t option
					= fun row sx ->
					if row >= nRows sx then Some sx
					else
						let v = Tableau.Matrix.getRow row sx.mat in
						try
							let col = Misc.findi (fun a -> not (Scalar.Rat.isZ a)) v in
							if col > List.length v - 2
							then None (* last column is the constant *)
				 			else
								{sx with
								obj = Objective.elim sx.obj (Tableau.Matrix.getRow row sx.mat) col;
								mat = Tableau.Matrix.pivot sx.mat row col;
								basis = sx.basis @ [col];
                                pivot = (add_pivots true row col sx);
								}
								|> chooseBasicVar (row + 1)
						with Not_found ->
							removeRow row sx |> chooseBasicVar row
				in
				fun sx ->
				chooseBasicVar (List.length sx.basis) sx

		  let (findFeasibleBasis : Objective.pivotStrgyT -> rowPivotStrgyT -> t -> Vec.t -> t option)
			 = fun st st_row sx0 point ->
			 match correction sx0 with
			 | None -> None
			 | Some sx ->
				 if isFeasible sx then Some sx
				 else
			 let sx' =
				buildInitFeasibilityPb sx
				|> push true st st_row point
			 in
			 if not (obj_value sx' |> ParamCoeff.is_zero) then None
			 else
				let sx' =
				  try
				     let row = Misc.findi ((=) 0) sx'.basis in
				     getReplacementForA sx' row
					  |> pivot false sx' row
				  with Not_found -> sx'
				in
				Some (buildFeasibleTab sx.obj sx')

		end

		let init_and_push : Objective.pivotStrgyT -> rowPivotStrgyT -> Vec.t -> t -> t option
			= fun st st_row point sx ->
			match Init.findFeasibleBasis st st_row sx point with
			| None -> begin
				print_endline ("init -> unfeasible");
				None
				end
			| Some sx ->
				Debug.exec (Some (push false st st_row point sx))
					DebugTypes.Detail (lazy (to_string sx))
	end

    let isCanon : t -> bool
        = fun sx ->
        if List.length sx.basis <> nRows sx
        then false
        else
            let chkOccurMat : int -> Tableau.Matrix.t -> bool
                = fun i m ->
                1 = List.fold_left (fun cnt v ->
                    if Q.sign (Tableau.Vector.get i v) = 0 then cnt else cnt + 1
                ) 0 m
            in
            let chkOccur : int -> t -> bool
                = fun i sx' ->
                chkOccurMat i sx'.mat
                && Objective.get i sx'.obj |> ParamCoeff.is_zero
            in
            List.for_all2 (fun i r ->
                Q.equal (Tableau.Vector.get i r) Q.one
                && chkOccur i sx
            ) sx.basis sx.mat

	module Build = struct
		module Poly = ParamCoeff.Poly

		let obj_buildOfPoly : Poly.t list -> Poly.t -> Objective.t * Naming.t
		  = let module VSet = Set.Make (struct type varT = Poly.V.t type t = varT let compare = Poly.V.cmp end) in
			 let gatherParams1 : Poly.t -> VSet.t
				= fun p ->
				Poly.to_list_expanded p
				|> List.map Pervasives.fst
				|> List.concat
				|> List.fold_left (fun s x -> VSet.add x s) VSet.empty
				(*|> VSet.remove Poly.V.u*)
			 in
			 let gatherParams : Poly.t list -> (int * Poly.V.t) list
				= fun l ->
				List.map gatherParams1 l
				|> List.fold_left VSet.union VSet.empty
				|> VSet.elements
				|> List.mapi (fun i x -> (i, x))
			 in
			 fun lin cst ->
			 if not (List.for_all Poly.is_affine lin && Poly.is_affine cst)
			 then Pervasives.invalid_arg "Obj._buildOfPoly"
			 else
				let l = gatherParams (cst :: lin) in
				let nm =
			List.fold_left (fun nm' (i, x) -> Naming.allocAt Naming.Param x i nm')
						 Naming.empty
						 l
				in
				let lin' = List.map (ParamCoeff.ofPoly (Naming.to_index nm Naming.Param) (List.length l)) lin in
				let cst' = ParamCoeff.ofPoly (Naming.to_index nm Naming.Param) (List.length l) cst in
				(Objective.mk lin' cst', nm)

		let obj_of_polyList : Poly.t list -> Objective.t * Naming.t
		  = fun l ->
		  if List.length l < 1 then Pervasives.invalid_arg "Obj.of_polyList"
		  else
			 let l' = List.rev l in
			 obj_buildOfPoly (List.tl l' |> List.rev) (List.hd l')

		let obj_of_polySparseList : int -> (int * Poly.t) list -> Poly.t -> Objective.t * Naming.t
		  = let rec fill : int -> int -> (int * Poly.t) list -> Poly.t list
				= fun n i ->
				function
				| [] -> if i < n then Poly.z :: fill n (i + 1) [] else []
				| ((x, a) :: l') as l ->
			 if n <= i || x < i then Pervasives.invalid_arg "Obj.of_polySparseList"
			 else if x = i then a :: fill n (i + 1) l'
			 else Poly.z :: fill n (i + 1) l
			 in
			 fun n l a ->
			 obj_buildOfPoly (List.sort (fun (i, _) (i', _) -> Pervasives.compare i i') l |> fill n 0) a

		let obj_of_poly : Poly.t -> Poly.V.t list -> Objective.t * Naming.t
		  = fun p l ->
		  let lin = List.map (fun x -> Poly.monomial_coefficient_poly p (Poly.MonomialBasis.mk [x,1])) l in
		  let cst = Poly.sub p
			(List.fold_left
				(fun p1 x -> Poly.add
					p1
					(Poly.mul
						(Poly.monomial_coefficient_poly p (Poly.MonomialBasis.mk [x,1]))
						(Poly.fromVar x)))
			Poly.z l)
		  |> Poly.mul Poly.negU in
		  obj_buildOfPoly lin cst

		(** row_from_constraint p mb converts the Poly.t p into a row*)
		let rec (row_from_constraint : Poly.t -> Poly.V.t list -> Tableau.Vector.t)
		  = fun p vars ->
		  match vars with
		  | [] -> [Scalar.Rat.neg (Poly.get_constant p)]
		  | var :: tail -> let coeff = Poly.monomial_coefficient p (Poly.MonomialBasis.mk [var,1]) in
					coeff::(row_from_constraint p tail);;

        let from_poly : Poly.V.t list -> Poly.t list -> Poly.t list -> Poly.t -> t
            = fun vars ineqs eqs obj ->
            if List.length vars + List.length ineqs < List.length ineqs + List.length eqs
            then Pervasives.invalid_arg "PSplx.Build.from_poly: variables"
            else
                if List.exists Poly.isZ ineqs || List.exists Poly.isZ eqs
                then Pervasives.invalid_arg "PSplx.Build.from_poly: constraints"
                else
                    let (o, nm) = obj_of_poly obj vars in
                    mk o
                        (List.map (fun r -> row_from_constraint r vars) (ineqs @ eqs))
                        [] (Naming.mkVar vars nm)
                    |> addSlacks (List.length ineqs)
	end
end
