open PSplx

let trivial_constraint : 'c Factory.t -> 'c Cons.t
    = fun factory ->
    Cons.mkTriv factory Cstr_type.Le Scalar.Rat.u

let update_new_col : int -> (Cs.t -> Scalar.Rat.t) -> 'c t -> unit
    = fun i_row init_f sx ->
    Array.set sx.new_col i_row init_f

module Init = struct

    let init_row : (int -> 'c Cons.t -> Q.t) -> Q.t -> int -> 'c t -> unit
        = fun init_f cst i_row sx ->
        List.iteri (fun i_col cons ->
            Array.set sx.tab.(i_row) i_col (init_f i_col cons)
        ) sx.cstrs;
        Array.set sx.tab.(i_row) (constant_index sx) cst

    let init_matrix : int -> int -> 'c t -> unit
        = fun n_rows n_cols sx ->
        sx.tab <- Tableau.init n_rows n_cols

    let init_cstrs : 'c Cons.t list -> 'c t -> 'c t
        = fun cstrs sx ->
        { sx with cstrs = cstrs}

    (* Must be called BEFORE normalize*)
    let init_new_col : int -> 'c t -> unit
        = fun n_rows sx ->
        sx.new_col <- Array.make n_rows (fun _ -> Scalar.Rat.z)

    let mk_obj : int list -> 'c t -> unit
        = fun obj_cstrs sx ->
        let null_param_coeff = Cs.top in
        let param_coeffs = List.mapi (fun i cons ->
            if List.mem i obj_cstrs
            then Cons.get_c cons
            else null_param_coeff
            ) sx.cstrs
        in
        sx.obj <- Objective.mk param_coeffs null_param_coeff

    let normalize : int list -> Cs.Vec.t -> var_set -> 'c t -> unit
        = fun obj_cstrs point set sx ->
        Debug.log DebugTypes.Normal (lazy (Printf.sprintf
            "Building normalization constraint on point %s"
            (Cs.Vec.to_string Var.to_string point)));
        let f cstr = Cs.eval cstr point |> Scalar.Rat.neg in
        init_row (fun i_col cons ->
            if List.mem i_col obj_cstrs
            then f (Cons.get_c cons)
            else Scalar.Rat.z
        ) Scalar.Rat.u 0 sx;
        sx.sets <- sx.sets @ [set];
        update_new_col 0 f sx

    let init_var_set : 'c t -> unit
        = fun sx ->
        let n = List.length sx.cstrs in
        let rec init_var_set_rec i map =
            if i = n
            then map
            else init_var_set_rec (i+1) (VarMap.add i 1 map)
        in
        sx.get_set <- init_var_set_rec 0 VarMap.empty
end

let init : 'c Cons.t list -> int -> int list -> Cs.Vec.t -> 'c t
    = fun cstrs n_rows obj_cstrs normalization_point ->
    let sx = Init.init_cstrs cstrs empty in
    Init.init_matrix n_rows ((List.length cstrs + 1)) sx;
    Init.init_var_set sx;
    Init.mk_obj obj_cstrs sx;
    Init.init_new_col n_rows sx;
    Init.normalize obj_cstrs normalization_point 1 sx;
    sx

let elim_from : int -> Var.t -> int list -> var_set -> 'c t -> unit
    = fun i_row var i_cstrs set sx ->
    let f cstr = Cs.Vec.get cstr.Cs.v var in
    Init.init_row (fun i_col cons ->
        if List.mem i_col i_cstrs
        then f (Cons.get_c cons)
        else Scalar.Rat.z
    ) Scalar.Rat.z i_row sx;
    sx.sets <- sx.sets @ [set];
    update_new_col i_row f sx

let var_equal : int -> Var.t -> int list -> int list -> var_set -> 'c t -> unit
    = fun i_row var i_cstrs1 i_cstrs2 set sx ->
    let f cstr = Cs.Vec.get cstr.Cs.v var in
    Init.init_row (fun i_col cons ->
        let cstr = Cons.get_c cons in
        if List.mem i_col i_cstrs1
        then f cstr
        else if List.mem i_col i_cstrs2
            then (f cstr) |> Scalar.Rat.neg
            else Scalar.Rat.z
    ) Scalar.Rat.z i_row sx;
    sx.sets <- sx.sets @ [set];
    update_new_col i_row f sx

let cste_equal : int -> int list -> int list -> var_set -> 'c t -> unit
    = fun i_row i_cstrs1 i_cstrs2 set sx ->
    let f cstr = cstr.Cs.c |> Scalar.Rat.neg in
    Init.init_row (fun i_col cons ->
        let cstr = Cons.get_c cons in
        if List.mem i_col i_cstrs1
        then f cstr
        else if List.mem i_col i_cstrs2
            then (f cstr) |> Scalar.Rat.neg
            else Scalar.Rat.z
    ) Scalar.Rat.z i_row sx;
    sx.sets <- sx.sets @ [set];
    update_new_col i_row f sx

let set_var_set : decision_variable -> var_set -> 'c t -> unit
    = fun var set sx ->
    sx.get_set <- VarMap.add var set sx.get_set

let add_col : 'c Cons.t -> 'c t -> 'c t
    = fun cons sx ->
    let i_new_col = Tableau.nCols sx.tab - 1 in
    let cstr = Cons.get_c cons in
    sx.obj <- Objective.add_col sx.obj cstr i_new_col;
    sx.get_set <- VarMap.add i_new_col 1 sx.get_set;
    let new_col = Array.map (fun f -> f cstr) sx.new_col in
    Debug.log DebugTypes.Normal (lazy (Printf.sprintf
        "Initial column to add:[%s]"
        (Array.to_list new_col
            |> List.map Scalar.Rat.to_string
            |> String.concat ";")));
    let sx' = {sx with
        cstrs = sx.cstrs @ [cons];
        tab = Tableau.addCol (fun i_row -> Array.get new_col i_row) sx.tab;
    } in
    let pcoeff' = List.fold_left (fun pcoeff f_pivot ->
        f_pivot (pcoeff, i_new_col, sx'.tab)
    ) (Objective.get i_new_col sx'.obj) sx'.pivots
    in
    sx'.obj <- {sx'.obj with
        lin = List.mapi (fun i_col pcoeff -> if i_col = i_new_col then pcoeff' else pcoeff) sx'.obj.lin;
    };
    sx'
