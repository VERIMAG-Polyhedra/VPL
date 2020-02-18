open PSplx
open PSplxBuild

module Cs = Cstr.Rat

exception Unbounded_problem
exception Infeasible_problem

module Explore = struct

    let add_pivot_obj : int -> Cs.t ->  pivotT
        = fun i_row pcoeff (pcoeff', i_new_col, tab) ->
        Cs.mulc_no_exc tab.(i_row).(i_new_col) pcoeff
        |> Cs.add pcoeff'

    let add_pivots: bool -> int -> Cs.t -> Tableau.col -> pivotT
        = fun init_phase i_row pcoeff col (pcoeff', i_new_col, tab) ->
        let bk' = Q.div tab.(i_row).(i_new_col) col.(i_row) in
        Tableau.iteri_col (fun i_row' coeff ->
            if i_row' = i_row
            then bk'
            else Q.mul bk' col.(i_row')
                |> Q.sub coeff
        ) i_new_col tab;
        if init_phase
        then pcoeff' (* objective must not be affected by the initialization phase*)
        else begin
            Cs.mulc_no_exc bk' pcoeff
            |> Cs.mulc_no_exc Scalar.Rat.negU
            |> Cs.add pcoeff'
        end

    let pivot : bool -> 'c t -> int -> int -> unit
        = fun init_phase sx i_row i_col ->
        Debug.log DebugTypes.Detail (lazy
            (Printf.sprintf "Pivoting column %i and row %i" i_col i_row));
        sx.pivots <- sx.pivots @ [
            add_pivots init_phase i_row (Objective.get i_col sx.obj) (Tableau.getCol i_col sx.tab)
        ];
        sx.basis <- List.mapi (fun i j -> if i = i_row then i_col else j) sx.basis;
        Tableau.pivot i_row i_col sx.tab;
        sx.obj <- Objective.elim sx.tab i_row i_col sx.obj

    let fix_tableau : bool -> int -> 'c t -> unit
        = fun init_phase i_row sx ->
        Debug.log DebugTypes.Detail
            (lazy (Printf.sprintf "fix_tableau' : row %i of tableau\n%s" i_row (to_string sx)));
        let row_var_set = VarMap.find (List.nth sx.basis i_row) sx.get_set
        and n_cols = Tableau.nCols sx.tab in
        let i_col = match Misc.array_fold_left_i (fun i_col res coeff ->
            (* Last column is constant part *)
            if i_col >= n_cols - 1
            then res
            else
                match res with
                | Some (_, coeff_res) ->
                    if Scalar.Rat.lt coeff coeff_res
                    && VarMap.find i_col sx.get_set = row_var_set
                    then Some (i_col, coeff)
                    else res
                | None -> if Scalar.Rat.lt coeff Scalar.Rat.z
                    && VarMap.find i_col sx.get_set = row_var_set
                    then Some (i_col, coeff)
                    else None
            ) None sx.tab.(i_row)
            with Some (i,_) -> i | None -> raise Infeasible_problem
        in
        pivot init_phase sx i_row i_col

    let rec check_tableau : bool -> 'c t -> unit
        = fun init_phase sx ->
        let i_col = constant_index sx in
        try let i_row = Misc.array_findi (fun _ row ->
                Q.lt row.(i_col) Q.zero
            ) sx.tab in
            fix_tableau init_phase i_row sx;
            check_tableau init_phase sx
        with Not_found -> ()

    let get_row_pivot : 'c t -> int -> int
        = fun sx i_col ->
        Debug.log DebugTypes.Detail (lazy(Printf.sprintf
            "Looking for leaving variable for entering variable %i"
            i_col));
        let set_col = VarMap.find i_col sx.get_set in
        Tableau.fold_left_cols (fun (i_row, min) a b ->
            let set_row = VarMap.find (List.nth sx.basis i_row) sx.get_set in
            if set_row = set_col && Q.sign a > 0
            then let x = Q.div b a in
                match min with
                | None -> (i_row + 1, Some (i_row, x))
                | Some (_, value) ->
                    if Q.lt x value
                    then (i_row + 1, Some (i_row, x))
                    else (i_row + 1, min)
            else (i_row + 1, min)
        ) (0, None) i_col (constant_index sx) sx.tab
        |> Stdlib.snd
        |> function
        | None -> raise Unbounded_problem
        | Some (i_min,_) -> i_min

    let rec push' : bool -> Cs.Vec.t -> 'c t -> unit
        = fun init_phase point sx ->
        Debug.log DebugTypes.Detail
            (lazy (Printf.sprintf "push' : \n%s" (to_string sx)));
        match Objective.getPivotCol point sx.obj with
        | Objective.OptReached -> check_tableau init_phase sx
        | Objective.PivotOn i_col ->
            let i_row = get_row_pivot sx i_col in
            pivot init_phase sx i_row i_col;
            push' init_phase point sx

    let push : bool -> Cs.Vec.t -> 'c t -> unit
        = fun init_phase point sx ->
        Debug.log DebugTypes.Title
            (lazy "Parametric simplex");
        Debug.log DebugTypes.MInput
            (lazy (Printf.sprintf "Point %s\n%s"
                (Cs.Vec.to_string Var.to_string point)
                (to_string sx)));
        push' init_phase point sx;
        Debug.log DebugTypes.MOutput (lazy (to_string sx))
end

module Init = struct

    let getReplacementForA : 'c t -> int -> int
        = fun sx i_row ->
        let max_col = (Tableau.nCols sx.tab) - 2 in
        let set_row = List.nth sx.sets i_row in
        try Misc.array_findi (fun i_col coeff ->
                i_col < max_col
                && not (Scalar.Rat.isZ coeff)
                && set_row = VarMap.find i_col sx.get_set
            ) sx.tab.(i_row)
        with Not_found -> Stdlib.failwith "t.a_still_in_basis"

    let buildAuxiliaryPB : 'c Factory.t -> 'c t -> 'c t * (int list)
        = fun factory sx ->
        Debug.log DebugTypes.Detail (lazy "Building auxiliary problem");
        (* One auxiliary variable is created for each variable set *)
        let var_sets = VarMap.bindings sx.get_set
            |> List.fold_left (fun acc (_,var_set) ->
                if List.mem var_set acc
                then acc
                else var_set :: acc
            ) []
        in
        let i_last_col = (Tableau.nCols sx.tab) - 1 in
        let get_set' = Misc.fold_left_i (fun i acc var_set ->
            VarMap.add (i + i_last_col) var_set acc
        ) sx.get_set var_sets in
        let i_new_cols = List.mapi (fun i _ ->
            i + i_last_col
        ) var_sets in
        let cstrs' = sx.cstrs
            @ List.map (fun _ -> Cons.mkTriv factory Cstr_type.Le Scalar.Rat.u) i_new_cols
        in
        let sx' = Init.init_cstrs cstrs' empty in
        Init.init_var_set sx';
        Init.mk_obj i_new_cols sx';
        Init.init_new_col (Tableau.nRows sx.tab) sx';
        let tab' = List.fold_left (fun tab var_set ->
            Tableau.addCol (fun i_row ->
                if List.nth sx.sets i_row = var_set
                && Q.lt (sx.tab.(i_row).(i_last_col)) Q.zero
                then Q.minus_one
                else Q.zero
            ) tab
        ) sx.tab var_sets
        in
        let sx'' = {sx' with
            tab = tab';
            get_set = get_set';
            basis = sx.basis;
            sets = sx.sets;
            new_col = sx.new_col;
            pivots = sx.pivots;
        } in
        (* Removing null columns *)
        let (sx'', i_new_cols') = List.fold_right (fun i_col (sx,l) ->
            if Tableau.getCol i_col sx.tab
                |> Array.for_all (Scalar.Rat.equal Scalar.Rat.z)
            then (remCol i_col sx, Misc.pop (=) l i_col)
            else (sx, l)
        ) i_new_cols (sx'', i_new_cols)
        in
        Debug.log DebugTypes.Detail (lazy (Printf.sprintf
            "Auxiliary problem: \n%s"
            (to_string sx'')));
        (sx'',i_new_cols')


    exception Empty_row

    let chooseBasicVar : int -> 'c t -> bool
        = fun i_row sx ->
        try
            Debug.log DebugTypes.Detail (lazy (Printf.sprintf
                "Looking for basic variable in row %i, of variable set %i"
                i_row
                (List.nth sx.sets i_row)));
            let row_var_set = List.nth sx.sets i_row in
            let i_col = match Misc.array_fold_left_i (fun i_col res coeff ->
                match res with
                | Some _ -> res
                | None -> if not (Scalar.Rat.isZ coeff)
                    && VarMap.find i_col sx.get_set = row_var_set
                    then Some i_col
                    else None
                ) None sx.tab.(i_row)
                with Some i -> i | None -> raise Not_found
            in
            if i_col > (Tableau.nCols sx.tab) - 2
            then false (* last column is the constant *)
            else begin
                sx.basis <- sx.basis @ [i_col];
                sx.pivots <- sx.pivots @ [
                    Explore.add_pivots false i_row (Objective.get i_col sx.obj) (Tableau.getCol i_col sx.tab)
                ];
                Tableau.pivot i_row i_col sx.tab;
                sx.obj <- Objective.elim sx.tab i_row i_col sx.obj;
                true
            end
        with Not_found -> raise Empty_row

    let correction : 'c t -> bool
        = let rec correction_rec
            = fun i_row sx ->
            if i_row >= nRows sx
            then true
            else (chooseBasicVar i_row sx)
                && (correction_rec (i_row + 1) sx)
        in
        fun sx ->
        correction_rec (List.length sx.basis) sx

    let buildFeasibleTab : Objective.t -> 'c t -> unit
        = let syncObjWithBasis : Tableau.t -> int list -> Objective.t -> Objective.t
            = fun mat basis obj ->
            Misc.fold_left_i (fun i_row obj i_col' ->
                Objective.elim mat i_row i_col' obj
            ) obj basis
        in
        fun o sx ->
        List.iteri (fun i_row i_col' ->
            let pcoeff = Cs.mulc_no_exc Scalar.Rat.negU (Objective.get i_col' o) in
            sx.pivots <- sx.pivots @ [
                Explore.add_pivot_obj i_row pcoeff
            ]
        ) sx.basis;
        sx.obj <- syncObjWithBasis sx.tab sx.basis o

    let findFeasibleBasis : 'c Factory.t -> 'c t -> Cs.Vec.t -> bool
        = fun factory sx point ->
        if not(correction sx)
        then false
        else begin
            Debug.log DebugTypes.Detail (lazy (Printf.sprintf
                "Correction gave: \n%s"
                (to_string sx)));
            if isFeasible sx
            then true
            else begin
                let (sx', aux_vars) = buildAuxiliaryPB factory sx in
                Explore.push true point sx';
                Debug.log DebugTypes.Detail (lazy (Printf.sprintf
                    "Auxiliary problem after exploration: \n%s"
                    (to_string sx')));
                let obj_value = objValue sx' in
                if not (Cs.Vec.equal obj_value.Cs.v Cs.Vec.nil && Scalar.Rat.isZ obj_value.Cs.c)
                then false
                else begin
                    List.iter (fun i_col ->
                        try
                            let i_row = Misc.findi ((=) i_col) sx'.basis in
                            getReplacementForA sx' i_row
                            |> Explore.pivot false sx' i_row
                        with Not_found -> ()
                    ) aux_vars;
                    List.iter (fun i_col ->
                        sx'.tab <- Tableau.remCol i_col sx'.tab;
                    ) (List.rev aux_vars);
                    (* List.rev necessary : columns must be removed from the right to the left*)
                    buildFeasibleTab sx.obj sx';
                    copy_in sx' sx;
                    true
                end
            end
        end
end

let init_and_push : 'c Factory.t -> Cs.Vec.t -> 'c t -> bool
    = fun factory point sx ->
    if Init.findFeasibleBasis factory sx point
    then begin
        Explore.push false point sx;
        Debug.exec true DebugTypes.Detail (lazy (to_string sx))
        end
    else Debug.exec false DebugTypes.Normal
        (lazy "Initialization: the PLP problem is infeasible")
