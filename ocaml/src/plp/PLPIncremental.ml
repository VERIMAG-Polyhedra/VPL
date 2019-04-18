include PLPCore

let fix_tableau : 'c PSplx.t -> unit
    = fun sx ->
    Debug.log DebugTypes.Detail (lazy (Printf.sprintf
        "Fixing tableau %s"
        (PSplx.to_string sx)));
    let tab = sx.tab in
    let pivot_coeff = tab.(0).(List.nth sx.basis 0) in
    if Scalar.Rat.equal pivot_coeff Scalar.Rat.z
    then failwith "fix_tableau: wrong basic variable"
    else begin
        (* Scaling normalization row *)
        Array.iteri (fun i_col coeff ->
            Scalar.Rat.div coeff pivot_coeff
            |> Array.set tab.(0) i_col
        ) tab.(0);
        (* Scaling normalization row according to other basic variables. *)
        List.iteri (fun i_row i_col ->
            let coeff = tab.(0).(i_col) in
            Array.iteri (fun i_col' c ->
                Scalar.Rat.mul coeff c
                |> Scalar.Rat.sub tab.(0).(i_col')
                |> Array.set tab.(0) i_col'
            ) tab.(i_row)
        ) (List.tl sx.basis);
        Debug.log DebugTypes.Detail (lazy (Printf.sprintf
            "Tableau fixed: %s"
            (PSplx.to_string sx)))
    end

let renormalize : Vector.Rat.t -> 'c PSplx.t -> unit
    = fun point sx ->
    Debug.log DebugTypes.Normal (lazy (Printf.sprintf
        "Renormalizing region on point %s"
        (Vector.Rat.to_string Var.to_string point)));
    let f cstr = Cs.eval cstr point |> Scalar.Rat.neg in
    PSplxBuild.Init.init_row (fun i_col cons ->
        if PSplx.VarMap.find i_col sx.get_set = 1
        then f (List.nth sx.cstrs i_col |> fst)
        else Scalar.Rat.z
    ) Scalar.Rat.u 0 sx;
    PSplxBuild.update_new_col 0 f sx;
    fix_tableau sx

    exception EmptyInput

    (** @return None if the old normalization point is still ok
        @return a new normalization point otherwise
        @raise EmptyInput if the new polyhedron is empty *)
    let check_normalization_point : Cs.t list -> 'c Cons.t -> Cs.Vec.t -> Vector.Symbolic.t option
        = fun cstrs (new_cstr,_) old_normalization_point ->
        if Cs.satisfy old_normalization_point new_cstr
        then begin
            Debug.log DebugTypes.Normal (lazy(Printf.sprintf
                "Old normalization point %s is still ok"
                (Cs.Vec.to_string Var.to_string old_normalization_point)));
                Printf.sprintf
                    "Old normalization point %s is still ok"
                    (Cs.Vec.to_string Var.to_string old_normalization_point)
                    |> print_endline;
            None
        end else begin
            Debug.log DebugTypes.Normal (lazy(Printf.sprintf
                "Old normalization point %s violates the new constraint"
                (Cs.Vec.to_string Var.to_string old_normalization_point)));
                Printf.sprintf
                    "Old normalization point %s violates the new constraint"
                    (Cs.Vec.to_string Var.to_string old_normalization_point)
                    |> print_endline;
            let cstrs' = new_cstr :: cstrs in
            let cstrs'' = List.mapi
                (fun i cstr -> i, cstr)
                cstrs'
            in
            let horizon = Cs.getVars cstrs' |> Var.horizon in
            match Opt.getAsg horizon cstrs'' with
            | None -> raise EmptyInput
            | Some pl -> Some pl
        end

let add_column_to_region : 'c Region.t -> 'c Cons.t -> ('c Region.t * ExplorationPoint.t list) option
    = fun reg cons ->
    Debug.log DebugTypes.Normal (lazy(Printf.sprintf
        "Adding constraint %s to region: \n%s"
        (Cons.to_string Var.to_string cons)
        (Region.to_string reg)));
    let regid = reg.id in
    let prev_frontiers = Region.get_cstrs reg in
    let sx' = PSplxBuild.add_col cons reg.Region.sx in
    Debug.log DebugTypes.Normal (lazy(Printf.sprintf
        "New simplex tableau:\n%s"
        (PSplx.to_string sx')));
    let i_new_col = (Tableau.nCols sx'.tab) - 2 in
    let new_frontier = Objective.get i_new_col sx'.PSplx.obj in
    let cstrs = new_frontier :: prev_frontiers
        |> Misc.rem_dupl Cs.equal
	  	|> List.filter (fun c -> is_trivial c |> not)
    in
    match Exec.correct_point Cone cstrs with
    | None -> None
    | Some point ->
        let bounds = Exec.get_boundaries Cone cstrs point in
        let new_frontiers = List.map Boundary.get_cstr bounds in
        (* -2 because the last column contains the current objective value *)
        if List.mem new_frontier new_frontiers
            && not (List.mem new_frontier prev_frontiers)
        then begin
            Debug.log DebugTypes.Normal (lazy("The frontier is irredundant"));
            match Exec.exec Cone sx' point with
            | None -> None
            | Some reg -> begin
                let reg' = {reg with id = regid} in
                (* A new id was given by the execution. *)
                reg_id := !reg_id - 1;
                let new_frontier = Objective.get i_new_col reg.sx.PSplx.obj
                in
                let todo = List.fold_left (fun todo ((cs,point) as b,_) ->
                    if Cs.equalSyn cs new_frontier
                    then ExplorationPoint.Direction (regid, b) :: todo
                    else todo
                    ) [] reg'.r
                in Some (reg', todo)
                end
        end
        else begin(* The new frontier is redundant *)
            Debug.log DebugTypes.Normal (lazy("The frontier is redundant"));
            Some ({reg with sx = sx'}, [])
        end

let add_column : 'c Factory.t -> 'c config -> Cs.t list -> 'c Region.t list -> 'c Cons.t -> Vector.Symbolic.t
    -> ('c Region.t * 'c Cons.t) list * Vector.Symbolic.t
    = fun factory config ineqs regs cons old_point ->
    let new_point = match check_normalization_point ineqs cons (Rtree.map Vec.ofSymbolic old_point) with
        | Some new_point -> List.iter (fun reg ->
                renormalize (Rtree.map Vec.ofSymbolic new_point) reg.Region.sx
            ) regs;
            new_point
        | None -> old_point
    in
    let (regs', todo, n_update, n_deleted) =
        List.fold_left (fun (regs, todo, n_update, n_deleted) reg ->
            match add_column_to_region reg cons with
            | None -> (regs, todo, n_update, n_deleted+1)
            | Some (reg', todo') -> (
                reg' :: regs,
                todo @ todo',
                n_update + (if todo' = [] then 0 else 1),
                n_deleted
            )
    ) ([],[], 0, 0) regs
    in
    let (max_id, map) = List.fold_left (fun (max_id,map) reg ->
        let map' = MapV.add reg.Region.id reg map
        and max_id' = if max_id < reg.id then reg.id else max_id in
        (max_id', map')
    ) (0, MapV.empty) regs' in
    reg_id := max_id;
    let plp = {
        regs = map;
        todo = todo;
    } in
    let regs'' = exec config (List.hd regs').Region.sx plp
    |> get_results factory in
    Printf.sprintf
        "initial regions : %i, Regions modified : %i, new regions : %i, regions deleted : %i"
        (List.length regs)
        n_update
        ((List.length regs'') - (List.length regs))
        n_deleted
        |> print_endline;
    (regs'', new_point)
