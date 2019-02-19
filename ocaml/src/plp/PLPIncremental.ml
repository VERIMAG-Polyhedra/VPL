include PLPCore

let add_column_to_region : 'c Region.t -> 'c Cons.t -> 'c Region.t * ExplorationPoint.t list
    = fun reg cons ->
    let regid = reg.id in
    let sx' = PSplxBuild.add_col cons reg.Region.sx in
    let frontiers = Region.extract sx' in
    (* Frontier associated with the new column: *)
    let i_new_col = (Tableau.nCols sx'.tab) - 2 in
    (* -2 because the last column contains the current objective value *)
    let new_frontier = Objective.get i_new_col sx'.PSplx.obj in
    if List.mem new_frontier frontiers
    then (* The new frontier is irredundant *)
        match Exec.exec Cone sx' reg.point with
        | None -> failwith "add_column_to_region"
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
            in (reg', todo)
            end
    else (* The new frontier is redundant *)
        ({reg with sx = sx'}, [])

(*
let add_column : (Region.t * 'c Cons.t) list -> ParamCoeff.t -> Tableau.Vector.t -> (Region.t * 'c Cons.t) list
    = fun regs pcoeff column ->
    List.map (fun (reg, cons) -> add_column_to_region reg)
*)
