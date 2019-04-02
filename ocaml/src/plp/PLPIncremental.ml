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


let add_column : 'c Factory.t -> 'c config -> 'c Region.t list -> 'c Cons.t -> ('c Region.t * 'c Cons.t) list
    = fun factory config regs cons ->
    let (regs', todo) = List.fold_left (fun (regs, todo) reg ->
        let (reg', todo') = add_column_to_region reg cons in
        reg' :: regs, todo @ todo'
    ) ([],[]) regs
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
    exec config (List.hd regs').Region.sx plp
    |> get_results factory
