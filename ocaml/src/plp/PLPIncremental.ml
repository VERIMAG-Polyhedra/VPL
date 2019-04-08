include PLPCore

exception EmptyInput

(** @return None if the old normalization point is still ok
    @return a new normalization point otherwise
    @raise EmptyInput if the new polyhedron is empty *)
let check_normalization_point : Cs.t list -> 'c Cons.t -> Cs.Vec.t -> Vector.Symbolic.t option
    = fun cstrs (new_cstr,_) old_normalization_point ->
    if Cs.satisfy old_normalization_point new_cstr
    then begin
        Debug.log DebugTypes.Detail (lazy(Printf.sprintf
            "Old normalization point %s is still ok"
            (Cs.Vec.to_string Var.to_string old_normalization_point)));
        None
    end else begin
        Debug.log DebugTypes.Detail (lazy(Printf.sprintf
            "Old normalization point %s violates the new constraint"
            (Cs.Vec.to_string Var.to_string old_normalization_point)));
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

let add_column_to_region : 'c Region.t -> 'c Cons.t -> 'c Region.t * ExplorationPoint.t list
    = fun reg cons ->
    Debug.log DebugTypes.Detail (lazy(Printf.sprintf
        "Adding constraint %s to region: \n%s"
        (Cons.to_string Var.to_string cons)
        (Region.to_string reg)));
    let regid = reg.id in
    (* TODO : is it necessary to extract? *)
    let prev_frontiers = Region.extract reg.Region.sx in
    let sx' = PSplxBuild.add_col cons reg.Region.sx in
    Debug.log DebugTypes.Detail (lazy(Printf.sprintf
        "New simplex tableau:\n%s"
        (PSplx.to_string sx')));
    let frontiers = Region.extract sx' in
    (* Frontier associated with the new column: *)
    let i_new_col = (Tableau.nCols sx'.tab) - 2 in
    (* -2 because the last column contains the current objective value *)
    let new_frontier = Objective.get i_new_col sx'.PSplx.obj in
    Debug.log DebugTypes.Detail (lazy(Printf.sprintf
        "Frontier associated with the new column : %s"
        (Cs.to_string Var.to_string new_frontier)));
    if List.mem new_frontier frontiers
        && not (List.exists (Cs.equal new_frontier) prev_frontiers)
    then begin
        Debug.log DebugTypes.Detail (lazy("The frontier is irredundant"));
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
    end
    else begin(* The new frontier is redundant *)
        Debug.log DebugTypes.Detail (lazy("The frontier is redundant"));
        ({reg with sx = sx'}, [])
    end

let add_column : 'c Factory.t -> 'c config -> Cs.t list -> 'c Region.t list -> 'c Cons.t -> Vector.Symbolic.t
    -> ('c Region.t * 'c Cons.t) list * Vector.Symbolic.t
    = fun factory config ineqs regs cons old_point ->
    let new_point = match check_normalization_point ineqs cons (Rtree.map Vec.ofSymbolic old_point) with
        | Some new_point ->
            let new_point_rat = Rtree.map Vec.ofSymbolic new_point in
            List.iter (fun reg ->
                (*let (solution,_) = PSplx.objValueCert factory reg.Region.sx in*)
                Renormalization.renormalize_sx new_point_rat reg.Region.sx
            ) regs;
            new_point
        | None -> old_point
    in
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
    let regs = exec config (List.hd regs').Region.sx plp
    |> get_results factory in
    (regs, new_point)
