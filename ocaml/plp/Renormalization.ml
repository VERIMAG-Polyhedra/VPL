include PLPCore

let renormalize_frontier : Vector.Rat.t -> Cs.t -> Cs.t -> Cs.t
    = fun new_point h f ->
        let hx = Cs.eval h new_point
        and fx = Cs.eval f new_point in
        let cmp = Scalar.Rat.cmpz fx in
        if cmp = 0
        then (* fx = 0 *) f
        else if cmp > 0
            then (* fx < 0*)
                let lambda = Scalar.Rat.div (Scalar.Rat.neg fx) hx in
                Cs.mulc_no_exc lambda h
                |> Cs.add f
            else (* fx > 0*)
                let lambda = Scalar.Rat.div fx hx in
                Cs.mulc_no_exc (Scalar.Rat.neg lambda) h
                |> Cs.add f
(*
let point_in_new_region : Vector.Rat.t -> 'c PSplx.t -> Vector.Rat.t
    = fun new_point sx ->
    let solution = Cs.canon (PSplx.objValue sx) in
    let frontiers' = List.map (renormalize_frontier new_point solution) sx.obj.lin in
    let horizon = Cs.getVars frontiers' |> Var.horizon in
    let cstrs = List.mapi (fun i cstr -> i, cstr) frontiers' in
    match Opt.getAsg horizon cstrs with
    | None -> failwith "point_in_new_region"
    | Some pl -> Rtree.map Vec.ofSymbolic pl
*)

let renormalize_objective : Vector.Rat.t -> Objective.t -> Cs.t -> Objective.t
    = fun new_point obj solution -> {obj with
        lin = List.map (renormalize_frontier new_point solution) obj.lin;
    }

let renormalize_sx : Vector.Rat.t -> 'c PSplx.t -> unit
    = fun new_point sx ->
    Debug.log DebugTypes.Detail (lazy (Printf.sprintf
        "Renormalizing on %s of tableau %s"
        (Vector.Rat.to_string Var.to_string new_point)
        (PSplx.to_string sx)));
    List.iteri (fun i_col (cstr,_) ->
        let coeff = if PSplx.VarMap.find i_col sx.get_set = 1
            then Cs.eval cstr new_point |> Scalar.Rat.neg
            else Scalar.Rat.z
        in
        Array.set sx.tab.(0) i_col coeff
    ) sx.cstrs;
    Array.set sx.tab.(0) ((Tableau.nCols sx.tab) - 1) Scalar.Rat.u;
    let f cstr = Cs.eval cstr new_point |> Scalar.Rat.neg in
    PSplxBuild.update_new_col 0 f sx;
    let basic_col = (List.nth sx.basis 0) in
    if not (Scalar.Rat.equal Scalar.Rat.u (Tableau.get 0 basic_col sx.tab))
    then PSplxExec.Explore.pivot false sx 0 basic_col;
    let solution = Cs.canon (PSplx.objValue sx) in
    sx.obj <- renormalize_objective new_point sx.obj solution;
    (*PSplxExec.Explore.push false (point_in_new_region new_point sx) sx;*)
    Debug.log DebugTypes.Detail (lazy (Printf.sprintf
        "Tableau fixed: %s"
        (PSplx.to_string sx)))

(*


let renormalize_objective : Vector.Rat.t -> Objective.t -> Cs.t -> Objective.t
    = fun new_point obj solution -> {obj with
        lin = List.map (renormalize_frontier new_point solution) obj.lin;
    }

let renormalize_sx : Vector.Rat.t -> Cs.t -> 'c PSplx.t -> unit
    = fun new_point solution sx ->
    Debug.log DebugTypes.Detail (lazy (Printf.sprintf
        "Renormalizing on %s of tableau %s"
        (Vector.Rat.to_string Var.to_string new_point)
        (PSplx.to_string sx)));
    let coeff = Tableau.get 0 ((Tableau.nCols sx.tab) - 1) sx.tab in
    let basic_col = List.nth sx.basis 0 in
     (* We want the basic variable associated to the normalization constraint to keep the same value.
        So, we scale all coefficients so that *)
    List.iteri (fun i_col cstr ->
        if i_col <> basic_col
        then
            Cs.eval cstr new_point
            |> Cs.mul coeff
            |> Array.set sx.tab.(0) i_col
    ) sx.obj.lin;
    sx.obj <- renormalize_objective new_point sx.obj solution;
    Debug.log DebugTypes.Detail (lazy (Printf.sprintf
        "Tableau fixed: %s"
        (PSplx.to_string sx)))
*)
