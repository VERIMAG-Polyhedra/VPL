module Debug = DebugTypes.Debug(struct let name = "ProjIncl" end)
module Cs = Cstr.Rat
(*
let regsToCs : ('c PLP.Region.t * 'c Cons.t) list
    -> 'c Cons.t list * (Cs.t list * 'c Cons.t) list
    = fun regs ->
    Debug.log DebugTypes.MOutput
        (lazy (Printf.sprintf "Regions: \n%s\n"
            (Misc.list_to_string
                (fun (reg,sol) -> Printf.sprintf "%s --> %s"
                    (Cons.to_string Var.to_string sol)
                    (PLP.Region.to_string reg)) regs "\n")));
    Debug.log DebugTypes.Title (lazy "Building result from regions");
    let sols = Cons.clean (List.split regs |> Pervasives.snd) in
    let regions = List.map (fun (reg,sol) ->
        (PLP.Region.get_cstrs reg, sol)
    ) regs in
    Debug.log DebugTypes.Title (lazy "Result has been built from regions");
    (sols, regions)

let explore : 'c Factory.t -> 'c PSplx.t -> 'c Cons.t list * (Cs.t list * 'c Cons.t) list
    = fun factory tab ->
    let config = { PLP.std_config with
        PLP.reg_t = (if !Flags.sum_lambda_1 then PLP.NCone else PLP.Cone);
    }
    in
    match PLP.run factory config tab with
    | None -> ([],[])
    | Some regs -> regsToCs regs
*)

let proj_incl' : 'c1 Factory.t -> 'c2 Factory.t -> Cs.Vec.t -> Var.t list -> 'c1 Cons.t list
    -> 'c2 Cons.t list -> 'c1 Cons.t list option
	= fun factory1 factory2 normalization_point vars p1 p2 ->
    if List.length p1 = 0
    then if List.length p2 = 0
        then Some []
        else None
    else
        let p1' = Join.mk_constraints_p1 factory1 factory2 p1
        and p2' = Join.mk_constraints_p2 factory1 factory2 p2 in
        let len_p1' = List.length p1' in
        let i_cstrs1 = List.mapi (fun i _ -> i) p1'
        and i_cstrs2 = List.mapi (fun i _ -> i + len_p1') p2' in
        let conss = p1' @ p2' in
    	let params = List.map Cons.get_c conss
            |> Cs.getVars
            |> Var.Set.elements in
        let n_vars = List.length vars in
        let n_rows = n_vars + (List.length params) + 2 in
        let sx = PSplxBuild.init conss n_rows i_cstrs1 normalization_point in
        List.iter (fun i_col ->
            PSplxBuild.set_var_set i_col 2 sx
        ) i_cstrs2;
        (* Projection constraints: *)
        List.iteri (fun i_row var ->
            PSplxBuild.elim_from (i_row + 1) var i_cstrs1 1 sx
        ) vars;
        (* Inclusion constraints for variables: *)
        List.iteri (fun i_row var ->
            PSplxBuild.var_equal (i_row + n_vars + 1) var i_cstrs1 i_cstrs2 2 sx
        ) params;
        (* Projection constraint for constant: *)
        PSplxBuild.cste_equal (n_rows - 1) i_cstrs1 i_cstrs2 1 sx;
        Debug.log DebugTypes.Normal (lazy (Printf.sprintf "Initial simplex tableau: %s"
            (PSplx.to_string sx)));
        let factory_mix = Cons.discr_factory factory1 factory2 in
        try
            let regs = PLP.run_classic factory_mix sx in
        	let res = Join.filter_trivial regs
        	|> Join.rem_dupl
            |> Join.get_join_cert
            |> Pervasives.fst
            in Some res
        with PSplxExec.Infeasible_problem -> None

let proj_incl : 'c1 Factory.t -> 'c2 Factory.t -> Cs.Vec.t -> Var.t list -> 'c1 Cons.t list
    -> 'c2 Cons.t list -> 'c1 Cons.t list option
	= fun factory1 factory2 normalization_point vars p1 p2 ->
    Debug.log DebugTypes.Title (lazy "Building ProjInclusion");
    Debug.log DebugTypes.MInput (lazy (Printf.sprintf
        "eliminating %s from %s\nusing normalization point %s"
        (Misc.list_to_string Var.to_string vars " ; ")
        (Misc.list_to_string (Cons.to_string Var.to_string) p1 " ; ")
        (Cs.Vec.to_string Var.to_string normalization_point)));
    let res = proj_incl'  factory1 factory2 normalization_point vars p1 p2 in
    Debug.log DebugTypes.MOutput (lazy (
        match res with
        | None -> "Inclusion does not hold"
        | Some p -> Misc.list_to_string (Cons.to_string Var.to_string) p " ; ")
    );
    res
