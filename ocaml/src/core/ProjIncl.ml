module Debug = DebugTypes.Debug(struct let name = "ProjIncl" end)
module Cs = Cstr.Rat

let get_left_cert : (('c1,'c2) Cons.discr_t) Cons.t -> 'c1 Cons.t
    = fun (cstr, (cert,_)) ->
    (cstr, cert)

let get_left_cert_region : (('c1,'c2) Cons.discr_t) PLP.Region.t -> 'c1 PLP.Region.t
    = fun reg -> {reg with
        sx = {reg.sx with
            cstrs = List.map get_left_cert reg.sx.cstrs;
        };
    }

let get_cert : ((('c1,'c2) Cons.discr_t) PLP.Region.t * (('c1,'c2) Cons.discr_t) Cons.t) list option
    -> ('c1 PLP.Region.t * 'c1 Cons.t) list
    = function
    | None -> []
    | Some regions ->
        List.fold_left (fun acc (reg, cons) ->
            (get_left_cert_region reg, get_left_cert cons) :: acc
        ) [] regions

(* TODO : remove [vars] from function parameters *)
let proj_incl' : 'c1 Factory.t -> 'c2 Factory.t -> Cs.Vec.t -> Var.t list -> 'c1 Cons.t list
    -> 'c2 Cons.t list -> ('c1 PLP.Region.t * 'c1 Cons.t) list option
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
        let n_rows = (List.length params) + 2 in
        let sx = PSplxBuild.init conss n_rows i_cstrs1 normalization_point in
        List.iter (fun i_col ->
            PSplxBuild.set_var_set i_col 2 sx
        ) i_cstrs2;
        (* Projection constraints: *)
        List.iteri (fun i_row var ->
            PSplxBuild.elim_from (i_row + 1) var i_cstrs1 1 sx
        ) vars;
        (* Inclusion constraints for variables: *)
        let params_to_equal = Misc.subtract params vars in
        List.iteri (fun i_row var ->
            PSplxBuild.var_equal (i_row + n_vars + 1) var i_cstrs1 i_cstrs2 2 sx
        ) params_to_equal;
        (* Projection constraint for constant: *)
        PSplxBuild.cste_equal (n_rows - 1) i_cstrs1 i_cstrs2 2 sx;
        Debug.log DebugTypes.Normal (lazy (Printf.sprintf "Initial simplex tableau: %s"
            (PSplx.to_string sx)));
        let factory_mix = Cons.discr_factory factory1 factory2 in
        try
            let regs = PLP.run_classic factory_mix sx in
        	let res = (*Join.filter_trivial regs *)
            	(*Join.rem_dupl regs*) (* TODO : il faut merger les régions !! *)
                get_cert regs
            in Some res
        with PSplxExec.Infeasible_problem -> None

let proj_incl : 'c1 Factory.t -> 'c2 Factory.t -> Cs.Vec.t -> Var.t list -> 'c1 Cons.t list
    -> 'c2 Cons.t list -> ('c1 PLP.Region.t * 'c1 Cons.t) list option
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
        | Some p -> Misc.list_to_string (Cons.to_string Var.to_string)
            (List.split p |> snd) " ; ")
    );
    res
