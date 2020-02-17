module Debug = DebugTypes.Debug(struct let name = "Proj" end)
module Cs = Cstr.Rat

let regsToCs : ('c PLP.Region.t * 'c Cons.t) list -> ('c PLP.Region.t * 'c Cons.t) list
    = fun regs ->
    Debug.log DebugTypes.MOutput
        (lazy (Printf.sprintf "Regions: \n%s\n"
            (Misc.list_to_string
                (fun (reg,sol) -> Printf.sprintf "%s --> %s"
                    (Cons.to_string Var.to_string sol)
                    (PLP.Region.to_string reg)) regs "\n")));
    regs
(* Cons.clean (List.split regs |> Pervasives.snd) *)

let explore : 'c Factory.t -> 'c PSplx.t -> ('c PLP.Region.t * 'c Cons.t) list
    = fun factory tab ->
    let config = { PLP.std_config with
        PLP.reg_t = (if !Flags.sum_lambda_1 then PLP.NCone else PLP.Cone);
    }
    in
    match PLP.run factory config tab with
    | None -> []
    | Some regs -> regsToCs regs

let proj : 'c Factory.t -> Cs.Vec.t -> Var.t list -> 'c Cons.t list
    -> ('c PLP.Region.t * 'c Cons.t) list
	= fun factory normalization_point vars ineqs ->
    Debug.log DebugTypes.Title (lazy "Building Projection");
    Debug.log DebugTypes.MInput (lazy (Printf.sprintf
        "eliminating %s from %s\nusing normalization point %s"
        (Misc.list_to_string Var.to_string vars " ; ")
        (Misc.list_to_string (Cons.to_string Var.to_string) ineqs " ; ")
        (Cs.Vec.to_string Var.to_string normalization_point)));
    if List.length ineqs = 0
    then []
    else
        let conss = ineqs @ [PSplxBuild.trivial_constraint factory] in
        let obj_cstrs = List.mapi (fun i _ -> i) conss in
        let n_rows = (List.length vars) + 1 in
        let sx = PSplxBuild.init conss n_rows obj_cstrs normalization_point in
        List.iteri (fun i_row var ->
            PSplxBuild.elim_from (i_row + 1) var obj_cstrs 1 sx
        ) vars;
        Debug.log DebugTypes.Normal (lazy (Printf.sprintf "Initial simplex tableau: %s"
            (PSplx.to_string sx)));
        explore factory sx
