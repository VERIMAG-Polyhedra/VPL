module Make (D: AbstractDomain.Type) = struct
    include NamedDomain.Make(D)

    module Track = struct
        let file = Config.log_file

        let () =
            let out_channel = Stdlib.open_out_gen [Open_creat; Open_wronly; Open_trunc] 0o640 !file in
            "typedef int abs_value;\ntypedef int var;\nvoid main(){\n"
            |> Stdlib.output_string out_channel;
            Stdlib.close_out(out_channel)

        let track : string -> unit
            = fun s ->
            if !Flags.log_trace
            then begin
                let out_channel = Stdlib.open_out_gen [Open_wronly ; Open_append] 0o640 !file in
                Stdlib.output_string out_channel ("\t" ^ s ^ "\n");
                Stdlib.close_out(out_channel)
            end

        let unary : string -> (t -> t) -> (t -> t)
            = fun op_name op p ->
            let p' = op p in
            Printf.sprintf "{abs_value %s = %s(%s);}"
                p'.name op_name p.name
            |> track;
            p'

        let binary : string -> (t -> t -> t) -> (t -> t -> t)
            = fun op_name op p1 p2 ->
            let p' = op p1 p2 in
            Printf.sprintf "{abs_value %s = %s(%s,%s);}"
                p'.name op_name p1.name p2.name
            |> track;
            p'

        let unary_other : string -> (t -> 'a) -> (t -> 'a)
            = fun op_name op p ->
            let res = op p in
            Printf.sprintf "%s(%s);"
                op_name p.name
            |> track;
            res

        let binary_other : string -> (t -> t -> 'a) -> (t -> t -> 'a)
            = fun op_name op p1 p2 ->
            let res = op p1 p2 in
            Printf.sprintf "%s(%s,%s);"
                op_name p1.name p2.name
            |> track;
            res
    end

    (*****************************************)
    (************ BINARY OPERATORS ***********)
    (*****************************************)
    let meet = Track.binary "meet" meet
    let minkowski = Track.binary "minkowski" minkowski
    let join = Track.binary "join" join
    let widen = Track.binary "widen" widen
    let proj_incl = Track.binary_other "proj_incl" proj_incl

    (*****************************************)
    (************  OTHER OPERATORS ***********)
    (*****************************************)
    let is_bottom = Track.unary_other "isBottom" is_bottom
    let itvize expr = Track.unary_other "get_itv" (itvize expr)
    let size = Track.unary_other "size" size
    let leq = Track.binary_other "includes" leq
    let get_cstrs = Track.unary_other "get_cstrs" get_cstrs

    let assume cond p =
        let p' = assume cond p in
        Printf.sprintf "{abs_value %s = guard(%s, %s);}"
            p'.name p.name
            (b_expr_to_string cond)
        |> Track.track;
        p'

    let assume_back cond p =
        match assume_back cond p with
        | None -> None
        | Some p' -> begin
            Printf.sprintf "{abs_value %s = assume_back(%s, %s);}"
                p'.name p.name
                (b_expr_to_string cond)
            |> Track.track;
            Some p'
        end

    let assign terms p =
        let p' = assign terms p in
        Printf.sprintf "{abs_value %s = assign(%s, %s);}"
            p'.name p.name
            (List.map (fun (var, aexpr) ->
                Printf.sprintf "%s = %s"
                (var_to_string var)
                (a_expr_to_string aexpr)
            ) terms
            |> String.concat ", ")
        |> Track.track;
        p'

    let project vars p =
        let p' = project vars p in
        Printf.sprintf "{abs_value %s = elim(%s, %s);}"
            p'.name p.name
            (List.map var_to_string vars |> String.concat ", ")
        |> Track.track;
        p'

    let project_vars vars p =
        let p' = project_vars vars p in
        Printf.sprintf "{abs_value %s = elim(%s, %s);}"
            p'.name p.name
            (List.map var_to_string vars |> String.concat ", ")
        |> Track.track;
        p'

end

module MakeAbstractDomain (Coeff: Scalar.Type) = struct
    module Cs = Cstr.Rat
    open WrapperTraductors
    module I = Interface(Coeff)
    open I

    module Make (D: HighLevelDomain) : AbstractDomain.Type
        with type a_expr = Term.t
        and type b_expr = Cond.t
        and type var = Var.t
        = struct

        type var = Var.t
        type a_expr = Term.t
        type b_expr = Cond.t
        include D

        let var_to_string = Var.to_string' "x"
        let a_expr_to_string = Term.to_string_c
        let b_expr_to_string = Cond.to_string_c

        let get_upper_bound a b = getUpperBound b a
        let get_lower_bound a b = getLowerBound b a
        let itvize a b = itvize b a

        let size p =
            let rep = match backend_rep p with
                | Some (p',_) -> p'
                | _ -> Stdlib.failwith "size"
            in
            Pol.size rep

        module FactoryUnit = FactoryMaker.Make(FactoryMaker.Unit)

        let split_in_half p =
            let (rep, toVar) = match backend_rep p with
                | Some (p',(ofVar, toVar)) ->
                    let (_,_,toVar') = PedraQOracles.export_backend_rep (p',(ofVar,toVar)) in
                    (p', toVar')
                | _ -> Stdlib.failwith "split_in_half"
            in
            let rep_unit = FactoryUnit.convert rep in
            match Pol.split_in_half FactoryUnit.factory rep_unit with
            | None -> Stdlib.failwith "split_in_half: unbounded polyhedron"
            | Some cstr ->
                let cond1 = [Pol.Cs.rename_f toVar cstr]
                    |> Cond.of_cstrs
                and cond2 = [Pol.Cs.rename_f toVar (Pol.Cs.compl cstr)]
                    |> Cond.of_cstrs
                in
                let p1 = assume cond1 p
                and p2 = assume cond2 p in
                [p1 ; p2]

        let diff p1 p2 =
             let compl : Pol.Cs.t -> Pol.Cs.t
                = fun cstr ->
                (* Complement that keeps the same comparison sign *)
                {(Cs.compl cstr) with Cs.typ = Cs.get_typ cstr}
            in
            let (_, rep2, toVar2) = match backend_rep p1, backend_rep p2 with
                | Some (p1',_), Some (p2', (ofVar2, toVar2)) ->
                    let (_,_,toVar2') = PedraQOracles.export_backend_rep (p2',(ofVar2,toVar2))
                    in
                    (p1',p2', toVar2')
                | _, _ -> Stdlib.failwith "diff"
            in
            let p2_ineqs = Pol.get_ineqs rep2
                |> List.map (fun (cstr,_) -> Pol.Cs.rename_f toVar2 cstr)
            in
            if p2_ineqs = []
            then [p1]
            else
                let hd = List.hd p2_ineqs in
                let fst_res = assume (Cond.of_cstrs [compl hd]) p1
                and fst_cont = assume (Cond.of_cstrs [hd]) p1
                in
                List.fold_left
                    (fun (res,cont) ineq ->
                        let pol_cont = List.hd cont in
                        (assume (Cond.of_cstrs [compl ineq]) pol_cont :: res,
                         assume (Cond.of_cstrs [ineq]) pol_cont :: cont))
                    ([fst_res],[fst_cont]) (List.tl p2_ineqs)
                |> Stdlib.fst

        let get_cstrs : t -> Cstr.Rat.t list
    		= fun p ->
    		let (rep, toVar) = match backend_rep p with
                | Some (p',(ofVar, toVar)) ->
                    let (_,_,toVar') = PedraQOracles.export_backend_rep (p',(ofVar,toVar)) in
                    (p', toVar')
                | _ -> Stdlib.failwith "get_cond"
            in
            Pol.get_cstr rep
            |> List.map (fun cstr -> Pol.Cs.rename_f toVar cstr)

        let get_b_expr : t -> b_expr
            = fun p ->
            let (rep, toVar) = match backend_rep p with
                | Some (p',(ofVar, toVar)) ->
                    let (_,_,toVar') = PedraQOracles.export_backend_rep (p',(ofVar,toVar)) in
                    (p', toVar')
                | _ -> Stdlib.failwith "get_cond"
            in
            Pol.get_cstr rep
            |> List.map (fun cstr -> Pol.Cs.rename_f toVar cstr)
            |> Cond.of_cstrs

        let get_vars : t -> var list
            = fun p ->
            let (rep, toVar) = match backend_rep p with
                | Some (p',(ofVar,toVar)) ->
                    let (_,_,toVar') = PedraQOracles.export_backend_rep (p',(ofVar,toVar)) in
                    (p', toVar')
                | _ -> Stdlib.failwith "get_vars"
            in
            Pol.varSet rep
            |> Var.Set.elements

        let spawn : t -> Vec.t
            = fun p ->
            let (rep, ofVar) = match backend_rep p with
                | Some (p',(ofVar,toVar)) ->
                    let (_,ofVar',_) = PedraQOracles.export_backend_rep (p',(ofVar,toVar)) in
                    (p', ofVar')
                | _ -> Stdlib.failwith "get_vars"
            in
            FactoryUnit.convert rep
            |> Pol.spawn FactoryUnit.factory
            |> Vec.rename_f ofVar

        let satisfy : Vec.t -> t -> bool
            = fun point p ->
            let (rep, toVar) = match backend_rep p with
                | Some (p',(ofVar,toVar)) ->
                    let (_,_,toVar') = PedraQOracles.export_backend_rep (p',(ofVar,toVar)) in
                    (p', toVar')
                | _ -> Stdlib.failwith "get_vars"
            in
            Vec.rename_f toVar point
            |> Pol.satisfy rep
    end
end
