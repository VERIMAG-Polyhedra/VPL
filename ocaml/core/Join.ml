module Cs = Cstr.Rat
module Debug = DebugTypes.Debug(struct let name = "Join" end)

let filter_trivial : ((('c1,'c2) Cons.discr_t) PLP.Region.t * (('c1,'c2) Cons.discr_t) Cons.t) list option -> ((('c1,'c2) Cons.discr_t) PLP.Region.t * (('c1,'c2) Cons.discr_t) Cons.t) list option
	= function
	| None -> None
	| Some sols -> Some (List.filter
		(fun (_,cons) ->
			Cs.tellProp (Cons.get_c cons) <> Cs.Trivial
			&& not(Cs.Vec.equal (Cons.get_c cons |> Cs.get_v) Cs.Vec.nil))
		sols)

let rem_dupl : ((('c1,'c2) Cons.discr_t) PLP.Region.t * (('c1,'c2) Cons.discr_t) Cons.t) list option -> ((('c1,'c2) Cons.discr_t) PLP.Region.t * (('c1,'c2) Cons.discr_t) Cons.t) list option
	= function
	| None -> None
	| Some sols -> Some (Misc.rem_dupl
		(fun (_,cons1) (_,cons2) -> Cs.equal (Cons.get_c cons1) (Cons.get_c cons2))
		sols)

let get_join_cert : ((('c1,'c2) Cons.discr_t) PLP.Region.t * (('c1,'c2) Cons.discr_t) Cons.t) list option -> 'c1 Cons.t list * 'c2 Cons.t list
    = function
    | None -> ([],[])
    | Some regions ->
        List.fold_left (fun (l1,l2) (_, (cstr,(cert1,cert2))) ->
            (cstr, cert1) :: l1, (cstr, cert2) :: l2
        ) ([],[]) regions

let mk_constraints_p1 : 'c1 Factory.t -> 'c2 Factory.t -> 'c1 Cons.t list -> (('c1,'c2) Cons.discr_t) Cons.t list
    = fun factory1 factory2 conss ->
    let top2 = factory2.top in
    let (triv_cstr, triv_cert) = Cons.mkTriv factory1 Cstr_type.Le Scalar.Rat.u in
    List.map (fun (cstr, cert) ->
        (cstr, (cert, top2))
    ) conss
    @
    [(triv_cstr, (triv_cert, top2))]

let mk_constraints_p2 : 'c1 Factory.t -> 'c2 Factory.t -> 'c2 Cons.t list -> (('c1,'c2) Cons.discr_t) Cons.t list
    = fun factory1 factory2 conss ->
    let top1 = factory1.top in
    let (triv_cstr, triv_cert) = Cons.mkTriv factory2 Cstr_type.Le Scalar.Rat.u in
    List.map (fun (cstr, cert) ->
        (cstr, (top1, cert))
    ) conss
    @
    [(triv_cstr, (top1, triv_cert))]

let join' factory1 factory2 epsilon_opt normalization_point p1 p2 =
    let p1' = mk_constraints_p1 factory1 factory2 p1
    and p2' = mk_constraints_p2 factory1 factory2 p2 in
    let len_p1' = List.length p1' in
    let i_cstrs1 = List.mapi (fun i _ -> i) p1'
    and i_cstrs2 = List.mapi (fun i _ -> i + len_p1') p2' in
    (* Only constraints of p1 appear in the objective function. *)
    let conss = p1' @ p2' in
	let params = List.map Cons.get_c conss
        |> Cs.getVars
        |> Var.Set.elements in
	let params = match epsilon_opt with
		| Some epsilon -> Misc.pop Var.equal params epsilon
		| None -> params
	in
	let n_rows = (List.length params) + 2 in
	let sx = PSplxBuild.init conss n_rows i_cstrs1 normalization_point in
	List.iteri (fun i_row var ->
        PSplxBuild.var_equal (i_row + 1) var i_cstrs1 i_cstrs2 1 sx
    ) params;
    PSplxBuild.cste_equal (n_rows - 1) i_cstrs1 i_cstrs2 1 sx;
    Debug.log DebugTypes.Normal (lazy (Printf.sprintf "Initial simplex tableau: %s"
        (PSplx.to_string sx)));
    let factory_mix = Cons.discr_factory factory1 factory2 in
	let regs = PLP.run_classic factory_mix sx in
	filter_trivial regs
	|> rem_dupl
    |> get_join_cert

(** Returns the convex hull of the given inequalities (no equality should be given), and the next identifer. *)
let join : 'c1 Factory.t -> 'c2 Factory.t -> Var.t option -> Vector.Rat.t
    -> 'c1 Cons.t list -> 'c2 Cons.t list -> 'c1 Cons.t list * 'c2 Cons.t list
	= fun factory1 factory2 epsilon_opt normalization_point p1 p2 ->
	Debug.log DebugTypes.Title (lazy "Join");
	Debug.log DebugTypes.MInput
		(lazy (Printf.sprintf
            "First polyhedron : %s\nSecond Polyhedron : %s\nNormalization point : %s"
			(Misc.list_to_string (Cons.to_string Var.to_string) p1 "\n")
			(Misc.list_to_string (Cons.to_string Var.to_string) p2 "\n")
            (Vector.Rat.to_string Var.to_string normalization_point)));
	let (conss1, conss2) = join' factory1 factory2 epsilon_opt normalization_point p1 p2 in
	Debug.log DebugTypes.MOutput
		(lazy (Printf.sprintf "Polyhedron1 : %s\nPolyhedron2 : %s"
			(Misc.list_to_string (Cons.to_string_ext factory1 Var.to_string) conss1 "\n")
			(Misc.list_to_string (Cons.to_string_ext factory2 Var.to_string) conss2 "\n")));
	(conss1, conss2)
