module Cs = Cstr.Rat
module Vec = Cs.Vec

let build : 'c Factory.t -> Vec.t -> 'c Cons.t list -> 'c PSplx.t
	= fun factory normalization_point cstrs ->
	let conss = cstrs @ [PSplxBuild.trivial_constraint factory] in
	let obj_cstrs = List.mapi (fun i _ -> i) conss in
	let n_rows = 1 in
	PSplxBuild.init conss n_rows obj_cstrs normalization_point

type 'c regionsT = {
	mapping : ('c PLP.Region.t * 'c Cons.t) list;
	interior_point : Vec.t ;
}

let regions_to_string' : ('c PLP.Region.t * 'c Cons.t) list -> string
	= fun regs ->
	Printf.sprintf "\tRegions : \n%s"
		(Misc.list_to_string
			(fun (reg,cons) -> Printf.sprintf "%s\n%s\n"
				(PLP.Region.to_string reg)
				(Cons.to_string Var.to_string cons))
			regs "\n")

let regions_to_string : 'c regionsT -> string
	= fun regs ->
	regions_to_string' regs.mapping

let to_plp : 'c Factory.t -> Vec.t -> 'c Cons.t list -> 'c regionsT
	= fun factory normalization_point conss ->
	let sx = build factory normalization_point conss in
	match PLP.run_classic factory sx with
	| None -> Stdlib.failwith "PoltoPLP.to_plp"
	| Some regs -> {
		mapping = regs;
		interior_point = normalization_point;
	}

(*
module ReNormalize = struct

	(** Computes the denominantor of lambda from the new point (homogenized) and the polyhedron face. *)
	let compute_denominator : Vec.t -> Vec.t -> Vec.Coeff.t
		= fun polyhedron_face new_point ->
		Vec.dot_product polyhedron_face new_point
		|> Vec.Coeff.neg

	let compute_numerator : Vec.t -> Vec.t -> Vec.Coeff.t
		= fun vec_to_normalize new_point ->
		Vec.dot_product vec_to_normalize new_point

	let homogenize_point : Vec.t -> Var.t -> Vec.t
		= fun point additional_var ->
		Vec.set point additional_var Vec.Coeff.u

	let homogenize_cstr : Var.t -> Cs.t -> Vec.t
		= fun additional_var cstr ->
		[cstr.Cs.c , additional_var]
			|> Vec.mk
			|> Vec.sub cstr.Cs.v

	let remove_additional_var : Var.t -> Vec.t -> Vec.t
		= fun additional_var point ->
		Vec.set point additional_var Vec.Coeff.z
		(*
		Vec.get point additional_var
		|> Vec.divr point
		|> fun point -> Vec.set point additional_var Vec.Coeff.z
		*)
		(* TODO faut il diviser tous les coefficients par le coefficient de la variable additionnelle? *)

	let renormalize_vec : Vec.Coeff.t -> Var.t -> Vec.t -> Vec.t -> Vec.t -> Vec.t
		= fun denominator _ new_point polyhedron_face vec ->
		let lambda = Vec.Coeff.div (compute_numerator vec new_point) denominator in
		Vec.add vec (Vec.mulc lambda polyhedron_face)

	let renormalize_boundary : Vec.Coeff.t -> Var.t -> Vec.t -> Vec.t -> PLP.Boundary.t -> PLP.Boundary.t
		= fun denominator additional_var new_point polyhedron_face (cstr,point_other_side) ->
		let renormalize_vec = renormalize_vec denominator additional_var new_point polyhedron_face in
		let vec' = homogenize_cstr additional_var cstr
			|> renormalize_vec
		and point_other_side' = homogenize_point point_other_side additional_var
			|> renormalize_vec
			|> remove_additional_var additional_var
		in
		let cstr' = Cs.mk2 (Cs.get_typ cstr)
			(Vec.set vec' additional_var Vec.Coeff.z)
			(Vec.get vec' additional_var |> Vec.Coeff.neg) in
		(cstr',point_other_side')

	let renormalize_region : Var.t -> Vec.t -> (PLP.Region.t * 'c Cons.t) -> (PLP.Region.t * 'c Cons.t)
		= fun additional_var new_point (reg,cons) ->
		let polyhedron_face = Cons.get_c cons
			|> homogenize_cstr additional_var in
		let new_point = homogenize_point new_point additional_var in
		let denominator = compute_denominator polyhedron_face new_point in
		let renormalize_boundary = renormalize_boundary denominator additional_var new_point polyhedron_face in
		let boundaries = List.map
			(fun (boundary,i) -> renormalize_boundary boundary, i)
			reg.PLP.Region.r
		in
		let point' = homogenize_point reg.PLP.Region.point additional_var
			|> renormalize_vec denominator additional_var new_point polyhedron_face
			|> remove_additional_var additional_var
		in
		({reg with PLP.Region.r = boundaries ; PLP.Region.point = point'},
		 cons)

	let renormalize : 'c regionsT -> Vec.t -> 'c regionsT
		= fun regs new_point ->
		let additional_var = List.map (fun (_,cons) -> Cons.get_c cons) regs.mapping
			|> Cs.getVars
			|> Var.horizon
		in
		let mapping' = List.map (renormalize_region additional_var new_point) regs.mapping
		in
		{mapping = mapping' ; interior_point = new_point}

end
*)
