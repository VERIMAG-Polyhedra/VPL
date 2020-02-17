module Debug = DebugTypes.Debug(struct let name = "Horacle" end)

module CP = CstrPoly
module Poly = CP.Poly

(* cIndex (1, 0, 3) représente C_1^1 * C_2^0 * C_3^3 *)
type cIndex = Index.Int.t
(* varIndex (1, 0, 3) représente x_1^1 * x_2^0 * x_3^3 *)
type varIndex = Index.Int.t
(* boundIndex (1, 0, 3) représente 1xC_1 + 0xC_2 + 3xC_3 *)
type boundIndex = Index.Rat.t

type t =
| Ci of cIndex
| VarBounds of varIndex * (boundIndex list)
| VarCi of varIndex * cIndex

let to_string : t -> string
	= function
	| Ci id -> Printf.sprintf "Ci(%s)" (Index.Int.to_string id)
	| VarBounds (id_v,id_c) -> Printf.sprintf "VB(%s, %s)"
        (Index.Int.to_string id_v)
        (Misc.list_to_string Index.Rat.to_string id_c " ; ")
	| VarCi (id_v,id_c) -> Printf.sprintf "VC(%s,%s)"
        (Index.Int.to_string id_v)
        (Index.Int.to_string id_c)

let eq : t -> t -> bool
	= fun h1 h2 ->
	match (h1,h2) with
	| (Ci i1, Ci i2) -> Index.Int.equal i1 i2
	| (VarBounds (i1,j1), VarBounds (i2,j2)) ->
        Index.Int.equal i1 i2
		&& List.length j1 = List.length j2
		&& (List.for_all2 (fun k1 k2 -> Index.Rat.equal k1 k2) j1 j2)
	| (VarCi (i1,j1), VarCi (i2,j2)) ->
        Index.Int.equal i1 i2 && Index.Int.equal j1 j2
	| (_,_) -> false

let is_linear : t -> bool
	= function
	| Ci id -> Index.Int.is_unitary id
	| VarBounds(vI,bIs) ->
		((Index.Int.is_null vI) && (List.length bIs = 1))
	  ||
		((Index.Int.is_unitary vI) && (List.length bIs = 0))
	| VarCi (vI,cI) ->
		((Index.Int.is_null vI) && (Index.Int.is_unitary cI))
	  ||
		((Index.Int.is_unitary vI) && (Index.Int.is_null cI))

let computeVarIndex : Index.Int.t -> Var.t list -> Poly.t
    = fun id vl ->
    Poly.mk_list [List.combine vl id, Q.of_int 1]

let computeBoundIndex : Index.Rat.t -> Poly.t list -> Poly.t
    = fun id polyhedron ->
    List.fold_left2 (fun p ci q ->
        Poly.add p (Poly.mulc ci q)
    ) Poly.z polyhedron (Index.Rat.data id)

let computeBoundIndexList : Index.Rat.t list -> Poly.t list -> Poly.t
    = fun il polyhedron ->
    List.map (fun i ->
        computeBoundIndex i polyhedron
    ) il
    |> Poly.prod

module Cert = struct
	module Poly = CstrPoly.Poly
	module V = Var

	type squares = (V.t * int) list

    let squares_to_string : squares -> string
        = fun sq ->
        Misc.list_to_string
            (fun (v,i) -> Printf.sprintf "%s^%i" (V.to_string v) i)
            sq "."

	type schweighofer = Scalar.Rat.t * (cIndex * squares * boundIndex list)

    let schweighofer_to_string : schweighofer -> string
        = fun (coeff, (cI, sq, bIs)) ->
        Printf.sprintf "%s.(%s * %s * %s)"
            (Scalar.Rat.to_string coeff)
            (Index.Int.to_string cI)
            (squares_to_string sq)
            (Misc.list_to_string Index.Rat.to_string bIs "*")

    let to_string : schweighofer list -> string
        = fun ss ->
        Misc.list_to_string schweighofer_to_string ss " ; "

	let varIndex_to_squares : V.t list -> varIndex -> squares
		= fun vars id ->
		Index.Int.data id
		|> List.map (fun i -> i/2) (* divided by 2 because of the Coq type *)
		|> List.combine vars
		|> List.filter
			(fun (_,exp) -> exp > 0)

	(** [hi_to_cert n_cstrs vars coeff hi] *)
	let hi_to_cert : int -> V.t list -> Scalar.Rat.t -> t -> schweighofer
		= fun n_cstrs vars coeff -> function
		| Ci cId -> (coeff, (cId, [], []))
		| VarBounds (varId, bIs) -> (coeff, (Index.Int.init n_cstrs, varIndex_to_squares vars varId, bIs))
		| VarCi (varId , cId) -> (coeff, (cId, varIndex_to_squares vars varId, []))

end
