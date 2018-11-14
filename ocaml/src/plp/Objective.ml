module Cs = ParamCoeff.Cs

type choiceT
  = OptReached
  | PivotOn of int

let choice_equal : choiceT -> choiceT -> bool
    = fun ch ch' ->
    match ch, ch' with
    | OptReached, OptReached -> true
    | PivotOn i, PivotOn j -> i = j
    | _, _ -> false

let choice_to_string : choiceT -> string
    = function
    | OptReached -> "OptReached"
    | PivotOn i -> "PivotOn " ^ Pervasives.string_of_int i

type pivotStrgyT = Bland

let pivotStrgy_to_string : pivotStrgyT -> string
    = function
    | Bland -> "Bland"

type t = {
    lin : ParamCoeff.t list;
	cst : ParamCoeff.t;
}

let empty : t = {
    lin = [];
	cst = ParamCoeff.empty
}

let pr : (int -> string) -> t -> string
    = fun f o ->
    (List.map (ParamCoeff.pr f) o.lin |> String.concat " ; ")
    ^ " = "
    ^ ParamCoeff.pr f o.cst

let to_string : t -> string
    = pr ParamCoeff.paramDfltPr

let prWithVars : (int -> string) -> (int -> string) -> t -> string
    = fun fp fv o ->
    List.mapi (fun i p ->
        Printf.sprintf "(%s).%s" (fv i) (ParamCoeff.pr fp p)
    ) o.lin
    |> String.concat ", "

let getColumnWidth : (int -> string) -> t -> ParamCoeff.t -> int
    = fun f _ c -> ParamCoeff.pr f c |> String.length

let getColumnsWidth : (int -> string) -> t -> int list
    = fun f o ->
    List.map (getColumnWidth f o) (o.lin @ [o.cst])

let pretty_print : (int -> string) -> t -> int list -> string
    = let pr1 : (int -> string) -> t -> ParamCoeff.t -> int -> string
	   = fun f o p i ->
	   let nb_spaces = i - getColumnWidth f o p in
	   [String.init (nb_spaces/2) (fun _ -> ' ');
	    ParamCoeff.pr f p;
	    String.init (if nb_spaces mod 2 = 1 then nb_spaces/2 + 1 else nb_spaces/2) (fun _ -> ' ')]
	   |> String.concat ""
	 in
	 fun f o l ->
	 List.map2 (pr1 f o) (o.lin @ [o.cst]) l
	 |> String.concat " | "

let get : int -> t -> ParamCoeff.t
    = fun i o ->
    if 0 <= i && i < List.length o.lin
    then List.nth o.lin i
    else Pervasives.invalid_arg "Tableau.get"

let value : t -> ParamCoeff.t
    = fun o -> o.cst

let nVars : t -> int
    = fun o -> List.length o.lin

let nParams : t -> int
    = fun o -> ParamCoeff.nParams o.cst

let equal : t -> t -> bool
    = fun o o' ->
    List.length o.lin = List.length o'.lin
    && List.for_all2 ParamCoeff.equal o.lin o'.lin
    && ParamCoeff.equal o.cst o'.cst

let mk : ParamCoeff.t list -> ParamCoeff.t -> t
    = fun l b ->
    let n = ParamCoeff.nParams b in
    if not (List.for_all (fun c -> ParamCoeff.nParams c = n) l)
    then Pervasives.invalid_arg "Objective.mk"
    else {
        lin = l;
        cst = b;
    }

let mkSparse : int -> (int * ParamCoeff.t) list -> ParamCoeff.t -> t
  = let rec fill : int -> int -> int -> (int * ParamCoeff.t) list -> ParamCoeff.t list
	   = fun np n i ->
	   function
	   | [] -> if i < n then ParamCoeff.mkSparse np [] Scalar.Rat.z :: fill np n (i + 1) [] else []
	   | ((x, a) :: l') as l ->
	 if n <= i || x < i then Pervasives.invalid_arg "mkSparse"
	 else if x = i then a :: fill np n (i + 1) l'
	 else ParamCoeff.mkSparse np [] Scalar.Rat.z :: fill np n (i + 1) l
	 in
	 fun n l a ->
	 let np = ParamCoeff.nParams a in
	 mk (List.sort (fun (i, _) (i', _) -> Pervasives.compare i i') l |> fill np n 0) a

let elim : t -> Tableau.Vector.t -> int -> t
    = fun o v col ->
    if Q.sign (Tableau.Vector.get col v) = 0
    then Pervasives.invalid_arg "elim"
    else
        let coeff = ParamCoeff.mul Scalar.Rat.negU (List.nth o.lin col) in
        let normV = Tableau.Vector.mul_scalar v (Tableau.Vector.get col v|> Q.inv) in
        let cst = Tableau.Vector.last normV in
        let lin = Misc.sublist normV 0 (Tableau.Vector.size normV - 1)
        in {
            lin = List.map (fun x -> ParamCoeff.mul x coeff) lin
                |> List.map2 ParamCoeff.add o.lin;
            cst = ParamCoeff.add o.cst (ParamCoeff.mul cst coeff);
        }

let foldi : (int -> ParamCoeff.t -> 'a -> 'a) -> t -> 'a -> 'a
    = fun f o a ->
    List.fold_left (fun (i, a') c ->
        (i + 1, f i c a')
    ) (0, a) o.lin
    |> Pervasives.snd

let add_col : t -> ParamCoeff.t -> int -> t
    = fun o c i ->
    if 0 <= i && i <= nVars o && ParamCoeff.nParams c = nParams o
    then {o with
        lin = Misc.sublist o.lin 0 i @ c :: Misc.sublist o.lin i (nVars o)
    }
    else Pervasives.invalid_arg "add_col"

let rm_col : t -> int -> t
    = fun o i ->
    if 0 <= i && i < nVars o
    then {o with lin = Misc.sublist o.lin 0 i @ Misc.sublist o.lin (i + 1) (nVars o)}
    else Pervasives.invalid_arg "rm_col"

module type PivotType = sig

    (** The type of vectors used to instantiate the simplex tableau. *)
    module Vec : Vector.Type with module V = Var.Positive and module M = Rtree

    module Naming : Naming.Type with module Vec = Vec

    (** [getPivotCol f h s cx o] returns what the next step is according to
    context [cx] and pivoting strategy [s]. Function [f] defines the correspondence
    between parameter indices and VPL variables. [h] must be greater than any
    [Vec.V.t] returned by [f] and is used to generated internal variables. *)
    val getPivotCol : (int -> Vec.V.t) -> Vec.V.t -> pivotStrgyT -> Naming.t -> Vec.t -> t -> choiceT
end

module Pivot (Vec : Vector.Type with module V = Var.Positive and module M = Rtree) = struct
	module Vec = Vec
	module Coeff = Vec.Coeff
	module Naming = Naming.Naming(Vec)

	let point_to_fun : Naming.t -> Vec.t -> int -> Coeff.t
		= fun names p i ->
		try
			let i = Naming.to_vpl names i in
			let (_,nb) = List.find
				(fun (v,_) -> Vec.V.cmp v i = 0)
				(Vec.toList p) in (* XXX toList? *)
			nb
		with Not_found -> Coeff.z

	let sat : ParamCoeff.t -> (int -> Coeff.t) -> Coeff.t
		=	fun c f ->
		List.fold_left (fun (i, v) c -> (i + 1, Coeff.add v (Coeff.mul (Coeff.ofQ c) (f i))))
			(0, c.ParamCoeff.cst |> Scalar.Rat.toQ |> Coeff.ofQ) c.ParamCoeff.lin
		|> Pervasives.snd

	type signDecT = StrictPos | StrictNeg

	let decide_sign : (int -> Vec.V.t) -> Vec.V.t -> Naming.t -> Vec.t -> ParamCoeff.t -> signDecT
		= fun _ _ names point c ->
		let q = sat c (point_to_fun names point) in
		if Coeff.cmp q Coeff.z < 0
		then StrictNeg
		else StrictPos

	let getCol_Bland : (int -> Vec.V.t) -> Vec.V.t -> Naming.t -> Vec.t -> t -> choiceT
		= let f = fun tr h names _ point i c ->
			function None ->
				(if ParamCoeff.is_constant c
					then if Q.sign (ParamCoeff.getCst c) < 0 then Some (PivotOn i) else None
					else match decide_sign tr h names point c with
						| StrictNeg -> Some (PivotOn i)
						| StrictPos -> None)
				| Some _ as r -> r
			in
			fun tr h names point o ->
			match foldi (f tr h names o point) o None with
			| None -> OptReached
			| Some ch -> ch

	let getPivotCol : (int -> Vec.V.t) -> Vec.V.t -> pivotStrgyT -> Naming.t -> Vec.t -> t -> choiceT
		  	= fun f h ->
		  	function
		  	| Bland -> getCol_Bland f h
end
