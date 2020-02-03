module Cs = Cstr.Rat

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
    | PivotOn i -> "PivotOn " ^ string_of_int i

type t = {
    lin : Cs.t list;
	cst : Cs.t;
}

let empty : t = {
    lin = [];
	cst = Cs.top
}

let pr : (Var.t -> string) -> t -> string
    = fun f o ->
    (List.map (Cs.to_string f) o.lin |> String.concat " ; ")
    ^ " = "
    ^ Cs.to_string f o.cst

let to_string : t -> string
    = pr Var.to_string

let param_coeff_to_string : (Var.t -> string) -> Cs.t -> string
    = fun f cstr ->
    if Cs.Coeff.isZ cstr.c
    then Cs.Vec.to_string f cstr.v
    else
        if Cs.Coeff.lt cstr.c Cs.Coeff.z
        then Printf.sprintf "%s + %s"
            (Cs.Vec.to_string f cstr.v)
            (Cs.Coeff.neg cstr.c |> Cs.Coeff.to_string)
        else Printf.sprintf "%s %s"
            (Cs.Vec.to_string f cstr.v)
            (Cs.Coeff.neg cstr.c |> Cs.Coeff.to_string)

let getColumnWidth : (Var.t -> string) -> t -> Cs.t -> int
    = fun f _ c -> param_coeff_to_string f c |> String.length

let getColumnsWidth : (Var.t -> string) -> t -> int list
    = fun f o ->
    List.map (getColumnWidth f o) (o.lin @ [o.cst])

let pretty_print : (Var.t -> string) -> t -> int list -> string
    = let pr1 : (Var.t -> string) -> t -> Cs.t -> int -> string
	   = fun f o p i ->
	   let nb_spaces = i - getColumnWidth f o p in
	   [String.init (nb_spaces/2) (fun _ -> ' ');
	    param_coeff_to_string f p;
	    String.init (nb_spaces/2 + (nb_spaces mod 2)) (fun _ -> ' ')]
	   |> String.concat ""
	 in
	 fun f o l ->
	 List.map2 (pr1 f o) (o.lin @ [o.cst]) l
	 |> String.concat " | "

let get : int -> t -> Cs.t
    = fun i o ->
    if 0 <= i && i < List.length o.lin
    then List.nth o.lin i
    else Stdlib.invalid_arg "Tableau.get"

(* The constant part of the objective is the opposite of the objective value. *)
let value : t -> Cs.t
    = fun o -> o.cst |> Cs.mulc_no_exc Scalar.Rat.negU

let nVars : t -> int
    = fun o -> List.length o.lin

let mk : Cs.t list -> Cs.t -> t
    = fun l b -> {
        lin = l;
        cst = b;
    }

let elim : Tableau.t -> int -> int -> t -> t
    = fun tab i_row i_col obj  ->
    if not (Scalar.Rat.equal Scalar.Rat.u tab.(i_row).(i_col))
    then Stdlib.invalid_arg "elim"
    else
        let coeff = Cs.mulc_no_exc Scalar.Rat.negU (List.nth obj.lin i_col) in
        let lin  = List.mapi (fun i_col' pcoeff ->
            Cs.mulc_no_exc tab.(i_row).(i_col') coeff
            |> Cs.add pcoeff
        ) obj.lin
        and cst = Cs.mulc_no_exc tab.(i_row).((Tableau.nCols tab) -1) coeff
        |> Cs.add obj.cst
        in {
            lin = lin;
            cst = cst;
        }

let foldi : (int -> Cs.t -> 'a -> 'a) -> t -> 'a -> 'a
    = fun f o a ->
    List.fold_left (fun (i, a') c ->
        (i + 1, f i c a')
    ) (0, a) o.lin
    |> Stdlib.snd

let add_col : t -> Cs.t -> int -> t
    = fun o c i ->
    if 0 <= i && i <= nVars o
    then {o with
        lin = Misc.sublist o.lin 0 i @ c :: Misc.sublist o.lin i (nVars o)
    }
    else Stdlib.invalid_arg "add_col"

let getPivotCol : Cs.Vec.t -> t -> choiceT
	= fun point obj ->
    let res = Misc.fold_left_i (fun i_col res cstr ->
        match res with
        | Some _ -> res
        | None ->
            (*  Strict inequalities can cause infinite looping in the simplex.
                A parametric coefficient x < 0 is considered negative on 0.
            *)
            if Cs.satisfy point {cstr with Cs.typ = Cstr_type.Le}
            then None
            else Some (PivotOn i_col)
    ) None obj.lin
    in
    match res with
	| None -> OptReached
	| Some ch -> ch
