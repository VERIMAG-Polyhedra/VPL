module Cs = Cstr.Rat

module Debug = DebugTypes.Debug(struct let name = "PSplx" end)

type decision_variable = int
type var_set = int

module VarMap = Map.Make (struct type t = decision_variable let compare = Stdlib.compare end)

type pivotT = Cs.t * int * Tableau.t -> Cs.t

type 'c t = {
    mutable obj : Objective.t; (** Objective function *)
    mutable tab : Tableau.t; (** Constraint matrix *)
    mutable get_set : var_set VarMap.t; (** map associating decision variables to their set *)
    mutable basis : int list;
    mutable sets : var_set list; (** Associates a var_set to each row *)
    cstrs : 'c Cons.t list; (** The constraint associated to each variable. *)
    mutable new_col : (Cs.t -> Scalar.Rat.t) array;
    mutable pivots : pivotT list;
}

let empty : 'c t = {
    obj = Objective.empty;
    tab = Tableau.empty;
    get_set = VarMap.empty;
    basis = [];
    sets = [];
    cstrs = [];
    new_col = [||];
    pivots = [];
}

let copy : 'c t -> 'c t
    = fun sx -> {
        obj = sx.obj;
        get_set = sx.get_set;
        basis = sx.basis;
        sets = sx.sets;
        tab = Tableau.copy sx.tab;
        cstrs = sx.cstrs;
        new_col = Array.copy sx.new_col;
        pivots = sx.pivots;
    }

(** Copy sx in sx' (do not copy the cstrs field) *)
let copy_in : 'c t -> 'c t -> unit
    = fun sx sx' ->
    sx'.obj <- sx.obj;
    sx'.get_set <- sx.get_set;
    sx'.basis <- sx.basis;
    sx'.sets <- sx.sets;
    sx'.tab <- Tableau.copy sx.tab;
    sx'.new_col <- Array.copy sx.new_col;
    sx'.pivots <- sx.pivots

let nVars : 'c t -> int
    = fun sx ->
    (Tableau.nCols sx.tab) - 1

let nParams : 'c t -> int
    = fun sx ->
    List.length sx.cstrs

let getParams : 'c t -> Var.Set.t
    = fun sx ->
    List.map Cons.get_c sx.cstrs
    |> Cs.getVars

let var_to_string : int -> string
    = fun i ->
    if i <= -1
    then "?"
    else (*"λ"*) "p" ^ (string_of_int i)

let nRows : 'c t -> int
    = fun sx ->
    Tableau.nRows sx.tab

let objValue : 'c t -> Cs.t
    = fun sx ->
    Objective.value sx.obj

let constant_index sx = (Tableau.nCols sx.tab) - 1

let getCurVal : 'c t -> (decision_variable * Q.t) list
    = fun sx ->
    let i_col = constant_index sx in
    List.mapi (fun i_row var ->
        (var, sx.tab.(i_row).(i_col))
    ) sx.basis

let objValueCert : 'c Factory.t -> 'c t -> 'c Cons.t
	= fun factory sx ->
	let cert = List.fold_left (fun res (i_col,q) ->
		if Q.equal q Q.zero
		then res
		else
            let (_,cert) = List.nth sx.cstrs i_col in
            Factory.add_mul factory res cert q
    ) factory.top (getCurVal sx)
    in
    (Cs.canon (objValue sx), cert)

let isFeasible : 'c t -> bool
    = fun sx ->
    let i_col = constant_index sx in
    Array.for_all (fun row ->
        Q.geq row.(i_col) Q.zero
    ) sx.tab

(* PRETTY PRINTERS *)
let t_get_width_column_vector : 'c t -> int list
    = fun sx ->
    let l = List.map (fun (cstr,_) ->
        Cs.to_string Var.to_string cstr
        |> String.length
    ) sx.cstrs
    @ [0]
    |> List.map2 Stdlib.max
        (Tableau.get_width_column_vector sx.tab)
    |>  List.map2 Stdlib.max
        (Objective.getColumnsWidth Var.to_string sx.obj)
    in
    List.mapi (fun i n ->
        let n' = var_to_string i |> String.length in
        Stdlib.max n n'
    ) (Misc.sublist l 0 (List.length l - 1))
    @ [List.nth l (List.length l - 1)]

let to_string : 'c t -> string
    = fun sx0 ->
    let sx =
        let l = (nRows sx0) - (sx0.basis |> List.length) in
        if l < 0 then Stdlib.failwith "PSplx.to_string"
        else
            let rec gen : int -> int list
                = fun i -> if i = 0 then [] else -1 :: gen (i - 1)
            in
            {sx0 with basis = sx0.basis @ gen l}
    in
    let width_columns = t_get_width_column_vector sx in
    let sub_width_columns = Misc.sublist width_columns 0 ((List.length width_columns) - 1) in
    let var_names = List.mapi (fun i width_col ->
        let s = var_to_string i in
        let nb_spaces = width_col - (String.length s) in
        [Misc.string_repeat " " (nb_spaces/2) ; s ; Misc.string_repeat " " (nb_spaces/2 + nb_spaces mod 2) ; " | "]
        |> String.concat ""
    ) sub_width_columns
    |> String.concat ""
    and var_set = List.mapi (fun i width_col ->
        let s = try VarMap.find i sx.get_set |> string_of_int
            with Not_found -> "?" in
        let nb_spaces = width_col - (String.length s) in
        [Misc.string_repeat " " (nb_spaces/2) ; s ; Misc.string_repeat " " (nb_spaces/2 + nb_spaces mod 2) ; " | "]
        |> String.concat ""
    ) sub_width_columns
    |> String.concat ""
    and obj = Objective.pretty_print Var.to_string sx.obj width_columns
    and sep = List.map (fun i ->
        String.concat "" [Misc.string_repeat "-" i ; " | "]
    ) width_columns
    |> String.concat ""
    and rows = List.mapi (fun i_row var ->
        Printf.sprintf "%s%s (%s)\n"
            (Tableau.pretty_print_row i_row sx.tab width_columns) (var_to_string var)
            (try List.nth sx.sets i_row |> string_of_int
                with _ -> "?")
    ) sx.basis
    |> String.concat ""
    and cstrs = List.mapi (fun i (cstr,_) ->
        let s = Cs.to_string Var.to_string cstr in
        let nb_spaces = (List.nth width_columns i) - (String.length s) in
        Printf.sprintf "%s%s%s"
            (Misc.string_repeat " " (nb_spaces/2))
            s
            (Misc.string_repeat " " (nb_spaces/2 + (nb_spaces mod 2)))
    ) sx.cstrs
    |> String.concat " | "
    in
    Printf.sprintf "\n%s\n%s\n%s\n%s\n%s\n%s\n"
        var_names cstrs var_set obj sep rows

let print : 'c t -> unit
    = fun sx ->
    to_string sx
    |> Stdlib.print_endline

let remCol : int -> 'c t -> 'c t
    = fun i_col sx -> {sx with
        tab = Tableau.remCol i_col sx.tab;
        obj = {sx.obj with
            Objective.lin = Misc.popi sx.obj.Objective.lin i_col;
        };
        cstrs = Misc.popi sx.cstrs i_col;
    }
