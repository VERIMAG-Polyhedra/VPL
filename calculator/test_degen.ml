open Vpl;;
open PSplxBuild;;
module Cs = Cstr.Rat
module F = FactoryMaker.Make(FactoryMaker.Unit)

(** Polyhedron variables *)
let x = Var.fromInt 1
and y = Var.fromInt 2
and z = Var.fromInt 3
(** Slack variables *)
and s1 = Var.fromInt 4
and s2 = Var.fromInt 5
and s3 = Var.fromInt 6
and s4 = Var.fromInt 7
and s5 = Var.fromInt 8;;

let vars = [x;y;z;s1;s2;s3;s4;s5];;
let slacks = [s1;s2;s3;s4;s5];;

(** Parameters *)
let n1 = Var.fromInt 1
and n2 = Var.fromInt 2
and n3 = Var.fromInt 3;;

(* Objective constraints *)
let c = Cs.eq [] Scalar.Rat.z;;
let cstrs = [
    Cs.le [Scalar.Rat.u, n1] Scalar.Rat.z;
    Cs.le [Scalar.Rat.u, n2] Scalar.Rat.z;
    Cs.le [Scalar.Rat.u, n3] Scalar.Rat.z;
    c ;  c ; c ; c ; c
]
|> List.map F.mkCons;;

let n_rows = 5;;

let sx = Init.init_cstrs cstrs PSplx.empty;;
Init.init_matrix n_rows ((List.length cstrs + 1)) sx;;
Init.init_var_set sx;;
Init.mk_obj [0;1;2] sx;;

(** Constraints of the pyramid. *)
[
    Cs.eq [Scalar.Rat.negU, z] Scalar.Rat.u; (* z >= -1 -> - z <= 1 *)
    Cs.le [Scalar.Rat.u, z ; Scalar.Rat.of_float (-0.5), x] Scalar.Rat.u; (* z <= 1/2 x + 1 -> z - 1/2 x <= 1*)
    Cs.le [Scalar.Rat.u, z ; Scalar.Rat.of_float (-0.5), y] Scalar.Rat.u; (* z <= 1/2 y + 1 -> z - 1/2 y <= 1*)
    Cs.le [Scalar.Rat.u, z ; Scalar.Rat.of_float 0.5, x] Scalar.Rat.u; (* z <= -1/2x + 1*)
    Cs.le [Scalar.Rat.u, z ; Scalar.Rat.of_float 0.5, y] Scalar.Rat.u; (* z <= -1/2y + 1*)
]
|> List.map2 (fun s_var cstr -> {cstr with
    Cs.typ = Eq;
    Cs.v = Cs.Vec.set cstr.Cs.v  s_var Scalar.Rat.u;
}) slacks
|> List.iteri (fun i_row cstr ->
    Init.init_row (fun i_col _ ->
        Cs.Vec.get cstr.Cs.v (List.nth vars i_col)
    ) cstr.Cs.c i_row sx;
    sx.sets <- sx.sets @ [1]
);;


PSplx.to_string sx |> print_endline;;

Debugger.print_enable();;
Debugger.set_colors();;
PSplx.Debug.enable_all();;
PLP.Debug.enable_all();;
let res = PLP.run_classic F.factory sx;;
