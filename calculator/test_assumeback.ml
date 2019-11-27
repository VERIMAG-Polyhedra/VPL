open Vpl;;
open Calculator;;
open Notations;;

Flags.proj := Flags.Proj_PLP (Flags.Float);;

Debugger.enable();;
Debugger.print_enable();;
Debugger.set_colors();;
Proj.Debug.enable_all();;
PSplx.Debug.enable_all();;
PLP.Debug.enable_all();;

(*
(* Parsing a polyhedron: constraints are separated by ','. *)
let p1 = parse "x >= 0, x <= 1, y >= -10, x + y < 25, z > 2y , z <= 35";;
print p1;;

let p2 = parse "x >=1/4, x <= 1/2";;

(*let p3 = p1 |- "y,z";;
print p3;;*)

let p3 = match VPL.proj_incl p1 p2 with Some p -> p;;
print p3;;

let mk_poly s = PolyParser.one_poly PolyLexer.token (Lexing.from_string s);;

let ineq2 = UserInterface.Atom (mk_poly "x", LE, mk_poly "3/4");;

let p5 = VPL.assume_back ineq2 p3;;

let ineq = UserInterface.Atom (mk_poly "x + y", LE, mk_poly "3");;

let p4 = VPL.assume_back ineq p3;;
*)


(* Exemple papier : *)
let p1 = parse "x >= 0, x <= 2";;
let p2 = parse "x >= y, y >= 1";;
let p_join = p1 || (p2 |- "y");;

let point =

let get_res = function  | Some x -> x | None -> failwith "None";;

let p_join_1 = VPL.proj_incl p_join p1 |> get_res;;
let p_join_2 = VPL.proj_incl p_join p2 |> get_res;;

let mk_poly s = PolyParser.one_poly PolyLexer.token (Lexing.from_string s);;
(*
let ineq = UserInterface.Atom (mk_poly "x", LE, mk_poly "2");;
let p_join_1' = VPL.assume_back ineq p_join_1;;
(* -> OK *)
let p_join_2' = VPL.assume_back ineq p_join_2;;
(* -> infeasible *)
*)

let ineq' = UserInterface.Atom (mk_poly "x", GE, mk_poly "y");;
let p_join_1'' = VPL.assume_back ineq' p_join_1;;
(* -> OK *)
let p_join_2'' = VPL.assume_back ineq' p_join_2;;
(* -> OK *)

let ineq'' = UserInterface.Atom (mk_poly "y", GE, mk_poly "1");;
(*let p_join_1''' = VPL.assume_back ineq'' p_join_1'';;*)
(* -> infeasible *)
let p_join_2''' = VPL.assume_back ineq'' p_join_2'';;
(* -> UNBOUNDED *)
