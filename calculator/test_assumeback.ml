open Vpl;;
open Calculator;;
open Notations;;

(* Parsing a polyhedron: constraints are separated by ','. *)
let p1 = parse "x >= 0, x <= 1, y >= -10, x + y < 25, z > 2y , z <= 35";;
print p1;;

Flags.proj := Flags.Proj_PLP (Flags.Float);;

Debugger.enable();;
Debugger.print_enable();;
Debugger.set_colors();;
Proj.Debug.enable_all();;
PSplx.Debug.enable_all();;
PLP.Debug.enable_all();;

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
