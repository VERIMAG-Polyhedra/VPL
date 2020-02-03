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

let p1 = parse "x >= -1, y >= -1, z >= -1, x + y + z <= 1";;
print p1;;

let point = [
    Scalar.Rat.of_string "-1/2", Var.fromInt 8;
]
|> Vector.Rat.mk;;

let p2 = VPL.set_point point p1;;

let p3 = p2 |- "z";;
