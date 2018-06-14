open Arg;;
open Vpl;;
Debug.enable();
Debug.print_enable();
Debug.set_colors();;

module Cmd = struct

    let debug : bool ref = ref true

    let spec_list = [
        ("-debug", Unit (fun () -> debug:= true ),"Enable debug mode");
    ]

    let anon_fun s = failwith ("unsupported argument "^s)

    let usage_msg = "VPL tests. Available options:"
end

open Cmd;;

Arg.parse spec_list anon_fun usage_msg;;

module Tests = struct
    let datatypes =
        let tcs = List.map Test.run [Var_t.ts ; VarMap_t.ts ; Scalar_t.ts ; Vector_t.ts ; Cstr_t.ts] in
        Test.prState "Datatypes" (Test.suite "Datatypes" tcs Test.stateZ)

    let plp =
        let tcs = List.map Test.run [Tableau_t.ts ; ParamCoeff_t.ts ; Objective_t.ts; PSplx_t.ts (*; Min_t.ts*)] in
        Test.prState "PLP" (Test.suite "PLP" tcs Test.stateZ)

    let misc = Test.prState "MISC"
    	(Test.suite "MISC" [Misc_t.ts()] Test.stateZ)

    let lin = Test.prState "Linearization"
    	(Test.suite "Linearization" [IndexBuild_t.ts() ; HOtypes_t.ts() ; Poly_t.ts()] Test.stateZ)

    let lp = Test.prState "Linear Programming"
    	(Test.suite "Linear Programming" [Splx_t.ts() ; Opt_t.ts()] Test.stateZ)

    let core = Test.prState "Core"
    	(Test.suite "Core" [EqSet_t.ts() ; IneqSet_t.ts() ; ProjBuild_t.ts()] Test.stateZ)

    (*
    let calculator = Test.prState "Calculator"
    	(Test.suite "Calculator" [Calculator_t.ts] Test.stateZ)
    *)
    let ts: string list
    	= [ datatypes ; plp ; misc ; lin ; lp ; core ; (*calculator ;*) Pol_t.ts]
end;;

String.concat "\n" Tests.ts
	|> print_endline;;
