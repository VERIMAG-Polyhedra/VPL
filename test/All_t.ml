let datatypes = T.prState "Datatypes" 
	(T.suite "Datatypes" [Var_t.ts ; VarMap_t.ts ; Scalar_t.ts ; Vector_t.ts ; Cstr_t.ts] T.stateZ)

let plp = T.prState "PLP"
	(T.suite "PLP" [Tableau_t.ts ; ParamCoeff_t.ts ; Objective_t.ts; PSplx_t.ts (*; Min_t.ts*)] T.stateZ)

let misc = T.prState "MISC"
	(T.suite "MISC" [Misc_t.ts ; IndexBuild_t.ts ; HOtypes_t.ts ; Poly_t.ts] T.stateZ)
	
let lp = T.prState "Linear Programming"
	(T.suite "Linear Programming" [Splx_t.ts ; Opt_t.ts] T.stateZ)

let core = T.prState "Core"
	(T.suite "Core" [EqSet_t.ts ; IneqSet_t.ts ; ProjBuild_t.ts] T.stateZ)

(*
let calculator = T.prState "Calculator"
	(T.suite "Calculator" [Calculator_t.ts] T.stateZ)
*)
let ts: string list
	= [ datatypes ; plp ; misc ; lp ; core ; (*calculator ;*) Pol_t.ts ];;

String.concat "\n" ts
	|> print_endline;;
