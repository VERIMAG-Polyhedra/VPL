type stateT

val stateZ : stateT

type testT = stateT -> stateT

val succeed: stateT -> stateT

val fail: string -> string -> stateT -> stateT

val skip: stateT -> stateT

val suite: string -> testT list -> testT

(* equals name to_string equal expected_result actual_result state*)
val equals : string -> ('a -> string) -> ('a -> 'a -> bool) -> 'a -> 'a -> stateT -> stateT
(*
val run: testT -> string
*)
val prState: string -> stateT -> string
