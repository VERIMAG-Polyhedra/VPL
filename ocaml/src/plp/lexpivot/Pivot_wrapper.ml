type lexpivot

external create : int -> lexpivot = "create"

external set_A : lexpivot -> int -> int -> int -> int -> unit = "set_A"

external set_b : lexpivot -> int -> int -> int -> unit = "set_b"

external set_column: lexpivot -> int -> int -> int -> unit = "set_column"

external print : lexpivot -> unit = "print"

(** Solves and destroy the pivot object *)
external solve : lexpivot -> int = "solve"
