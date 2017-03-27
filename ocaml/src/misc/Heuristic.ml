let min : Cstr.Rat.Positive.t list -> Flags.min_method
	= fun l ->
	if List.length l >= 10
	then Flags.Classic
	else Flags.Classic

(** Only deal with inequalities. *)
let proj : Cstr.Rat.Positive.t list -> Flags.proj_method
	= fun l ->
	if List.length l >= 10
	then Flags.Proj_PLP Flags.Float
	else Flags.FM

let join : Cstr.Rat.Positive.t list -> Cstr.Rat.Positive.t list -> Flags.join_method
	= fun p1 p2 ->
	if max(List.length p1) (List.length p2) >= 5
	then Flags.Join_PLP Flags.Float
	else Flags.Baryc
