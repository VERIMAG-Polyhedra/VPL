(** Only deal with inequalities. *)
let proj : Cstr.Rat.t list -> Flags.proj_method
	= fun l ->
	if List.length l >= 10
	then Flags.Proj_PLP Flags.Float
	else Flags.FM

let join : Cstr.Rat.t list -> Cstr.Rat.t list -> Flags.join_method
	= fun p1 p2 ->
	if max(List.length p1) (List.length p2) >= 5
	then Flags.Join_PLP Flags.Float
	else Flags.Baryc

let apply_proj: Cstr.Rat.t list -> ('a -> 'b) -> 'a -> 'b
	= fun cstrs f a ->
	match !Flags.proj with
	| Flags.PHeuristic -> begin
		let proj_method_old : Flags.proj_method ref = Flags.proj in
		Flags.proj := proj cstrs;
		let res = f a in
		Flags.proj := !proj_method_old;
		res
		end
	| _ -> f a

let apply_join: Cstr.Rat.t list -> Cstr.Rat.t list -> ('a -> 'b) -> 'a -> 'b
	= fun cstrs1 cstrs2 f a ->
	match !Flags.join with
	| Flags.JHeuristic -> begin
		let join_method_old : Flags.join_method ref = Flags.join in
		Flags.join := join cstrs1 cstrs2;
		let res = f a in
		Flags.join := !join_method_old;
		res
		end
	| _ -> f a
