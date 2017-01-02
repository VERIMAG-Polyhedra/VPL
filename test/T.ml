type stateT = {
	path: string list;
	msg: string;
	tcCnt: int;
	failCnt: int;
	ignCnt: int;
}

let prPath: string list -> string
= fun l -> String.concat ":" (List.rev l)

let prState: string -> stateT -> string
= fun modu state ->
	if state.failCnt > 0 then
		let plural =
			if state.failCnt = 1 then
				""
			else
				"s"
		in
		Printf.sprintf "%s\n%i failure%s (%i ignored) out of %i test cases in group %s."
			state.msg state.failCnt plural state.ignCnt state.tcCnt modu
	else
		Printf.sprintf "Ok: no failures (%i ignored) in %i test cases in group %s."
			state.ignCnt state.tcCnt modu

let stateZ: stateT
= {
	path = [];
	msg = "";
	tcCnt = 0;
	failCnt = 0;
	ignCnt = 0;
}

type testT = stateT -> stateT

let succeed: stateT -> stateT
= fun state -> {state with tcCnt = state.tcCnt + 1}

let fail: string -> string -> stateT -> stateT
= fun name err state ->
	{state with
		msg = Printf.sprintf "%s\n%s\n%s\n" state.msg (prPath (name::state.path)) err;
		tcCnt = state.tcCnt + 1;
		failCnt = state.failCnt + 1
	}

let skip: stateT -> stateT
= fun s -> {s with ignCnt = s.ignCnt + 1}

let suite: string -> testT list -> testT
= fun name tcs ->
	fun state ->
		let nState = List.fold_left (fun st tc -> tc st) {state with path = name::state.path} tcs in
		{nState with path = state.path}

let equals : string -> ('a -> string) -> ('a -> 'a -> bool) -> 'a -> 'a -> stateT -> stateT
  = fun nm pr eq e a state ->
  if eq e a
  then succeed state
  else fail nm (Printf.sprintf "expected %s, but got %s\n" (pr e) (pr a)) state
  (*
let run: testT -> string
= fun t -> prState (t stateZ)
*)
