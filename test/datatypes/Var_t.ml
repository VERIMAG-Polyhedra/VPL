open Vpl

module Positive = struct
	module V = Var.Positive
	(* V.next *)
	let nextTs: T.testT
	=
		let chk (name, var, expected) = fun state ->
			if V.next var = expected then
				T.succeed state
			else
				T.fail name "not equal" state
		in
		let tcs: (string * V.t * V.t) list = [
		"nil", V.XH, V.XO V.XH;
		"cons0", V.XO V.XH, V.XI V.XH;
		"cons1", V.XI V.XH, V.XO (V.XO V.XH);
		"rec0", V.XO (V.XO (V.XI (V.XI V.XH))),
			V.XO (V.XI (V.XO (V.XO V.XH)));
		"rec1", V.XI (V.XI (V.XI (V.XI V.XH))),
			V.XO (V.XO (V.XO (V.XO (V.XO V.XH))));
		] in
		T.suite "next" (List.map chk tcs)

	(* V.cmp *)
	let cmpTs: T.testT
	=
		let chk (name, ans, x1, x2) = fun state ->
			let res = V.cmp x1 x2 in
			if ans = res then
				T.succeed state
			else
				T.fail name (string_of_int res) state
		in
		let tcs = [
			"eq0", 0, V.XH, V.XH;
			"eq1", 0, V.XO V.XH, V.XO V.XH;
			"eq2", 0, V.XI V.XH, V.XI V.XH;
			"eq3", 0, V.XO (V.XI V.XH), V.XO (V.XI V.XH);
			"eq4", 0, V.XI (V.XI V.XH), V.XI (V.XI V.XH);
			"eq5", 0, V.XO (V.XO V.XH), V.XO (V.XO V.XH);
			"eq6", 0, V.XI (V.XO V.XH), V.XI (V.XO V.XH);

			"lt0", -1, V.XH, V.XO V.XH;
			"lt1", -1, V.XH, V.XI V.XH;
			"lt2", -1, V.XO V.XH, V.XI V.XH;
			"lt3", -1, V.XO V.XH, V.XO (V.XO V.XH);
			"lt4", -1, V.XO V.XH, V.XI (V.XO V.XH);

			"gt0", 1, V.XO V.XH, V.XH;
			"gt1", 1, V.XI V.XH, V.XH;
			"gt2", 1, V.XO (V.XO V.XH), V.XH;
			"gt3", 1, V.XO (V.XI V.XH), V.XO (V.XO V.XH);
			"gt4", 1, V.XO (V.XO (V.XI V.XH)), V.XO (V.XO V.XH)
		] in
		T.suite "cmp" (List.map chk tcs)

	(* V.horizon *)
	let horizonTs
	=
		let a = V.XH in
		let b = V.next a in
		let c = V.next b in
		let d = V.next c in
		let e = V.next d in
		let f = V.next e in
		let g = V.next f in
		let h = V.next g in
		let i = V.next h in
		let j = V.next i in
		let k = V.next j in
		let l = V.next k in
		let chk (name, h, vars) state =
			let h1 = V.horizon (V.Set.of_list vars) in
			if h = h1 then
				T.succeed state
			else
				T.fail name "not equal" state
		in
		let tcs = [
			"empty", a, [];
			"one0", b, [a];
			"one1", g, [f];
			"ordered0", f, [c; d; e];
			"ordered1", l, [a; d; j; k];
			"gen0", j, [b; a; g; i; c]
		] in
		T.suite "horizon" (List.map chk tcs)

	let intConvTcs = [
		"one", 1, V.XH;
		"two", 2, V.XO V.XH;
		"three", 3, V.XI V.XH;
		"four", 4, V.XO (V.XO V.XH)
	]

	(* V.toInt *)
	let toIntTs: T.testT
	= (* XXX: test the overflow case *)
		let chk (name, i, p) = fun state ->
			let res = V.toInt p in
			if i = res then
				T.succeed state
			else
				T.fail name "not equal" state
		in
		T.suite "toInt" (List.map chk intConvTcs)

	(* V.fromInt *)
	let fromIntTs: T.testT
	=
		let chk (name, i, p) state =
			let res = V.fromInt i in
			if res = p then
				T.succeed state
			else
				T.fail name "not equal" state
		in
		T.suite "fromInt" (List.map chk intConvTcs)

	let ts: T.testT
	= T.suite Var.Positive.name [nextTs; cmpTs; horizonTs; toIntTs; fromIntTs]
end

let ts : T.testT
		= T.suite "Var" [Positive.ts]
