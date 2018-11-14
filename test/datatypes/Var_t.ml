open Vpl

module VT = Var_type

module Positive = struct
	module V = Var.Positive
	(* V.next *)
	let nextTs: Test.t
        = fun () ->
		let chk (name, var, expected) = fun state ->
			if V.next var = expected then
				Test.succeed state
			else
				Test.fail name "not equal" state
		in
		let tcs: (string * V.t * V.t) list = [
		"nil", VT.XH, VT.XO VT.XH;
		"cons0", VT.XO VT.XH, VT.XI VT.XH;
		"cons1", VT.XI VT.XH, VT.XO (VT.XO VT.XH);
		"rec0", VT.XO (VT.XO (VT.XI (VT.XI VT.XH))),
			VT.XO (VT.XI (VT.XO (VT.XO VT.XH)));
		"rec1", VT.XI (VT.XI (VT.XI (VT.XI VT.XH))),
			VT.XO (VT.XO (VT.XO (VT.XO (VT.XO VT.XH))));
		] in
		Test.suite "next" (List.map chk tcs)

	(* V.cmp *)
	let cmpTs: Test.t
	   = fun () ->
		let chk (name, ans, x1, x2) = fun state ->
			let res = V.cmp x1 x2 in
			if ans = res then
				Test.succeed state
			else
				Test.fail name (string_of_int res) state
		in
		let tcs = [
			"eq0", 0, VT.XH, VT.XH;
			"eq1", 0, VT.XO VT.XH, VT.XO VT.XH;
			"eq2", 0, VT.XI VT.XH, VT.XI VT.XH;
			"eq3", 0, VT.XO (VT.XI VT.XH), VT.XO (VT.XI VT.XH);
			"eq4", 0, VT.XI (VT.XI VT.XH), VT.XI (VT.XI VT.XH);
			"eq5", 0, VT.XO (VT.XO VT.XH), VT.XO (VT.XO VT.XH);
			"eq6", 0, VT.XI (VT.XO VT.XH), VT.XI (VT.XO VT.XH);

			"lt0", -1, VT.XH, VT.XO VT.XH;
			"lt1", -1, VT.XH, VT.XI VT.XH;
			"lt2", -1, VT.XO VT.XH, VT.XI VT.XH;
			"lt3", -1, VT.XO VT.XH, VT.XO (VT.XO VT.XH);
			"lt4", -1, VT.XO VT.XH, VT.XI (VT.XO VT.XH);

			"gt0", 1, VT.XO VT.XH, VT.XH;
			"gt1", 1, VT.XI VT.XH, VT.XH;
			"gt2", 1, VT.XO (VT.XO VT.XH), VT.XH;
			"gt3", 1, VT.XO (VT.XI VT.XH), VT.XO (VT.XO VT.XH);
			"gt4", 1, VT.XO (VT.XO (VT.XI VT.XH)), VT.XO (VT.XO VT.XH)
		] in
		Test.suite "cmp" (List.map chk tcs)

	(* V.horizon *)
	let horizonTs : Test.t
	   = fun () ->
		let a = VT.XH in
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
				Test.succeed state
			else
				Test.fail name "not equal" state
		in
		let tcs = [
			"empty", a, [];
			"one0", b, [a];
			"one1", g, [f];
			"ordered0", f, [c; d; e];
			"ordered1", l, [a; d; j; k];
			"gen0", j, [b; a; g; i; c]
		] in
		Test.suite "horizon" (List.map chk tcs)

	let intConvTcs = [
		"one", 1, VT.XH;
		"two", 2, VT.XO VT.XH;
		"three", 3, VT.XI VT.XH;
		"four", 4, VT.XO (VT.XO VT.XH)
	]

	(* V.toInt *)
	let toIntTs: Test.t
	   =  fun () ->
       (* XXX: test the overflow case *)
		let chk (name, i, p) = fun state ->
			let res = V.toInt p in
			if i = res then
				Test.succeed state
			else
				Test.fail name "not equal" state
		in
		Test.suite "toInt" (List.map chk intConvTcs)

	(* V.fromInt *)
	let fromIntTs: Test.t
	   = fun () ->
		let chk (name, i, p) state =
			let res = V.fromInt i in
			if res = p then
				Test.succeed state
			else
				Test.fail name "not equal" state
		in
		Test.suite "fromInt" (List.map chk intConvTcs)

	let ts: Test.t
	   = fun () ->
       List.map Test.run [nextTs ; cmpTs ; horizonTs ; toIntTs ; fromIntTs]
       |> Test.suite Var.Positive.name
end

let ts : Test.t
	= fun () ->
    Test.suite "Var" [Positive.ts ()]
