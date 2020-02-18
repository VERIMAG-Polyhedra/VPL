open Vpl

(* Var.next *)
let nextTs: Test.t
    = fun () ->
	let chk (name, var, expected) = fun state ->
		if Var.next var = expected then
			Test.succeed state
		else
			Test.fail name "not equal" state
	in
	let tcs: (string * Var.t * Var.t) list = [
	"nil", Var.XH, Var.XO Var.XH;
	"cons0", Var.XO Var.XH, Var.XI Var.XH;
	"cons1", Var.XI Var.XH, Var.XO (Var.XO Var.XH);
	"rec0", Var.XO (Var.XO (Var.XI (Var.XI Var.XH))),
		Var.XO (Var.XI (Var.XO (Var.XO Var.XH)));
	"rec1", Var.XI (Var.XI (Var.XI (Var.XI Var.XH))),
		Var.XO (Var.XO (Var.XO (Var.XO (Var.XO Var.XH))));
	] in
	Test.suite "next" (List.map chk tcs)

(* Var.cmp *)
let cmpTs: Test.t
   = fun () ->
	let chk (name, ans, x1, x2) = fun state ->
		let res = Var.cmp x1 x2 in
		if ans = res then
			Test.succeed state
		else
			Test.fail name (string_of_int res) state
	in
	let tcs = [
		"eq0", 0, Var.XH, Var.XH;
		"eq1", 0, Var.XO Var.XH, Var.XO Var.XH;
		"eq2", 0, Var.XI Var.XH, Var.XI Var.XH;
		"eq3", 0, Var.XO (Var.XI Var.XH), Var.XO (Var.XI Var.XH);
		"eq4", 0, Var.XI (Var.XI Var.XH), Var.XI (Var.XI Var.XH);
		"eq5", 0, Var.XO (Var.XO Var.XH), Var.XO (Var.XO Var.XH);
		"eq6", 0, Var.XI (Var.XO Var.XH), Var.XI (Var.XO Var.XH);

		"lt0", -1, Var.XH, Var.XO Var.XH;
		"lt1", -1, Var.XH, Var.XI Var.XH;
		"lt2", -1, Var.XO Var.XH, Var.XI Var.XH;
		"lt3", -1, Var.XO Var.XH, Var.XO (Var.XO Var.XH);
		"lt4", -1, Var.XO Var.XH, Var.XI (Var.XO Var.XH);

		"gt0", 1, Var.XO Var.XH, Var.XH;
		"gt1", 1, Var.XI Var.XH, Var.XH;
		"gt2", 1, Var.XO (Var.XO Var.XH), Var.XH;
		"gt3", 1, Var.XO (Var.XI Var.XH), Var.XO (Var.XO Var.XH);
		"gt4", 1, Var.XO (Var.XO (Var.XI Var.XH)), Var.XO (Var.XO Var.XH)
	] in
	Test.suite "cmp" (List.map chk tcs)

(* Var.horizon *)
let horizonTs : Test.t
   = fun () ->
	let a = Var.XH in
	let b = Var.next a in
	let c = Var.next b in
	let d = Var.next c in
	let e = Var.next d in
	let f = Var.next e in
	let g = Var.next f in
	let h = Var.next g in
	let i = Var.next h in
	let j = Var.next i in
	let k = Var.next j in
	let l = Var.next k in
	let chk (name, h, vars) state =
		let h1 = Var.horizon (Var.Set.of_list vars) in
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
	"one", 1, Var.XH;
	"two", 2, Var.XO Var.XH;
	"three", 3, Var.XI Var.XH;
	"four", 4, Var.XO (Var.XO Var.XH)
]

(* Var.toInt *)
let toIntTs: Test.t
   =  fun () ->
   (* XXX: test the overflow case *)
	let chk (name, i, p) = fun state ->
		let res = Var.toInt p in
		if i = res then
			Test.succeed state
		else
			Test.fail name "not equal" state
	in
	Test.suite "toInt" (List.map chk intConvTcs)

(* Var.fromInt *)
let fromIntTs: Test.t
   = fun () ->
	let chk (name, i, p) state =
		let res = Var.fromInt i in
		if res = p then
			Test.succeed state
		else
			Test.fail name "not equal" state
	in
	Test.suite "fromInt" (List.map chk intConvTcs)

let ts: Test.t
   = fun () ->
   List.map Test.run [nextTs ; cmpTs ; horizonTs ; toIntTs ; fromIntTs]
   |> Test.suite "Var"
