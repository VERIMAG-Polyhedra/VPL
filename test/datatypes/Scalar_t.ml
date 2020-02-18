open Vpl

module Make_Tests (Coeff : Scalar.Type) = struct
	type sign_t = P | Z | N

	let sign a = if a < 0 then N else if a > 0 then P else Z
	let pr = function P -> "P" | Z -> "Z" | N -> "N"

	(* Coeff.cmp *)
	let cmpTs: Test.t
	   = fun () ->
		let chk (t, n1, n2, s) = fun state ->
			let r = sign (Coeff.cmp n1 n2) in
			if r = s then
				Test.succeed state
			else
				let e = "Coeff.cmp " ^ (Coeff.to_string n1) ^ " " ^ (Coeff.to_string n2) ^ ": " ^ (pr r) in
				Test.fail t e state
		in
		let tcs = [
			"zz0", Coeff.z, Coeff.z, Z;
			"zu0", Coeff.z, Coeff.u, N;
			"zu1", Coeff.z, Coeff.negU, P;
			"uz0", Coeff.u, Coeff.z, P;
			"uz1", Coeff.negU, Coeff.z, N;
			"uu0", Coeff.u, Coeff.u, Z;
			"uu1", Coeff.u, Coeff.negU, P;
			"uu2", Coeff.negU, Coeff.u, N;
			"pz0", Coeff.of_int 2, Coeff.z, P;
			"zp0", Coeff.z, Coeff.of_int 2, N;
			"pp0", Coeff.of_int 2, Coeff.of_int 2, Z;
			"pp1", Coeff.of_int 2, Coeff.of_int 3, N;
			"pp2", Coeff.of_int 3, Coeff.of_int 2, P;
			"nn0", Coeff.of_int (-2), Coeff.of_int (-2), Z;
			"nn1", Coeff.of_int (-2), Coeff.of_int (-3), P;
			"nn2", Coeff.of_int (-3), Coeff.of_int (-2), N;
			"pn0", Coeff.of_int 2, Coeff.of_int (-2), P;
			"np0", Coeff.of_int (-2), Coeff.of_int 2, N
		] in
		Test.suite "cmp" (List.map chk tcs)

	(* Coeff.cmpz *)
	let cmpzTs: Test.t
	   = fun () ->
		let chk (t, n, s) = fun state ->
			let r = sign (Coeff.cmpz n) in
			if r = s then
				Test.succeed state
			else
				let e = "Coeff.cmpz " ^ (Coeff.to_string n) ^ ": " ^ (pr r) in
				Test.fail t e state
		in
		let tcs = [
			"z", Coeff.z, Z;
			"u0", Coeff.u, N;
			"u1", Coeff.negU, P;
			"p0", Coeff.of_int 3, N;
			"p1", Coeff.mk 10 2, N;
			"n0", Coeff.of_int (-4), P
		] in
		Test.suite "cmpz" (List.map chk tcs)

	(* Coeff.neg *)
	let negTs: Test.t
	   = fun () ->
		let chk_res (t, n1, n2) = fun state ->
			let r = Coeff.neg n1 in
			if Coeff.cmp n2 r = 0 then
				Test.succeed state
			else
				let e = "Coeff.neg " ^ (Coeff.to_string n1) ^ " = " ^ (Coeff.to_string r) in
				Test.fail t e state
		in
		let chk_comp (t, n1, _) = fun state ->
			let r = Coeff.neg (Coeff.neg n1) in
			if Coeff.cmp n1 r = 0 then
				Test.succeed state
			else
				let e = "Coeff.neg (Coeff.neg " ^ (Coeff.to_string n1) ^ ") = " ^ (Coeff.to_string r) in
				Test.fail t e state
		in
		let tcs = [
			"z", Coeff.z, Coeff.z;
			"u", Coeff.u, Coeff.negU;
			"p0", Coeff.of_int 2, Coeff.of_int (-2);
			"p1", Coeff.of_int 5, Coeff.of_int (-5);
			"n0", Coeff.negU, Coeff.u;
			"n1", Coeff.of_int (-15), Coeff.of_int 15
		] in
		Test.suite "neg" [
			Test.suite "res" (List.map chk_res tcs);
			Test.suite "comp" (List.map chk_comp tcs)
		]

	(* Coeff.inv *)
	(*
	let invKoTs: Test.t
	= fun state ->
		let r = Coeff.inv Coeff.z in
		if not (Coeff.isReal r) then
			Test.succeed state
		else
			let e = "Coeff.inv Coeff.z = " ^ (Coeff.to_string r) in
			Test.fail "ko" e state
	*)

	let invOkTs: Test.t
	   = fun () ->
		let chk_res (t, n1, n2) = fun state ->
			let n = Coeff.inv n1 in
			if Coeff.cmp n2 n = 0 then
				Test.succeed state
			else
				let e = "Coeff.inv " ^ (Coeff.to_string n1) ^ " = " ^ (Coeff.to_string n) in
				Test.fail t e state
		in
		let chk_comp (t, n1, _) = fun state ->
			let n = Coeff.inv (Coeff.inv n1) in
			if Coeff.cmp n1 n = 0 then
				Test.succeed state
			else
				let e = "Coeff.inv (Coeff.inv " ^ (Coeff.to_string n1) ^ ") = " ^ (Coeff.to_string n) in
				Test.fail t e state
		in
		let tcs = [
			"u0", Coeff.u, Coeff.u;
			"u1", Coeff.negU, Coeff.negU;
			"p0", Coeff.of_int 2, Coeff.mk 2 1;
			"n0", Coeff.of_int (-2), Coeff.mk 2 (-1)
		] in
		Test.suite "ok" [
			Test.suite "res" (List.map chk_res tcs);
			Test.suite "comp" (List.map chk_comp tcs)
		]

	(* DM note: invKoTs does a divide by zero in F# *)
	let invTs: Test.t
	   =  fun () ->
       Test.suite "inv" [(* invKoTs; *) invOkTs ()]

	(* Coeff.add *)
	let addTs: Test.t
	   = fun () ->
		let chk_res (t, n1, n2, n) = fun state ->
			let r = Coeff.add n1 n2 in
			if Coeff.cmp n r = 0 then
				Test.succeed state
			else
				let e = (Coeff.to_string n1) ^ " + " ^ (Coeff.to_string n2) ^ " = " ^ (Coeff.to_string r) in
				Test.fail t e state
		in
		let chk_sym (t, n1, n2, _) = fun state ->
			let r1 = Coeff.add n1 n2 in
			let r2 = Coeff.add n2 n1 in
			if Coeff.cmp r1 r2 = 0 then
				Test.succeed state
			else
				let e1 = (Coeff.to_string n1) ^ " + " ^ (Coeff.to_string n2) ^ " = " ^ (Coeff.to_string r1) in
				let e2 = (Coeff.to_string n2) ^ " + " ^ (Coeff.to_string n1) ^ " = " ^ (Coeff.to_string r2) in
				Test.fail t (e1 ^ ", " ^ e2) state
		in
		let tcs = [
			"zz", Coeff.z, Coeff.z, Coeff.z;
			"u0", Coeff.z, Coeff.u, Coeff.u;
			"u1", Coeff.u, Coeff.u, Coeff.of_int 2;
			"p0", Coeff.of_int 2, Coeff.u, Coeff.of_int 3;
			"p1", Coeff.of_int 10, Coeff.of_int 15, Coeff.of_int 25;
			"n0", Coeff.negU, Coeff.z, Coeff.negU;
			"n1", Coeff.negU, Coeff.u, Coeff.z;
			"n2", Coeff.of_int (-5), Coeff.u, Coeff.of_int (-4)
		] in
		Test.suite "add" [
			Test.suite "res" (List.map chk_res tcs);
			Test.suite "sym" (List.map chk_sym tcs)
		]

	(* Coeff.mult *)
	let multTs: Test.t
	   = fun () ->
		let chk_res (t, n1, n2, n) = fun state ->
			let r = Coeff.mul n1 n2 in
			if Coeff.cmp n r = 0 then
				Test.succeed state
			else
				let e = (Coeff.to_string n1) ^ " + " ^ (Coeff.to_string n2) ^ " = " ^ (Coeff.to_string r) in
				Test.fail t e state
		in
		let chk_sym (t, n1, n2, _) = fun state ->
			let r1 = Coeff.mul n1 n2 in
			let r2 = Coeff.mul n2 n1 in
			if Coeff.cmp r1 r2 = 0 then
				Test.succeed state
			else
				let e1 = (Coeff.to_string n1) ^ " + " ^ (Coeff.to_string n2) ^ " = " ^ (Coeff.to_string r1) in
				let e2 = (Coeff.to_string n2) ^ " + " ^ (Coeff.to_string n1) ^ " = " ^ (Coeff.to_string r2) in
				Test.fail t (e1 ^ ", " ^ e2) state
		in
		let tcs = [
			"z0", Coeff.z, Coeff.z, Coeff.z;
			"z1", Coeff.z, Coeff.u, Coeff.z;
			"z2", Coeff.z, Coeff.of_int 2, Coeff.z;
			"z3", Coeff.z, Coeff.of_int (-2), Coeff.z;
			"u0", Coeff.u, Coeff.u, Coeff.u;
			"u1", Coeff.u, Coeff.of_int 2, Coeff.of_int 2;
			"u2", Coeff.u, Coeff.of_int (-2), Coeff.of_int (-2);
			"p0", Coeff.of_int 2, Coeff.of_int 3, Coeff.of_int 6;
			"p1", Coeff.of_int 10, Coeff.of_int 15, Coeff.of_int 150;
			"n0", Coeff.negU, Coeff.u, Coeff.negU;
			"n1", Coeff.negU, Coeff.negU, Coeff.u;
			"n2", Coeff.of_int (-5), Coeff.of_int 5, Coeff.of_int (-25);
			"n2", Coeff.of_int (-5), Coeff.of_int (-5), Coeff.of_int 25
		] in
		Test.suite "mult" [
			Test.suite "res" (List.map chk_res tcs);
			Test.suite "sym" (List.map chk_sym tcs)
		]

	(* Coeff.div *)
	(*
	let divKoTs: Test.t
	=
		let chk (t, n, d) = fun state ->
			let r = Coeff.div n d in
			if not (Coeff.isReal r) then
				Test.succeed state
			else
				let e = (Coeff.to_string r) ^ "is real" in
				Test.fail t e state
		in
		let tcs = [
			"zz", Coeff.z, Coeff.z;
			"u0", Coeff.u, Coeff.z;
			"u1", Coeff.negU, Coeff.z;
			"p0", Coeff.of_int 2, Coeff.z;
			"n0", Coeff.of_int (-5), Coeff.z
		] in
		Test.suite "ko" (List.map chk tcs)
	*)

	let divOkTs: Test.t
	   = fun () ->
		let chk (t, n, d, r) = fun state ->
			let r1 = Coeff.div n d in
			if Coeff.cmp r r1 = 0 then
				Test.succeed state
			else
				let e = "Coeff.div " ^ (Coeff.to_string n) ^ " " ^ (Coeff.to_string d) ^ " = " ^ (Coeff.to_string r1) in
				Test.fail t e state
		in
		let tcs = [
			"u0", Coeff.z, Coeff.u, Coeff.z;
			"u1", Coeff.z, Coeff.negU, Coeff.z;
			"u2", Coeff.u, Coeff.u, Coeff.u;
			"u3", Coeff.negU, Coeff.u, Coeff.negU;
			"u4", Coeff.of_int 2, Coeff.u, Coeff.of_int 2;
			"u5", Coeff.of_int 2, Coeff.negU, Coeff.of_int (-2);
			"p0", Coeff.of_int 4, Coeff.of_int 2, Coeff.of_int 2;
			"p1", Coeff.of_int 3, Coeff.of_int 2, Coeff.mk 2 3;
			"p2", Coeff.of_int 6, Coeff.of_int 4, Coeff.mk 2 3;
			"n0", Coeff.of_int (-4), Coeff.of_int 2, Coeff.of_int (-2);
			"n1", Coeff.of_int 4, Coeff.of_int (-2), Coeff.of_int (-2);
			"n2", Coeff.of_int (-4), Coeff.of_int (-2), Coeff.of_int 2
		] in
		Test.suite "ok" (List.map chk tcs)

	(* DM NOTE: divide by zero crashes F# *)
	let divTs: Test.t
	   =  fun () ->
       Test.suite "div" [(*divKoTs; *) divOkTs ()]

	let ts: Test.t
	   = fun () ->
       List.map Test.run [cmpTs; cmpzTs; negTs; invTs; addTs; multTs; divTs]
       |> Test.suite Coeff.name
end

module Rat = struct
	include Make_Tests(Scalar.Rat)
end
module Symbolic = struct
	include Make_Tests(Scalar.Symbolic)
	(* test suite for Splx.Symbolic *)

	(* Scalar.Symbolic.z *)
	let zTs: Test.t
	= fun () ->
        fun state ->
		let z1 = Scalar.Symbolic.z in
		let z2 = Scalar.Symbolic.ofQ Scalar.Rat.z in
		if Scalar.Symbolic.cmp z1 z2 = 0 then
			Test.succeed state
		else
			let e = (Scalar.Symbolic.to_string z1) ^ " <> " ^ (Scalar.Symbolic.to_string z2) in
			Test.fail "z" e state

	(* Scalar.Symbolic.cmp *)
	let cmpTs: Test.t
	   = fun () ->
		let eq = 0 in
		let lt = -1 in
		let gt = 1 in
		let chk (t, v1, v2, er) = fun state ->
			let ar = Scalar.Symbolic.cmp v1 v2 in
			let r =
				match ar, er with
				| 0, n when n = eq -> true
				| n1, n2 when n1 < 0 && n2 = lt -> true
				| n1, n2 when n1 > 0 && n2 = gt -> true
				| _, _ -> false
			in
			if r then
				Test.succeed state
			else
				let to_s i = if i = 0 then "=" else if i < 0 then "<" else ">" in
				let e = (Scalar.Symbolic.to_string v1) ^ " (expected: " ^ (to_s er) ^
					", actual: " ^ (to_s ar) ^ ")" in
				Test.fail t e state
		in
		let tcs = [
			"z", Scalar.Symbolic.z, Scalar.Symbolic.z, eq;
			"zpd", Scalar.Symbolic.z, Scalar.Symbolic.pdelta Scalar.Rat.z, lt;
			"znd", Scalar.Symbolic.z, Scalar.Symbolic.ndelta Scalar.Rat.z, gt;
			"pdz", Scalar.Symbolic.pdelta Scalar.Rat.z, Scalar.Symbolic.z, gt;
			"ndz", Scalar.Symbolic.ndelta Scalar.Rat.z, Scalar.Symbolic.z, lt;
			"01", Scalar.Symbolic.ofQ Scalar.Rat.z, Scalar.Symbolic.ofQ Scalar.Rat.u, lt;
			"10", Scalar.Symbolic.ofQ Scalar.Rat.u, Scalar.Symbolic.ofQ Scalar.Rat.z, gt;
			"0-1", Scalar.Symbolic.ofQ Scalar.Rat.z, Scalar.Symbolic.ofQ Scalar.Rat.negU, gt;
			"-10", Scalar.Symbolic.ofQ Scalar.Rat.negU, Scalar.Symbolic.ofQ Scalar.Rat.z, lt;
			"12", Scalar.Symbolic.ofQ Scalar.Rat.u, Scalar.Symbolic.ofQ (Scalar.Rat.of_int 2), lt;
			"21", Scalar.Symbolic.ofQ (Scalar.Rat.of_int 2), Scalar.Symbolic.ofQ Scalar.Rat.u, gt;
			"-12", Scalar.Symbolic.ofQ Scalar.Rat.negU, Scalar.Symbolic.ofQ (Scalar.Rat.of_int 2), lt;
			"2-1", Scalar.Symbolic.ofQ (Scalar.Rat.of_int 2), Scalar.Symbolic.ofQ Scalar.Rat.negU, gt
		] in
		Test.suite "cmp" (List.map chk tcs)

	(* Scalar.Symbolic.hasDelta *)
	let hasDeltaTs: Test.t
	   = fun () ->
		let chk (t, res, v) = fun state ->
			if Scalar.Symbolic.hasDelta v = res then
				Test.succeed state
			else
				Test.fail t "not equal" state
		in
		let tcs = [
			"z", false, Scalar.Symbolic.z;
			"int0", false, Scalar.Symbolic.ofQ Scalar.Rat.u;
			"int1", false, Scalar.Symbolic.ofQ (Scalar.Rat.of_int 2);
			"int2", false, Scalar.Symbolic.ofQ (Scalar.Rat.of_int (-2));
			"rat0", false, Scalar.Symbolic.ofQ (Scalar.Rat.mk 3 2);
			"pdelta0", true, Scalar.Symbolic.pdelta Scalar.Rat.z;
			"pdelta1", true, Scalar.Symbolic.pdelta Scalar.Rat.u;
			"pdelta2", true, Scalar.Symbolic.adddelta (Scalar.Symbolic.pdelta (Scalar.Rat.of_int 2));
			"pdelta3", true, Scalar.Symbolic.subdelta
				(Scalar.Symbolic.adddelta (Scalar.Symbolic.pdelta (Scalar.Rat.of_int 2)));

			"ndelta0", true, Scalar.Symbolic.ndelta Scalar.Rat.z;
			"ndelta1", true, Scalar.Symbolic.subdelta (Scalar.Symbolic.ndelta Scalar.Rat.u);
			"no0", false, Scalar.Symbolic.adddelta (Scalar.Symbolic.ndelta Scalar.Rat.u);
			"no1", false, Scalar.Symbolic.subdelta (Scalar.Symbolic.pdelta Scalar.Rat.u)
		] in
		Test.suite "hasDelta" (List.map chk tcs)

	(* Scalar.Symbolic.mulr *)
	let mulrTs: Test.t
	   = fun () ->
		let chk (t, ar, er) = fun state ->
			if Scalar.Symbolic.cmp ar er = 0 then
				Test.succeed state
			else
				let estr = "ar = " ^ (Scalar.Symbolic.to_string ar) ^ ", er = " ^ (Scalar.Symbolic.to_string er) in
				Test.fail t estr state
		in
		let tcs = [
			"z", Scalar.Symbolic.mulr Scalar.Rat.u Scalar.Symbolic.z, Scalar.Symbolic.z;
			"u", Scalar.Symbolic.mulr Scalar.Rat.u (Scalar.Symbolic.ofQ Scalar.Rat.u),
				Scalar.Symbolic.ofQ Scalar.Rat.u;
			"pu", Scalar.Symbolic.mulr Scalar.Rat.u (Scalar.Symbolic.pdelta Scalar.Rat.u),
				Scalar.Symbolic.pdelta Scalar.Rat.u;
			"nu", Scalar.Symbolic.mulr Scalar.Rat.u (Scalar.Symbolic.ndelta Scalar.Rat.u),
				Scalar.Symbolic.ndelta Scalar.Rat.u;
			"pn", Scalar.Symbolic.mulr (Scalar.Rat.of_int 2) (Scalar.Symbolic.pdelta Scalar.Rat.u),
				Scalar.Symbolic.add (Scalar.Symbolic.pdelta (Scalar.Rat.of_int 2))
					(Scalar.Symbolic.pdelta Scalar.Rat.z);
		] in
		Test.suite "mulr" (List.map chk tcs)

	(* Scalar.Symbolic.adddelta *)
	let adddeltaTs: Test.t
	   = fun () ->
		let chk (t, i, o) = fun state ->
			let r = Scalar.Symbolic.adddelta i in
			if Scalar.Symbolic.cmp o r = 0 then
				Test.succeed state
			else
				let e =
					"expected: " ^ (Scalar.Symbolic.to_string o) ^
					", got: " ^ (Scalar.Symbolic.to_string r)
				in
				Test.fail t e state
		in
		let tcs = [
			"z", Scalar.Symbolic.z, Scalar.Symbolic.pdelta Scalar.Rat.z;
			"u", Scalar.Symbolic.ofQ Scalar.Rat.u, Scalar.Symbolic.pdelta Scalar.Rat.u;
			"pos", Scalar.Symbolic.ofQ (Scalar.Rat.of_int 2), Scalar.Symbolic.pdelta (Scalar.Rat.of_int 2);
			"ndelta", Scalar.Symbolic.ndelta Scalar.Rat.u, Scalar.Symbolic.ofQ Scalar.Rat.u;
			"deltas", Scalar.Symbolic.pdelta Scalar.Rat.u,
				Scalar.Symbolic.add (Scalar.Symbolic.pdelta Scalar.Rat.u) (Scalar.Symbolic.pdelta Scalar.Rat.z)
		] in
		Test.suite "adddelta" (List.map chk tcs)

	(* Scalar.Symbolic.subdelta *)
	let subdeltaTs: Test.t
	   = fun () ->
		let chk (t, i, o) = fun state ->
			let r = Scalar.Symbolic.subdelta i in
			if Scalar.Symbolic.cmp o r = 0 then
				Test.succeed state
			else
				let e =
					"expected: " ^ (Scalar.Symbolic.to_string o) ^
					", got: " ^ (Scalar.Symbolic.to_string r)
				in
				Test.fail t e state
		in
		let tcs = [
			"z", Scalar.Symbolic.z, Scalar.Symbolic.ndelta Scalar.Rat.z;
			"u", Scalar.Symbolic.ofQ Scalar.Rat.u, Scalar.Symbolic.ndelta Scalar.Rat.u;
			"pos", Scalar.Symbolic.ofQ (Scalar.Rat.of_int 2), Scalar.Symbolic.ndelta (Scalar.Rat.of_int 2);
			"pdelta", Scalar.Symbolic.pdelta Scalar.Rat.u, Scalar.Symbolic.ofQ Scalar.Rat.u;
			"deltas", Scalar.Symbolic.ndelta Scalar.Rat.u,
				Scalar.Symbolic.add (Scalar.Symbolic.ndelta Scalar.Rat.u) (Scalar.Symbolic.ndelta Scalar.Rat.z)
		] in
		Test.suite "subdelta" (List.map chk tcs)

	let ts: Test.t
	   = fun () ->
       List.map Test.run [cmpTs; cmpzTs; negTs; invTs; addTs; multTs; divTs ; zTs; hasDeltaTs; mulrTs; adddeltaTs; subdeltaTs]
       |> Test.suite Scalar.Symbolic.name
end

module Float = struct
	include Make_Tests(Scalar.Float)
end

let ts: Test.t
	= fun () ->
    Test.suite "Scalar" [Rat.ts() ; Symbolic.ts() ; Float.ts()]
