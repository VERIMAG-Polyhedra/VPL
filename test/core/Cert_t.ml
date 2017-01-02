module Test (Cs : Cstr.Rat.Type) = struct 
	
	module Cert = Cert.Cert(Cs)
	
	let mk l = List.map (fun (i, a) -> (i, Cs.Vec.Coeff.mk1 a)) l

	let mkc t v c = Cs.mk t (List.map (fun (c, v) -> (Cs.Vec.Coeff.mk1 c, v)) v) (Cs.Vec.Coeff.mk1 c)

	let eq = mkc Cstr.Eq
	let le = mkc Cstr.Le

	(* Cert.isEqF *)
	let isEqFTs: T.testT
	=
		let chk (name, l1, l2, expected) = fun state ->
			let f1 = Cert.mkF (mk l1) in
			     let f2 = Cert.mkF (mk l2) in
			if Cert.isEqF f1 f2 = expected then
				T.succeed state
			else
				T.fail name "bad conclusion" state
		in
		let tcs = [
			"empty", [], [], true;
			"one_empty0", [], [1, 1], false;
			"one_empty1", [1, 1], [], false;
			"one_same0", [1, 1], [1, 1], true;
			"one_same1", [2, 1], [2, 1], true;
			"one_same2", [1, 2], [1, 2], true;
			"one_other0", [1, 1], [2, 1], false;
			"one_other1", [1, 1], [1, 2], false;
			"many_same0", [1, 2; 2, 3], [1, 2; 2, 3], true;
			"many_order0", [2, 3; 1, 2], [1, 2; 2, 3], true;
			"many_other0", [1, 2; 2, 3], [1, 2; 3, 3], false;
			"many_other1", [1, 2; 2, 3], [3, 2; 2, 3], false;
			"many_other2", [1, 2; 2, 3], [1, 1; 2, 3], false;
			"many_other3", [1, 2; 2, 2], [1, 2; 2, 3], false;
		] in
		T.suite "isEqF" (List.map chk tcs)

	(* Cert.mkF *)
	let mkFTs: T.testT
	=
		let chk (name, l) = fun state ->
			let f = mk l in
			try
				let _ = Cert.mkF f in
				T.fail name "Invalid_argument not raised" state
			with
			| Invalid_argument "Cert.mkF" -> T.succeed state
			| _ -> T.fail name "unexpected exception raised" state
		in
		let tcs = [
			"override0", [0, 1; 0, 1];
			"override1", [0, 1; 0, 2];
			"override2", [0, 1; 1, 1; 1, 3];
			"override3", [0, 1; 1, 2; 0, 2];
			"z0", [0, 0];
			"z1", [0, 1; 1, 0];
			"z2", [1, 2; 0, 2; 3, 0]
		] in
		T.suite "mkF" (List.map chk tcs)

	(* Cert.mergeF *)
	let mergeFTs: T.testT
	=
		let chk (name, ll, r) = fun state ->
			let lf = List.map (fun (l, i) -> (Cert.mkF (mk l), Cs.Vec.Coeff.mk1 i)) ll in
			let rf = Cert.mkF (mk r) in
			let f = Cert.mergeF lf in
			if Cert.isEqF rf f then
				T.succeed state
			else
				let err =
					let sumStr =
						List.fold_left (fun s (f, i) -> s ^ "\t" ^ (Cs.Vec.Coeff.to_string i) ^ " * [" ^ (Cert.prF f) ^ "]\n") "" lf
					in
					Printf.sprintf "\nexpected: %s\ngot: %s\nas the result of the sum of:\n%s\n"
						(Cert.prF rf) (Cert.prF f) sumStr
				in
				T.fail name err state
		in
		let tcs = [
			"empty0", [], [];
			"empty1", [[], 0], [];
			"empty2", [[], 0; [], 0; [], 0], [];
			"empty3", [[], 1; [], 2; [], 3], [];
			"empty4", [[0, 1], 0; [], 1], [];
			"one0", [[0, 1], 1; [], 1], [0, 1];
			"one1", [[0, 1], 2; [], 1], [0, 2];
			"one2", [[0, 2], 2; [], 1], [0, 4];
			"one3", [[1, 2; 0, 1], 2; [], 1], [1, 4; 0, 2];
			"disjoined0", [[1,1; 2,1], 1; [3,1; 0,1], 1], [1,1; 2,1; 3,1; 0,1];
			"disjoined1", [[1,1; 2,1], 2; [3,1; 0,1], 1], [1,2; 2,2; 3,1; 0,1];
			"disjoined2", [[1,1; 2,1], 2; [3,1; 0,1], 3], [1,2; 2,2; 3,3; 0,3];
			"overlap0", [[1,1; 2,1], 2; [2,1; 0,1], 3], [1,2; 2,5; 0,3]
		] in
		T.suite "mergeF" (List.map chk tcs)

	let ts: T.testT
	= T.suite Cs.Vec.V.name [isEqFTs; mkFTs; mergeFTs]
end

module Positive = Test(Cstr.Rat.Positive)

module Int = Test(Cstr.Rat.Int)

let ts: T.testT
	= T.suite "Cert" [Positive.ts ; Int.ts]
