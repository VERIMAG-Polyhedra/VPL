open Vpl

module Test (M: VarMap.Type) = struct
	module V = M.V

	(* M.set *)
	let setNilConsTs: T.testT
	=
		let chk (name, tree) = fun state ->
			if M.is_empty tree
			then T.fail name "rt is nil" state
			else T.succeed state
		in
		let tcs = [
			"_", M.set 0 M.empty V.u 1;
			"O_", M.set 0 M.empty (V.fromLeft V.u) 1;
			"I_", M.set 0 M.empty (V.fromRight V.u) 1;
			"OO_", M.set 0 M.empty (V.fromLeft (V.fromLeft V.u)) 1;
			"OI_", M.set 0 M.empty (V.fromLeft (V.fromRight V.u)) 1;
			"IO_", M.set 0 M.empty (V.fromRight (V.fromLeft V.u)) 1;
			"II_", M.set 0 M.empty (V.fromRight (V.fromRight V.u)) 1;
		] in
		T.suite "nilCons" (List.map chk tcs)

	let setOrderTs: T.testT
	=
		let chk (name, tree1, tree2) = fun state ->
			if M.equal (=) tree1 tree2 then
				T.succeed state
			else
				T.fail name "not equal" state
		in
		let set = M.set 0 in
		let tcs = [
			"_/O_",
				set (set M.empty V.u 1) (V.fromLeft V.u) 2,
				set (set M.empty (V.fromLeft V.u) 2) V.u 1;
			"_/I_",
				set (set M.empty V.u 1) (V.fromRight V.u) 2,
				set (set M.empty (V.fromRight V.u) 2) V.u 1;
			"O_/I_",
				set (set M.empty (V.fromLeft V.u) 1) (V.fromRight V.u) 2,
				set (set M.empty (V.fromRight V.u) 2) (V.fromLeft V.u) 1;
			"_/OO_",
				set (set M.empty V.u 1) (V.fromLeft (V.fromLeft V.u)) 2,
				set (set M.empty (V.fromLeft (V.fromLeft V.u)) 2) V.u 1;
			"_/OI_",
				set (set M.empty V.u 1) (V.fromLeft (V.fromRight V.u)) 2,
				set (set M.empty (V.fromLeft (V.fromRight V.u)) 2) V.u 1;
			"_/IO_",
				set (set M.empty V.u 1) (V.fromRight (V.fromLeft V.u)) 2,
				set (set M.empty (V.fromRight (V.fromLeft V.u)) 2) V.u 1;
			"_/II_",
				set (set M.empty V.u 1) (V.fromRight (V.fromRight V.u)) 2,
				set (set M.empty (V.fromRight (V.fromRight V.u)) 2) V.u 1
		] in
		T.suite "order" (List.map chk tcs)

	let setTs: T.testT
	= T.suite "set" [setNilConsTs; setOrderTs]

	(* M.find *)
	let findTs: T.testT
	=
		let chk (name, predicate, tree, expected) = fun state ->
			let pr
			= function
				| None -> "None"
				| Some _ -> "Some _"
			in
			let actual = M.find predicate tree in
			if List.exists ((=) actual) expected then
				T.succeed state
			else
				T.fail name (pr actual) state
		in
		let tcs = [
			"nil0", (fun _ -> Some 0), M.empty, [None];
			"nil1", (fun _ -> None), M.empty, [None];
			"yes0", (fun _ -> Some 0), M.mk 0 [V.u, 1], [Some (V.u, 0)];
			"yes1", (fun _ -> Some 0), M.mk 0 [V.u, 1; V.fromLeft V.u, 2],
				[Some (V.u, 0); Some (V.fromLeft V.u, 0)];

			"no0", (fun _ -> None), M.mk 0 [V.u, 1], [None];
			"no1", (fun i -> if i = 0 then Some 0 else None), M.mk 0 [V.u, 1; V.fromLeft V.u, 2], [None];
			"ret0", (fun i -> Some i), M.mk 0 [V.u, 1; V.fromLeft V.u, 2],
				[Some (V.u, 1); Some (V.fromLeft V.u, 2)];
			"ret0", (fun i -> if i = 1 then Some 10 else None), M.mk 0 [V.u, 1; V.fromRight V.u, 2],
				[Some (V.u, 10)]
		] in
		T.suite "find" (List.map chk tcs)

	(* M.pathsGet *)
	
	let pathsGetTs: T.testT
	=
		let set = M.set false in
		let chk (name, tree, paths) = fun state ->
			let res = M.pathsGet tree in
			if V.Set.equal (V.Set.of_list paths) res
			then T.succeed state
			else T.fail name "not equal" state
		in
		let tcs = [
			"nil", M.empty, [];
			"unit0", set M.empty V.u true, [V.u];
			"unit1", set M.empty V.u false, [];
			"unit2", set M.empty (V.fromLeft V.u) true, [V.fromLeft V.u];
			"unit3", set M.empty (V.fromRight V.u) true, [V.fromRight V.u];
			"m0", set (set M.empty (V.fromLeft V.u) true) V.u true, [V.u; V.fromLeft V.u];
			"m1", set (set M.empty (V.fromRight V.u) true) V.u true, [V.u; V.fromRight V.u]
		] in
		T.suite "pathsGet" (List.map chk tcs)
	
	(* M.toList *)
	let toListTs: T.testT
	=
		let set = M.set false in
		let chk (name, tree, paths) = fun state ->
			let res = M.toList tree in
			if res = paths then
				T.succeed state
			else
				T.fail name "not equal" state
		in
		let tcs = [
			"nil", M.empty, [];
			"unit0", set M.empty V.u true, [V.u, true];
			"unit1", set M.empty V.u false, [V.u, false];
			"unit2", set (set M.empty V.u false) (V.fromLeft V.u) true, [V.u, false; V.fromLeft V.u, true];
			"unit3", set (set M.empty V.u false) (V.fromRight V.u) true, [V.u, false; V.fromRight V.u, true];
			"m0", set (set M.empty V.u true) (V.fromLeft V.u) true, [V.u, true; V.fromLeft V.u, true];
			"m1", set (set M.empty V.u true) (V.fromRight V.u) true, [V.u, true; V.fromRight V.u, true]
		] in
		T.suite "toList" (List.map chk tcs)
	
	let ts: T.testT
		= T.suite V.name [setTs; findTs; pathsGetTs; toListTs]

end

module Rtree = Test (Rtree)

module VarMap = struct
	module Int = Test (VarMap.VarMap(Var.Int))
	
	(*module String = Test (VarMap.VarMap(Var.String))*)
	
	let ts: T.testT
		= T.suite "VarMap" [Int.ts]
end

let ts: T.testT
	= T.suite "VarMap" [Rtree.ts ; VarMap.ts]
		
