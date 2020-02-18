open Vpl

(* Rtree.set *)
let setNilConsTs: Test.t
   = fun () ->
	let chk (name, tree) = fun state ->
		if Rtree.is_empty tree
		then Test.fail name "rt is nil" state
		else Test.succeed state
	in
	let tcs = [
		"_", Rtree.set 0 Rtree.empty Var.u 1;
		"O_", Rtree.set 0 Rtree.empty (Var.fromLeft Var.u) 1;
		"I_", Rtree.set 0 Rtree.empty (Var.fromRight Var.u) 1;
		"OO_", Rtree.set 0 Rtree.empty (Var.fromLeft (Var.fromLeft Var.u)) 1;
		"OI_", Rtree.set 0 Rtree.empty (Var.fromLeft (Var.fromRight Var.u)) 1;
		"IO_", Rtree.set 0 Rtree.empty (Var.fromRight (Var.fromLeft Var.u)) 1;
		"II_", Rtree.set 0 Rtree.empty (Var.fromRight (Var.fromRight Var.u)) 1;
	] in
	Test.suite "nilCons" (List.map chk tcs)

let setOrderTs: Test.t
   = fun () ->
	let chk (name, tree1, tree2) = fun state ->
		if Rtree.equal (=) tree1 tree2 then
			Test.succeed state
		else
			Test.fail name "not equal" state
	in
	let set = Rtree.set 0 in
	let tcs = [
		"_/O_",
			set (set Rtree.empty Var.u 1) (Var.fromLeft Var.u) 2,
			set (set Rtree.empty (Var.fromLeft Var.u) 2) Var.u 1;
		"_/I_",
			set (set Rtree.empty Var.u 1) (Var.fromRight Var.u) 2,
			set (set Rtree.empty (Var.fromRight Var.u) 2) Var.u 1;
		"O_/I_",
			set (set Rtree.empty (Var.fromLeft Var.u) 1) (Var.fromRight Var.u) 2,
			set (set Rtree.empty (Var.fromRight Var.u) 2) (Var.fromLeft Var.u) 1;
		"_/OO_",
			set (set Rtree.empty Var.u 1) (Var.fromLeft (Var.fromLeft Var.u)) 2,
			set (set Rtree.empty (Var.fromLeft (Var.fromLeft Var.u)) 2) Var.u 1;
		"_/OI_",
			set (set Rtree.empty Var.u 1) (Var.fromLeft (Var.fromRight Var.u)) 2,
			set (set Rtree.empty (Var.fromLeft (Var.fromRight Var.u)) 2) Var.u 1;
		"_/IO_",
			set (set Rtree.empty Var.u 1) (Var.fromRight (Var.fromLeft Var.u)) 2,
			set (set Rtree.empty (Var.fromRight (Var.fromLeft Var.u)) 2) Var.u 1;
		"_/II_",
			set (set Rtree.empty Var.u 1) (Var.fromRight (Var.fromRight Var.u)) 2,
			set (set Rtree.empty (Var.fromRight (Var.fromRight Var.u)) 2) Var.u 1
	] in
	Test.suite "order" (List.map chk tcs)

let setTs: Test.t
   = fun () ->
   Test.suite "set" [setNilConsTs (); setOrderTs ()]

(* Rtree.find *)
let findTs: Test.t
   = fun () ->
	let chk (name, predicate, tree, expected) = fun state ->
		let pr
		= function
			| None -> "None"
			| Some _ -> "Some _"
		in
		let actual = Rtree.find predicate tree in
		if List.exists ((=) actual) expected then
			Test.succeed state
		else
			Test.fail name (pr actual) state
	in
	let tcs = [
		"nil0", (fun _ -> Some 0), Rtree.empty, [None];
		"nil1", (fun _ -> None), Rtree.empty, [None];
		"yes0", (fun _ -> Some 0), Rtree.mk 0 [Var.u, 1], [Some (Var.u, 0)];
		"yes1", (fun _ -> Some 0), Rtree.mk 0 [Var.u, 1; Var.fromLeft Var.u, 2],
			[Some (Var.u, 0); Some (Var.fromLeft Var.u, 0)];

		"no0", (fun _ -> None), Rtree.mk 0 [Var.u, 1], [None];
		"no1", (fun i -> if i = 0 then Some 0 else None), Rtree.mk 0 [Var.u, 1; Var.fromLeft Var.u, 2], [None];
		"ret0", (fun i -> Some i), Rtree.mk 0 [Var.u, 1; Var.fromLeft Var.u, 2],
			[Some (Var.u, 1); Some (Var.fromLeft Var.u, 2)];
		"ret0", (fun i -> if i = 1 then Some 10 else None), Rtree.mk 0 [Var.u, 1; Var.fromRight Var.u, 2],
			[Some (Var.u, 10)]
	] in
	Test.suite "find" (List.map chk tcs)

(* Rtree.pathsGet *)

let pathsGetTs: Test.t
   = fun () ->
	let set = Rtree.set false in
	let chk (name, tree, paths) = fun state ->
		let res = Rtree.pathsGet tree in
		if Var.Set.equal (Var.Set.of_list paths) res
		then Test.succeed state
		else Test.fail name "not equal" state
	in
	let tcs = [
		"nil", Rtree.empty, [];
		"unit0", set Rtree.empty Var.u true, [Var.u];
		"unit1", set Rtree.empty Var.u false, [];
		"unit2", set Rtree.empty (Var.fromLeft Var.u) true, [Var.fromLeft Var.u];
		"unit3", set Rtree.empty (Var.fromRight Var.u) true, [Var.fromRight Var.u];
		"m0", set (set Rtree.empty (Var.fromLeft Var.u) true) Var.u true, [Var.u; Var.fromLeft Var.u];
		"m1", set (set Rtree.empty (Var.fromRight Var.u) true) Var.u true, [Var.u; Var.fromRight Var.u]
	] in
	Test.suite "pathsGet" (List.map chk tcs)

(* Rtree.toList *)
let toListTs: Test.t
   = fun () ->
	let set = Rtree.set false in
	let chk (name, tree, paths) = fun state ->
		let res = Rtree.toList tree in
		if res = paths then
			Test.succeed state
		else
			Test.fail name "not equal" state
	in
	let tcs = [
		"nil", Rtree.empty, [];
		"unit0", set Rtree.empty Var.u true, [Var.u, true];
		"unit1", set Rtree.empty Var.u false, [Var.u, false];
		"unit2", set (set Rtree.empty Var.u false) (Var.fromLeft Var.u) true, [Var.u, false; Var.fromLeft Var.u, true];
		"unit3", set (set Rtree.empty Var.u false) (Var.fromRight Var.u) true, [Var.u, false; Var.fromRight Var.u, true];
		"m0", set (set Rtree.empty Var.u true) (Var.fromLeft Var.u) true, [Var.u, true; Var.fromLeft Var.u, true];
		"m1", set (set Rtree.empty Var.u true) (Var.fromRight Var.u) true, [Var.u, true; Var.fromRight Var.u, true]
	] in
	Test.suite "toList" (List.map chk tcs)

let ts: Test.t
	= fun () ->
    List.map Test.run [setTs ; findTs ; pathsGetTs ; toListTs]
    |> Test.suite "Rtree"
