open Vpl

module Vector_t
  = struct

  let init_ts : T.testT
    = let chk : string * int * (int -> Q.t) * Tableau.Vector.t -> T.testT
	= fun (nm, i, f, r) st ->
	let v = Tableau.Vector.init i f in
	T.equals nm (fun _ -> "unimplemented") Tableau.Vector.equal r v st
      in
      [
	"const", 1, List.nth [Q.one], [Q.one];
	"two", 2, List.nth [Q.one; Q.of_int 2], [Q.one; Q.of_int 2]
      ]
      |> List.map chk
      |> T.suite "init"

  let consAppend_ts : T.testT
    = let chk : string * Q.t * Tableau.Vector.t * Tableau.Vector.t -> T.testT
	= fun (nm, a, v, r) st ->
	let v' = Tableau.Vector.consAppend a v in
	T.equals nm (fun _ -> "unimplemented") Tableau.Vector.equal r v' st
      in
      [
		   "const", Q.one, [Q.zero], [Q.one; Q.zero];
		   "one", Q.of_int 2, [Q.zero; Q.one], [Q.zero; Q.of_int 2; Q.one]
      ]
      |> List.map chk
      |> T.suite "consAppend"

  let ts : T.testT
    = [
    init_ts;
    consAppend_ts
  ] |> T.suite "Vector"

end

module Matrix_t
  = struct

  let m
    = fun l -> List.map (List.map (fun (a, b) -> Q.of_ints a b)) l

  let pivot_ts : T.testT
    = let chk : string * int * int * Tableau.Matrix.t * Tableau.Matrix.t -> T.testT
	= fun (nm, row, col, inM, em) st ->
	let am = Tableau.Matrix.pivot inM row col in
	T.equals nm (fun _ -> "unimplemented") Tableau.Matrix.equal em am st
      in
      let tcs
	= [
	"regular", 0, 0,
	m [[2,1; 1,1];
	   [1,1; 0,1]],
	m [[1,1; 1,2];
	   [0,1; -1,2]]
      ] in
      T.suite "pivot" (List.map chk tcs)

  let add_col_ts : T.testT
    = let chk : string * Tableau.Vector.t * int * Tableau.Matrix.t * Tableau.Matrix.t -> T.testT
	= fun (nm, v, i, m, r) st ->
	let m' = Tableau.Matrix.add_col m v i in
	T.equals nm (fun _ -> "unimplemented") Tableau.Matrix.equal r m' st
      in
      let v = fun l -> List.map Q.of_int l in
      let m = fun l -> List.map v l in
      [
	"first", v [0; 0; 1], 0,
	m [[1; 1];
	   [2; 2];
	   [3; 3]],
	m [[0; 1; 1];
	   [0; 2; 2];
	   [1; 3; 3]];

	"last", v [0; 0; 1], 1,
	m [[1; 1];
	   [2; 2];
	   [3; 3]],
	m [[1; 0; 1];
	   [2; 0; 2];
	   [3; 1; 3]]
      ]
      |> List.map chk
      |> T.suite "add_col"

  let ts : T.testT
    = T.suite "Matrix" [
		    pivot_ts;
		    add_col_ts
		  ]
end
	
let range_ts : T.testT
  = let chk : string * int * int * int list -> T.testT
      = fun (nm, b, e, el) st ->
      let al = Misc.range b e in
      T.equals nm
		   (fun l -> Printf.sprintf "[%s]"(String.concat ";" (List.map Pervasives.string_of_int l)))
		   (fun l l' ->
		    try List.for_all2 (=) l l'
		    with Invalid_argument _ -> false)
		   el al st
    in
    let tcs : (string * int * int * int list) list
      = [
      "regular", 1, 3, [1; 2];
      "empty", 1, 1, [];
      "crossed", 2, 1, []
    ] in
    T.suite "range" (List.map chk tcs)
;;

let ts : T.testT
  = T.suite "Matrix" [
		  Vector_t.ts;
		  Matrix_t.ts;
		  range_ts
		]
