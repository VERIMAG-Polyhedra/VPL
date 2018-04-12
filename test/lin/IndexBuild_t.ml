open Vpl

let mk = Index.Int.mk
let mkl l = List.map Index.Int.mk l

module Liste = struct

	let components_ts : T.testT
		 = let chk : string * Index.Int.t * IndexBuild.Liste.t -> T.testT
		= fun (nm, ind, eil) state ->
		let ail = IndexBuild.Liste.components ind in
		T.equals nm IndexBuild.Liste.to_string IndexBuild.Liste.equal eil ail state
		   in
		   let tcs : (string * Index.Int.t * IndexBuild.Liste.t) list
		= [
			 "empty", mk [], [] ;
			 "singleton", mk [7], mkl [[1];[1];[1];[1];[1];[1];[1]] ;
			 "increasing", mk [0;1;3], mkl [[0;1;0] ; [0;0;1] ; [0;0;1] ; [0;0;1]] ;
			 "decreasing", mk [3;1;0], mkl [[0;1;0] ; [1;0;0] ; [1;0;0] ; [1;0;0]] ;
			 "any", mk [1;2;1], mkl [[1;0;0] ; [0;1;0] ; [0;1;0] ; [0;0;1]] ;
		  ] in
		   T.suite "components" (List.map chk tcs)

	let get_preds_ts : T.testT
		 = let chk : string * Index.Int.t * IndexBuild.Liste.t -> T.testT
		= fun (nm, ind, eil) state ->
		let ail = IndexBuild.Liste.get_preds ind in
		T.equals nm IndexBuild.Liste.to_string IndexBuild.Liste.equal eil ail state
		   in
		   let tcs : (string * Index.Int.t * IndexBuild.Liste.t) list
		= [
			 "empty", mk [], [] ;
			 "singleton", mk [7], mkl [[6];[5];[4];[3];[2];[1]] ;
			 "increasing", mk [0;1;3], mkl [[0;0;1] ; [0;0;2] ; [0;0;3] ; [0;1;0] ; [0;1;1] ; [0;1;2]] ;
			 "decreasing", mk [3;1;0], mkl [[1;0;0] ; [2;0;0] ; [3;0;0] ; [0;1;0] ; [1;1;0] ; [2;1;0]] ;
			 "any size 3", mk [1;2;1], mkl [[0;0;1] ; [0;1;0] ; [0;1;1] ; [0;2;0] ; [0;2;1] ; [1;0;0] ; [1;0;1] ; [1;1;0] ; [1;1;1] ; [1;2;0]]  ;
			 "any size 4", mk [1;0;2;1], mkl [[0;0;0;1] ; [0;0;1;1] ; [0;0;2;1] ; [1;0;0;0] ; [1;0;1;0] ; [1;0;1;1] ; [1;0;0;1] ; [1;0;2;0] ; [0;0;1;0] ; [0;0;2;0]] ;
		  ] in
		   T.suite "get_preds" (List.map chk tcs)

	let le_ts : T.testT
		 = let chk : string * int * int * IndexBuild.Liste.t -> T.testT
		= fun (nm, dim, val_max, eil) state ->
		let ail = IndexBuild.Liste.le dim val_max in
		T.equals nm IndexBuild.Liste.to_string IndexBuild.Liste.equal eil ail state
		   in
		   let tcs : (string * int * int * IndexBuild.Liste.t) list
		= [
			 "dim null", 0, 12, [] ;
			 "val null", 5, 0, [] ;
			 "dim 2", 2, 3, mkl [[1;0];[2;0];[3;0];[0;1];[0;2];[0;3];[1;1];[1;2];[2;1]] ;
			 "dim 3", 3, 2, mkl [[1;0;0];[0;1;0];[0;0;1];[1;1;0];[1;0;1];[0;1;1];[2;0;0];[0;2;0];[0;0;2]]
		  ] in
		   T.suite "le" (List.map chk tcs)

	let ts : T.testT
		= [
	    components_ts ;
		 get_preds_ts ;
		 le_ts
	  	] |> T.suite "Liste"
end

module Map = struct

	let check_map : IndexBuild.Map.t -> IndexBuild.Liste.t -> bool
		= fun map il ->
		List.for_all
			(fun i ->
				Index.Int.one_coeff_nn i
			 ||
					(IndexBuild.MapI.mem i map
			  	 &&
					(Index.Int.equal
					i
					(Index.Int.sumI (IndexBuild.MapI.find i map))))
			)
			il
	let compute_ts : T.testT
		= let chk : string * IndexBuild.Liste.t -> T.testT
		= fun (nm, il) state ->
		let map = IndexBuild.Map.compute il in
			if check_map map il
			then T.succeed state
			else T.fail nm (Printf.sprintf "map check failed : from index list %s\nmap : %s\n"
				(IndexBuild.Liste.to_string il)
				(IndexBuild.Map.to_string map)) state
		   in
		   let tcs : (string * IndexBuild.Liste.t) list
		= [
		  	 "one value", mkl [[2]] ;
		  	 "one nonnull coeff", mkl [[0;2;0;0]] ;
			 "singleton", mkl [[7;1]] ;
			 "included", mkl [[7;12;1] ; [6;10;0]] ;
			 "disjoint", mkl [[7;1;0;2;0] ; [0;0;3;0;2]] ;
			 "redundancy", mkl [[7;1;0;2;0] ; [0;0;3;0;2] ; [7;1;0;2;0]] ;
			 "longer", mkl [[4;1;2] ; [2;1;4] ; [3;1;6] ; [5;1;2] ; [6;2;0] ; [1;2;3] ; [2;1;4]]
		  ] in
		   T.suite "compute" (List.map chk tcs)

	let init_map = IndexBuild.Map.compute (mkl [[4;1;2] ; [2;1;4] ; [3;1;6] ; [5;1;2] ; [6;2;0] ; [1;2;3] ; [2;1;4]])

	let compute_list_from_map_ts : T.testT
		= let chk : string * IndexBuild.Liste.t * IndexBuild.Map.t -> T.testT
		= fun (nm, il, map) state ->
		let map = IndexBuild.Map.compute_list_from_map il map in
			if check_map map il
			then T.succeed state
			else T.fail nm (Printf.sprintf "map check failed : from index list %s\nmap : %s\n"
				(IndexBuild.Liste.to_string il)
				(IndexBuild.Map.to_string map)) state
		   in
		   let tcs : (string * IndexBuild.Liste.t * IndexBuild.Map.t) list
		= [
		  	 "one value", mkl [[2]], init_map ;
		  	 "one nonnull coeff", mkl [[0;2;0;0]], init_map ;
			 "singleton", mkl [[7;1]], init_map ;
			 "included", mkl [[7;12;1] ; [6;10;0]], init_map ;
			 "disjoint", mkl [[7;1;0;2;0] ; [0;0;3;0;2]], init_map ;
			 "redundancy", mkl [[7;1;0;2;0] ; [0;0;3;0;2] ; [7;1;0;2;0]], init_map ;
			 "longer", mkl [[4;1;2] ; [2;1;4] ; [3;1;6] ; [5;1;2] ; [6;2;0] ; [1;2;3] ; [2;1;4]], init_map ;
			 "no map", mkl [[4;1;2] ; [2;1;4] ; [3;1;6] ; [5;1;2] ; [6;2;0] ; [1;2;3] ; [2;1;4]], IndexBuild.MapI.empty
		  ] in
		   T.suite "compute_list_from_map" (List.map chk tcs)

	let compute_from_map_ts : T.testT
		= let chk : string * Index.Int.t * IndexBuild.Map.t -> T.testT
		= fun (nm, ind, map) state ->
		let (il,map) = IndexBuild.Map.compute_from_map ind map in
			if check_map map [ind] && (Index.Int.one_coeff_nn ind || Index.Int.equal ind (Index.Int.sumI (il)))
			then T.succeed state
			else T.fail nm (Printf.sprintf "map check failed : from index %s\nmap : %s\n"
				(Index.Int.to_string ind)
				(IndexBuild.Map.to_string map)) state
		   in
		   let tcs : (string * Index.Int.t * IndexBuild.Map.t) list
		= [
		  	 "one value", mk [2;1;2], init_map ;
			 "singleton", mk [7;1;3], init_map ;
			 "one nonnull coeff", mk [0;2;0], init_map ;
			 "no map", mk [2;3], IndexBuild.MapI.empty
		  ] in
		   T.suite "compute_from_map" (List.map chk tcs)

	let ts : T.testT
		= [
	   	compute_ts;
	   	compute_list_from_map_ts;
	   	compute_from_map_ts
	  	] |> T.suite "Map"
end

let ts : T.testT
		= [
		 Liste.ts;
		 Map.ts
	  	] |> T.suite "IndexBuild"
