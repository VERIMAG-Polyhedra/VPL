open Vpl

let mk = Index.Int.mk ;;
let mkl l = List.map Index.Int.mk l;;

module IndexList = struct

	let components_ts : Test.t
		= fun () ->
        let chk : string * Index.Int.t * IndexBuild.IndexList.t -> (Test.stateT -> Test.stateT)
		= fun (nm, ind, eil) state ->
		let ail = IndexBuild.IndexList.components ind in
		Test.equals nm IndexBuild.IndexList.to_string IndexBuild.IndexList.equal eil ail state
		   in
		   let tcs : (string * Index.Int.t * IndexBuild.IndexList.t) list
		= [
			 "empty", mk [], [] ;
			 "singleton", mk [7], mkl [[1];[1];[1];[1];[1];[1];[1]] ;
			 "increasing", mk [0;1;3], mkl [[0;1;0] ; [0;0;1] ; [0;0;1] ; [0;0;1]] ;
			 "decreasing", mk [3;1;0], mkl [[0;1;0] ; [1;0;0] ; [1;0;0] ; [1;0;0]] ;
			 "any", mk [1;2;1], mkl [[1;0;0] ; [0;1;0] ; [0;1;0] ; [0;0;1]] ;
		  ] in
		   Test.suite "components" (List.map chk tcs)

	let get_preds_ts : Test.t
		= fun () ->
        let chk : string * Index.Int.t * IndexBuild.IndexList.t -> (Test.stateT -> Test.stateT)
		= fun (nm, ind, eil) state ->
		let ail = IndexBuild.IndexList.get_preds ind in
		Test.equals nm IndexBuild.IndexList.to_string IndexBuild.IndexList.equal eil ail state
		   in
		   let tcs : (string * Index.Int.t * IndexBuild.IndexList.t) list
		= [
			 "empty", mk [], [] ;
			 "singleton", mk [7], mkl [[6];[5];[4];[3];[2];[1]] ;
			 "increasing", mk [0;1;3], mkl [[0;0;1] ; [0;0;2] ; [0;0;3] ; [0;1;0] ; [0;1;1] ; [0;1;2]] ;
			 "decreasing", mk [3;1;0], mkl [[1;0;0] ; [2;0;0] ; [3;0;0] ; [0;1;0] ; [1;1;0] ; [2;1;0]] ;
			 "any size 3", mk [1;2;1], mkl [[0;0;1] ; [0;1;0] ; [0;1;1] ; [0;2;0] ; [0;2;1] ; [1;0;0] ; [1;0;1] ; [1;1;0] ; [1;1;1] ; [1;2;0]]  ;
			 "any size 4", mk [1;0;2;1], mkl [[0;0;0;1] ; [0;0;1;1] ; [0;0;2;1] ; [1;0;0;0] ; [1;0;1;0] ; [1;0;1;1] ; [1;0;0;1] ; [1;0;2;0] ; [0;0;1;0] ; [0;0;2;0]] ;
		  ] in
		   Test.suite "get_preds" (List.map chk tcs)

	let le_ts : Test.t
		= fun () ->
        let chk : string * int * int * IndexBuild.IndexList.t -> (Test.stateT -> Test.stateT)
		= fun (nm, dim, val_max, eil) state ->
		let ail = IndexBuild.IndexList.le dim val_max in
		Test.equals nm IndexBuild.IndexList.to_string IndexBuild.IndexList.equal eil ail state
		   in
		   let tcs : (string * int * int * IndexBuild.IndexList.t) list
		= [
			 "dim null", 0, 12, [] ;
			 "val null", 5, 0, [] ;
			 "dim 2", 2, 3, mkl [[1;0];[2;0];[3;0];[0;1];[0;2];[0;3];[1;1];[1;2];[2;1]] ;
			 "dim 3", 3, 2, mkl [[1;0;0];[0;1;0];[0;0;1];[1;1;0];[1;0;1];[0;1;1];[2;0;0];[0;2;0];[0;0;2]]
		  ] in
		   Test.suite "le" (List.map chk tcs)

	let ts : Test.t
		= fun () -> [
	     components_ts() ;
		 get_preds_ts() ;
		 le_ts()
	  	] |> Test.suite "Liste"
end

module Map = struct

	let check_map : IndexBuild.Map.t -> IndexBuild.IndexList.t -> bool
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

	let compute_ts : Test.t
		= fun () ->
        let chk : string * IndexBuild.IndexList.t -> (Test.stateT -> Test.stateT)
		= fun (nm, il) state ->
		let map = IndexBuild.Map.compute il in
			if check_map map il
			then Test.succeed state
			else Test.fail nm (Printf.sprintf "map check failed : from index list %s\nmap : %s\n"
				(IndexBuild.IndexList.to_string il)
				(IndexBuild.Map.to_string map)) state
		   in
		   let tcs : (string * IndexBuild.IndexList.t) list
		= [
		  	 "one value", mkl [[2]] ;
		  	 "one nonnull coeff", mkl [[0;2;0;0]] ;
			 "singleton", mkl [[7;1]] ;
			 "included", mkl [[7;12;1] ; [6;10;0]] ;
			 "disjoint", mkl [[7;1;0;2;0] ; [0;0;3;0;2]] ;
			 "redundancy", mkl [[7;1;0;2;0] ; [0;0;3;0;2] ; [7;1;0;2;0]] ;
			 "longer", mkl [[4;1;2] ; [2;1;4] ; [3;1;6] ; [5;1;2] ; [6;2;0] ; [1;2;3] ; [2;1;4]]
		  ] in
		   Test.suite "compute" (List.map chk tcs)

	let compute_from_map_ts : Test.t
		= fun () ->
        let chk : string * Index.Int.t * IndexBuild.Map.t -> (Test.stateT -> Test.stateT)
		= fun (nm, ind, map) state ->
		let (il,map) = IndexBuild.Map.compute_from_map ind map in
			if check_map map [ind] && (Index.Int.one_coeff_nn ind || Index.Int.equal ind (Index.Int.sumI (il)))
			then Test.succeed state
			else Test.fail nm (Printf.sprintf "map check failed : from index %s\nmap : %s\n"
				(Index.Int.to_string ind)
				(IndexBuild.Map.to_string map)) state
		   in
		   let init_map = IndexBuild.Map.compute (mkl [[4;1;2] ; [2;1;4] ; [3;1;6] ; [5;1;2] ; [6;2;0] ; [1;2;3] ; [2;1;4]]) in
		   let tcs : (string * Index.Int.t * IndexBuild.Map.t) list
		= [
		  	 "one value", mk [2;1;2], init_map ;
			 "singleton", mk [7;1;3], init_map ;
			 "one nonnull coeff", mk [0;2;0], init_map ;
			 "no map", mk [2;3], IndexBuild.MapI.empty
		  ] in
		   Test.suite "compute_from_map" (List.map chk tcs)

   let compute_list_from_map_ts : Test.t
		= fun () ->
       let chk : string * IndexBuild.IndexList.t * IndexBuild.Map.t -> (Test.stateT -> Test.stateT)
		= fun (nm, il, map) state ->
		let map = IndexBuild.Map.compute_list_from_map il map in
			if check_map map il
			then Test.succeed state
			else Test.fail nm (Printf.sprintf "map check failed : from index list %s\nmap : %s\n"
				(IndexBuild.IndexList.to_string il)
				(IndexBuild.Map.to_string map)) state
		   in
		   let init_map = IndexBuild.Map.compute (mkl [[4;1;2] ; [2;1;4] ; [3;1;6] ; [5;1;2] ; [6;2;0] ; [1;2;3] ; [2;1;4]]) in
		   let tcs : (string * IndexBuild.IndexList.t * IndexBuild.Map.t) list
		= [
			 "singleton", mkl [[5;1;0]], init_map ;
		  	 "one nonnull coeff", mkl [[0;5;0]], init_map ;
			 "included", mkl [[7;12;1] ; [6;10;0]], init_map ;
			 "already_in", mkl [[6;2;0]], init_map ;
			 "disjoint", mkl [[7;0;3] ; [0;3;0]], init_map ;
			 "redundancy", mkl [[7;1;0] ; [3;0;2] ; [7;1;0]], init_map ;
			 "longer", mkl [[4;1;2] ; [2;1;4] ; [3;1;6] ; [5;1;2] ; [6;2;0] ; [1;2;3] ; [2;1;4]], init_map ;
			 "no map", mkl [[4;1;2] ; [2;1;4] ; [3;1;6] ; [5;1;2] ; [6;2;0] ; [1;2;3] ; [2;1;4]], IndexBuild.MapI.empty
		  ] in
		   Test.suite "compute_list_from_map" (List.map chk tcs)


	let ts : Test.t
		= fun () -> [
	   	 compute_ts();
	   	 compute_from_map_ts();
		 compute_list_from_map_ts();
	  	] |> Test.suite "Map"

end

let ts : Test.t
		= fun () -> [
		 IndexList.ts();
		 Map.ts()
	  	] |> Test.suite "IndexBuild"
