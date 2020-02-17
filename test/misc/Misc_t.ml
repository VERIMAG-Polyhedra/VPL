open Vpl

  let sublist_ts : Test.t
  	= fun () ->
    let chk (nm, l, b, e, el) = fun state ->
  			let eq = (fun l l' ->
		      try List.for_all2 (=) l l'
		      with Invalid_argument _ -> false)
		   in
			let al = Misc.sublist l b e in
			if eq el al  then
				Test.succeed state
			else
				let e = Printf.sprintf "[%s]"(String.concat ";" (List.map Stdlib.string_of_int l)) in
				Test.fail nm e state
		in
      let tcs : (string * int list * int * int * int list) list
	= [
	"regular", [0;1;2;3;4], 0, 2, [0;1];
	"invalid0", [], 0, 2, [];
	"invalid1", [], 2, -1, [];
	"invalid3", [0;1;2;3;4], 2, 1, []
      ] in
      Test.suite "sublist" (List.map chk tcs)

  let ts : Test.t
    = fun () ->
    [ sublist_ts () ] |> Test.suite "Misc"
