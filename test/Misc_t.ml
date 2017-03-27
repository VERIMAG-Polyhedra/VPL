open Vpl

  let sublist_ts : T.testT
  	= let chk (nm, l, b, e, el) = fun state ->
  			let eq = (fun l l' ->
		      try List.for_all2 (=) l l'
		      with Invalid_argument _ -> false)
		   in
			let al = Misc.sublist l b e in
			if eq el al  then
				T.succeed state
			else
				let e = Printf.sprintf "[%s]"(String.concat ";" (List.map Pervasives.string_of_int l)) in
				T.fail nm e state
		in
      let tcs : (string * int list * int * int * int list) list
	= [
	"regular", [0;1;2;3;4], 0, 2, [0;1];
	"invalid0", [], 0, 2, [];
	"invalid1", [], 2, -1, [];
	"invalid3", [0;1;2;3;4], 2, 1, []
      ] in
      T.suite "sublist" (List.map chk tcs)

  let ts : T.testT
    = [
    sublist_ts
  ] |> T.suite "Misc"
