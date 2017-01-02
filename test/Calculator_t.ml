(* TODO: il faut mettre à jour ce fichier avec les nouvelles interfaces. *)
open Calculator

let var s = Ident.toVar s

let coeff = Poly.Coeff.mk1 

let parser_ts : T.testT
	= let parse s = 
		PolyParser.one_poly PolyLexer.token (Lexing.from_string s)
		|> Expr.to_poly
	in
	let chk (nm, s, ep) = 
		fun state -> 
		let ap = parse s in
		let ep = Lazy.force ep in
		if Poly.equal ep ap
		then T.succeed state
		else 
		let error_string = Printf.sprintf "%s: expected %s but got %s" nm (Poly.to_string ep) (Poly.to_string ap) in
		T.fail nm error_string state
	in
	let tcs : (string * string * Poly.t Lazy.t ) list
	= [
		"empty", "", (lazy (Poly.mk [])) ;
		"one var 1", "x", (lazy (Poly.mk3 [[var "x",1],coeff 1])) ;
		"one var 2", "2x", (lazy (Poly.mk3 [[var "x",1],coeff 2])) ;
		"one var 3", "2*x", (lazy (Poly.mk3 [[var "x",1],coeff 2])) ;
		"two var 1","x + y",  (lazy (Poly.mk3 [[var "x",1 ],coeff 1; [ var "y",1],coeff 1])) ;
		"two var 2","2x + 4*y",  (lazy (Poly.mk3 [[var "x",1 ],coeff 2; [ var "y",1],coeff 4])) ;
		"two var 3","x + z*25",  (lazy (Poly.mk3 [[var "x",1 ],coeff 1; [ var "z",1],coeff 25])) ;
		"coeff","2x + 2*x", (lazy (Poly.mk3 [[var "x",1],coeff 4])) ;
		"multi var 1","2x + 2*z + -40y", (lazy (Poly.mk3 [[var "x",1 ],coeff 2; [ var "y",1],coeff (-40) ; [ var "z",1],coeff 2])) ;
		"var product 1","x*y", (lazy (Poly.mk3 [[var "x",1 ;var "y",1],coeff 1])) ;
		"var product 2","2x*2*y", (lazy (Poly.mk3 [[var "x",1 ;var "y",1],coeff 4])) ;
		"var product 2","x + 2x*2*y - z", (lazy (Poly.mk3 [[var "x",1 ;var "y",1],coeff 4;[var "x",1],coeff 1;[var "z",1],coeff (-1)])) ;
		"parentheses_var", "(x)", (lazy (Poly.mk3 [[var "x",1], coeff 1])) ;
		"parentheses_nb", "(3)", (lazy (Poly.mk3 [[], coeff 3])) ;
		"parentheses_sum", "(x+y)", (lazy (Poly.mk3 [[var "x",1], coeff 1 ; [var "y",1], coeff 1])) ;
		"parentheses_prod", "(x*y)", (lazy (Poly.mk3 [[var "x",1 ; var "y",1], coeff 1])) ;
	] in
	List.map chk tcs 
	|> T.suite "parser polynome"

module Cs = Pol.Cs
	let _ = Ident.addVars ["x";"y";"z"]
	let x = Ident.toVar "x"
	let y = Ident.toVar "y"
	let z = Ident.toVar "z"

let p (l: Cs.t list): VPL.P.t =
	match VPL.P.addM VPL.P.top l with
	| None -> failwith "Pol.mk returned None"
	| Some poly -> poly


	let varPr: Cs.Vec.V.t -> string
	= fun _x ->
		let vars: (Cs.Vec.V.t * string) list
		= [x, "x"; y, "y"; z, "z"]
		in
		try
			List.assoc _x vars
		with
		| Not_found -> "v" ^ (Cs.Vec.V.to_string _x)

	let mkc t v c =
		Cs.mk t (List.map (fun (i, v) -> (Cs.Vec.Coeff.mk1 i, v)) v) (Cs.Vec.Coeff.mk1 c)

	let eq = mkc Cstr.Eq
	let le = mkc Cstr.Le
	let lt = mkc Cstr.Lt

	let v iv = Cs.Vec.mk (List.map (fun (i, v) -> (Cs.Vec.Coeff.mk1 i, v)) iv)

let parser_pol_ts : T.testT
	= let equal : VPL.P.t -> VPL.P.t -> bool
		= fun p1 p2 ->
		VPL.P.incl p1 p2 && VPL.P.incl p2 p1
	in let chk (nm, s, ep) = 
		fun state -> 
		let ap = parse s |> Print.vpl_to_pol in
		let ep = Lazy.force ep in
		if equal ep ap
		then T.succeed state
		else 
		let error_string = Printf.sprintf "%s: expected %s but got %s" nm (VPL.P.to_string Ident.get_string  ep) (VPL.P.to_string Ident.get_string  ap) in
		T.fail nm error_string state
	in
	let tcs : (string * string * VPL.P.t Lazy.t ) list
		= [
		"empty", "", (lazy (p [])) ;
		"one var 1", "x = 1", (lazy (p [eq [1, x] 1])) ;
		"one var 2", "2z = 0", (lazy (p [eq [2, z] 0])) ;
		"two var 1", " 2x +  y = 8", (lazy (p [eq [2, x;1,y] 8])) ;
		"two var 2", "x=0 ;  y= 0", (lazy (p [eq [2, x] 0;eq [1, y] 0])) ;
		"two var 2", "-25*x +z = 3", (lazy (p [eq [(-25), x; 1, z] 3])) ;
		"three var 1", "-25*x +z = y", (lazy (p [eq [(-25), x; 1, z; -1,y] 0])) ;
		"three var 2", "x=0 ;  y<= z", (lazy (p [eq [1, x] 0; le [1, y; -1, z] 0])) ;
		"greater", "x - y + z > 4 ;  y >= z", (lazy (p [lt [-1, x;1,y;-1,z] (-4); le [-1, y; 1, z] 0])) ;
		"implicit equality", "x >= 8 , x <= 8", (lazy (p [eq [1, x] 8])) ;
		"multiple", "x >= 187/187 , y < 0", (lazy (p [le [-1, x] (-1); lt [1, y] 0])) ;
		"fraction N operations", " (2/3 + 5/3 - (-1/3))*x = -1 * (-10/3 + 4/6)", (lazy (p [eq [1,x] 1])) ;
		] 
	in
	List.map chk tcs 
	|> T.suite "parser polyhedre"



let parser_var_list_ts : T.testT
	= let parse = string_to_var_list 
	in
	let equal : string list -> string list -> bool
	= fun sl1 sl2 -> Misc.list_eq (fun s stl-> List.fold_left (fun b st -> b || (String.compare st s)=0) true stl) sl1 sl2
	and to_string : string list -> string =
		fun sl -> String.concat "," sl
	in
	let chk (nm, s, esl) = 
		fun state -> 
		let asl = parse s in
		if equal esl asl
		then T.succeed state
		else 
		let error_string = Printf.sprintf "%s: expected %s but got %s" nm (to_string esl) (to_string asl) in
		T.fail nm error_string state
	in
	let tcs : (string * string * string list ) list
	= [
		"empty", "", [] ;
		"one","x2",["x2"];
		"complicated","complicated123",["complicated123"];
		"multiple 1","x2,x1,co",["x2";"x1";"co"];
		"multiple 2","x2;x1;co",["x2";"x1";"co"];
		"multiple 3","x2,x1;co,coc",["x2";"x1";"co";"coc"];
		"multiple 3","x2,x1;co,coc",["x2";"x1";"co";"coc"];
		"doublon","x2,x1;co,coc;x1",["x2";"x1";"co";"coc"];
	]
	in
	List.map chk tcs 
	|> T.suite "parser var list"


let ts : T.testT
	= T.suite "Calculatrice" [
		parser_ts;
		parser_pol_ts;
		parser_var_list_ts;
	]

