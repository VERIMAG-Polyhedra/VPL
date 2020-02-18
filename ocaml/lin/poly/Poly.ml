open Poly_type

exception Div_by_non_constant

module type Type = Type

module Make (Vec: Vector.Type) = struct

    module Vec = Vec
    module Coeff = Vec.Coeff

	type exp = int (* >= 0*)

	module MonomialBasis = struct

		type t = (Var.t * exp) list

        let null : t = []

        let is_null : t -> bool
            = fun m ->
            m = []

		let to_string_param : t -> string -> string
			= fun m s ->
			String.concat ""
				(List.map
					(fun (v,e) ->
					match e with
					| 0 | 1 -> Var.to_string' s v
					| _ -> Printf.sprintf "%s^%i" (Var.to_string' s v) e)
					m)

		let to_string : t -> string
			= fun m ->
			to_string_param m "x"

        let rec get_exponent : Var.t -> t -> int
            = fun var -> function
            | [] -> 0
            | (v,e) :: tl ->
                let i = Var.cmp var v in
                if i = 0
                then e
                else if i < 0
                    then 0
                    else get_exponent var tl

		let rec compare : t -> t -> int
			= fun m1 m2 ->
			match m1, m2 with
			| [], [] -> 0
			| [], _ :: _ -> -1
 		 	| _ :: _, [] -> 1
 		 	| (x1,e1) :: m1', (x2,e2) :: m2' ->
     		   	let i = Var.cmp x1 x2 in
     		    	if i = 0
     		    	then
     		    		let j = Stdlib.compare e1 e2 in
     		    		if j = 0
    		     		then compare m1' m2'
    		     		else j
    		     	else i

		let equal : t -> t -> bool
			= fun m1 m2 ->
			compare m1 m2 = 0

		let rename : t -> Var.t -> Var.t -> t
			= fun m v v' ->
			List.map (fun (var,e) ->
                if Var.equal var v
                then (v',e)
                else (var,e)
            ) m

		let eval : t -> (Var.t -> Coeff.t) -> Coeff.t
			= fun m f_eval ->
			List.fold_left (fun c (v,e) ->
                Coeff.mul c (Coeff.pow (f_eval v) e)
            ) Coeff.u m

        let degree : t -> int
            = fun m ->
            List.fold_left (fun acc (_,e) ->
                acc + e
            ) 0 m

        let is_linear : t -> bool
            = fun m ->
            degree m <= 1

		let well_formed : t -> bool
			= fun m ->
			List.for_all (fun (v,e) ->
                Var.cmp v Var.u >= 0 && e >= 0
            ) m

		let mk : (Var.t * exp) list -> t
			= fun l ->
			if well_formed l
            then
                let m = List.filter (fun (_,e) -> e > 0) l
                |> List.fast_sort (fun (v1,_) (v2,_) -> Var.cmp v1 v2)
                in
                List.fold_right (fun (v,e) m ->
                    match m with
                    | [] -> [v,e]
                    | (v',e') :: m' -> if Var.equal v v'
                        then (v, e+e') :: m'
                        else (v,e) :: m
                ) m []
			else invalid_arg ("SxPoly.Poly.MonomialBasis.mk")

        let rec add_var : (Var.t * exp) -> t -> t
            = fun (v,e) -> function
            | [] -> [v,e]
            | (v',e') :: l as m ->
                let i = Var.cmp v v' in
                if i = 0
                then (v, e + e') :: l
                else if i < 0
                    then (v,e) :: m
                    else (v',e') :: add_var (v,e) l

        let rec remove_var : Var.t -> t -> t
            = fun v -> function
            | [] -> []
            | (v',e') :: l as m ->
                let i = Var.cmp v v' in
                if i = 0
                then l
                else if i < 0
                    then m
                    else (v',e') :: remove_var v l

        let rec remove_var_exp : Var.t -> int -> t -> t
            = fun v e -> function
            | [] -> []
            | (v',e') :: l as m ->
                let i = Var.cmp v v' in
                if i = 0
                then let new_e = e' - e in
                    if new_e <= 0
                    then l
                    else (v,new_e) :: l
                else if i < 0
                    then m
                    else (v',e') :: remove_var_exp v e l

		let mk_expanded : Var.t list -> t
			= function
            | [] -> []
            | l ->
    			let l_sorted = List.fast_sort Var.cmp l in
                let res = List.fold_left (fun res v ->
    					let (v',e') = List.hd res in
    					if Var.equal v v'
    					then (v',e'+1) :: List.tl res
    					else (v,1)::res
                ) [List.hd l_sorted, 1] (List.tl l_sorted)
    			|> List.rev
    			in
                if well_formed res
    			then res
    			else invalid_arg ("SxPoly.Poly.MonomialBasis.mk_list")

		let to_list : t -> (Var.t * exp) list
			= fun m -> m

		let to_list_expanded : t -> Var.t list
			= fun m ->
			List.fold_left (fun res (v,e) ->
				res
				@
				Misc.init_list e	(fun _ -> v)
            ) [] m

        let change_variable : (t -> t option) -> t -> t
			= fun ch m ->
			match ch m with
			| Some m' -> m'
			| None -> m

        let partial_derivative : Var.t -> t -> t
            = fun var m ->
            (* Fold right to maintain order *)
            List.fold_right (fun (v,e) acc ->
                if Var.equal v var
                then if e > 1
                    then (v,e-1) :: acc
                    else acc
                else (v,e) :: acc
            ) m []

        let rec sub : t -> t -> (t * bool)
			= fun m1 m2 ->
			match (m1,m2) with
			| (_,[]) -> (m1,true)
			| ([],_) -> ([],false)
			| ((v1,e1)::tail1, (v2,e2)::tail2) ->
                if Var.equal v1 v2
				then
                    if e1 >= e2
                    then
                        if e1 = e2
                        then sub tail1 tail2
                        else
                            let (l,b) = sub tail1 tail2 in
                            ((v1, e1 - e2) :: l, b)
                    else ([],false)
				else
                    let (l,b) = sub tail1 m2 in
                    ((v1,e1)::l,b)

        let get_vars : t -> Var.Set.t
            = fun m ->
            List.map fst m
            |> Var.Set.of_list
	end

	module Monomial = struct

		type t = (MonomialBasis.t * Coeff.t)

        let null : t = (MonomialBasis.null, Coeff.z)

        let is_null : t -> bool
            = fun (m,c) ->
            MonomialBasis.is_null m
            && Coeff.isZ c

		let to_string : t -> string
			= fun m -> let (vlist, c) = m in
			match vlist with
			| [] -> Coeff.to_string c
			| _ -> if Coeff.equal c (Coeff.of_int 1)
				then MonomialBasis.to_string vlist
				else if Coeff.lt c (Coeff.of_int 0)
					then String.concat "" ["(";Coeff.to_string c;")*";MonomialBasis.to_string vlist]
					else String.concat "" [Coeff.to_string c ; "*" ; MonomialBasis.to_string vlist]

		let compare : t -> t -> int
			= fun (m1,c1) (m2,c2) ->
			let i = MonomialBasis.compare m1 m2 in
            if i = 0
            then Coeff.cmp c1 c2
            else i

		let equal : t -> t -> bool
			= fun (m1,c1) (m2,c2) ->
			MonomialBasis.equal m1 m2 && Coeff.equal c1 c2

		let canonO : t -> t option
			= fun (m, a) ->
  			if not (Coeff.well_formed_nonnull a)
  			then None
 			else Some (MonomialBasis.mk m, a)

		let canon : t -> t
		  = fun m ->
		  match canonO m with
		  | Some m' -> m'
		  | None -> invalid_arg ("SxPoly.SxPoly.Monomial.canon : " ^ (to_string m))

		let mk : MonomialBasis.t -> Coeff.t -> t
	  		= fun m a -> canon (m, a)

		let mk_list : (Var.t * exp) list -> Coeff.t -> t
	  		= fun m a -> canon (MonomialBasis.mk m, a)

	  	let mk_expanded : Var.t list -> Coeff.t -> t
	  		= fun m a -> canon (MonomialBasis.mk_expanded m, a)

		let data : t -> MonomialBasis.t * Coeff.t
			= fun (m,c) -> (m,c)

        let get_exponent : Var.t -> t -> int
            = fun var (m,_) ->
            MonomialBasis.get_exponent var m

        let mul : t -> t -> t
            = let rec mul_list l1 l2 =
            match l1,l2 with
            | [],[] -> []
            | [],x | x,[] -> x
            | ((v1,e1) as m1)::t1, ((v2,e2) as m2)::t2 ->
            (* we keep the variables in a sorted order *)
            let c = Var.cmp v1 v2 in
            if c = 0
            then (v1,(e1 + e2))::(mul_list t1 t2)
            else if c < 0
                then m1::(mul_list t1 l2)
                else m2::(mul_list l1 t2)
        in
        fun (m1, c1) (m2, c2) ->
        let coeff = Coeff.mul c1 c2 in
        if Coeff.isZ coeff
        then null
        else (mul_list m1 m2), coeff

		let is_constant : t -> bool
            = fun (m,_) ->
            MonomialBasis.is_null m

		let is_linear : t -> bool
            = fun (m,_) ->
            MonomialBasis.is_linear m

		let eval : t -> (Var.t -> Coeff.t) -> Coeff.t
			= fun (m,c) e ->
			if is_constant (m,c)
			then c
			else Coeff.mul (MonomialBasis.eval m e) c

		let eval_partial : t -> (Var.t -> Coeff.t option) -> t
			= fun (m,c) f_eval ->
			List.fold_left (fun (m',c') (v,e) ->
                match (f_eval v) with
				| Some c2 -> mul (m',c') (MonomialBasis.null, c2)
				| None -> mul (m',c') ([v,e], Coeff.u)
			) ([], c) m

        let get_vars : t -> Var.Set.t
            = fun (m,_) ->
            MonomialBasis.get_vars m

        let change_variable : (MonomialBasis.t -> MonomialBasis.t option) -> t -> t
			= fun ch (m,c) ->
			(MonomialBasis.change_variable ch m, c)

        let partial_derivative : Var.t -> t -> t
            = fun var (m,c) ->
            if List.exists (fun (v,_) -> Var.equal var v) (MonomialBasis.to_list m)
            then (MonomialBasis.partial_derivative var m, c)
            else null
	end

	type t = Monomial.t list

	let compare : t -> t -> int
		= fun p1 p2 ->
			match (p1,p2) with
			| ([],[]) -> 0
			| (_,[]) -> 1
			| ([],_) -> -1
			| (m1::tl1, m2::tl2) -> let x = Monomial.compare m1 m2 in
			match x with
				| 0 -> compare tl1 tl2
				| _ -> x

	let to_string : t -> string
	  = fun p ->
	  List.map Monomial.to_string p
	  |> String.concat " + "
	  |> fun s -> if String.length s = 0 then "0" else s

	let canon : t -> t
  		= let rec (collapseDups : t -> t)
      		= function
      			| [] | _ :: [] as p -> p
      			| m :: (m' :: p' as p) ->
                let (mb,_) = m
                and (mb',_) = m' in
	 			if MonomialBasis.compare mb mb' = 0
	 			then collapseDups ((fst m, Coeff.add (snd m) (snd m')) :: p')
	 			else m :: collapseDups p
    		in
    		let fixConstant
     		 = fun p ->
      			let (cst, m) = List.partition (fun (m, _) -> MonomialBasis.compare m MonomialBasis.null = 0) p in
     	 			([], List.fold_left (fun n (_, a) -> Coeff.add n a) Coeff.z cst) :: m
    		in
    		fun p ->
    		fixConstant p
    		|> List.filter (fun (_, a) -> Coeff.well_formed_nonnull a)
    		|> List.map Monomial.canonO
    		|> List.map
				(function Some m -> m
				| None ->
					to_string p
					|> Printf.sprintf "SxPoly.canon: Monomial.canon on %s"
					|> failwith)
    		|> List.sort Monomial.compare
    		|> collapseDups
    		|> List.filter (fun (_, a) ->
				if Coeff.equal a Coeff.z
				then false
				else if Coeff.well_formed_nonnull a
					then true
					else
						to_string p
						|> Printf.sprintf "SxPoly.canon: Coeff.well_formed_nonnull on %s"
						|> failwith)
			|> function _ :: _ as p' -> p' | [] -> [[], Coeff.z]


	let mk : Monomial.t list -> t
		= fun l -> canon l

	let mk_list : ((Var.t * exp) list * Coeff.t) list -> t
		= fun l -> canon l

	let mk_expanded_list : (Var.t list * Coeff.t) list -> t
		= fun l ->
		List.map
			(fun (vars,c) -> Monomial.mk_expanded vars c)
			l
		|> canon

    let fromVar : Var.t -> t
		= fun v ->
		mk_list [([v,1], Coeff.u)]

	let data : t -> Monomial.t list
		= fun p -> p

	let to_list_expanded : t -> (Var.t list * Coeff.t) list
		= fun p ->
		List.map (fun (m,c) ->
            MonomialBasis.to_list_expanded m, c
        ) p

	let cste : Coeff.t -> t
		= fun i ->
		[(MonomialBasis.null,i)] |> canon

	let z : t
		= cste Coeff.z

	let u : t
		= cste Coeff.u

	let negU : t
        = cste Coeff.negU

	let is_constant : t -> bool
		= fun p ->
		match p with
		| [] -> true
		| [m] -> Monomial.is_constant m
		| _ :: _ -> false

	let isZ : t -> bool
		= fun p ->
		if p = [] then true (* nécessaire? *)
		else if List.length p = 1
			then let (mono,coeff) = List.hd p in
				MonomialBasis.compare mono MonomialBasis.null = 0 && Coeff.equal coeff Coeff.z
			else false

	let is_linear : t -> bool
        = List.for_all Monomial.is_linear

    let get_max_exponent : Var.t -> t -> int
        = fun var p ->
        List.map (Monomial.get_exponent var) p
        |> Misc.max Stdlib.compare

    let change_variable : (MonomialBasis.t -> MonomialBasis.t option) -> t -> t
        = fun ch l ->
        List.map (Monomial.change_variable ch) l
        |> canon

	let add : t -> t -> t
		= let rec add_rec
			= fun p1 p2 ->
			match (p1,p2) with
			| ([],poly2) -> poly2
			| (poly1,[]) -> poly1
			| ((m1,c1) :: tail1, (m2,c2) :: tail2) ->
				let comp = MonomialBasis.compare m1 m2 in
				if comp = 0
				then if Coeff.equal (Coeff.add c1 c2) Coeff.z
					then add_rec tail1 tail2
					else (m1,Coeff.add c1 c2)::(add_rec tail1 tail2)
				else if comp < 0 (*m1 < m2*)
					then (m1,c1)::(add_rec tail1 ((m2,c2)::tail2))
					else (m2,c2)::(add_rec ((m1,c1)::tail1) tail2)
		in fun p1 p2 ->
		add_rec p1 p2 |> canon

    let add_cste : t -> Coeff.t -> t
		= fun p cst ->
        add [MonomialBasis.null, cst] p

	let mul : t -> t -> t
		= let rec mul_rec
			= fun p1 p2 ->
			match (p1,p2) with
			| ([],_) -> []
			| (m::tail1,p2) -> List.fold_left
				add (mul_rec tail1 p2) (List.map (fun m2 -> [Monomial.mul m m2]) p2)
		in fun p1 p2 ->
		mul_rec p1 p2 |> canon

	(* XXX: naïve implem*)
	let mulc : t -> Coeff.t -> t
		= fun p c ->
		mul p (cste c)

    let div : t -> t -> t
        = fun p1 p2 ->
        if is_constant p2
        then
            let (_,c) = List.hd p2 in
            mulc p1 (Coeff.div Coeff.u c)
        else
            raise Div_by_non_constant

	let neg : t -> t
		= fun p ->
		mulc p Coeff.negU

	(* XXX: naïve implem *)
	let sub : t -> t -> t
		= fun p1 p2 ->
		add p1 (mul negU p2)

	let sum : t list -> t
		= fun l ->
		List.fold_left (fun r p -> add r p) z l

	let prod : t list -> t
		= fun l ->
		List.fold_left (fun r p -> mul r p) u l

	let pow : t -> int -> t
		= fun p i ->
		Misc.init_list i (fun _ -> p) |> prod

	let equal : t -> t -> bool
		= fun p1 p2 ->
		List.length p1 = List.length p2
		&&
		List.for_all2 Monomial.equal p1 p2

	let rename : t -> Var.t -> Var.t -> t
		= fun p v v'->
		List.map (fun (m,c) -> (MonomialBasis.rename m v v',c)) p
		|> canon

	let rec monomial_coefficient : t -> MonomialBasis.t -> Coeff.t
		= fun p m ->
		match (p,m) with
		| ([],_) -> Coeff.z
		| ((m1,c)::tail, m2) ->
            let cmp = MonomialBasis.compare m1 m2 in
            if cmp = 0
			then c
			else if cmp < 0
				then monomial_coefficient tail m
				else Coeff.z

    let get_constant : t -> Coeff.t
		= fun p ->
		monomial_coefficient p MonomialBasis.null

	let sub_monomial : t -> MonomialBasis.t -> t
		= fun p m ->
		Misc.pop
			(fun (m1,_) (m2,_) -> MonomialBasis.equal m1 m2)
			p (m,Coeff.z)

    let monomial_coefficient_poly : t -> MonomialBasis.t -> t
		= let rec monomial_coefficient_poly_rec : t -> MonomialBasis.t -> t
			= fun p m ->
			match p with
			| [] -> []
			| (m1,c)::tail ->
                if List.length m1 >= List.length m
                    && MonomialBasis.compare (Misc.sublist m1 0 (List.length m)) m > 0 (* m1 > m *)
				then []
				else let (l,b) = MonomialBasis.sub m1 m in
                    if b
					then add [(l,c)] (monomial_coefficient_poly_rec tail m)
					else monomial_coefficient_poly_rec tail m
		in fun p m ->
		monomial_coefficient_poly_rec p m |> canon

	let get_linear_part : t -> Var.t list -> t
		= fun p _ ->
		List.filter Monomial.is_linear p
        |> canon

	let get_vars : t -> Var.Set.t
		= fun p ->
		List.fold_left (fun acc m ->
            Monomial.get_vars m
            |> Var.Set.union acc
        ) Var.Set.empty p

    let horizon : t list -> Var.t
        = fun l ->
		List.fold_left (fun acc p ->
            get_vars p
            |> Var.Set.union acc
        ) Var.Set.empty l
		|> Var.horizon

	let eval : t -> (Var.t -> Coeff.t) -> Coeff.t
		= fun p e ->
		List.fold_left (fun c m ->
            Coeff.add c (Monomial.eval m e)
        ) Coeff.z p

	let eval_partial : t -> (Var.t -> Coeff.t option) -> t
		= fun p e ->
		List.fold_left (fun p m ->
            add p [(Monomial.eval_partial m e)]
        ) [] p

    let partial_derivative : Var.t -> t -> t
        = fun var p ->
        List.fold_left
            (fun acc m ->
                let (m,c) = Monomial.partial_derivative var m in
                if Coeff.isZ c
                then acc
                else (m,c) :: acc)
            z p
        |> canon

    let gradient : t -> t Rtree.t
        = fun p ->
        List.fold_left
            (fun tree var ->
                Rtree.set z tree var (partial_derivative var p))
            Rtree.empty
            (get_vars p |> Var.Set.elements)

	let toCstr : t -> (Vec.t * Coeff.t)
		= fun p ->
		if is_linear p
		then
			let vec = List.fold_left (fun l (m,c) ->
        			if not (MonomialBasis.is_null m)
        			then (c, List.hd m |> fst) :: l
        			else l
                ) [] (List.map Monomial.data (data p))
				|> Vec.mk
			and cste = get_constant p
			in
			(vec,cste)
		else invalid_arg "handelman.polyToCstr: not affine polynomial"

	let ofCstr : Vec.t -> Coeff.t -> t
		= fun vec cste ->
		let l = vec
		|> Vec.toList
			|> List.map (fun (x,n) -> Monomial.mk_list [x,1] n)
			|> mk
		in
		add_cste l cste

	let of_string : string -> t
   	    = fun s ->
    	PolyParser.one_prefixed_poly PolyLexer.token2 (Lexing.from_string s)
    	|> List.map (fun (vl,q) -> (vl, Coeff.ofQ q))
        |> mk_expanded_list

	module Invariant = struct

  		let rec helper_sorted : ('a -> 'a -> bool) -> 'a list -> bool
	       = fun f ->
        	function
       		| [] | _ :: [] -> true
       		| x1 :: ((x2 :: _) as l') -> f x1 x2 && helper_sorted f l'

  		module Monom = struct

    		let check_sorted : Monomial.t -> bool
      		    = fun (l, _) ->
                helper_sorted (fun (x,_) (x',_) -> Var.cmp x x' <= 0) l

    		let check : Monomial.t -> bool
      		    = fun (vlist,c) ->
                Coeff.well_formed_nonnull c && check_sorted (vlist,c)
  		end

  		(* strict negativity enforces no duplicates *)
		let check_sorted : t -> bool
    	   = helper_sorted (fun m m' -> Monomial.compare m m' < 0)

  		let check : t -> bool
    		= fun p ->
    		match p with
    		| [] -> false
    		| [[], a] when Coeff.equal a Coeff.z -> true
    		| _ -> List.for_all Monom.check p && check_sorted p

  		let checkOrFail : t -> unit
    		= fun p ->
    		if not (check p)
    		then p
	 		|> to_string
	 		|> Printf.sprintf "SxPoly.Invariant.checkOrFail: %s"
	 		|> failwith
	end

end
