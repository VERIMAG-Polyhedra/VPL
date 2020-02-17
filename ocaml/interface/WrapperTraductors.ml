module Annot = struct
    (* type des annotations possibles: à compléter ! *)
  	type t =
	| Old (* used in assign / guassign *)
	| Unused (* a default unused case representing future annotations *)

	let to_string : t -> string
  		= function
  		| Old -> "Old"
  		| Unused -> "Unused"
end

type cmpT = Cstr_type.cmpT_extended

type binl = AND | OR

let binl_to_string = function
	| AND -> "&&"
	| OR -> "||"

module CP = CstrPoly
module Polynomial = CP.Poly

module type TermType = sig
  type t

  val to_string : (Var.t -> string) -> t -> string

  val to_poly : t -> Polynomial.t

  val to_cp : cmpT * t -> CP.t
end

module type LowLevelDomain = sig

  module Term : TermType

  type cert

  type t

  val top: t

  val bottom: t

  val is_bottom: t -> bool

  (** If the polyhedron is bottom, returns the associated certificate. Otherwise, returns None. *)
  val get_bottom_cert : t -> cert option

  (* atomic assume *)
  val assume: (cmpT * Term.t) list -> t -> t

  val meet : t -> t -> t

  val join: t -> t -> t

  val minkowski : t -> t -> t

  val project: Var.t list -> t -> t

  val widen: t -> t -> t

  val proj_incl: t -> t -> t option

  val rename: Var.t -> Var.t -> t -> t

  (** Test the inclusion of two polyhedra. *)
  val leq: t -> t -> bool

  val to_string: (Var.t -> string) -> t -> string

  val getUpperBound : t -> Term.t -> Pol.bndT option

  val getLowerBound : t -> Term.t -> Pol.bndT option

  val itvize : t -> Term.t -> Pol.itvT

  type rep = PedraQOracles.t
  val backend_rep : t -> (rep * ((ProgVar.PVar.t -> ProgVar.PVar.t) * (ProgVar.PVar.t -> ProgVar.PVar.t))) option

  val get_regions : t -> t list
  val set_point : Vector.Rat.t -> t -> t
  val assume_back : (cmpT * Term.t) list -> t -> t option

end

module Interface (Coeff: Scalar.Type) = struct

	module Term = struct
	(* copié de VPLInterface2 *)

		type t =
		| Var of Var.t
		| Cte of Coeff.t
		| Add of t * t
		| Sum of t list
		| Opp of t
		| Mul of t * t
		| Prod of t list
        | Div of t * t
		| Poly of Polynomial.t
		| Annot of Annot.t * t

		let rec to_poly : t -> Polynomial.t
			= function
			| Var x -> Polynomial.fromVar x
			| Cte c -> Polynomial.cste (Coeff.toQ c)
			| Add  (p1,p2) -> Polynomial.add (to_poly p1) (to_poly p2)
			| Sum l -> Polynomial.sum (List.map to_poly l)
			| Opp p -> Polynomial.neg (to_poly p)
			| Mul (p1,p2) -> Polynomial.mul (to_poly p1) (to_poly p2)
			| Prod l -> Polynomial.prod (List.map to_poly l)
            | Div (p1,p2) -> Polynomial.div (to_poly p1) (to_poly p2)
			| Poly p -> p
			| Annot (_,p) -> to_poly p

		let to_cp : cmpT * t -> CP.t
			= fun (cmp,t) ->
			let p = to_poly t in
			let (cmp',p') =
			Cstr_type.(match cmp with
			| EQ -> (Eq, p)
			| LE -> (Le, Polynomial.neg p)
			| LT -> (Lt, Polynomial.neg p)
			| GE -> (Le, p)
			| GT -> (Lt, p)
			| NEQ -> Stdlib.failwith "VPLInterface.Term.to_cp: NEQ unimplemented")
			in
			CP.mk cmp' p'

		let rec to_string : (Var.t -> string) -> t -> string
			= fun varPr -> function
			| Var v -> varPr v
			| Cte c -> Coeff.to_string c
			| Add (t1,t2) -> Printf.sprintf "%s + %s" (to_string varPr t1) (to_string varPr t2)
			| Sum l -> String.concat " + " (List.map (to_string varPr) l)
			| Opp t -> Printf.sprintf "-(%s)" (to_string varPr t)
			| Mul (t1,t2) ->  Printf.sprintf "(%s) * (%s)" (to_string varPr t1) (to_string varPr t2)
			| Prod l -> String.concat " * " (List.map (fun t -> Printf.sprintf "(%s)" (to_string varPr t)) l)
            | Div (t1,t2) ->  Printf.sprintf "(%s) / (%s)" (to_string varPr t1) (to_string varPr t2)
            | Poly p -> Polynomial.to_string p
			| Annot (annot,t) -> Printf.sprintf "%s (%s)" (Annot.to_string annot) (to_string varPr t)

        let rec to_string_c : t -> string
			= function
			| Var v -> Var.to_string' "x" v
			| Cte c -> Coeff.to_string c
			| Add (t1,t2) -> Printf.sprintf "%s + %s" (to_string_c t1) (to_string_c t2)
			| Sum l -> String.concat " + " (List.map (to_string_c) l)
			| Opp t -> Printf.sprintf "-(%s)" (to_string_c t)
			| Mul (t1,t2) ->  Printf.sprintf "(%s) * (%s)" (to_string_c t1) (to_string_c t2)
			| Prod l -> String.concat " * " (List.map (fun t -> Printf.sprintf "(%s)" (to_string_c t)) l)
            | Div (t1,t2) ->  Printf.sprintf "(%s) / (%s)" (to_string_c t1) (to_string_c t2)
            | Poly p -> Polynomial.to_string p
			| Annot (annot,t) -> Printf.sprintf "%s (%s)" (Annot.to_string annot) (to_string_c t)

		let of_cstr : Pol.Cs.t -> t
			= fun cstr ->
			let l = Pol.Cs.get_v cstr
				|> Pol.Cs.Vec.toList
				|> List.map (fun (var,coeff) -> Mul (Var var, Cte (Coeff.ofQ coeff)))
			and c = Pol.Cs.get_c cstr |> Pol.Cs.Vec.Coeff.neg |> fun c -> Cte (Coeff.ofQ c)
			in
			Sum (c::l)

        let rec rename_f : (Var.t -> Var.t) -> t -> t
            = fun f -> function
            | Var v -> Var (f v)
            | Opp t -> Opp (rename_f f t)
			| Add (t1,t2) -> Add (rename_f f t1, rename_f f t2)
            | Mul (t1,t2) -> Mul (rename_f f t1, rename_f f t2)
            | Div (t1,t2) -> Div (rename_f f t1, rename_f f t2)
            | Sum l -> Sum (List.map (rename_f f) l)
			| Prod l -> Prod (List.map (rename_f f) l)
            | Poly _ -> invalid_arg "Term: rename_f"
			| Annot (annot,t) -> Annot (annot, rename_f f t)
            | t -> t
	end

  module Cond =
  struct

    type t =
      | Basic of bool
      | Atom of Term.t * cmpT * Term.t
      | BinL of t * binl * t
      | Not of t

    let rec to_string : (Var.t -> string) -> t -> string
    	= fun varPr -> function
      | Basic b -> string_of_bool b
      | Atom (t1,cmp,t2) -> Printf.sprintf "%s %s %s"
      	(Term.to_string varPr t1) (Cstr.cmpT_extended_to_string cmp) (Term.to_string varPr t2)
      | BinL (c1, bin, c2) -> Printf.sprintf "(%s %s %s)"
      	(to_string varPr c1) (binl_to_string bin) (to_string varPr c2)
      | Not c -> Printf.sprintf "¬ (%s)" (to_string varPr c)

    let rec to_string_c : t -> string
      = function
      | Basic b -> if b then "1" else "0"
      | Atom (t1,cmp,t2) -> Printf.sprintf "%s %s %s"
      	(Term.to_string_c t1) (Cstr.cmpT_extended_to_string cmp) (Term.to_string_c t2)
      | BinL (c1, bin, c2) -> Printf.sprintf "(%s %s %s)"
      	(to_string_c c1) (binl_to_string bin) (to_string_c c2)
      | Not c -> Printf.sprintf "! (%s)"  (to_string_c c)

    let of_cstrs : Pol.Cs.t list -> t
		= fun cstrs ->
		List.map Term.of_cstr cstrs
		|> List.fold_left
			(fun cond term -> let atom = Atom (term, Cstr_type.LE, Term.Cte Coeff.z) in
				BinL (cond, AND, atom))
			(Basic true)
  end

  (* je coupe "Type" en 2 (avec renommage en Domain) *)

  module type LowLevelDomain = LowLevelDomain with module Term = Term

  module type HighLevelDomain = sig

    include LowLevelDomain

    val assume: Cond.t -> t -> t

    val assume_back : Cond.t -> t -> t option

    val asserts: Cond.t -> t -> bool

    val assign: (Var.t * Term.t) list -> t -> t

    val guassign: (Var.t list) -> Cond.t -> t -> t

    val project_vars : Var.t list -> t -> t
  end

end

(* translation of basic datatypes *)
let import_certvar: Var.t -> ProgVar.PVar.t
= PedraQOracles.varToProgVar
let export_certvar: ProgVar.PVar.t -> Var.t
= PedraQOracles.progVarToVar

module CQNum = NumC.QNum
module CZNum = NumC.ZNum

let import_Q: Scalar.Rat.t -> CQNum.t
= PedraQOracles.nToNumC

let import_Z: Scalar.Int.t -> CZNum.t
= PedraQOracles.zToCoqZ

let export_Q: CQNum.t -> Scalar.Rat.t
= PedraQOracles.nToNb

let export_Z_as_Q: CZNum.t -> Scalar.Rat.t
= fun z -> Scalar.Int.toQ (PedraQOracles.coqZToZ z)

module CAnnot = ASTerm.TopLevelAnnot

let import_annot: Annot.t -> CAnnot.t option
  = function
  | Annot.Old -> Some (CAnnot.OLD)
  | _ -> None

let export_annot: CAnnot.t -> Annot.t option
  = function
  | CAnnot.OLD -> Some (Annot.Old)
  | _ -> None

module QItv = Itv.QItv

let import_QbndT: Pol.bndT -> QItv.bndT =
  function
  | Pol.Infty -> QItv.Infty
  | Pol.Open b -> QItv.Open (import_Q b)
  | Pol.Closed b -> QItv.Closed (import_Q b)

let export_QbndT: QItv.bndT -> Pol.bndT =
  function
  | QItv.Infty -> Pol.Infty
  | QItv.Open b -> Pol.Open (export_Q b)
  | QItv.Closed b -> Pol.Closed (export_Q b)

module ZItv = ZNoneItv.ZNItv
module Zbnd = ZNone.ZN

let export_ZbndT: Zbnd.t -> Pol.bndT =
  function
  | None -> Pol.Infty
  | Some b -> Pol.Closed (export_Z_as_Q b)

(* translation of a sequence of a binary assocative operation
   into a well-balanced tree (with minimal height)
*)
let rec balance_bin_assoc (zero: 'a) (bin: 'a -> 'a -> 'a) (l: 'a list) (acc: 'a list): 'a
    = match l with
    | [] ->
       (match acc with
       | [] -> zero
       | [x] -> x
       | x::y::l -> balance_bin_assoc zero bin l [bin x y])
    | [x] -> balance_bin_assoc zero bin [] (x::acc)
    | x::y::l -> balance_bin_assoc zero bin l ((bin x y)::acc)

let import_bin_assoc (f: 'a -> 'b) (zero: 'b) (bin: 'b -> 'b -> 'b) (l: 'a list): 'b
   = balance_bin_assoc zero bin (List.map f l) []

(* translation of cmpT *)
let import_cmpT (f: 'a -> 'b) (t1: 'a) (c:cmpT) (t2:'a): 'b * NumC.cmpG * 'b
  = let t1 = f t1 in
    let t2 = f t2 in
    match c with
    | Cstr_type.EQ -> (t1, NumC.Eq, t2)
    | Cstr_type.NEQ -> (t1, NumC.Neq, t2)
    | Cstr_type.LE -> (t1, NumC.Le, t2)
    | Cstr_type.LT -> (t1, NumC.Lt, t2)
    | Cstr_type.GE -> (t2, NumC.Le, t1)
    | Cstr_type.GT -> (t2, NumC.Lt, t1)

let export_cmpT (c: NumC.cmpG): cmpT
  = match c with
    | NumC.Eq -> Cstr_type.EQ
    | NumC.Neq -> Cstr_type.NEQ
    | NumC.Le -> Cstr_type.LE
    | NumC.Lt -> Cstr_type.LT

let export_s_cmpT (c: NumC.cmpT): cmpT
  = match c with
    | NumC.EqT -> Cstr_type.EQ
    | NumC.LeT -> Cstr_type.LE
    | NumC.LtT -> Cstr_type.LT

(********************)
(* translation on Q *)
(********************)

module QInterface = Interface(Scalar.Rat)
module QTerm = QInterface.Term
module QCond = QInterface.Cond
module CQCond = ASCond.QCond
module CQTerm = CQCond.Term

let rec import_QTerm: QTerm.t -> CQTerm.t
  = function
  | QTerm.Var x -> CQTerm.Var (import_certvar x)
  | QTerm.Cte c -> CQTerm.Cte (import_Q c)
  | QTerm.Add (t1, t2) -> CQTerm.Add (import_QTerm t1, import_QTerm t2)
  | QTerm.Sum l -> import_bin_assoc import_QTerm (CQTerm.Cte CQNum.z) (fun t1 t2 -> CQTerm.Add (t1,t2)) l
  | QTerm.Opp t -> CQTerm.Opp (import_QTerm t)
  | QTerm.Mul (t1, t2) -> CQTerm.Mul (import_QTerm t1, import_QTerm t2)
  | QTerm.Prod l -> import_bin_assoc import_QTerm (CQTerm.Cte CQNum.u) (fun t1 t2 -> CQTerm.Mul (t1,t2)) l
  | QTerm.Div (_,_) | QTerm.Poly _ -> Stdlib.failwith "import_QTerm: unimplemented"
  | QTerm.Annot (a, t) ->
     (match import_annot a with
     | Some a -> CQTerm.Annot (a, import_QTerm t)
     | None -> (* Skip annotation *) import_QTerm t)

let rec export_QTerm: CQTerm.t -> QTerm.t
  = function
  | CQTerm.Var x -> QTerm.Var (export_certvar x)
  | CQTerm.Cte c -> QTerm.Cte (export_Q c)
  | CQTerm.Add (t1, t2) -> QTerm.Add (export_QTerm t1, export_QTerm t2)
  | CQTerm.Opp t -> QTerm.Opp (export_QTerm t)
  | CQTerm.Mul (t1, t2) -> QTerm.Mul (export_QTerm t1, export_QTerm t2)
  | CQTerm.Annot (a, t) ->
     (match export_annot a with
     | Some a -> QTerm.Annot (a, export_QTerm t)
     | None -> (* Skip annotation *) export_QTerm t)

let rec import_QCond: QCond.t -> CQCond.t
  = function
  | QCond.Basic b -> CQCond.Basic b
  | QCond.Atom (t1,c,t2) -> let (t1,c,t2) = import_cmpT import_QTerm t1 c t2 in
                            CQCond.Atom (c, t1, t2)
  | QCond.BinL (c1, AND, c2) -> CQCond.BinL (ASCond.AND, import_QCond c1, import_QCond c2)
  | QCond.BinL (c1, OR, c2) -> CQCond.BinL (ASCond.OR, import_QCond c1, import_QCond c2)
  | QCond.Not c -> CQCond.Not (import_QCond c)


(***********************************************************)
(* translation on Z: mostly a copy-paste from the one of Q *)
(***********************************************************)

module ZInterface = Interface(Scalar.Int)
module ZTerm = ZInterface.Term
module ZCond = ZInterface.Cond
module CZCond = ASCond.ZCond
module CZTerm = CZCond.Term


let rec import_ZTerm: ZTerm.t -> CZTerm.t
  = function
  | ZTerm.Var x -> CZTerm.Var (import_certvar x)
  | ZTerm.Cte c -> CZTerm.Cte (import_Z c)
  | ZTerm.Add (t1, t2) -> CZTerm.Add (import_ZTerm t1, import_ZTerm t2)
  | ZTerm.Sum l -> import_bin_assoc import_ZTerm (CZTerm.Cte CZNum.z) (fun t1 t2 -> CZTerm.Add (t1,t2)) l
  | ZTerm.Opp t -> CZTerm.Opp (import_ZTerm t)
  | ZTerm.Mul (t1, t2) -> CZTerm.Mul (import_ZTerm t1, import_ZTerm t2)
  | ZTerm.Prod l -> import_bin_assoc import_ZTerm (CZTerm.Cte CZNum.u) (fun t1 t2 -> CZTerm.Mul (t1,t2)) l
  | ZTerm.Div (_,_) | ZTerm.Poly _ -> Stdlib.failwith "import_ZTerm: unimplemented"
  | ZTerm.Annot (a, t) ->
     (match import_annot a with
     | Some a -> CZTerm.Annot (a, import_ZTerm t)
     | None -> (* Skip annotation *) import_ZTerm t)

let rec import_ZCond: ZCond.t -> CZCond.t
  = function
  | ZCond.Basic b -> CZCond.Basic b
  | ZCond.Atom (t1,c,t2) -> let (t1,c,t2) = import_cmpT import_ZTerm t1 c t2 in
                            CZCond.Atom (c, t1, t2)
  | ZCond.BinL (c1, AND, c2) -> CZCond.BinL (ASCond.AND, import_ZCond c1, import_ZCond c2)
  | ZCond.BinL (c1, OR, c2) -> CZCond.BinL (ASCond.OR, import_ZCond c1, import_ZCond c2)
  | ZCond.Not c -> CZCond.Not (import_ZCond c)

module Vec = Vector.Rat

module QAffTerm = struct
   type t = Vec.t * Scalar.Rat.t

   let to_string : (Var.t -> string) -> t -> string
   	= fun varPr (v,cste) ->
   	Printf.sprintf "%s + %s"
   		(Vec.to_string varPr v)
   		(Scalar.Rat.to_string cste)

   let to_poly : t -> Polynomial.t
   	= fun (vec,cste) ->
   	Polynomial.ofCstr vec cste

   let to_cp : cmpT * t -> CP.t
		= fun (cmp,t) ->
		let p = to_poly t in
		let (cmp',p') =
		Cstr_type.(match cmp with
		| EQ -> (Eq, p)
		| LE -> (Le, Polynomial.neg p)
		| LT -> (Lt, Polynomial.neg p)
		| GE -> (Le, p)
		| GT -> (Lt, p)
		| NEQ -> Stdlib.failwith "VPLInterface.Term.to_cp: NEQ unimplemented")
		in
		CP.mk cmp' p'
end


module CQAffTerm = LinTerm.QAffTerm

let export_QAffTerm: CQAffTerm.t -> QAffTerm.t =
  fun t -> (PedraQOracles.ltToVec t.CQAffTerm.lin, export_Q t.CQAffTerm.cte)

module type QLowLevelDomain = LowLevelDomain with module Term = QAffTerm

module CQCstr = CstrC.Cstr
