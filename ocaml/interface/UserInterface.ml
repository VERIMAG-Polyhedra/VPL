(**
Available polyhedral domains.
There are three levels of certification:
{ul
    {- {b No certification}: no certificate is produced}
    {- {b OCaml certification}: Each operator of the domain produces a certificate, which is checked by an OCaml procedure.
    }
    {- {b Coq certification}: In addition to guarantees offered by OCaml certification, certificates are here checked by a Coq-proved procedure.
    }
}

All domains fulfil a common interface (see {!module-type:Vpl__AbstractDomain.Type}).
*)


(**/**)
open WrapperTraductors


(** Exception that can be raised whenever a conversion from a user-defined term into VPL terms fails. *)
exception Out_of_Scope

(** Type for user-defined variables. *)
module type Ident_T = sig
    (** Type of variables *)
	type t

	(** Conversion into VPL variables of type {!val:Var.Positive.t}. *)
	val toVar: t -> Var.t

    (** Conversion from VPL variables. *)
    val ofVar : Var.t -> t

	(** Conversion into string. *)
	val to_string: t -> string
end

(** Types of domains available in the VPL. *)
module type Domain_T = sig
    (** Type of coefficients used by the domain. *)
    module Coeff : Scalar.Type

    include AbstractDomain.Type with
        type a_expr = Interface(Coeff).Term.t and
        type b_expr = Interface(Coeff).Cond.t and
        type var = Var.t
end

type 'a user_cond =
| Basic of bool
| Atom of 'a * cmpT * 'a
| BinL of 'a user_cond * binl * 'a user_cond
| Not of 'a user_cond

module MakeCustom
    (D : Domain_T)
    (Ident : Ident_T)
    (Expr : sig
        type t
        val to_term: t -> Interface(D.Coeff).Term.t
        val of_term : Interface(D.Coeff).Term.t -> t
    end) = struct

    module Coeff = D.Coeff
    module I = Interface(D.Coeff)
    module Cond = I.Cond
    module Term = I.Term

    (* NB: We cannot include D because the module would not be of type
       AbstractDomain.Type (because of types var, a_expr and b_expr). *)
	type t = D.t

    type var = Ident.t

    type cert = D.cert

    type a_expr = Expr.t

    type b_expr = a_expr user_cond

    let rec to_cond : b_expr -> Cond.t
        = function
        | Basic b -> Cond.Basic b
        | Atom (e1,cmp,e2) -> Cond.Atom (Expr.to_term e1, cmp, Expr.to_term e2)
        | BinL (c1,bl,c2) -> Cond.BinL(to_cond c1, bl, to_cond c2)
        | Not c -> Cond.Not (to_cond c)

    let rec of_cond : Cond.t -> b_expr
        = function
        | Cond.Basic b -> Basic b
        | Cond.Atom (e1,cmp,e2) -> Atom (Expr.of_term e1, cmp, Expr.of_term e2)
        | Cond.BinL (c1,bl,c2) -> BinL(of_cond c1, bl, of_cond c2)
        | Cond.Not c -> Not (of_cond c)

    let top = D.top
    let bottom = D.bottom
    let is_bottom = D.is_bottom
    let get_bottom_cert = D.get_bottom_cert
    let meet = D.meet
    let minkowski = D.minkowski
    let join = D.join
    let widen = D.widen
    let proj_incl = D.proj_incl
    let leq = D.leq
    let size = D.size
    let set_point = D.set_point
    let split_in_half = D.split_in_half
    let get_regions = D.get_regions
    let diff = D.diff
    let spawn = D.spawn
    let satisfy = D.satisfy
    let get_cstrs = D.get_cstrs

    let assume: b_expr -> t -> t
        = fun c p ->
        D.assume (to_cond c) p

	let assume_back: b_expr -> t -> t option
        = fun c p ->
        D.assume_back (to_cond c) p

    let asserts: b_expr -> t -> bool
        = fun c p ->
        D.asserts (to_cond c) p

    let assign: (Ident.t * Expr.t) list -> t -> t
        = fun l p ->
        D.assign
            (List.map (fun (v,e) -> Ident.toVar v, Expr.to_term e) l)
            p

    let project: Ident.t list -> t -> t
        = fun vars p ->
        let vars' = List.map Ident.toVar vars in
        D.project vars' p

    let project_vars: Ident.t list -> t -> t
        = fun vars p ->
        let vars' = List.map Ident.toVar vars in
        D.project_vars vars' p

    let get_upper_bound : Expr.t -> t -> Pol.bndT option
        = fun expr p ->
        D.get_upper_bound (Expr.to_term expr) p

    let get_lower_bound : Expr.t -> t -> Pol.bndT option
        = fun expr p ->
        D.get_lower_bound (Expr.to_term expr) p

    let itvize : Expr.t -> t -> Pol.itvT
        = fun expr p ->
        D.itvize (Expr.to_term expr) p

    let var_to_string = Ident.to_string

    let to_string f =
        D.to_string (fun var -> Ident.ofVar var |> var_to_string)

    let a_expr_to_string : a_expr -> string
        = fun expr ->
        Expr.to_term expr
        |> Term.to_string Var.to_string

    let b_expr_to_string : b_expr -> string
        = fun cond ->
        to_cond cond
        |> Cond.to_string Var.to_string

    let get_b_expr : t -> b_expr
        = fun p ->
        D.get_b_expr p
        |> of_cond

    let get_vars : t -> var list
        = fun p ->
        D.get_vars p
        |> List.map Ident.ofVar

end

module Make (D : Domain_T) = MakeCustom(D)
    (struct
        type t = Var.t
        let toVar x = x
        let ofVar x = x
        let to_string = Var.to_string
    end)
    (struct
        type t = Interface(D.Coeff).Term.t
        let to_term t = t
        let of_term t = t
    end)

module Lift_Ident (I : sig
    type t
	val compare: t -> t -> int
	val to_string: t -> string
    end)
    = struct

    type t = I.t

    let compare = I.compare

    module Map_var_to_t = Map.Make(struct
		type t = Var.t
		let compare = Var.cmp
		end)

	module Map_t_to_var = Map.Make(I)


	type map_var_to_t = t Map_var_to_t.t
	type map_t_to_var = Var.t Map_t_to_var.t

    type mapsT = {
		var_to_t : map_var_to_t ;
		t_to_var : map_t_to_var ;
		next : Var.t
    }

	let emptyMaps = {
		var_to_t = Map_var_to_t.empty ;
		t_to_var = Map_t_to_var.empty ;
		next = Var.u
    }

    let maps : mapsT ref = ref emptyMaps

    let print_maps : unit -> unit
		= fun () ->
		Printf.sprintf "map var to t : \n\t%s\nmap t to var: \n\t%s"
			(Misc.list_to_string
				(fun (v,v') -> Printf.sprintf "%s -> %s"
					(Var.to_string v)
                    (I.to_string v'))
				(Map_var_to_t.bindings !maps.var_to_t)
				" ; ")
			(Misc.list_to_string
				(fun (v',v) -> Printf.sprintf "%s -> %s"
					(I.to_string v')
                    (Var.to_string v))
				(Map_t_to_var.bindings !maps.t_to_var)
				" ; ")
		|> print_endline

    let mem : t -> bool
		= fun s -> Map_t_to_var.mem s !maps.t_to_var

    let addVars : t list -> unit
		= fun vars ->
        maps := List.fold_left
			(fun m var ->
			if mem var
            then m
			else {
				var_to_t = Map_var_to_t.add m.next var m.var_to_t ;
				t_to_var = Map_t_to_var.add var m.next m.t_to_var ;
				next = Var.next m.next
            })
			!maps vars

    let toVar : t -> Var.t
		= fun var ->
        if mem var
        then Map_t_to_var.find var !maps.t_to_var
        else begin
            let new_var = !maps.next in
            maps := {
				var_to_t = Map_var_to_t.add new_var var !maps.var_to_t ;
				t_to_var = Map_t_to_var.add var new_var !maps.t_to_var ;
				next = Var.next !maps.next
            };
            new_var
        end


	let ofVar : Var.t -> t
		= fun s -> Map_var_to_t.find s !maps.var_to_t

    let to_string: t -> string
		= fun var -> I.to_string var

	let get_string : Var.t -> string
		= fun var ->
		Map_var_to_t.find var !maps.var_to_t
        |> I.to_string

    let rename : t -> t -> unit
        = fun fromX toY ->
        if Map_t_to_var.mem toY !maps.t_to_var
        then Printf.sprintf "rename: new variable name %s already exists" (I.to_string toY)
            |> Stdlib.invalid_arg
        else try
            let var = Map_t_to_var.find fromX !maps.t_to_var in
            let t_to_var' = Map_t_to_var.add toY var !maps.t_to_var
            |> Map_t_to_var.remove fromX
            and var_to_t' = Map_var_to_t.add var toY !maps.var_to_t in
            maps := {!maps with
                t_to_var = t_to_var';
                var_to_t = var_to_t';
            }
        with Not_found -> Printf.sprintf "rename: variable %s does not exist" (I.to_string fromX)
            |> Stdlib.invalid_arg

    let remove : t -> unit
        = fun var ->
        if Map_t_to_var.mem var !maps.t_to_var
        then let var' = Map_t_to_var.find var !maps.t_to_var in
            maps := { !maps with
            t_to_var = Map_t_to_var.remove var !maps.t_to_var;
            var_to_t = Map_var_to_t.remove var' !maps.var_to_t;
        }
        else Printf.sprintf "remove: variable %s does not exist" (I.to_string var)
            |> Stdlib.invalid_arg

end
(**/**)

(** Uncertified domain on Q. *)
module UncertifiedQ = Make(Domains.UncertifiedQ)

(** Uncertified domain on Z. *)
module UncertifiedZ = Make(Domains.UncertifiedZ)

(** OCaml certified domain on Q. *)
module OCamlCertifiedQ = Make(Domains.OCamlCertifiedQ)

(** OCaml certified domain on Z. *)
module OCamlCertifiedZ = Make(Domains.OCamlCertifiedZ)

(** Coq certified domain on Q. *)
module CoqCertifiedQ = Make(Domains.CoqCertifiedQ)

(** Coq certified domain on Z. *)
module CoqCertifiedZ = Make(Domains.CoqCertifiedZ)
