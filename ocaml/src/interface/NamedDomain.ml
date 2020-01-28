exception ReportHandled

let report : exn -> unit
	= function
    | CertcheckerConfig.CertCheckerFailure (_,s) as e -> begin
        Printf.sprintf "An exception was raised: %s\n%s"
		(Printexc.to_string e)
        (Printexc.get_backtrace ())
		|> print_endline;
        print_endline s
    end
    | e -> begin
	   Printf.sprintf "An exception was raised: %s\n%s"
		(Printexc.to_string e)
        (Printexc.get_backtrace ())
		|> print_endline
    end

(** Handles exception report *)
let handle : 'a Lazy.t -> 'a
    = fun a ->
    try Lazy.force a with
    | e -> begin
        report e;
        Stdlib.raise e
    end

type 'a named_of = {
    value: 'a;
    name: string;
}

module Make(D : AbstractDomain.Type) = struct

    type t = D.t named_of

    type var = D.var
    type cert = D.cert
    type a_expr = D.a_expr
    type b_expr = D.b_expr

    let var_to_string = D.var_to_string

    let a_expr_to_string = D.a_expr_to_string

    let b_expr_to_string = D.b_expr_to_string

	let next_name : int ref = ref 0

	let new_name : unit -> string
		= fun () -> begin
		let res = !next_name in
		next_name := !next_name + 1;
		Printf.sprintf "P%i" res
		end

    let mk: string -> D.t -> t
        = fun s p -> {
            value = p;
            name = s;
        }

    let top : t
        = mk "top()" D.top

    let bottom : t
        = mk "bot()" D.bottom

    module Lift = struct

        let unary : (D.t -> D.t) -> (t -> t)
            = fun op p -> {
                value = op p.value;
                name = new_name();
            }

        let binary : (D.t -> D.t -> D.t) -> (t -> t -> t)
            = fun op p1 p2 -> {
                value = op p1.value p2.value;
                name = new_name();
            }

        let unary_list : (D.t -> D.t list) -> (t -> t list)
            = fun op p ->
            op p.value
            |> List.map (fun p -> {
                value = p;
                name = new_name();
            })

        let binary_list : (D.t -> D.t -> D.t list) -> (t -> t -> t list)
            = fun op p1 p2 ->
            op p1.value p2.value
            |> List.map (fun p -> {
                value = p;
                name = new_name();
            })

        let unary_other : (D.t -> 'a) -> (t -> 'a)
            = fun op p ->
            op p.value

        let binary_other : (D.t -> D.t -> 'a) -> (t -> t -> 'a)
            = fun op p1 p2 ->
            op p1.value p2.value
    end

    (*****************************************)
    (************ UNARY OPERATORS ************)
    (*****************************************)
    let assume cond = Lift.unary (D.assume cond)
    let assign terms = Lift.unary (D.assign terms)
    let project vars = Lift.unary (D.project vars)
    let project_vars vars = Lift.unary (D.project_vars vars)
    let set_point point = Lift.unary (D.set_point point)

    (*****************************************)
    (************ BINARY OPERATORS ***********)
    (*****************************************)
    let meet = Lift.binary D.meet
    let minkowski = Lift.binary D.minkowski
    let join = Lift.binary D.join
    let widen = Lift.binary D.widen

    (*****************************************)
    (************* LIST OPERATORS ************)
    (*****************************************)
    let diff = Lift.binary_list D.diff
    let get_regions = Lift.unary_list D.get_regions
    let split_in_half = Lift.unary_list D.split_in_half

    (*****************************************)
    (************  OTHER OPERATORS ***********)
    (*****************************************)
    let is_bottom = Lift.unary_other D.is_bottom
    let get_bottom_cert = Lift.unary_other D.get_bottom_cert
    let asserts cond = Lift.unary_other (D.asserts cond)
    let to_string var_to_string = Lift.unary_other (D.to_string var_to_string)
    let get_upper_bound expr = Lift.unary_other (D.get_upper_bound expr)
    let get_lower_bound expr = Lift.unary_other (D.get_lower_bound expr)
    let itvize expr = Lift.unary_other (D.itvize expr)
    let size = Lift.unary_other D.size
    let leq = Lift.binary_other D.leq
    let get_cstrs = Lift.unary_other D.get_cstrs
    let get_b_expr = Lift.unary_other D.get_b_expr
    let get_vars = Lift.unary_other D.get_vars
    let spawn = Lift.unary_other D.spawn
    let satisfy point = Lift.unary_other (D.satisfy point)

    let proj_incl p1 p2 = match D.proj_incl p1.value p2.value with
        | None -> None
        | Some p -> Some {
            value = p;
            name = new_name();
        }

    let assume_back cond p = match D.assume_back cond p.value with
	| None -> None
	| Some p -> Some {
		value = p;
		name = new_name();
	}

end
