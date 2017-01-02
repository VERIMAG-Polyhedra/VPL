(** This module allows to tune the algorithms used in the polyhedral operators. *)

(** Type of linear solver for the new minimization.
{ul 
	{- [Glpk]: floating point LP binded to the VPL using ocaml-glpk}
	{- [Splx]: rational LP of the VPL}}
*)
type lp = Glpk | Splx

(** Choice of the minimization method.
{ul 
	{- [Classic]: classical way that uses LP}
	{- [New2]: New method based on constraint evaluation + LP}}
*)
type min_method = Classic | New2 of lp | MHeuristic | Apron (* Apron for tests only *)

(** Choice of the point type.
{ul 
	{- [Rat]: Rational points}
	{- [Symbolic]: Rational points with symbolic error}
	{- [Float]: Floating points}}
*)
type scalar = Rat | Symbolic | Float

let scalar_to_string : scalar -> string
	= function
	| Rat -> "Rat"
	| Symbolic -> "Symbolic"
	| Float -> "Float"

(** Choice of the projection method.
{ul 
	{- [FM]: classical way that uses the Fourier-Motzkin algorithm}
	{- [Proj_PLP]: method based on Parametric Linear Programming}}
*)
type proj_method = FM | Proj_PLP of scalar | PHeuristic

(** Choice of the join method.
{ul 
	{- [Baryc]: algorithm based on convex combinations + projection }
	{- [Join_PLP]: method based on Parametric Linear Programming}}
*)
type join_method = Baryc | Join_PLP of scalar | JHeuristic

(** Default choice for minimization. *)
let min : min_method ref = ref (Classic)

let min_to_string : unit -> string
	= fun () ->
	match !min with
	| Classic -> "Classic"
	| New2 Glpk -> "New2:Glpk"
	| New2 Splx -> "New2:Splx"
	| MHeuristic -> "Heuristic"
	| Apron -> "Apron"
	
(** Default choice for projection. *)
let proj : proj_method ref = ref (FM)

let proj_to_string : unit -> string
	= fun () ->
	match !proj with
	| FM -> "Fourier_Motzkin"
	| Proj_PLP (scalar) -> "Proj_PLP(" ^ (scalar_to_string scalar) ^ ")"
	| PHeuristic -> "Heuristic"
	
(** Default choice for join. *)
let join : join_method ref = ref (Baryc)

let join_to_string : unit -> string
	= fun () ->
	match !join with
	| Baryc -> "Barycentric"
	| Join_PLP (scalar) -> "Join_PLP(" ^ (scalar_to_string scalar) ^ ")"
	| JHeuristic -> "Heuristic"
	
(** If set to true, the Handelman linearization will loop, meaning that it will iterate on the last result obtained.
The linearization of polynomial [g] on the starting polyhedron [P] will give
H_0 = linearize(P,g)
H_i = linearize(H_(i-1), g)
*)	
let handelman_loop : bool ref = ref true

(** Sets a timeout to the Handelman linearization. 
Combined to {!val:handelman_loop}, the linearization returns the last result found. *)
let handelman_timeout : int option ref = ref (Some 10)

(** If set to [Some n], the parametric simplex will be distributed over n processes. *)
let distributed_plp : int option ref = ref None

(** If set to [true], the system of equalities of a polyhedron will be in row echelon form. *)
let row_echelon_equalities : bool ref = ref true

