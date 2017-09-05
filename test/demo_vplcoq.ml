(* ML source file of an exectuable running
   the examples of DemoPLTests.v (in directory coq/)
*)

open Vpl;;
open CertcheckerConfig;;
open Debugging;;

Printf.printf "*** run tests generated from Coq...\n";;
(*
Debug.enable();;
Debug.print_enable();;
Debug.set_colors();;
*)
(** options (fixées en dur !) *)
let firstfail_mode = false;;

(** Déclaration de variables *)
module Var = Var.Positive

let nextVar = ref Var.XH;;

let newvar _ =
  let x = !nextVar in
  nextVar := Var.next !nextVar;
    (PedraQOracles.varToProgVar x);;

module VB =
struct

let x = newvar "x";;
let y = newvar "y";;
let q = newvar "q";;
let r = newvar "r";;
let a = newvar "a";;
let x1 = newvar "x1";;
let x2 = newvar "x2";;
let y1 = newvar "y1";;
let y2 = newvar "y2";;
let aux = newvar "aux";;
let lb = newvar "lb" ;;
let ub = newvar "ub" ;;
let c = newvar "c";;

end;;
open VB;;

(** Execution des tests **)

open DemoPLVerifier;;

let tests = ref 0 ;;
let failed = ref 0;;

let incr r = r:=!r+1;;

let afficheok attendu =
  if attendu then
    print_string " passed as expected\n"
  else
    print_string " failed as expected\n";;

let afficheko attendu =
  incr failed;
  if attendu then
    print_string "ko (failed vs passed expected)\n"
  else
    print_string "ko (passed vs failed expected)\n";;

let firstfail_launch nom test attendu =
  Printf.printf "*** %s: " nom;
  if (verifier test)=attendu
  then (
     afficheok attendu
  ) else (
     afficheko attendu
  )


let launch nom test attendu =
try
  firstfail_launch nom test attendu
with CertCheckerFailure(st, mesg) ->
  if st = DEMO then (
    Printf.printf " failure %s !\n" mesg ;
    if attendu
    then afficheko true
    else afficheok false
  ) else (
    Printf.printf " %s: %s !\n" (if st=CERT then "certificate error" else "internal error from cert checker") mesg;
    incr failed;
  )

let rec launch_list l attendu =
    match l with
      | [] -> print_newline();
      | (nom,test)::ll ->
	incr tests;
	if firstfail_mode then
           firstfail_launch (CoqPr.charListTr nom) test attendu
        else
           launch (CoqPr.charListTr nom) test attendu;
	launch_list ll attendu;;

module Ex = DemoPLTests.Examples(VB);;

launch_list Ex.tests_ok true;;
launch_list Ex.tests_ko false;;

let nbfails = !failed in
if nbfails=0 then
  Printf.printf "*** Passed %d tests\n" !tests
else (
  Printf.printf "*** %d failure(s) in %d tests:\n" nbfails !tests;
  exit 1);;
