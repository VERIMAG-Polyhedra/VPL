open ASCond
open ASTerm
open BinInt
open BinNums
open Datatypes
open DemoPLVerifier
open NumC
open ProgVar
open String0

module type VariableBasis =
 sig
  val x : PVar.t

  val y : PVar.t

  val q : PVar.t

  val r : PVar.t

  val a : PVar.t

  val x1 : PVar.t

  val x2 : PVar.t

  val y1 : PVar.t

  val y2 : PVar.t

  val aux : PVar.t

  val lb : PVar.t

  val ub : PVar.t

  val c : PVar.t
 end

module Examples :
 functor (B:VariableBasis) ->
 sig
  val idZ : coq_Z -> ZNum.t

  val idTerm : ZCond.Term.term -> ZTerm.term

  val div2 : ZCond.cond -> statement

  val basic_tests_ok : (char list*statement) list

  val basic_tests_ko : (char list*statement) list

  val coq_CTE : coq_Z

  val barycentre : char list -> coq_Z -> coq_Z -> coq_Z -> statement

  val test_barycentre : statement

  val imul4 :
    char list -> ZTerm.term -> ZTerm.term -> ZTerm.term -> ZTerm.term ->
    ZTerm.term -> ZTerm.term -> (char list*statement) list

  val imul30 :
    char list -> ZTerm.term -> ZTerm.term -> ZTerm.term -> ZTerm.term ->
    ZTerm.term -> (char list*statement) list

  val imul31 :
    char list -> ZTerm.term -> ZTerm.term -> ZTerm.term -> ZTerm.term ->
    (char list*statement) list

  val imul32 :
    char list -> ZTerm.term -> ZTerm.term -> ZTerm.term -> ZTerm.term ->
    (char list*statement) list

  val itv_tests_ok : (char list*statement) list

  val itv_tests_ko : (char list*statement) list

  val tests_ok : (char list*statement) list

  val tests_ko : (char list*statement) list
 end
