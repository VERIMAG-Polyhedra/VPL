open CoqAddOn
open CstrC
open CstrLCF
open Datatypes
open Debugging
open List0
open NumC
open ProgVar

module Cs :
 sig
  type t = Cstr.t list

  val pr : t -> char list

  val to_string : (PVar.t -> char list) -> t -> char list

  val isEq : t -> t -> bool

  val isContrad : t -> bool

  val isFree : PVar.t -> t -> bool

  val rename : PVar.t -> PVar.t -> t -> t

  type cstr =
    Cstr.t
    (* singleton inductive, whose constructor was Build_cstr *)

  val rep : cstr -> Cstr.t

  val m_top : cstr

  val m_triv : cmpT -> QNum.t -> cstr

  val m_add : cstr -> cstr -> cstr

  val m_mul : QNum.t -> cstr -> cstr

  val m_to_le : cstr -> cstr

  val m_merge : cstr -> cstr -> cstr

  val certCstrLCF : (Cstr.t, cstr) cstrLCF

  val x_unwrap : cstr list -> t -> t

  val unwrap : cstr list -> t

  val join : t -> t -> cstr list -> cstr list -> t

  val x_wrap : t -> cstr list -> cstr list

  val wrap : t -> cstr list

  val wrap2 : t -> t -> cstr list*cstr list

  val geti : nat -> t -> Cstr.t -> Cstr.t

  val default : Cstr.t
 end
