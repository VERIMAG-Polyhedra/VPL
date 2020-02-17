open CoqAddOn
open CstrC
open CstrLCF
open Datatypes
open Debugging
open List0
open NumC
open ProgVar

module Cs =
 struct
  type t = Cstr.t list

  (** val pr : t -> char list **)

  let pr cs =
    concat nl (map Cstr.pr cs)

  (** val to_string : (PVar.t -> char list) -> t -> char list **)

  let to_string f cs =
    concat nl (map (Cstr.to_string f) cs)

  (** val isEq : t -> t -> bool **)

  let rec isEq l1 l2 =
    match l1 with
    | [] -> (match l2 with
             | [] -> true
             | _::_ -> false)
    | c1::l1' ->
      (match l2 with
       | [] -> false
       | c2::l2' -> if Cstr.isEq c1 c2 then isEq l1' l2' else false)

  (** val isContrad : t -> bool **)

  let isContrad = function
  | [] ->
    (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
      CERT
      ('i'::('s'::('C'::('o'::('n'::('t'::('r'::('a'::('d'::(' '::('e'::('x'::('p'::('e'::('c'::('t'::('s'::(' '::('a'::(' '::('s'::('i'::('n'::('g'::('l'::('e'::('t'::('o'::('n'::[])))))))))))))))))))))))))))))
      false
  | c::l0 ->
    (match l0 with
     | [] -> Cstr.isContrad c
     | _::_ ->
       (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
         CERT
         ('i'::('s'::('C'::('o'::('n'::('t'::('r'::('a'::('d'::(' '::('e'::('x'::('p'::('e'::('c'::('t'::('s'::(' '::('a'::(' '::('s'::('i'::('n'::('g'::('l'::('e'::('t'::('o'::('n'::[])))))))))))))))))))))))))))))
         false)

  (** val isFree : PVar.t -> t -> bool **)

  let rec isFree x = function
  | [] -> true
  | c::cs' -> if Cstr.isFree x c then isFree x cs' else false

  (** val rename : PVar.t -> PVar.t -> t -> t **)

  let rec rename x y = function
  | [] -> []
  | c::cs' -> (Cstr.rename x y c)::(rename x y cs')

  type cstr =
    Cstr.t
    (* singleton inductive, whose constructor was Build_cstr *)

  (** val rep : cstr -> Cstr.t **)

  let rep c =
    c

  (** val m_top : cstr **)

  let m_top =
    Cstr.top

  (** val m_triv : cmpT -> QNum.t -> cstr **)

  let m_triv =
    Cstr.triv

  (** val m_add : cstr -> cstr -> cstr **)

  let m_add c1 c2 =
    Cstr.add (rep c1) (rep c2)

  (** val m_mul : QNum.t -> cstr -> cstr **)

  let m_mul coeff c =
    Cstr.mul coeff (rep c)

  (** val m_to_le : cstr -> cstr **)

  let m_to_le c =
    Cstr.to_le (rep c)

  (** val m_merge : cstr -> cstr -> cstr **)

  let m_merge c1 c2 =
    Cstr.merge (rep c1) (rep c2)

  (** val certCstrLCF : (Cstr.t, cstr) cstrLCF **)

  let certCstrLCF =
    { top = m_top; triv = m_triv; add = m_add; mul = m_mul; merge = m_merge;
      to_le = m_to_le; export = rep }

  (** val x_unwrap : cstr list -> t -> t **)

  let rec x_unwrap l acc =
    match l with
    | [] -> acc
    | c::l' -> x_unwrap l' ((rep c)::acc)

  (** val unwrap : cstr list -> t **)

  let unwrap l =
    x_unwrap l []

  (** val join : t -> t -> cstr list -> cstr list -> t **)

  let join _ _ cert1 cert2 =
    let res = unwrap cert1 in
    if isEq (unwrap cert2) res
    then res
    else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
           CERT
           ('j'::('o'::('i'::('n'::(':'::(' '::('b'::('o'::('t'::('h'::(' '::('s'::('i'::('d'::('e'::('s'::(' '::('d'::('o'::(' '::('n'::('o'::('t'::(' '::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))
           []

  (** val x_wrap : t -> cstr list -> cstr list **)

  let rec x_wrap l acc =
    match l with
    | [] -> acc
    | c::l' -> x_wrap l' (c::acc)

  (** val wrap : t -> cstr list **)

  let wrap l =
    x_wrap l []

  (** val wrap2 : t -> t -> cstr list*cstr list **)

  let wrap2 l1 l2 =
    (x_wrap l1 []),(x_wrap l2 [])

  (** val geti : nat -> t -> Cstr.t -> Cstr.t **)

  let geti =
    nth

  (** val default : Cstr.t **)

  let default =
    Cstr.top
 end
