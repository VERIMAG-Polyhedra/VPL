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

module Examples =
 functor (B:VariableBasis) ->
 struct
  (** val idZ : coq_Z -> ZNum.t **)

  let idZ x0 =
    x0

  (** val idTerm : ZCond.Term.term -> ZTerm.term **)

  let idTerm x0 =
    x0

  (** val div2 : ZCond.cond -> statement **)

  let div2 inv =
    Seq ((Ifs ((ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var
      B.x))), (Assign (B.a, (ZCond.Term.Var B.x))), (Assign (B.a,
      (ZCond.Term.Opp (ZCond.Term.Var B.x)))))), (Seq ((Assign (B.r,
      (ZCond.Term.Var B.a))), (Seq ((Assign (B.q, (ZCond.Term.Cte
      (idZ Z0)))), (While ((ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO Coq_xH)))), (ZCond.Term.Var B.r))), (Seq ((Assign
      (B.q, (ZCond.Term.Add ((ZCond.Term.Var B.q), (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))))))), (Assign (B.r,
      (sub (ZCond.Term.Var B.r) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xO Coq_xH))))))))), (ZCond.BinL (AND, (ZCond.Atom
      (Eq, (ZCond.Term.Var B.a), (ZCond.Term.Add ((ZCond.Term.Mul
      ((ZCond.Term.Var B.q), (ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))))),
      (ZCond.Term.Var B.r))))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var B.r))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var B.q))),
      inv)))))))))))))

  (** val basic_tests_ok : (char list*statement) list **)

  let basic_tests_ok =
    (('t'::('r'::('i'::('v'::('_'::('g'::('u'::('a'::('r'::('d'::('s'::('0'::[])))))))))))),(Seq
      ((Assume (ZCond.Atom (Eq, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH))))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('1'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))), (ZCond.Term.Var
      B.x))))), (Seq ((Assert (('p'::('o'::('s'::('t'::('2'::[]))))),
      (ZCond.Atom (Eq, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH)))))))), (Assert
      (('p'::('o'::('s'::('t'::('3'::[]))))), (ZCond.Atom (Neq,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO Coq_xH))))))))))))))))::((('t'::('r'::('i'::('v'::('_'::('g'::('u'::('a'::('r'::('d'::('s'::('1'::[])))))))))))),(Seq
      ((Assume (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH))))))))), (ZCond.Term.Var B.x)))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('1'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var B.x))))), (Seq ((Assume
      (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var B.x)))),
      (Seq ((Assert (('p'::('o'::('s'::('t'::('2'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var B.x))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('3'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH))))))))), (ZCond.Term.Var B.x))))), (Seq ((Assume (ZCond.Atom
      (Eq, (ZCond.Term.Var B.y), (ZCond.Term.Add ((ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ (Zpos Coq_xH)))))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('4'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH))))))))), (ZCond.Term.Var B.y))))), (Assert
      (('p'::('o'::('s'::('t'::('5'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var
      B.y))))))))))))))))))))::((('t'::('r'::('i'::('v'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::[]))))))))))),(Seq
      ((Assume (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH))))))))), (ZCond.Term.Var B.x)))), (Seq ((Assign (B.x,
      (ZCond.Term.Add ((ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('1'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH))))))))), (ZCond.Term.Var B.x))))), (Assert
      (('p'::('o'::('s'::('t'::('2'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var
      B.x))))))))))))::((('s'::('p'::('l'::('i'::('t'::('_'::('e'::('q'::[])))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH))))))))), (Seq ((Assign (B.x, (ZCond.Term.Add
      ((ZCond.Term.Var B.x), (ZCond.Term.Cte (idZ (Zpos Coq_xH))))))),
      (Assert (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (OR, (ZCond.Atom
      (Eq, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO Coq_xH)))))), (ZCond.BinL (OR, (ZCond.Atom (Eq,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))))),
      (ZCond.Atom (Eq, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO Coq_xH))))))))))))))))))::((('t'::('r'::('a'::('n'::('s'::('1'::[])))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.x))), (ZCond.Atom (Le, (ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ Z0))))))), (Assert
      (('p'::('o'::('s'::('t'::('1'::[]))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ Z0)))))))))::((('t'::('r'::('a'::('n'::('s'::('2'::[])))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.x))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Var B.y))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.y), (ZCond.Term.Cte (idZ Z0))))))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('1'::[]))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte (idZ Z0)))))), (Assert
      (('p'::('o'::('s'::('t'::('2'::[]))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ Z0)))))))))))::((('t'::('r'::('a'::('n'::('s'::('3'::[])))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.x))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Var B.y))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.y), (ZCond.Term.Var B.x1))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.x1), (ZCond.Term.Var B.x)))))))))),
      (Seq ((Assert (('p'::('o'::('s'::('t'::('1'::[]))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.x), (ZCond.Term.Var B.y))))), (Assert
      (('p'::('o'::('s'::('t'::('2'::[]))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.x), (ZCond.Term.Var
      B.x1))))))))))::((('t'::('r'::('a'::('n'::('s'::[]))))),(Seq ((Assume
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.x))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Var B.y))), (ZCond.Atom (Le,
      (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))),
      (ZCond.Term.Var B.y))), (ZCond.Term.Var B.x1)))))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('1'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.y), (ZCond.Term.Var B.x1))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('2'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Var B.x1))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('3'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var B.x1))))), (Seq ((Assume
      (ZCond.Atom (Le, (ZCond.Term.Add ((ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xO Coq_xH)))), (ZCond.Term.Var B.x1))), (ZCond.Term.Mul
      ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))), (ZCond.Term.Var
      B.y))))), (ZCond.Term.Var B.x)))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('4'::[]))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.x), (ZCond.Term.Var B.y))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('5'::[]))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.y), (ZCond.Term.Var B.x1))))), (Assert
      (('p'::('o'::('s'::('t'::('6'::[]))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.x), (ZCond.Term.Var
      B.x1))))))))))))))))))))::((('w'::('h'::('i'::('l'::('e'::('1'::[])))))),(Seq
      ((Assign (B.r, (ZCond.Term.Cte (idZ (Zpos Coq_xH))))), (Seq ((Assign
      (B.q, (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xI Coq_xH))))))), (Seq
      ((While ((ZCond.Atom (Le, (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Seq ((Assign (B.q,
      (ZCond.Term.Add ((ZCond.Term.Var B.q), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO Coq_xH)))))))), (Assign (B.r, (ZCond.Term.Add
      ((ZCond.Term.Var B.r), (ZCond.Term.Cte (idZ (Zpos Coq_xH))))))))),
      (ZCond.BinL (AND, (ZCond.Atom (Eq, (ZCond.Term.Var B.q),
      (ZCond.Term.Add ((ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xO Coq_xH)))), (ZCond.Term.Var B.r))), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))))))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('1'::[]))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))))))), (Assert
      (('p'::('o'::('s'::('t'::('2'::[]))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.q), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))))))))))))))))::((('w'::('h'::('i'::('l'::('e'::('2'::[])))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))), (ZCond.Term.Var
      B.y))), (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Var B.y),
      (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))), (ZCond.BinL
      (AND, (ZCond.Atom (Eq, (ZCond.Term.Var B.y), (ZCond.Term.Add
      ((ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))),
      (ZCond.Term.Var B.x))), (ZCond.Term.Var B.q))))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var B.q))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.q), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH))))))))))))))), (While ((ZCond.Atom (Le,
      (ZCond.Term.Var B.q), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Assign (B.q,
      (ZCond.Term.Var B.q))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))), (ZCond.Term.Var B.x))),
      (ZCond.Atom (Le, (ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xO Coq_xH)))), (ZCond.Term.Var B.x))), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))))))))))))::((('c'::('o'::('n'::('v'::('e'::('x'::('_'::('h'::('u'::('l'::('l'::('0'::[])))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))), (Seq ((Assign
      (B.a, (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))),
      (ZCond.Term.Var B.x))))), (Seq ((Ifs ((ZCond.Atom (Le, (ZCond.Term.Var
      B.x), (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (Assign
      (B.y, (ZCond.Term.Add ((ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))))), (Assign (B.y,
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Add ((sub (ZCond.Term.Var B.a) (ZCond.Term.Var B.x)),
      (ZCond.Term.Var B.y))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))), (ZCond.Term.Var
      B.r))), (ZCond.Atom (Le, (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))))))))))))))))))::((('c'::('o'::('n'::('v'::('e'::('x'::('_'::('h'::('u'::('l'::('l'::('1'::[])))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))), (Seq ((Assign
      (B.a, (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))),
      (ZCond.Term.Var B.x))))), (Seq ((Ifs ((ZCond.Atom (Le, (ZCond.Term.Var
      B.x), (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (Assume
      (ZCond.Atom (Le, (ZCond.Term.Var B.y), (ZCond.Term.Add ((ZCond.Term.Var
      B.x), (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))))))),
      (Assign (B.y,
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Add ((sub (ZCond.Term.Var B.a) (ZCond.Term.Var B.x)),
      (ZCond.Term.Var B.y))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))))))))))))))))::((('c'::('o'::('n'::('v'::('e'::('x'::('_'::('h'::('u'::('l'::('l'::('2'::[])))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))), (Seq ((Assign
      (B.a, (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))),
      (ZCond.Term.Var B.x))))), (Seq ((Ifs ((ZCond.Atom (Le, (ZCond.Term.Var
      B.x), (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (Assume
      (ZCond.Atom (Le, (ZCond.Term.Add ((ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (ZCond.Term.Var B.y)))),
      (Assign (B.y,
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Add ((sub (ZCond.Term.Var B.a) (ZCond.Term.Var B.x)),
      (ZCond.Term.Var B.y))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))), (ZCond.Term.Var
      B.r))))))))))))))::((('c'::('o'::('n'::('v'::('e'::('x'::('_'::('h'::('u'::('l'::('l'::('3'::[])))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))), (Seq ((Assign
      (B.a, (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))),
      (ZCond.Term.Var B.x))))), (Seq ((Ifs ((ZCond.Atom (Le, (ZCond.Term.Var
      B.x), (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (Assign
      (B.y, (ZCond.Term.Add ((ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))))), (Assume (ZCond.Atom (Le,
      (ZCond.Term.Var B.y),
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Add ((sub (ZCond.Term.Var B.a) (ZCond.Term.Var B.x)),
      (ZCond.Term.Var B.y))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))))))))))))))))::((('c'::('o'::('n'::('v'::('e'::('x'::('_'::('h'::('u'::('l'::('l'::('4'::[])))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))), (Seq ((Assign
      (B.a, (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))),
      (ZCond.Term.Var B.x))))), (Seq ((Ifs ((ZCond.Atom (Le, (ZCond.Term.Var
      B.x), (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (Assign
      (B.y, (ZCond.Term.Add ((ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))))), (Assume (ZCond.Atom (Le,
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))), (ZCond.Term.Var B.y)))))),
      (Seq ((Assign (B.r, (ZCond.Term.Add
      ((sub (ZCond.Term.Var B.a) (ZCond.Term.Var B.x)), (ZCond.Term.Var
      B.y))))), (Assert (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))),
      (ZCond.Term.Var
      B.r))))))))))))))::((('c'::('o'::('n'::('v'::('e'::('x'::('_'::('h'::('u'::('l'::('l'::('5'::[])))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))), (Seq ((Assign
      (B.a, (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))),
      (ZCond.Term.Var B.x))))), (Seq ((Ifs ((ZCond.Atom (Le, (ZCond.Term.Var
      B.x), (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (Assume
      (ZCond.Atom (Le, (ZCond.Term.Add ((ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (ZCond.Term.Var B.y)))),
      (Assume (ZCond.Atom (Le,
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))), (ZCond.Term.Var B.y)))))),
      (Seq ((Assign (B.r, (ZCond.Term.Add
      ((sub (ZCond.Term.Var B.a) (ZCond.Term.Var B.x)), (ZCond.Term.Var
      B.y))))), (Assert (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))),
      (ZCond.Term.Var
      B.r))))))))))))))::((('c'::('o'::('n'::('v'::('e'::('x'::('_'::('h'::('u'::('l'::('l'::('6'::[])))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))), (Seq ((Assign
      (B.a, (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))),
      (ZCond.Term.Var B.x))))), (Seq ((Ifs ((ZCond.Atom (Le, (ZCond.Term.Var
      B.x), (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (Assume
      (ZCond.Atom (Le, (ZCond.Term.Var B.y), (ZCond.Term.Add ((ZCond.Term.Var
      B.x), (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))))))),
      (Assume (ZCond.Atom (Le, (ZCond.Term.Var B.y),
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Add ((sub (ZCond.Term.Var B.a) (ZCond.Term.Var B.x)),
      (ZCond.Term.Var B.y))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))))))))))))))))::((('c'::('o'::('n'::('v'::('e'::('x'::('_'::('h'::('u'::('l'::('l'::('7'::[])))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))), (Seq ((Ifs
      ((ZCond.Atom (Le, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (Seq ((Assume (ZCond.Atom
      (Le, (ZCond.Term.Add ((ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (ZCond.Term.Var B.y)))),
      (Assign (B.a, (ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH)))), (ZCond.Term.Var B.x))))))), (Seq
      ((Assume (ZCond.Atom (Le,
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))), (ZCond.Term.Var B.y)))),
      (Assume (ZCond.Atom (Le, (ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH)))), (ZCond.Term.Var B.x))), (ZCond.Term.Var
      B.a)))))))), (Seq ((Assign (B.r, (ZCond.Term.Add
      ((sub (ZCond.Term.Var B.a) (ZCond.Term.Var B.x)), (ZCond.Term.Var
      B.y))))), (Assert (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))),
      (ZCond.Term.Var
      B.r))))))))))))::((('c'::('o'::('n'::('v'::('e'::('x'::('_'::('h'::('u'::('l'::('l'::('8'::[])))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))), (Seq ((Ifs
      ((ZCond.Atom (Le, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (Seq ((Assume (ZCond.Atom
      (Le, (ZCond.Term.Var B.y), (ZCond.Term.Add ((ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))))))), (Assign
      (B.a, (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))),
      (ZCond.Term.Var B.x))))))), (Seq ((Assume (ZCond.Atom (Le,
      (ZCond.Term.Var B.y),
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))))), (Assume (ZCond.Atom (Le,
      (ZCond.Term.Var B.a), (ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH)))), (ZCond.Term.Var B.x)))))))))), (Seq
      ((Assign (B.r, (ZCond.Term.Add
      ((sub (ZCond.Term.Var B.a) (ZCond.Term.Var B.x)), (ZCond.Term.Var
      B.y))))), (Assert (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))))))))))))))::((('d'::('i'::('v'::('2'::('_'::('o'::('k'::('0'::[])))))))),(Seq
      ((div2 (ZCond.Basic true)), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.q))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var B.r))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))))))))))))))::((('d'::('i'::('v'::('2'::('_'::('o'::('k'::('1'::[])))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Lt, (ZCond.Term.Cte
      (idZ (Zneg (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH))))))))), (ZCond.Term.Var B.x))), (ZCond.Atom (Lt,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH)))))))))))))), (Seq
      ((div2 (ZCond.Atom (Lt, (ZCond.Term.Var B.a), (ZCond.Term.Cte
         (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
           Coq_xH)))))))))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.q))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Var B.q), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.r))), (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))))))))))))))))))::((('d'::('i'::('v'::('2'::('_'::('o'::('k'::('2'::[])))))))),(Seq
      ((Assume (ZCond.Atom (Eq, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ Z0))))), (Seq
      ((div2 (ZCond.Atom (Eq, (ZCond.Term.Var B.a), (ZCond.Term.Cte
         (idZ Z0))))), (Assert (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL
      (AND, (ZCond.Atom (Eq, (ZCond.Term.Var B.q), (ZCond.Term.Cte
      (idZ Z0)))), (ZCond.Atom (Eq, (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ Z0)))))))))))))::((('d'::('i'::('v'::('2'::('_'::('o'::('k'::('3'::[])))))))),(Seq
      ((Assume (ZCond.Atom (Eq, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI
        Coq_xH)))))))))))), (Seq
      ((div2 (ZCond.Atom (Eq, (ZCond.Term.Var B.a), (ZCond.Term.Cte
         (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI
           Coq_xH)))))))))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.BinL (AND, (ZCond.Atom (Eq, (ZCond.Term.Var B.q),
      (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.r))), (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))))))))))))))))::[]))))))))))))))))))))))

  (** val basic_tests_ko : (char list*statement) list **)

  let basic_tests_ko =
    (('d'::('i'::('v'::('2'::('_'::('k'::('o'::('_'::('i'::('n'::('i'::('t'::[])))))))))))),(Seq
      ((div2 (ZCond.Atom (Lt, (ZCond.Term.Var B.a), (ZCond.Term.Cte
         (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
           Coq_xH)))))))))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.q), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))))))))))::((('d'::('i'::('v'::('2'::('_'::('k'::('o'::('_'::('p'::('o'::('s'::('t'::[])))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Lt, (ZCond.Term.Cte
      (idZ (Zneg (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH))))))))), (ZCond.Term.Var B.x))), (ZCond.Atom (Lt,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH)))))))))))))), (Seq ((div2 (ZCond.Basic true)), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var B.q))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.q), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.r))), (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))))))))))))))))))::((('d'::('i'::('v'::('2'::('_'::('k'::('o'::('_'::('p'::('r'::('e'::('s'::('e'::('r'::('v'::[]))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Lt, (ZCond.Term.Cte
      (idZ (Zneg (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH))))))))), (ZCond.Term.Var B.x))), (ZCond.Atom (Lt,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH)))))))))))))), (Seq
      ((div2 (ZCond.Atom (Le, (ZCond.Term.Var B.q), (ZCond.Term.Cte
         (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))))))),
      (Assert (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.q), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))))))))))))::((('d'::('i'::('v'::('2'::('_'::('k'::('o'::('_'::('i'::('n'::('t'::('e'::('g'::('e'::('r'::('_'::('i'::('n'::('c'::('o'::('m'::('p'::('l'::('e'::('t'::('u'::('d'::('e'::[])))))))))))))))))))))))))))),(Seq
      ((Assume (ZCond.Atom (Eq, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI
        Coq_xH)))))))))))), (Seq
      ((div2 (ZCond.Atom (Eq, (ZCond.Term.Var B.a), (ZCond.Term.Cte
         (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI
           Coq_xH)))))))))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.BinL (AND, (ZCond.Atom (Eq, (ZCond.Term.Var B.q),
      (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))))),
      (ZCond.Atom (Eq, (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))))))))))))))::((('a'::('s'::('s'::('e'::('r'::('t'::('k'::('o'::('1'::('-'::('2'::('-'::('3'::[]))))))))))))),(Seq
      ((Assign (B.x, (ZCond.Term.Cte (idZ (Zpos Coq_xH))))), (Seq ((Assert
      (('a'::('s'::('s'::('e'::('r'::('t'::('k'::('o'::('1'::[]))))))))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), (ZCond.Term.Cte (idZ Z0)))))),
      (Seq ((Assign (B.x,
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Cte (idZ (Zpos Coq_xH)))))), (Seq
      ((Assert (('a'::('s'::('s'::('e'::('r'::('t'::('o'::('k'::[])))))))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), (ZCond.Term.Cte (idZ Z0)))))),
      (Seq ((Assign (B.x, (ZCond.Term.Add ((ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ (Zpos Coq_xH))))))), (Seq ((Assert
      (('a'::('s'::('s'::('e'::('r'::('t'::('k'::('o'::('2'::[]))))))))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), (ZCond.Term.Cte (idZ Z0)))))),
      (Seq ((Assign (B.x, (ZCond.Term.Add ((ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ (Zpos Coq_xH))))))), (Assert
      (('a'::('s'::('s'::('e'::('r'::('t'::('k'::('o'::('3'::[]))))))))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))))))))))))))))))))))::[]))))

  (** val coq_CTE : coq_Z **)

  let coq_CTE =
    Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
      (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO
      (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
      Coq_xH)))))))))))))))))))))))

  (** val barycentre : char list -> coq_Z -> coq_Z -> coq_Z -> statement **)

  let barycentre name lbz ubz cz =
    Seq ((Assign (B.lb, (ZCond.Term.Cte (idZ lbz)))), (Seq ((Assign (B.ub,
      (ZCond.Term.Cte (idZ ubz)))), (Seq ((Assign (B.c, (ZCond.Term.Cte
      (idZ cz)))), (Seq ((Assert
      ((append name ('C'::('h'::('e'::('c'::('k'::('C'::[]))))))),
      (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var B.c))))),
      (Seq ((Assert
      ((append name
         ('C'::('h'::('e'::('c'::('k'::('B'::('o'::('u'::('n'::('d'::[]))))))))))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.lb), (ZCond.Term.Var B.ub))))), (Seq
      ((Guassign (B.x1, (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Z.opp coq_CTE))), (ZCond.Term.Var B.x1))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x1), (ZCond.Term.Cte (idZ coq_CTE)))))))), (Seq
      ((Guassign (B.x2, (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Z.opp coq_CTE))), (ZCond.Term.Var B.x2))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x2), (ZCond.Term.Cte (idZ coq_CTE)))))))), (Seq
      ((Guassign (B.y1, (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Z.opp coq_CTE))), (ZCond.Term.Var B.y1))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.y1), (ZCond.Term.Cte (idZ coq_CTE)))))))), (Seq
      ((Guassign (B.y2, (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Z.opp coq_CTE))), (ZCond.Term.Var B.y2))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.y2), (ZCond.Term.Cte (idZ coq_CTE)))))))), (Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.a))), (ZCond.Atom (Le, (ZCond.Term.Var B.a),
      (ZCond.Term.Var B.c)))))), (Seq ((Assume (ZCond.BinL (AND, (ZCond.Atom
      (Le, (ZCond.Term.Var B.lb),
      (sub (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))),
        (ZCond.Term.Var B.x1))) (ZCond.Term.Mul ((ZCond.Term.Cte
        (idZ (Zpos (Coq_xI Coq_xH)))), (ZCond.Term.Var B.x2)))))),
      (ZCond.Atom (Le,
      (sub (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))),
        (ZCond.Term.Var B.x1))) (ZCond.Term.Mul ((ZCond.Term.Cte
        (idZ (Zpos (Coq_xI Coq_xH)))), (ZCond.Term.Var B.x2)))),
      (ZCond.Term.Var B.ub)))))), (Seq ((Assume (ZCond.BinL (AND, (ZCond.Atom
      (Le, (ZCond.Term.Var B.lb), (ZCond.Term.Add ((ZCond.Term.Mul
      ((ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))),
      (ZCond.Term.Var B.y1))), (ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH)))), (ZCond.Term.Var B.y2))))))), (ZCond.Atom
      (Le, (ZCond.Term.Add ((ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var B.y1))),
      (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))),
      (ZCond.Term.Var B.y2))))), (ZCond.Term.Var B.ub)))))), (Seq ((Guassign
      (B.aux, (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.aux))), (ZCond.Atom (Le, (ZCond.Term.Var B.aux),
      (ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))))))))), (Seq ((Ifs
      ((ZCond.Atom (Eq, (ZCond.Term.Var B.aux), (ZCond.Term.Cte (idZ Z0)))),
      (Assign (B.r,
      (sub (ZCond.Term.Add ((ZCond.Term.Mul ((ZCond.Term.Var B.a),
        (sub (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))),
          (ZCond.Term.Var B.x1))) (ZCond.Term.Mul ((ZCond.Term.Cte
          (idZ (Zpos (Coq_xI Coq_xH)))), (ZCond.Term.Var B.x2)))))),
        (ZCond.Term.Mul ((ZCond.Term.Var B.c), (ZCond.Term.Add
        ((ZCond.Term.Mul ((ZCond.Term.Cte
        (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var B.y1))),
        (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))),
        (ZCond.Term.Var B.y2))))))))) (ZCond.Term.Mul ((ZCond.Term.Add
        ((ZCond.Term.Mul ((ZCond.Term.Cte
        (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var B.y1))),
        (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))),
        (ZCond.Term.Var B.y2))))), (ZCond.Term.Var B.a)))))), (Ifs
      ((ZCond.Atom (Eq, (ZCond.Term.Var B.aux), (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))))), (Assign (B.r, (ZCond.Term.Add ((ZCond.Term.Mul
      ((ZCond.Term.Var B.a),
      (sub
        (sub (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))),
          (ZCond.Term.Var B.x1))) (ZCond.Term.Mul ((ZCond.Term.Cte
          (idZ (Zpos (Coq_xI Coq_xH)))), (ZCond.Term.Var B.x2))))
        (ZCond.Term.Add ((ZCond.Term.Mul ((ZCond.Term.Cte
        (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var B.y1))),
        (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))),
        (ZCond.Term.Var B.y2)))))))), (ZCond.Term.Mul ((ZCond.Term.Var B.c),
      (ZCond.Term.Add ((ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var B.y1))),
      (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))),
      (ZCond.Term.Var B.y2))))))))))), (Assign (B.r, (ZCond.Term.Add
      ((ZCond.Term.Mul ((sub (ZCond.Term.Var B.c) (ZCond.Term.Var B.a)),
      (ZCond.Term.Add ((ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var B.y1))),
      (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))),
      (ZCond.Term.Var B.y2))))))), (ZCond.Term.Mul
      ((sub (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))),
         (ZCond.Term.Var B.x1))) (ZCond.Term.Mul ((ZCond.Term.Cte
         (idZ (Zpos (Coq_xI Coq_xH)))), (ZCond.Term.Var B.x2)))),
      (ZCond.Term.Var B.a))))))))))), (Seq ((Assert
      ((append name ('I'::('n'::('f'::[])))), (ZCond.Atom (Le,
      (ZCond.Term.Mul ((ZCond.Term.Var B.c), (ZCond.Term.Var B.lb))),
      (ZCond.Term.Var B.r))))), (Assert
      ((append name ('S'::('u'::('p'::[])))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.r), (ZCond.Term.Mul ((ZCond.Term.Var B.c),
      (ZCond.Term.Var B.ub))))))))))))))))))))))))))))))))))))

  (** val test_barycentre : statement **)

  let test_barycentre =
    Seq
      ((barycentre ('b'::('1'::[])) (Zneg (Coq_xO (Coq_xO (Coq_xI (Coq_xO
         (Coq_xO (Coq_xI Coq_xH))))))) (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO
         (Coq_xO (Coq_xI Coq_xH))))))) (Zpos (Coq_xI (Coq_xO Coq_xH)))), (Seq
      ((barycentre ('b'::('2'::[])) (Zneg (Coq_xO (Coq_xO (Coq_xI (Coq_xO
         Coq_xH))))) (Zpos (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) (Zpos Coq_xH)),
      (Seq
      ((barycentre ('b'::('3'::[])) (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))
         (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))) (Zpos (Coq_xO
         Coq_xH))), (Seq
      ((barycentre ('b'::('4'::[])) (Zneg (Coq_xO Coq_xH)) (Zpos (Coq_xI
         Coq_xH)) (Zpos (Coq_xI Coq_xH))), (Seq
      ((barycentre ('b'::('5'::[])) (Zneg (Coq_xO Coq_xH)) (Zpos (Coq_xO
         Coq_xH)) (Zpos (Coq_xI (Coq_xO Coq_xH)))), (Seq
      ((barycentre ('b'::('6'::[])) (Zneg Coq_xH) (Zpos Coq_xH) (Zpos (Coq_xI
         (Coq_xO Coq_xH)))), (Seq
      ((barycentre ('b'::('7'::[])) Z0 Z0 (Zpos (Coq_xI (Coq_xO Coq_xH)))),
      (barycentre ('b'::('8'::[])) (Zneg Coq_xH) (Zpos (Coq_xI (Coq_xO
        Coq_xH))) (Zpos (Coq_xI (Coq_xO Coq_xH)))))))))))))))))

  (** val imul4 :
      char list -> ZTerm.term -> ZTerm.term -> ZTerm.term -> ZTerm.term ->
      ZTerm.term -> ZTerm.term -> (char list*statement) list **)

  let imul4 s a0 b c0 d e f =
    ((append ('m'::('u'::('l'::('4'::('_'::[])))))
       (append s ('_'::('o'::('k'::('1'::[])))))),(Seq ((Assume (ZCond.BinL
      (AND, (ZCond.Atom (Le, a0, (ZCond.Term.Var B.x))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), b)), (ZCond.BinL (AND,
      (ZCond.Atom (Le, c0, (ZCond.Term.Var B.y))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.y), d))))))))), (Seq ((Assign (B.r, (ZCond.Term.Annot
      (ZCond.Term.Annot.INTERV, (ZCond.Term.Annot (ZCond.Term.Annot.STATIC,
      (ZCond.Term.Mul ((ZCond.Term.Var B.x), (ZCond.Term.Var B.y))))))))),
      (Assert (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom
      (Le, e, (ZCond.Term.Var B.r))), (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      f)))))))))))::(((append ('m'::('u'::('l'::('4'::('_'::[])))))
                        (append s ('_'::('o'::('k'::('2'::[])))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, a0, (ZCond.Term.Var B.x))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Var B.x), b)),
      (ZCond.BinL (AND, (ZCond.Atom (Le, c0, (ZCond.Term.Var B.y))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.y), d))))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Annot (ZCond.Term.Annot.INTERV, (ZCond.Term.Annot
      (ZCond.Term.Annot.STATIC, (ZCond.Term.Mul ((ZCond.Term.Var B.y),
      (ZCond.Term.Var B.x))))))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, e, (ZCond.Term.Var B.r))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      f)))))))))))::(((append ('m'::('u'::('l'::('4'::('_'::[])))))
                        (append s ('_'::('o'::('k'::('3'::[])))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, a0, (ZCond.Term.Var B.x))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Var B.x), b)),
      (ZCond.BinL (AND, (ZCond.Atom (Le, c0, (ZCond.Term.Var B.y))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.y), d))))))))), (Seq ((Assume
      (ZCond.Atom (Le, (ZCond.Term.Mul ((ZCond.Term.Var B.y), (ZCond.Term.Var
      B.x))), (ZCond.Term.Var B.r)))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le, e, (ZCond.Term.Var
      B.r))))))))))::(((append ('m'::('u'::('l'::('4'::('_'::[])))))
                         (append s ('_'::('o'::('k'::('4'::[])))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, a0, (ZCond.Term.Var B.x))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Var B.x), b)),
      (ZCond.BinL (AND, (ZCond.Atom (Le, c0, (ZCond.Term.Var B.y))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.y), d))))))))), (Seq ((Assume
      (ZCond.Atom (Le, (ZCond.Term.Var B.r), (ZCond.Term.Mul ((ZCond.Term.Var
      B.x), (ZCond.Term.Var B.y)))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      f)))))))))::[])))

  (** val imul30 :
      char list -> ZTerm.term -> ZTerm.term -> ZTerm.term -> ZTerm.term ->
      ZTerm.term -> (char list*statement) list **)

  let imul30 s a0 b c0 e f =
    ((append ('m'::('u'::('l'::('3'::('0'::('_'::[]))))))
       (append s ('_'::('o'::('k'::('1'::[])))))),(Seq ((Assume (ZCond.BinL
      (AND, (ZCond.Atom (Le, a0, (ZCond.Term.Var B.x))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), b)), (ZCond.Atom (Le, c0,
      (ZCond.Term.Var B.y)))))))), (Seq ((Assign (B.r, (ZCond.Term.Annot
      (ZCond.Term.Annot.INTERV, (ZCond.Term.Annot (ZCond.Term.Annot.STATIC,
      (ZCond.Term.Mul ((ZCond.Term.Var B.x), (ZCond.Term.Var B.y))))))))),
      (Assert (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom
      (Le, e, (ZCond.Term.Var B.r))), (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      f)))))))))))::(((append ('m'::('u'::('l'::('3'::('0'::('_'::[]))))))
                        (append s ('_'::('o'::('k'::('2'::[])))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, a0, (ZCond.Term.Var B.x))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Var B.x), b)),
      (ZCond.Atom (Le, c0, (ZCond.Term.Var B.y)))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Annot (ZCond.Term.Annot.INTERV, (ZCond.Term.Annot
      (ZCond.Term.Annot.STATIC, (ZCond.Term.Mul ((ZCond.Term.Var B.y),
      (ZCond.Term.Var B.x))))))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, e, (ZCond.Term.Var B.r))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.r), f)))))))))))::[])

  (** val imul31 :
      char list -> ZTerm.term -> ZTerm.term -> ZTerm.term -> ZTerm.term ->
      (char list*statement) list **)

  let imul31 s a0 b c0 e =
    ((append ('m'::('u'::('l'::('3'::('1'::('_'::[]))))))
       (append s ('_'::('o'::('k'::('1'::[])))))),(Seq ((Assume (ZCond.BinL
      (AND, (ZCond.Atom (Le, a0, (ZCond.Term.Var B.x))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), b)), (ZCond.Atom (Le, c0,
      (ZCond.Term.Var B.y)))))))), (Seq ((Assign (B.r, (ZCond.Term.Annot
      (ZCond.Term.Annot.INTERV, (ZCond.Term.Annot (ZCond.Term.Annot.STATIC,
      (ZCond.Term.Mul ((ZCond.Term.Var B.x), (ZCond.Term.Var B.y))))))))),
      (Assert (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le, e,
      (ZCond.Term.Var
      B.r))))))))))::(((append ('m'::('u'::('l'::('3'::('1'::('_'::[]))))))
                         (append s ('_'::('o'::('k'::('2'::[])))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, a0, (ZCond.Term.Var B.x))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Var B.x), b)),
      (ZCond.Atom (Le, c0, (ZCond.Term.Var B.y)))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Annot (ZCond.Term.Annot.INTERV, (ZCond.Term.Annot
      (ZCond.Term.Annot.STATIC, (ZCond.Term.Mul ((ZCond.Term.Var B.y),
      (ZCond.Term.Var B.x))))))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.Atom (Le, e, (ZCond.Term.Var B.r))))))))))::[])

  (** val imul32 :
      char list -> ZTerm.term -> ZTerm.term -> ZTerm.term -> ZTerm.term ->
      (char list*statement) list **)

  let imul32 s a0 b c0 f =
    ((append ('m'::('u'::('l'::('3'::('2'::('_'::[]))))))
       (append s ('_'::('o'::('k'::('1'::[])))))),(Seq ((Assume (ZCond.BinL
      (AND, (ZCond.Atom (Le, a0, (ZCond.Term.Var B.x))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), b)), (ZCond.Atom (Le, c0,
      (ZCond.Term.Var B.y)))))))), (Seq ((Assign (B.r, (ZCond.Term.Annot
      (ZCond.Term.Annot.INTERV, (ZCond.Term.Annot (ZCond.Term.Annot.STATIC,
      (ZCond.Term.Mul ((ZCond.Term.Var B.x), (ZCond.Term.Var B.y))))))))),
      (Assert (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.r),
      f)))))))))::(((append ('m'::('u'::('l'::('3'::('2'::('_'::[]))))))
                      (append s ('_'::('o'::('k'::('2'::[])))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, a0, (ZCond.Term.Var B.x))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Var B.x), b)),
      (ZCond.Atom (Le, c0, (ZCond.Term.Var B.y)))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Annot (ZCond.Term.Annot.INTERV, (ZCond.Term.Annot
      (ZCond.Term.Annot.STATIC, (ZCond.Term.Mul ((ZCond.Term.Var B.y),
      (ZCond.Term.Var B.x))))))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.r), f)))))))))::[])

  (** val itv_tests_ok : (char list*statement) list **)

  let itv_tests_ok =
    (('n'::('o'::('n'::('l'::('i'::('n'::('_'::('o'::('k'::[]))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte (idZ Z0))))))), (Seq ((Assign
      (B.x, (ZCond.Term.Mul ((ZCond.Term.Var B.y), (ZCond.Term.Var B.x))))),
      (Assert (('p'::('o'::('s'::('t'::[])))), (ZCond.Basic
      false))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('s'::('k'::('i'::('p'::('1'::('_'::('1'::('_'::('o'::('k'::[]))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Lt, (ZCond.Term.Var B.y),
      (ZCond.Term.Var B.x))), (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.r)))))), (Seq ((Assume
      (ZCond.Atom (Le, (ZCond.Term.Mul ((ZCond.Term.Var B.x), (ZCond.Term.Var
      B.r))), (ZCond.Term.Var B.y)))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte (idZ Z0)))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ Z0)))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('s'::('k'::('i'::('p'::('1'::('_'::('X'::('_'::('o'::('k'::[]))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Lt, (ZCond.Term.Var B.y),
      (ZCond.Term.Var B.x))), (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.r)))))), (Seq ((Assume
      (ZCond.Atom (Le, (ZCond.Term.Annot (ZCond.Term.Annot.SKIP_ORACLE,
      (ZCond.Term.Mul ((ZCond.Term.Var B.r), (ZCond.Term.Var B.x))))),
      (ZCond.Term.Var B.y)))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.BinL (AND, (ZCond.Atom (Lt, (ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ Z0)))), (ZCond.Atom (Lt, (ZCond.Term.Var B.y),
      (ZCond.Term.Cte
      (idZ Z0)))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('s'::('k'::('i'::('p'::('2'::('_'::('o'::('k'::[]))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Lt, (ZCond.Term.Var B.y),
      (ZCond.Term.Var B.x))), (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.r)))))), (Seq ((Assume
      (ZCond.Atom (Le, (ZCond.Term.Mul ((ZCond.Term.Var B.r), (ZCond.Term.Var
      B.x))), (ZCond.Term.Cte (idZ (Zpos Coq_xH)))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte (idZ (Zpos Coq_xH))))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('s'::('k'::('i'::('p'::('3'::('_'::('o'::('k'::[]))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Lt, (ZCond.Term.Var B.y),
      (ZCond.Term.Var B.x))), (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      (ZCond.Term.Cte (idZ (Zpos Coq_xH)))))))), (Seq ((Assume (ZCond.Atom
      (Le, (ZCond.Term.Mul ((ZCond.Term.Var B.r), (ZCond.Term.Var B.x))),
      (ZCond.Term.Var B.y)))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var
      B.x))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('s'::('k'::('i'::('p'::('4'::('_'::('o'::('k'::[]))))))))))))))),(Seq
      ((Assume (ZCond.Atom (Lt, (ZCond.Term.Var B.y), (ZCond.Term.Var
      B.x)))), (Seq ((Assume (ZCond.Atom (Le, (ZCond.Term.Mul
      ((ZCond.Term.Var B.r), (ZCond.Term.Var B.x))), (ZCond.Term.Var B.y)))),
      (Assert (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Lt,
      (ZCond.Term.Var B.y), (ZCond.Term.Var
      B.x))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('1'::('_'::('o'::('k'::[])))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zneg (Coq_xI Coq_xH)))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH))))))))), (Seq ((Assume (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))),
      (ZCond.Term.Annot (ZCond.Term.Annot.INTERV, (ZCond.Term.Mul
      ((ZCond.Term.Add ((ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))))), (ZCond.Term.Add ((ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ (Zpos Coq_xH)))))))))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var
      B.x))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('2'::('_'::('o'::('k'::[])))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zneg (Coq_xI Coq_xH)))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH))))))))), (Seq ((Assume (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))),
      (ZCond.Term.Mul ((ZCond.Term.Add ((ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))))), (ZCond.Term.Add ((ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ (Zpos Coq_xH)))))))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))),
      (ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var B.x))))),
      (ZCond.Atom (Eq, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH)))))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('2'::('_'::('1'::('_'::('o'::('k'::[])))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zneg (Coq_xI Coq_xH)))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH))))))))), (Seq ((Assume (ZCond.Atom (Lt,
      (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))),
      (ZCond.Term.Mul ((ZCond.Term.Add ((ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))))), (ZCond.Term.Add ((ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ (Zpos Coq_xH)))))))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))),
      (ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var B.x))))),
      (ZCond.Atom (Eq, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH)))))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('3'::('_'::('o'::('k'::[])))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Lt, (ZCond.Term.Cte
      (idZ (Zneg Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.Atom (Lt,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte (idZ (Zpos Coq_xH)))))))), (Seq
      ((Assume (ZCond.Atom (Neq, (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Mul
      ((ZCond.Term.Var B.x), (ZCond.Term.Var B.x)))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.Basic
      false))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::('1'::('_'::('o'::('k'::[]))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zneg (Coq_xI Coq_xH)))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH))))))))), (Seq ((Assign (B.r, (ZCond.Term.Mul
      ((ZCond.Term.Add ((ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))))), (ZCond.Term.Add ((ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ (Zpos Coq_xH))))))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (sub (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))),
        (ZCond.Term.Var B.x))) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))), (ZCond.Term.Var
      B.r))), (ZCond.Atom (Le, (ZCond.Term.Var B.r), (ZCond.Term.Add
      ((ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))),
      (ZCond.Term.Var B.x))), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::('2'::('_'::('o'::('k'::[]))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var B.x))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.y))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH))))))))))), (Seq ((Assign (B.y,
      (ZCond.Term.Mul ((ZCond.Term.Add ((ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO Coq_xH)))))), (ZCond.Term.Mul ((ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))))))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var
      B.x))), (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Mul
      ((ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (ZCond.Term.Var
      B.y))), (ZCond.Atom (Le, (ZCond.Term.Var B.y), (ZCond.Term.Mul
      ((ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))))))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::('3'::('_'::('o'::('k'::[]))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO Coq_xH))))))), (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var B.y)))))))), (Seq ((Assign
      (B.x, (ZCond.Term.Mul ((ZCond.Term.Mul
      ((sub (ZCond.Term.Var B.x) (ZCond.Term.Cte (idZ (Zpos Coq_xH)))),
      (ZCond.Term.Var B.x))),
      (sub (ZCond.Term.Add ((ZCond.Term.Var B.x), (ZCond.Term.Var B.y)))
        (ZCond.Term.Cte (idZ (Zpos Coq_xH)))))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (sub (ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zneg (Coq_xI Coq_xH)))),
        (ZCond.Term.Var B.y))) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))))),
      (ZCond.Term.Var B.x))), (ZCond.Atom (Le, (ZCond.Term.Var B.x),
      (ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))), (ZCond.Term.Add
      ((ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO Coq_xH))))))))))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::('4'::('_'::('o'::('k'::[]))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.y),
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Cte (idZ (Zpos Coq_xH))))))))))),
      (Seq ((Assign (B.r, (ZCond.Term.Annot (ZCond.Term.Annot.INTERV,
      (ZCond.Term.Mul ((ZCond.Term.Var B.x),
      (sub (ZCond.Term.Var B.y) (ZCond.Term.Var B.x)))))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::('5'::('_'::('0'::('_'::('o'::('k'::[]))))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Mul ((ZCond.Term.Var B.x),
      (sub (ZCond.Term.Var B.y) (ZCond.Term.Var B.x)))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::('5'::('_'::('0'::('_'::('X'::('_'::('o'::('k'::[]))))))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Annot (ZCond.Term.Annot.SKIP_ORACLE, (ZCond.Term.Mul
      ((ZCond.Term.Var B.x),
      (sub (ZCond.Term.Var B.y) (ZCond.Term.Var B.x)))))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::('5'::('_'::('2'::('_'::('o'::('k'::[]))))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))), (Seq ((Assign
      (B.r, (ZCond.Term.Mul ((ZCond.Term.Var B.x),
      (sub (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))
        (ZCond.Term.Var B.x)))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))))))))))))::((('d'::('o'::('u'::('b'::('l'::('e'::('_'::('i'::('n'::('t'::('e'::('r'::('v'::('a'::('l'::('i'::('z'::('a'::('t'::('i'::('o'::('n'::('_'::('u'::('n'::('s'::('a'::('t'::('_'::('o'::('k'::[]))))))))))))))))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.x))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Add ((ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))))), (ZCond.Term.Var B.y))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
        (Coq_xI (Coq_xI Coq_xH)))))))))))))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.c), (ZCond.Term.Cte
      (idZ (Zneg (Coq_xO Coq_xH))))))))))))), (Seq ((Assume (ZCond.Atom (Le,
      (ZCond.Term.Mul ((ZCond.Term.Var B.x),
      (sub (ZCond.Term.Var B.y) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xO Coq_xH))))))), (ZCond.Term.Var B.c)))), (Seq
      ((Assert (('p'::('o'::('s'::('t'::('1'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))))), (Seq
      ((Assert (('p'::('o'::('s'::('t'::('2'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))), (ZCond.Term.Var
      B.y))))), (Assert (('p'::('o'::('s'::('t'::('3'::[]))))), (ZCond.Basic
      false))))))))))))::((('b'::('a'::('r'::('y'::('c'::('e'::('n'::('t'::('r'::('e'::('_'::('o'::('k'::('1'::('_'::('1'::[])))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.a))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.a), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (ZCond.BinL (AND, (ZCond.Atom
      (Le, (ZCond.Term.Cte (idZ (Zneg (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))),
      (ZCond.Term.Var B.x))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zneg (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (ZCond.Term.Var
      B.y))), (ZCond.Atom (Le, (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))))))))))), (Seq
      ((Assign (B.r, (ZCond.Term.Add ((ZCond.Term.Mul ((ZCond.Term.Var B.a),
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Var B.y)))), (ZCond.Term.Mul
      ((ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))),
      (ZCond.Term.Var B.y))))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zneg (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))),
      (ZCond.Term.Var B.r))), (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))))))))))))))::((('b'::('a'::('r'::('y'::('c'::('e'::('n'::('t'::('r'::('e'::('_'::('o'::('k'::('1'::('_'::('2'::[])))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.a))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.a), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (ZCond.BinL (AND, (ZCond.Atom
      (Le, (ZCond.Term.Cte (idZ (Zneg (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))),
      (ZCond.Term.Var B.x))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zneg (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (ZCond.Term.Var
      B.y))), (ZCond.Atom (Le, (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))))))))))), (Seq
      ((Assign (B.r, (ZCond.Term.Add ((ZCond.Term.Mul ((ZCond.Term.Var B.a),
      (ZCond.Term.Var B.x))), (ZCond.Term.Mul
      ((sub (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))
         (ZCond.Term.Var B.a)), (ZCond.Term.Var B.y))))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Cte
      (idZ (Zneg (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))),
      (ZCond.Term.Var B.r))), (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))))))))))))))::((('b'::('a'::('r'::('y'::('c'::('e'::('n'::('t'::('r'::('e'::('_'::('o'::('k'::('2'::('_'::('1'::('_'::('X'::('1'::[]))))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.a))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Var B.a), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (ZCond.BinL (AND, (ZCond.Atom
      (Le, (ZCond.Term.Cte (idZ (Zneg Coq_xH))), (ZCond.Term.Var B.x))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ (Zpos Coq_xH))))), (ZCond.BinL (AND, (ZCond.Atom
      (Le, (ZCond.Term.Cte (idZ (Zneg Coq_xH))), (ZCond.Term.Var B.y))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos Coq_xH)))))))))))))))), (Seq ((Assign (B.r, (ZCond.Term.Add
      ((ZCond.Term.Mul ((ZCond.Term.Var B.a),
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Var B.y)))), (ZCond.Term.Mul
      ((ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))),
      (ZCond.Term.Var B.y))))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('1'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zneg (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var
      B.r))))), (Assert (('p'::('o'::('s'::('t'::('2'::[]))))), (ZCond.Atom
      (Le, (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))))))))))))::((('b'::('a'::('r'::('y'::('c'::('e'::('n'::('t'::('r'::('e'::('_'::('o'::('k'::('2'::('_'::('1'::('_'::('X'::('2'::[]))))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.a))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Var B.a), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (ZCond.BinL (AND, (ZCond.Atom
      (Le, (ZCond.Term.Cte (idZ (Zneg Coq_xH))), (ZCond.Term.Var B.x))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ (Zpos Coq_xH))))), (ZCond.BinL (AND, (ZCond.Atom
      (Le, (ZCond.Term.Cte (idZ (Zneg Coq_xH))), (ZCond.Term.Var B.y))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos Coq_xH)))))))))))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Annot (ZCond.Term.Annot.SKIP_ORACLE, (ZCond.Term.Add
      ((ZCond.Term.Mul ((ZCond.Term.Var B.a),
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Var B.y)))), (ZCond.Term.Mul
      ((ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))),
      (ZCond.Term.Var B.y))))))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zneg (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var B.r))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))))))))))))::((('b'::('a'::('r'::('y'::('c'::('e'::('n'::('t'::('r'::('e'::('_'::('o'::('k'::('2'::('_'::('2'::[])))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.a))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Var B.a), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (ZCond.BinL (AND, (ZCond.Atom
      (Le, (ZCond.Term.Cte (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Cte (idZ (Zpos Coq_xH))), (ZCond.Term.Var
      B.y))), (ZCond.Atom (Le, (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH))))))))))))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Add ((ZCond.Term.Mul ((ZCond.Term.Var B.a),
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Var B.y)))), (ZCond.Term.Mul
      ((ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))),
      (ZCond.Term.Var B.y))))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('1'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var
      B.r))))), (Assert (('p'::('o'::('s'::('t'::('2'::[]))))), (ZCond.Atom
      (Le, (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))))))))))))::((('b'::('a'::('r'::('y'::('c'::('e'::('n'::('t'::('r'::('e'::('_'::('o'::('k'::('2'::('_'::('2'::('_'::('X'::[])))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte (idZ Z0)),
      (ZCond.Term.Var B.a))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Var B.a), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (ZCond.BinL (AND, (ZCond.Atom
      (Le, (ZCond.Term.Cte (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Cte (idZ (Zpos Coq_xH))), (ZCond.Term.Var
      B.y))), (ZCond.Atom (Le, (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH))))))))))))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Annot (ZCond.Term.Annot.SKIP_ORACLE, (ZCond.Term.Add
      ((ZCond.Term.Mul ((ZCond.Term.Var B.a),
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Var B.y)))), (ZCond.Term.Mul
      ((ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))),
      (ZCond.Term.Var B.y))))))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var B.r))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))))))))))))::((('b'::('a'::('r'::('y'::('c'::('e'::('n'::('t'::('r'::('e'::('_'::('o'::('k'::('3'::[])))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.a))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.a), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.y), (ZCond.Term.Var B.x)))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Add ((ZCond.Term.Mul ((ZCond.Term.Var B.a),
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Var B.y)))), (ZCond.Term.Mul
      ((ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))),
      (ZCond.Term.Var B.y))))))), (Seq ((Assume (ZCond.Atom (Le,
      (ZCond.Term.Var B.y), (ZCond.Term.Var B.x)))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Var B.r), (ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var B.x))))),
      (ZCond.Atom (Le, (ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var B.y))),
      (ZCond.Term.Var
      B.r))))))))))))))::((('b'::('i'::('g'::('_'::('b'::('a'::('r'::('y'::('c'::('e'::('n'::('t'::('r'::('e'::('_'::('o'::('k'::[]))))))))))))))))),test_barycentre)::((('p'::('a'::('r'::('a'::('b'::('o'::('l'::('a'::('_'::('o'::('k'::[]))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Eq, (ZCond.Term.Var B.a),
      (ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))))), (ZCond.BinL (AND,
      (ZCond.Atom (Eq, (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.q), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH)))))))))))))))), (Seq ((Assume (ZCond.BinL (AND, (ZCond.Atom
      (Le, (ZCond.Term.Var B.y), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Var B.q)))))), (Seq ((Assign (B.r,
      (ZCond.Term.Mul ((ZCond.Term.Mul ((ZCond.Term.Var B.a), (ZCond.Term.Var
      B.x))), (ZCond.Term.Var B.x))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('1'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Mul ((ZCond.Term.Mul ((ZCond.Term.Var B.a), (ZCond.Term.Var
      B.y))), (ZCond.Term.Var B.y))), (ZCond.Term.Var B.r))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('2'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.r), (ZCond.Term.Mul ((ZCond.Term.Mul ((ZCond.Term.Var
      B.a), (ZCond.Term.Var B.q))), (ZCond.Term.Var B.q))))))), (Assert
      (('p'::('o'::('s'::('t'::('3'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.r), (ZCond.Term.Mul ((ZCond.Term.Var B.a),
      (sub (ZCond.Term.Mul ((ZCond.Term.Add ((ZCond.Term.Var B.q),
        (ZCond.Term.Var B.y))), (ZCond.Term.Var B.x))) (ZCond.Term.Mul
        ((ZCond.Term.Var B.q), (ZCond.Term.Var B.y)))))))))))))))))))))::((('p'::('a'::('r'::('a'::('b'::('o'::('l'::('a'::('_'::('o'::('k'::('1'::[])))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Eq, (ZCond.Term.Var B.a),
      (ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))))), (ZCond.BinL (AND,
      (ZCond.Atom (Eq, (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.q), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH)))))))))))))))), (Seq ((Assume (ZCond.BinL (AND, (ZCond.Atom
      (Le, (ZCond.Term.Var B.y), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Var B.q)))))), (Seq ((Assign (B.r,
      (ZCond.Term.Annot (ZCond.Term.Annot.SKIP_ORACLE,
      (sub (ZCond.Term.Add ((ZCond.Term.Mul ((ZCond.Term.Annot
        (ZCond.Term.Annot.INTERV, (ZCond.Term.Mul ((ZCond.Term.Var B.a),
        (sub (ZCond.Term.Var B.x) (ZCond.Term.Cte
          (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))),
        (sub (ZCond.Term.Var B.x) (ZCond.Term.Var B.y)))), (ZCond.Term.Mul
        ((ZCond.Term.Annot (ZCond.Term.Annot.INTERV, (ZCond.Term.Mul
        ((ZCond.Term.Mul ((ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))),
        (ZCond.Term.Var B.a))), (ZCond.Term.Cte
        (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))))), (ZCond.Term.Var
        B.x))))) (ZCond.Term.Mul ((ZCond.Term.Cte
        (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
          Coq_xH))))))))), (ZCond.Term.Var B.a)))))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('1'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Mul ((ZCond.Term.Mul ((ZCond.Term.Var B.a), (ZCond.Term.Var
      B.y))), (ZCond.Term.Var B.y))), (ZCond.Term.Var B.r))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('2'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.r), (ZCond.Term.Mul ((ZCond.Term.Mul ((ZCond.Term.Var
      B.a), (ZCond.Term.Var B.q))), (ZCond.Term.Var B.q))))))), (Assert
      (('p'::('o'::('s'::('t'::('3'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.r), (ZCond.Term.Mul ((ZCond.Term.Var B.a),
      (sub (ZCond.Term.Mul ((ZCond.Term.Add ((ZCond.Term.Var B.q),
        (ZCond.Term.Var B.y))), (ZCond.Term.Var B.x))) (ZCond.Term.Mul
        ((ZCond.Term.Var B.q), (ZCond.Term.Var B.y)))))))))))))))))))))::((('p'::('a'::('r'::('a'::('b'::('o'::('l'::('a'::('_'::('o'::('k'::('2'::[])))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Eq, (ZCond.Term.Var B.a),
      (ZCond.Term.Cte (idZ (Zpos (Coq_xO Coq_xH)))))), (ZCond.BinL (AND,
      (ZCond.Atom (Eq, (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.q), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH)))))))))))))))), (Seq ((Assume (ZCond.BinL (AND, (ZCond.Atom
      (Le, (ZCond.Term.Var B.y), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Var B.q)))))), (Seq ((Assign (B.r,
      (ZCond.Term.Annot (ZCond.Term.Annot.SKIP_ORACLE, (ZCond.Term.Add
      ((ZCond.Term.Add ((ZCond.Term.Add ((ZCond.Term.Mul ((ZCond.Term.Annot
      (ZCond.Term.Annot.INTERV, (ZCond.Term.Var B.a))),
      (sub (ZCond.Term.Mul ((ZCond.Term.Var B.x), (ZCond.Term.Cte
        (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
          Coq_xH)))))))))))), (ZCond.Term.Mul ((ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH))))))))), (ZCond.Term.Var B.a))))), (ZCond.Term.Mul
      ((ZCond.Term.Annot (ZCond.Term.Annot.INTERV, (ZCond.Term.Mul
      ((ZCond.Term.Var B.a),
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))),
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Var B.y)))))), (ZCond.Term.Mul
      ((ZCond.Term.Annot (ZCond.Term.Annot.INTERV, (ZCond.Term.Mul
      ((ZCond.Term.Var B.a), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))))),
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Cte
        (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))))))), (Seq
      ((Assert (('p'::('o'::('s'::('t'::('1'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Mul ((ZCond.Term.Mul ((ZCond.Term.Var B.a), (ZCond.Term.Var
      B.y))), (ZCond.Term.Var B.y))), (ZCond.Term.Var B.r))))), (Seq ((Assert
      (('p'::('o'::('s'::('t'::('2'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.r), (ZCond.Term.Mul ((ZCond.Term.Mul ((ZCond.Term.Var
      B.a), (ZCond.Term.Var B.q))), (ZCond.Term.Var B.q))))))), (Assert
      (('p'::('o'::('s'::('t'::('3'::[]))))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.r), (ZCond.Term.Mul ((ZCond.Term.Var B.a),
      (sub (ZCond.Term.Mul ((ZCond.Term.Add ((ZCond.Term.Var B.q),
        (ZCond.Term.Var B.y))), (ZCond.Term.Var B.x))) (ZCond.Term.Mul
        ((ZCond.Term.Var B.q), (ZCond.Term.Var B.y)))))))))))))))))))))::
      (app
        (imul4 ('p'::('p'::('p'::('p'::[])))) (ZCond.Term.Cte
          (idZ (Zpos (Coq_xI Coq_xH)))) (ZCond.Term.Cte
          (idZ (Zpos (Coq_xI (Coq_xI Coq_xH))))) (ZCond.Term.Cte
          (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))) (ZCond.Term.Cte
          (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))) (ZCond.Term.Cte
          (idZ (Zpos (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))) (ZCond.Term.Cte
          (idZ (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
            Coq_xH))))))))))
        (app
          (imul4 ('n'::('p'::('p'::('p'::[])))) (ZCond.Term.Cte
            (idZ (Zneg (Coq_xI (Coq_xI Coq_xH))))) (ZCond.Term.Cte
            (idZ (Zpos (Coq_xI Coq_xH)))) (ZCond.Term.Cte
            (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))) (ZCond.Term.Cte
            (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))) (ZCond.Term.Cte
            (idZ (Zneg (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
              Coq_xH))))))))) (ZCond.Term.Cte
            (idZ (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))))
          (app
            (imul4 ('n'::('n'::('p'::('p'::[])))) (ZCond.Term.Cte
              (idZ (Zneg (Coq_xI (Coq_xI Coq_xH))))) (ZCond.Term.Cte
              (idZ (Zneg (Coq_xI Coq_xH)))) (ZCond.Term.Cte
              (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))) (ZCond.Term.Cte
              (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))) (ZCond.Term.Cte
              (idZ (Zneg (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
                Coq_xH))))))))) (ZCond.Term.Cte
              (idZ (Zneg (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))
            (app
              (imul4 ('n'::('n'::('n'::('p'::[])))) (ZCond.Term.Cte
                (idZ (Zneg (Coq_xI (Coq_xI Coq_xH))))) (ZCond.Term.Cte
                (idZ (Zneg (Coq_xI Coq_xH)))) (ZCond.Term.Cte
                (idZ (Zneg (Coq_xI (Coq_xO Coq_xH))))) (ZCond.Term.Cte
                (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))
                (ZCond.Term.Cte
                (idZ (Zneg (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
                  Coq_xH))))))))) (ZCond.Term.Cte
                (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO
                  Coq_xH)))))))))
              (app
                (imul4 ('n'::('n'::('n'::('n'::[])))) (ZCond.Term.Cte
                  (idZ (Zneg (Coq_xI (Coq_xI Coq_xH))))) (ZCond.Term.Cte
                  (idZ (Zneg (Coq_xI Coq_xH)))) (ZCond.Term.Cte
                  (idZ (Zneg (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))
                  (ZCond.Term.Cte (idZ (Zneg (Coq_xI (Coq_xO Coq_xH)))))
                  (ZCond.Term.Cte
                  (idZ (Zpos (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))
                  (ZCond.Term.Cte
                  (idZ (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
                    Coq_xH))))))))))
                (app
                  (imul4 ('n'::('z'::('p'::('p'::[])))) (ZCond.Term.Cte
                    (idZ (Zneg (Coq_xI (Coq_xI Coq_xH))))) (ZCond.Term.Cte
                    (idZ Z0)) (ZCond.Term.Cte
                    (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))) (ZCond.Term.Cte
                    (idZ (Zpos (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))
                    (ZCond.Term.Cte
                    (idZ (Zneg (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO
                      (Coq_xO Coq_xH))))))))) (ZCond.Term.Cte (idZ Z0)))
                  (app
                    (imul30 ('z'::('p'::[])) (ZCond.Term.Cte (idZ Z0))
                      (ZCond.Term.Cte (idZ Z0)) (ZCond.Term.Cte
                      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))) (ZCond.Term.Cte
                      (idZ Z0)) (ZCond.Term.Cte (idZ Z0)))
                    (app
                      (imul30 ('z'::('n'::[])) (ZCond.Term.Cte (idZ Z0))
                        (ZCond.Term.Cte (idZ Z0)) (ZCond.Term.Cte
                        (idZ (Zneg (Coq_xI (Coq_xO Coq_xH)))))
                        (ZCond.Term.Cte (idZ Z0)) (ZCond.Term.Cte (idZ Z0)))
                      (app
                        (imul31 ('p'::('p'::('p'::[]))) (ZCond.Term.Cte
                          (idZ (Zpos (Coq_xI Coq_xH)))) (ZCond.Term.Cte
                          (idZ (Zpos (Coq_xI (Coq_xI Coq_xH)))))
                          (ZCond.Term.Cte
                          (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))
                          (ZCond.Term.Cte
                          (idZ (Zpos (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))
                        (app
                          (imul31 ('p'::('p'::('n'::[]))) (ZCond.Term.Cte
                            (idZ (Zpos (Coq_xI Coq_xH)))) (ZCond.Term.Cte
                            (idZ (Zpos (Coq_xI (Coq_xI Coq_xH)))))
                            (ZCond.Term.Cte
                            (idZ (Zneg (Coq_xI (Coq_xO Coq_xH)))))
                            (ZCond.Term.Cte
                            (idZ (Zneg (Coq_xI (Coq_xI (Coq_xO (Coq_xO
                              (Coq_xO Coq_xH)))))))))
                          (imul32 ('n'::('n'::('p'::[]))) (ZCond.Term.Cte
                            (idZ (Zneg (Coq_xI (Coq_xI Coq_xH)))))
                            (ZCond.Term.Cte (idZ (Zneg (Coq_xI Coq_xH))))
                            (ZCond.Term.Cte
                            (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))
                            (ZCond.Term.Cte
                            (idZ (Zneg (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))))))))))))))))))))))))))))))))))))))))

  (** val itv_tests_ko : (char list*statement) list **)

  let itv_tests_ko =
    (('n'::('o'::('n'::('l'::('i'::('n'::('_'::('s'::('k'::('i'::('p'::('1'::('_'::('X'::('_'::('k'::('o'::[]))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Lt, (ZCond.Term.Var B.y),
      (ZCond.Term.Var B.x))), (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.r)))))), (Seq ((Assume
      (ZCond.Atom (Le, (ZCond.Term.Annot (ZCond.Term.Annot.SKIP_ORACLE,
      (ZCond.Term.Opp (ZCond.Term.Mul ((ZCond.Term.Var B.r), (ZCond.Term.Opp
      (ZCond.Term.Var B.x))))))), (ZCond.Term.Var B.y)))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom (Lt,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte (idZ Z0)))), (ZCond.Atom (Lt,
      (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ Z0)))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('s'::('k'::('i'::('p'::('1'::('_'::('k'::('o'::[]))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Lt, (ZCond.Term.Var B.y),
      (ZCond.Term.Var B.x))), (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.r)))))), (Seq ((Assume
      (ZCond.Atom (Le, (ZCond.Term.Mul ((ZCond.Term.Var B.r), (ZCond.Term.Var
      B.x))), (ZCond.Term.Var B.y)))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom (Lt,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte (idZ Z0)))), (ZCond.Atom (Lt,
      (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ Z0)))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('1'::('_'::('k'::('o'::[])))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zneg (Coq_xI Coq_xH)))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH))))))))), (Seq ((Assume (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))),
      (ZCond.Term.Annot (ZCond.Term.Annot.SKIP_ORACLE, (ZCond.Term.Annot
      (ZCond.Term.Annot.INTERV, (ZCond.Term.Mul ((ZCond.Term.Add
      ((ZCond.Term.Var B.x), (ZCond.Term.Cte (idZ (Zpos Coq_xH))))),
      (ZCond.Term.Add ((ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos Coq_xH)))))))))))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO Coq_xH)))), (ZCond.Term.Var
      B.x))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::('3'::('_'::('k'::('o'::[]))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO Coq_xH))))))), (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var B.y)))))))), (Seq ((Assign
      (B.x, (ZCond.Term.Mul ((ZCond.Term.Mul
      ((sub (ZCond.Term.Var B.x) (ZCond.Term.Cte (idZ (Zpos Coq_xH)))),
      (ZCond.Term.Var B.x))),
      (sub (ZCond.Term.Add ((ZCond.Term.Var B.x), (ZCond.Term.Var B.y)))
        (ZCond.Term.Cte (idZ (Zpos Coq_xH)))))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ Z0)), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Add ((ZCond.Term.Mul ((ZCond.Term.Var
      B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))))))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::('4'::('_'::('k'::('o'::[]))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.y),
      (sub (ZCond.Term.Var B.x) (ZCond.Term.Cte (idZ (Zpos Coq_xH))))))))))),
      (Seq ((Assign (B.r, (ZCond.Term.Annot (ZCond.Term.Annot.SKIP_ORACLE,
      (ZCond.Term.Annot (ZCond.Term.Annot.INTERV, (ZCond.Term.Annot
      (ZCond.Term.Annot.STATIC, (ZCond.Term.Mul ((ZCond.Term.Var B.x),
      (sub (ZCond.Term.Var B.y) (ZCond.Term.Var B.x)))))))))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::('5'::('_'::('1'::('_'::('k'::('o'::[]))))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Annot (ZCond.Term.Annot.SKIP_ORACLE, (ZCond.Term.Annot
      (ZCond.Term.Annot.STATIC, (ZCond.Term.Mul ((ZCond.Term.Var B.x),
      (sub (ZCond.Term.Var B.y) (ZCond.Term.Var B.x)))))))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::('5'::('_'::('2'::('_'::('k'::('o'::[]))))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))), (Seq ((Assign
      (B.r, (ZCond.Term.Annot (ZCond.Term.Annot.SKIP_ORACLE, (ZCond.Term.Mul
      ((ZCond.Term.Var B.x),
      (sub (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))
        (ZCond.Term.Var B.x)))))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::('5'::('_'::('3'::('_'::('k'::('o'::[]))))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (ZCond.Atom (Eq,
      (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Mul ((ZCond.Term.Var B.x),
      (sub (ZCond.Term.Var B.y) (ZCond.Term.Var B.x)))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.Atom (Le, (ZCond.Term.Var B.r),
      (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))))))))::((('n'::('o'::('n'::('l'::('i'::('n'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::('5'::('_'::('4'::('_'::('k'::('o'::[]))))))))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))), (ZCond.Atom (Le,
      (ZCond.Term.Var B.x), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))), (Seq ((Assign
      (B.r, (ZCond.Term.Mul ((ZCond.Term.Var B.x),
      (sub (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))
        (ZCond.Term.Var B.x)))))), (Assert (('p'::('o'::('s'::('t'::[])))),
      (ZCond.Atom (Le, (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))))))))::((('b'::('a'::('r'::('y'::('c'::('e'::('n'::('t'::('r'::('e'::('_'::('k'::('o'::[]))))))))))))),(Seq
      ((Assume (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Cte
      (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.a))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Var B.a), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))))), (ZCond.BinL (AND, (ZCond.Atom
      (Le, (ZCond.Term.Cte (idZ (Zpos Coq_xH))), (ZCond.Term.Var B.x))),
      (ZCond.BinL (AND, (ZCond.Atom (Le, (ZCond.Term.Var B.x),
      (ZCond.Term.Cte (idZ (Zpos (Coq_xI Coq_xH)))))), (ZCond.BinL (AND,
      (ZCond.Atom (Le, (ZCond.Term.Cte (idZ (Zpos Coq_xH))), (ZCond.Term.Var
      B.y))), (ZCond.Atom (Le, (ZCond.Term.Var B.y), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI Coq_xH))))))))))))))))), (Seq ((Assign (B.r,
      (ZCond.Term.Annot (ZCond.Term.Annot.SKIP_ORACLE, (ZCond.Term.Annot
      (ZCond.Term.Annot.INTERV, (ZCond.Term.Add ((ZCond.Term.Mul
      ((ZCond.Term.Var B.a), (ZCond.Term.Var B.x))), (ZCond.Term.Mul
      ((sub (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH)))))
         (ZCond.Term.Var B.a)), (ZCond.Term.Var B.y))))))))))), (Assert
      (('p'::('o'::('s'::('t'::[])))), (ZCond.BinL (AND, (ZCond.Atom (Le,
      (ZCond.Term.Cte (idZ (Zpos (Coq_xI (Coq_xO Coq_xH))))), (ZCond.Term.Var
      B.r))), (ZCond.Atom (Le, (ZCond.Term.Var B.r), (ZCond.Term.Cte
      (idZ (Zpos (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))))))))))))::[])))))))))

  (** val tests_ok : (char list*statement) list **)

  let tests_ok =
    app basic_tests_ok itv_tests_ok

  (** val tests_ko : (char list*statement) list **)

  let tests_ko =
    app basic_tests_ko itv_tests_ko
 end
