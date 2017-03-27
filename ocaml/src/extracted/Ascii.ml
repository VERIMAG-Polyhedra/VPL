open BinNat
open BinNums
open Datatypes

(** val zero : char **)

let zero = '\000'

(** val one : char **)

let one = '\001'

(** val shift : bool -> char -> char **)

let shift = fun b c -> Char.chr (((Char.code c) lsl 1) land 255 + if b then 1 else 0)

(** val ascii_of_pos : positive -> char **)

let ascii_of_pos =
  let rec loop n p =
    match n with
    | O -> zero
    | S n' ->
      (match p with
       | Coq_xI p' -> shift true (loop n' p')
       | Coq_xO p' -> shift false (loop n' p')
       | Coq_xH -> one)
  in loop (S (S (S (S (S (S (S (S O))))))))

(** val ascii_of_N : coq_N -> char **)

let ascii_of_N = function
| N0 -> zero
| Npos p -> ascii_of_pos p

(** val ascii_of_nat : nat -> char **)

let ascii_of_nat a =
  ascii_of_N (N.of_nat a)
