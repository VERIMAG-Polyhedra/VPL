open BinNat
open BinNums
open Datatypes

val zero : char

val one : char

val shift : bool -> char -> char

val ascii_of_pos : positive -> char

val ascii_of_N : coq_N -> char

val ascii_of_nat : nat -> char
