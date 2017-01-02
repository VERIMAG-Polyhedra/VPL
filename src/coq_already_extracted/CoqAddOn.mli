open Ascii
open Datatypes
open List0
open String0

val nl : char list

val append1 : char list -> char -> char list

val concat : char list -> char list list -> char list

val substStr : char list -> char list list -> char list

val sprintfAux : char list -> char list list -> char list -> char list

val sprintf : char list -> char list list -> char list
