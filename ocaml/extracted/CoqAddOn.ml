open Ascii
open Datatypes
open List0
open String0

(** val nl : char list **)

let nl =
  (ascii_of_nat (S (S (S (S (S (S (S (S (S (S O)))))))))))::[]

(** val append1 : char list -> char -> char list **)

let append1 s c =
  append s (c::[])

(** val concat : char list -> char list list -> char list **)

let rec concat sep = function
| [] -> []
| h1::l1 ->
  (match l1 with
   | [] -> h1
   | _::_ -> append h1 (append sep (concat sep l1)))

(** val substStr : char list -> char list list -> char list **)

let substStr s = function
| [] ->
  append s
    ('['::('m'::('i'::('s'::('s'::('i'::('n'::('g'::(' '::('a'::('r'::('g'::(']'::[])))))))))))))
| h::_ -> append s h

(** val sprintfAux : char list -> char list list -> char list -> char list **)

let rec sprintfAux s l acc =
  match s with
  | [] -> acc
  | c::s1 ->
    if (=) '%' c
    then (match s1 with
          | [] ->
            append acc
              ('['::('b'::('a'::('d'::(' '::('%'::('_'::(']'::[]))))))))
          | c'::s2 ->
            if (=) 's' c'
            then sprintfAux s2 (tl l) (substStr acc l)
            else if (=) '%' c'
                 then sprintfAux s2 l (append1 acc c')
                 else sprintfAux s2 l
                        (append acc
                          ('['::('b'::('a'::('d'::(' '::('%'::('_'::(']'::[]))))))))))
    else if (=) '\\' c
         then (match s1 with
               | [] ->
                 append acc
                   ('['::('b'::('a'::('d'::(' '::('\\'::('_'::(']'::[]))))))))
               | c'::s2 ->
                 if (=) 'n' c'
                 then sprintfAux s2 l (append acc nl)
                 else if (=) '\\' c'
                      then sprintfAux s2 l (append1 acc c')
                      else sprintfAux s2 l
                             (append acc
                               ('['::('b'::('a'::('d'::(' '::('\\'::('_'::(']'::[]))))))))))
         else sprintfAux s1 l (append1 acc c)

(** val sprintf : char list -> char list list -> char list **)

let sprintf s l =
  sprintfAux s l []
