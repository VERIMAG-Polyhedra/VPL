type polyhedron

external mk : int -> int -> polyhedron = "new_poly"

external rm : polyhedron -> unit = "delete_poly"

external set_coeff : polyhedron -> int -> int -> float -> unit = "set_coeff"

external minimize : polyhedron -> unit = "minimize"

external is_empty : polyhedron -> bool = "is_empty"

external is_true : polyhedron -> int -> bool = "is_true"

external get_witness_coeff : polyhedron -> int -> int -> float = "get_witness_coeff"
