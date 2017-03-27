TEST = -I core -I datatypes -I lin -I plp

INCLUDES = $(TEST)

USE_VPL = -cflags '-I ../ocaml/_build/src' -lflags '-I ../ocaml/_build/src vpl.cmxa'

OCB_FLAGS = -use-ocamlfind $(USE_VPL) $(INCLUDES)

OCB = ocamlbuild $(OCB_FLAGS)

all : check

clean:
	$(OCB) -clean

check:
	$(OCB) -package vpl All_t.native
	./_build/All_t.native

.PHONY: clean check
