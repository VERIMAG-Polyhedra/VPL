TEST = -I core -I datatypes -I lin -I plp -I coq_extracted -I misc

INCLUDES = $(TEST)

USE_VPL =
#-cflags '-I ../ocaml/_build/src' -lflags '-I ../ocaml/_build/src vpl.cmxa'

OCB_FLAGS = -cflag -g -use-ocamlfind $(INCLUDES)
# -package vpl

OCB = ocamlbuild $(OCB_FLAGS)

all : check

clean:
	$(OCB) -clean

check:
	$(OCB) Run_tests.byte
	./_build/Run_tests.byte

check_debug:
	$(OCB) Run_tests.byte
	./_build/Run_tests.byte -debug

coq_update:
	$(MAKE) -C ../coq DemoExtractTests.vo
	./fix_extract.sh

demo_vplcoq_debug: coq_update
	$(OCB) -cflag -g -lflag -g demo_vplcoq.byte
        OCAMLRUNPARAM=b ./demo_vplcoq.byte

demo_vplcoq: coq_update
	$(OCB) demo_vplcoq.native
	./demo_vplcoq.native

.PHONY: clean check coq_update demo_vplcoq_debug demo_vplcoq
