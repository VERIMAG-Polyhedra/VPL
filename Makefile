# Defining default program for html
ifeq ($(OS),Windows_NT)
    OPEN := start
else
    UNAME := $(shell uname -s)
    ifeq ($(UNAME),Linux)
        OPEN := xdg-open
	else
		OPEN := open
	endif
endif

BUILD = _build/default

all: vpl

vpl:
	dune build

doc:
	dune build @doc

open_doc:
	$(OPEN) $(BUILD)/_doc/_html/index.html

clean:
	dune clean

install: vpl
	dune build @install
	dune install

uninstall:
	dune build @install
	dune uninstall

check: vpl
	./_build/default/test/run_tests.exe

# extract Coq files into the expected  ocaml/ subdir.
coq_update:
	$(MAKE) -j$(PROCMAX) -C coq/ OPT:="-opt" DemoExtract.vo

coq_extract:
	$(MAKE) -C coq/ cleanextract
	$(MAKE) -j$(PROCMAX) -C coq/ OPT:="-opt" DemoExtract.vo
	# Fixing a problem in extraction
	sed -i 's/let skip =/let skip : cdac =/g' ocaml/extracted/DomainGCL.ml

# targets for opam installation.
coq_build:
	$(MAKE) -j$(PROCMAX) -C coq/ OPT:="-opt" build

coq_install:
	$(MAKE) -C coq/ install

coq_uninstall:
	$(MAKE) -C coq/ uninstall

coq_clean:
	$(MAKE) -C coq clean

.PHONY: all vpl clean install uninstall doc open_doc check coq_update coq_extract coq_build coq_install coq_uninstall coq_clean
