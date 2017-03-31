all : vpl

vpl:
	cd ocaml; $(MAKE)

clean:
	$(MAKE) -C ocaml/ clean
	$(MAKE) -C test/ clean
	rm -f ocaml/setup.data ocaml/setup.log

to_opam:
	cd ocaml
	oasis2opam --local

allclean: clean coq_clean test_clean

install: vpl
	$(MAKE) -C ocaml/ install

uninstall:
	ocamlfind remove vpl

check:
	$(MAKE) -C test/ check

test_clean:
	$(MAKE) -C test/ clean

# extract Coq files into the expected  ocaml/ subdir.
coq_update:
	$(MAKE) -C coq/ DemoExtract.vo

coq_extract:
	$(MAKE) -C coq/ cleanextract
	$(MAKE) -C coq/ DemoExtract.vo

# targets for opam installation.
coq_build:
	$(MAKE) -C coq/ build

coq_install:
	$(MAKE) -C coq/ install

coq_uninstall:
	$(MAKE) -C coq/ uninstall

coq_clean:
	$(MAKE) -C coq clean

.PHONY: all vpl clean allclean install uninstall check coq_update coq_extract coq_build coq_install coq_uninstall coq_clean test_clean
