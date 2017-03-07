# requires: coq 8.6

.PHONY: coq clean lscoqfiles

COQOPT ?= -opt

COQLIB := Vpl

COQ_FILES := CoqAddOn Debugging OptionMonad NumC \
	PositiveMapAddOn ProgVar LinTerm \
	Impure ImpureConfig DomainInterfaces \
	CstrC CstrLCF ConsSet PedraQBackend \
	PredTrans DomainGCL \
	Ring_polynom_AddOn ASTerm Itv ZNone ZNoneItv LinearizeBackend  \
        ASAtomicCond ASCond AssignD DomainFunctors \
   Ring_polynom_AddOnQ Qop CIndex Map_poly \
	PedraQ PedraZ \
	DemoPLVerifier DemoPLTests

V := $(addsuffix .v, $(COQ_FILES))
VO := $(addsuffix .vo, $(COQ_FILES))
GLOB := $(addsuffix .glob, $(COQ_FILES))
DV := $(addsuffix .dv, $(COQ_FILES))

coq: $(VO)

DemoExtract.vo: $(VO) DemoExtract.v
	mkdir -p extracted
	cd extracted; (coqc $(COQOPT) -R ../ $(COQLIB) ../DemoExtract.v && rm -f ImpureConfig.mli)

DemoExtractTests.vo: DemoExtract.vo DemoPLTests.vo DemoExtractTests.v
	cd extracted; coqc $(COQOPT) -R ../ $(COQLIB) ../DemoExtractTests.v

%.vo: %.v
	coqc $(COQOPT)  -R . $(COQLIB) $<

%.dv: %.v
	coqdep -R . $(COQLIB) $< > $@

lscoqfiles:
	@echo ${V}

clean:
	rm -rf extracted *.vo *.dv *.glob *~ .*.aux

include $(DV)
