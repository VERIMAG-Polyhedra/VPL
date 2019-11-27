# VPL (Verified Polyhedra Library)

## GENERAL INFORMATIONS

The VPL is an Ocaml library allowing to compute with convex polyhedra.
It provides standard operators -- certified in Coq -- to use this library as an abstract domain of polyhedra.

Contributors: Alexis Fouilhé, Alexandre Maréchal, Sylvain Boulmé, Hang Yu, Michaël Périn, David Monniaux.
Developed at Verimag and supported by ANR Verasco and ERC Stator.

If you find a bug or have any comment, feel free to contact us at verimag-polyhedra-developers@univ-grenoble-alpes.fr

## INSTALLATION

1. __From [opam](https://opam.ocaml.org/)__

    First, add the following repository in your opam system:

        opam repo add vpl https://raw.githubusercontent.com/VERIMAG-Polyhedra/opam-vpl/master

    Then, install the following packages (depending on your needs):

    * `vpl-core`: the ocaml library

      ```
      opam install vpl-core
      ```

    * `coq-vpl`: the Coq library (only needed to get Coq proofs about VPL operators)

      ```
      opam install coq-vpl
      ```

    * `coq-vpltactic`: the [VplTactic](https://github.com/VERIMAG-Polyhedra/VplTactic) plugin for Coq (also install `coq-vpl` and `vpl-core`)

      ```
      opam install coq-vpltactic
      ```

  In case of trouble with this `opam` install, you should read [this](https://github.com/VERIMAG-Polyhedra/opam-vpl/blob/master/README.md#using-the-vpl-on-a-vagrantvirtualbox-virtual-machine).

2. __From sources__

    The VPL can use a C++ parallel algorithm to minimize the representation of polyhedra.
    This feature requires several dependencies.
    If you are not interested in this feature or do not want to install dependencies, a "vanilla" VPL can be installed (see point 1. below).

    1. __Without C++ dependencies__
        1. _Dependencies_

           The VPL requires the following packages:

           * [ocaml](http://caml.inria.fr/ocaml/index.en.html)
              __required version >= 4.02.3__

           * [zarith](https://forge.ocamlcore.org/projects/zarith)
              _available in OPAM_
              __tested with version 1.4.1__

        2. _Compiling the VPL_

           (Optional) To re-extract from the coq files, simply run at the root directory

                make coq_extract

           To compile the VPL, simply run from the root directory

                make vpl

           Tests can be run by typing

                make check

           Finally, to install the library with ocamlfind, type

                make install

           To uninstall the library from ocamlfind, run

                make uninstall

    2. __With C++ dependencies__
        1. _Dependencies_

           The VPL requires the following packages:

           * [ocaml](http://caml.inria.fr/ocaml/index.en.html)
              __required version >= 4.02.3__

           * [zarith](https://forge.ocamlcore.org/projects/zarith)
              _available in OPAM_
              __tested with version 1.4.1__

           * [glpk](https://www.gnu.org/software/glpk/)
              __required version >= 4.61__

           * [eigen](http://eigen.tuxfamily.org/)
              _debian package libeigen3-dev_
              __tested with version 3.3.3__

           * [flint](http://www.flintlib.org/)
              _debian package libflint-dev_
              __tested with version 2.5.2__

           * [coq](https://coq.inria.fr/)
              (mandatory only if you want to re-extract files from Coq)
              _available in OPAM_
              __required version 8.7__ (use coq-vpl.0.2 for coq 8.6)

              __NB__ the `ocaml/src/extracted/` directory already contains extracted files from Coq.

        2. _Compiling the VPL_

           (Optional) To re-extract from the coq files, simply run at the root directory

                make coq_extract

           To compile the VPL, simply run from the root directory

                make vpl_glpk

           Tests can be run by typing

                make check

           Finally, to install the library with ocamlfind, type

                make install

           To uninstall the library from ocamlfind, run

                make uninstall


## Using the VPL

There are several ways to use the library.

1. From Coq
(see opam package `coq-vpl`)

2. As a Coq tactic
(see [VplTactic](https://github.com/VERIMAG-Polyhedra/VplTactic))

3. As an OCaml library
(see opam package `vpl-core`)

As an OCaml library, the entry point of the VPL is the module UserInterface.
It contains a functor `Make` that must be provided with a polyhedral domain (from module `Domains`).
Polyhedral domains can work over Q or Z, and there are three levels of certification, which gives 6 possible domains to instantiate `Make` with.
The level of certifications are:

* _No certification_: no certificate is produced

* _OCaml certification_: Each operator of the domain produces a _certificate_, ie a witness of its computation that can be checked.

* _Coq certification_: In addition to guarantees offered by OCaml certification, certificates are here extracted from Coq proven types.

```
module domain = Vpl.UserInterface.Make(Vpl.Domains.CstrQ)
```
