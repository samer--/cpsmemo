# cpsmemo
Pure functional, monadic, nondeterministic, left recursive memoisation, using delimited control in OCaml.

This repository contains the code need to reproduce the results in the paper
[ref].

It includes a modified version of the parser of Frost, Hafiz and Callaghan (2007,2008), which
has been altered to make for a fairer comparison of run times.

You will need an OCaml development system with the package OCaml With Batteries to build and
run the OCaml based system, and a GHC (Glasgow Haskell Compiler) development system to build
and run Frost et al's code.
