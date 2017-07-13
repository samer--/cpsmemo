# cpsmemo
Pure functional, monadic, nondeterministic, left recursive memoisation, using delimited control in OCaml.

This repository contains the code need to reproduce the results in the paper
[ref].

It includes a modified version of the parser of Frost, Hafiz and Callaghan (2007,2008), which
has been altered to make for a fairer comparison of run times.

You will need an OCaml development system with the package OCaml With Batteries to build and
run the OCaml based system, and a GHC (Glasgow Haskell Compiler) development system to build
and run Frost et al's code. You will also need to `cabal install timeit` to install a Haskell
timing utility.

To run the OCaml test program, do `make test` at the shell prompt and then run commands such as

	./test sm 12
	./test sml 12
	./test sml 48
	./test smml 48

To run the Haskell test program, move to the `frost` subdirectory and build the code
with `make` at the shell prompt. Then the following commands will run various test
grammars on inputs of various lengths:

	./a.out sm aaa 12
	./a.out sml aaa 12
	./a.out sml aaa 48
	./a.out smml aaa 48

Execution times will be printed out.
