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

To run the OCaml test program, do `make testopt` at the shell prompt and then run commands such as

	./testopt sm 12
	./testopt sml 12
	./testopt sml 48
	./testopt smml 48

To run the Haskell test program, move to the `frost` subdirectory and build the code
with `make` at the shell prompt. Then the following commands will run various test
grammars on inputs of various lengths:

	./a.out sm aaa 12
	./a.out sml aaa 12
	./a.out sml aaa 48
	./a.out smml aaa 48

Execution times will be printed out.

Both systems can also be run in the OCaml or Haskell interactive environments. I recommend
`utop` for OCaml, and `ghci` comes with GHC.

## Direct style implementation

Directory ccmemo contains a direct style implementation using Oleg Kiselyov's delimcc library,
which you can install using OPAM.

## References

R. A. Frost, R. Hafiz, and P. C. Callaghan, 2007:
Modular and efficient top-down parsing for ambiguous left-recursive grammars. In Proceedings of the 10th International Conference on Parsing Technologies, pages 109–120. Association for Computational Linguistics.

R. A. Frost, R. Hafiz, and P. Callaghan, 2008: Parser combinators for ambiguous left-recursive grammars. In Practical Aspects of Declarative Languages, pages 167–181. Springer.
