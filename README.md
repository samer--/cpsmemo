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

Directory `ccmemo` contains a direct style implementation using Oleg Kiselyov's delimcc library,
which you can install using OPAM. Although compilation to native code is successful, the resulting
program crashes for some reason I don't know. Compiled to byte code, it is slightly slower than 
the monadic version.

## Haskell version

The version in subdirectory `haskell` is port of the main ideas from cpsmemo.ml,
taking advantage of (a) all the monad stuff already in Haskell (b) the recursive
`mdo` notation (backed up by `Control.Monad.Fix`) and (c) more polymorphism.

Because of more polymorphism, there seems to be no more need for the Dynamic module.

A notable feature of this version is that there is no longer any  need for explict open 
recursive style, which was, to be honest, a bit of a pain for large sets
of mutual recursive functions, such as in the Tomita2 grammar. In fact, Haskell's normal
recursive `let` is still no good for creating sets of recursive _monadic_ operations,
but it turns out that by using `Control.Monad.Fix` and the recursive `mdo` notation, we
can still get away without explicit open recursion -- the `mdo` notation invokes the
monadic fixed point operator `mfix` behind the scenes.

The end result is a lot less code than the OCaml version!

Fitting this into a system of type classes mirroring the functorial parameterisation of the 
OCaml version seems to be difficult and is not done yet. For example, it would be nice
to parameterise by a `MonadRef` (instead of fixing the ST monad) and by a nondeterministic 
monad transformer (instead of using `ListT`).

## References

R. A. Frost, R. Hafiz, and P. C. Callaghan, 2007:
Modular and efficient top-down parsing for ambiguous left-recursive grammars. In Proceedings of the 10th International Conference on Parsing Technologies, pages 109–120. Association for Computational Linguistics.

R. A. Frost, R. Hafiz, and P. Callaghan, 2008: Parser combinators for ambiguous left-recursive grammars. In Practical Aspects of Declarative Languages, pages 167–181. Springer.
