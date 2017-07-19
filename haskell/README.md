## Haskell implementation of CPS left recursive memoisation

This use a stack of monads to handle most of the plumbing: the
nondeterministic stateful continuation monad is

	NDC s n r = ContT (n r) (ST s)

where `ST s`, the strict state threading monad, is at the base
and is used to keep track of the memo tables, and `n` is any instance
of `MonadPlus`, and is used to represent the nondeterminism of the
the result type `r`. (`s` is a phantom type which serves to prevent
mixing up two separate state threads.)

This type is automatically an instance of `MonadRef`, so there is
no need to manually lift the `readRef` and `writeRef` operations
used to access the memo tables. It is not automatically an instance
of `MonadPlus`, so an instance is provided.

The type of functions that can be memoised is

	NDCK s n r a b = a -> NDC s n r b

It is called `NDCK` because it is the Kleisi arrow of the `NDC` monad,
an effectful morphism from `a` to `b`. `NDCK s n r` could conceivably
be declared an instance of the `Arrow` class, but I haven't done that.

The memo tables for a memoised arrow from `a` to `b` are represented 
internally by the type

	Data.Map.Map a (Data.Set.Set b, [b -> ST s (n r)])

where `[b -> ST s (n r)]` is a list of continuations waiting to receive
results from the arrow. However, when the memo tables are exposed
the list of continuations is stripped out and the type of the result is

	Table a b = Map a (Set b)

### Test program usage

	./test [s|sm|sml|smml] <N:int>

Runs and times one of the highly ambiguous grammars on a string
of N 'a's.

It seems to perform quite similarly to the OCaml version.
