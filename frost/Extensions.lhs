> {-# OPTIONS_GHC -fallow-undecidable-instances #-}
> module Extensions where

For various optional and non-H98 features

> import Control.Applicative

> import ParserResults
> import PP

> -- allow any Enum type to be used as a MemoLabel
> -- saves the user typing this
> instance Enum l => MemoLabel l where
> instance (Show l) => PP l where pp x = text $ show x

%-------------------------------------------------------------------------------
Overloaded function for building results from N values 
 - the functional dependency indicates that 'a' is determined by 'r'
 - the argument accumulates values in a list function
 - interpretation: extending result 'r' by adding more 'a' values

> class BuildNary a r | r -> a where
>	extend :: ([a] -> [a]) -> r 

> -- | main function for use here 
> -- | this discourages use as constant value, ie minimum is (a -> a)
> -- | constant use is possible, but seems not very useful and has 'id' overhead

> buildNary :: BuildNary a r => a -> r
> buildNary = \a -> extend (a:)


> -- | generic instance
> -- | just add new value to the "list"
> instance BuildNary a r => BuildNary a (a -> r) where
>	extend f a = extend $ f . (a:)


> -- | concrete instance for usual trees
> -- | wraps the accumulated list in a Branch
> instance BuildNary (Tree l a) (Tree l a) where
>	extend f = Branch $ f [] 

> {-# SPECIALIZE buildNary :: Tree l a -> Tree l a #-}
> {-# SPECIALIZE buildNary :: Tree l a -> Tree l a -> Tree l a #-}
> {-# SPECIALIZE buildNary :: Tree l a -> Tree l a -> Tree l a -> Tree l a #-}
> {-# SPECIALIZE buildNary :: Tree l a -> Tree l a -> Tree l a -> Tree l a -> Tree l a #-}


-- trying to extend this to sequences of applicative functors

<> class Applicative f => AppN f a r | r -> f a where
<>	more :: (f a) -> r 
<> instance AppN f a r => AppN f a (a -> r) where
<>	more f a = more $ f <*> a




%-------------------------------------------------------------------------------
Conor McBride's idiomatic brackets
 - taken from Epigram code, www.e-pig.org
 - currently: seems to duplicate results.

> class Applicative i => Idiomatic i f g | g -> f i where
>   -- iI_ :: f -> g
>   -- iI_ = idiomatic . pure
>   idiomatic :: i f -> g

> iI :: (BuildNary c d, Idiomatic e (c -> d) b) => b

> iI = idiomatic $ pure buildNary

> data Ii  =  Ii
> data Ig  =  Ig
> data If  =  If

> instance Applicative i    => Idiomatic i x (Ii -> i x) where
>   idiomatic xi Ii     = xi
> instance Idiomatic i f g  => Idiomatic i (s -> f) (i s -> g) where
>   idiomatic sfi si    = idiomatic (sfi <*> si)

> instance Idiomatic i f g  => Idiomatic i f (Ig -> i x -> g) where
>   idiomatic fi Ig xi  = idiomatic (fi <* xi)

<> instance (Idiomatic i f g, Monoid g) => Idiomatic i f (If -> Bool -> g) where
<>   idiomatic fi If True   = idiomatic fi
<>   idiomatic fi If False  = zero

%-------------------------------------------------------------------------------



