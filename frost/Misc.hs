module Misc (tests, specs, L(..), sA, sB, sI) where

import Prelude hiding(pred) 
import Data.IntMap(foldr')

import ParserResults	hiding (E) 
import Extensions

import PP


----- Highly Ambiguous Classical ---
-- Three diff forms
-- sA = right recursive
-- sB = direct left-recursive
-- sC = direct left-recursive with part-memoized

data L = A | B | C | C1 | D | D1 | E | F deriving (Show, Eq, Ord, Enum, Bounded)

instance MemoLabel L where
instance PP L where { pp l = text $ show l }

sX = memoize A $ (\l r -> Branch [l,r]) <$> termT "a" <*> sX <+> emptyT


sA  :: ALP String (Tree String L) (Tree String L)
sA   = memoize A 
     $ buildNary <$> termT "a" <*> sA <*> sA 
       <+> emptyT

sB   = memoize B 
     $ buildNary <$> sB <*> sB <*> termT "a" 
       <+> emptyT

sC   = memoize C 
     $ buildNary <$> sC <*> memoize C1 (buildNary <$> sC <*> termT "a") 
       <+> emptyT

-- memo the double-D subparser
sD   = memoize D 
     $ buildNary <$> memoize D1 (buildNary <$> sD <*> sD) <*> termT "a" 
       <+> emptyT


-- hidden LR (via option)
sE = memoize E $ buildNary <$> sF <*> sE <*> sE <*> termT "a" <+> emptyT
sF = memoize F $ termT "a" <+> emptyT

-- mutual rec

sG = memoize A $     buildNary <$> sG <*> termT "a" 
                 <+> buildNary <$> termT "a" <*> sH <+> sH
sH = memoize B $ buildNary <$> sH <*> termT "a" <+> emptyT


-- complex grammar, to test most things

sI = memoize A $     buildNary <$> termT "a" <*> sI <*> sI
                 <+> buildNary <$> sI <*> sI <*> termT "a"
                 <+> buildNary <$> emptyT <*> sI <*> sI <*> termT "a"
                 <+> buildNary <$> opt_x <*> sI <*> sI <*> termT "a"
                 <+> buildNary <$> sJ <*> sI <*> termT "a"
                 <+> buildNary <$> sK <*> sJ <*> termT "a"	-- lr chain in 2nd pos
		 <+>               termT "a"
sJ = memoize B $                   sI 
                 <+> buildNary <$> sJ <*> termT "a"
sK = memoize C $                   sI 
                 <+> buildNary <$> sJ <*> termT "a"


opt_x = termT "x" <+> emptyT		-- NB not memoized


{-
-- looking at cycles 
sL = memoize A $ sL *> term "a" <+> sL <+> empty 
sM = memoize B $ sM *> term "a" <+> empty <+> sM
sN = memoize C $ (sL <+> sM) *> (sL <+> sM)

sO = memoize D $ sO *> term "a" <+> sP <+> empty
sP = memoize E $ sO *> term "a" <+> sO <+> empty
-}
------------------------------------

qt :: Int -> ALP String (Tree String L) (Tree String L) -> IO ()
qt n g 
 = do 
      let inp = a_sentences n
      po putStrLn $ format format_table toEnum $ runALP g inp 0

--- INPUT -------------------------

tests :: GrammarTests String
tests 
 = [ (,) "aaa"    $ Parametric "aaa" $ a_sentences
   , (,) "fixed"  $ Sentences        $ test_sentences
   ]

-- for pp ambiguity
a_sentences
 = \i -> replicate i "a"

test_sentences
 = map words 
   [ "n v d n p d n"
   , "n v d n p d n p d n"
   ]

---------------------------------------
-- grammar specs

-- traverse = fmap (map (mapLabels show)) . rawResults
traverse = foldr' (\m n -> n + foldr' (\r n -> n+ result_size r) 0 m) 0 . rawResults
result_size tags = sum (map (\(Tag _ trees) -> sum (map tree_size trees)) tags)
sanitize p ts = traverse (runALP p ts 0)

specs 
 = fmap (\(a,b) -> (a, (sanitize b, tests)))
 $ [ ("sm"   ,  sA)
   , ("sml"  ,  sB)
   , ("smml" ,  sC)
   , ("sD"   ,  sD)
   ]
