module T_G1 (specs, tests) where

import Data.IntMap(foldr')
import ParserResults hiding (pp)
import PP(ps)
import Extensions


-------- tomita 1: 8 rules -------------

data Label = S | NP | PP | VP deriving (Show, Enum, Eq)
instance MemoLabel Label 
-- instance PP Label where pp = text . show




s  = memoize S  $     buildNary <$> np <*> vp
np = memoize NP $     termT "n" 
                  <+> buildNary <$> termT "d" <*> termT "n" 
		  <+> buildNary <$> np <*> pp
pp = memoize PP $     buildNary <$> termT "p" <*> np
vp = memoize VP $     buildNary <$> termT "v" <*> np 
                  <+> buildNary <$> vp <*> pp

start = s

------ END OF tomita 1 ----------

traverse = foldr' (\m n -> n + foldr' (\r n -> n+ result_size r) 0 m) 0 . rawResults
result_size tags = sum (map (\(Tag _ trees) -> sum (map tree_size trees)) tags)
sanitize p ts = traverse (runALP p ts 0)

specs = [ ("T1",  (sanitize start, tests)) ]

--- INPUT -------------------------

tests :: GrammarTests String
tests 
 = [ (,) "pp_ambig" $ Parametric "pp_ambig" $ pdn_sentences
   , (,) "fixed"    $ Sentences             $ test_sentences
   ]

-- for pp ambiguity
pdn_sentences
 = \i -> map (:[]) $ "nvdn" ++ concat (replicate i "pdn")	

test_sentences
 = map words 
   [ "n v d n p d n"
   , "n v d n p d n p d n"
   ]
