module T_G2 (specs, tests) where

import Data.Map(toList, mapKeys)
import Parser as P 
import ParserResults hiding (pp)
import qualified ParserResults as PR (pp)
import Extensions
import Prelude hiding(pred) 


data Label
 = L_adjm
 | L_advm
 | L_dir
 | L_nm
 | L_np
 | L_np0
 | L_np1
 | L_pp
 | L_s
 | L_vc
 | L_vp
 | L_start
   deriving (Show, Enum, Eq)

instance MemoLabel Label
instance PP Label where pp = text . show

-------- tomita 2: 40 rules -------------

adjm   = memoize L_adjm 
       $                   termT "j" 
         <+> buildNary <$> termT "j" <*> adjm
         <+> buildNary <$> advm <*> termT "j" 
         <+> buildNary <$> adjm <*> termT "c" <*> adjm

advm   = memoize L_advm
       $     buildNary <$> termT "a" <*> advm 
         <+> termT "a" 
         <+> buildNary <$> advm <*> termT "c" <*> advm

dir    = memoize L_dir
       $ buildNary <$> dir <*> termT "c" <*> dir
                        <+> buildNary <$> pp <*> vp
                        <+> vp 
                        <+> buildNary <$> vp <*> pp
nm     = memoize L_nm 
       $ termT "n" <+> buildNary <$> termT "n" <*> nm
np     = memoize L_np
       $ buildNary <$> np <*> termT "c" <*> np
                        <+> buildNary <$> np1 <*> termT "t" <*> s
                        <+> buildNary <$> np1 <*> s
                        <+> np1
np0    = memoize L_np0 
       $ nm <+> buildNary <$> adjm <*> nm
                           <+> buildNary <$> termT "d" <*> nm 
                           <+> buildNary <$> termT "t" <*> adjm <*> nm
np1    = memoize L_np1
       $ buildNary <$> adjm <*>  np0 <*> pp <*> pp
                         <+> buildNary <$> adjm <*> np0 <*> pp
                         <+> buildNary <$> adjm <*> np0
                         <+> buildNary <$> np0 <*> pp
                         <+> np0 
                         <+> buildNary <$> np0 <*> pp <*> pp
pp     = memoize L_pp
       $ buildNary <$> pp <*> termT "c" <*> pp
                        <+> buildNary <$> termT "p" <*> np
s      = memoize L_s
       $ buildNary <$> np <*> vp <*> pp <*> pp
                         <+> buildNary <$> np <*> vp <*> pp
                         <+> buildNary <$> np <*> vp
                         <+> buildNary <$> s <*> termT "c" <*> s
vc     = memoize L_vc
       $     buildNary <$> termT "x" <*> termT "v"
         <+> termT "v"
vp     = memoize L_vp
       $ buildNary <$> vc <*> np
         <+> buildNary <$> vp <*> termT "c" <*> vp
	 <+> vc

start1 = memoize L_start (dir <+> np <+> s)
start  = start1

------- END OF tomita 2 ------

stringy = fmap (map (mapLabels show)) . mapKeys show
sanitize p ts = stringy $  spanTable $ runALP p ts 0

specs = [ ("T2", (sanitize start, tests)) ]

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



