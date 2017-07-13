module T_G3 (specs, tests) where


import Parser as P
import ParserResults hiding (pp)
import qualified ParserResults as PR (pp)
import Extensions
import Prelude hiding(pred)

data Label
 = L_add
 | L_adjp
 | L_advp
 | L_comp
 | L_comp_np
 | L_imp
 | L_nm
 | L_np
 | L_np_np
 | L_np0
 | L_np1
 | L_postadjp
 | L_pp
 | L_pp_np
 | L_qadj
 | L_qadv
 | L_qnp
 | L_relc
 | L_relnp
 | L_s
 | L_sdec
 | L_sdec_np
 | L_subj
 | L_t_here
 | L_toinf
 | L_toinf_np
 | L_vp_en
 | L_vp_en_np
 | L_vp_inf
 | L_vp_inf_np
 | L_vp_ing
 | L_vp_ing_np
 | L_vp_tense
 | L_vp_tense_np
 | L_whq
 | L_ynq
 | L_ynq_np
   deriving (Show, Enum)

instance MemoLabel Label
instance PP Label where pp = text . show


--------- tomita 3 : 220 rules ---

add          = memoize L_add     $
                   (vp_en_np)
             <+>   (buildNary <$> termT "b" <*> termT "o" <*> np)
             <+>   (termT "i")
             <+>   (termT "c")
             <+>   (pp)
             <+>   (buildNary <$> termT "d" <*> termT "t" <*> np)
             <+>   (vp_ing)

adjp        = memoize L_adjp    $
                   (buildNary <$> advp <*> termT "a")
             <+>   (termT "j")
             <+>   (buildNary <$> advp <*> termT "u")
             <+>   (termT "u")

advp        = memoize L_advp    $
                   (buildNary <$> termT "a" <*> termT "d")
             <+>   (termT "d")


comp        = memoize L_comp    $
                   (np)
             <+>   (vp_ing)
             <+>   (postadjp)
             <+>   (buildNary <$> termT "t" <*> sdec)
             <+>   (vp_en)
             <+>   (buildNary <$> qnp <*> sdec_np)
             <+>   (buildNary <$> qadv <*> toinf)
             <+>   (buildNary <$> qadv <*> sdec)
             <+>   (buildNary <$> qnp <*> toinf_np)

comp_np     = memoize L_comp_np $
                   (vp_en_np)
             <+>   (vp_ing_np)
             <+>   (np_np)

imp         = memoize L_imp     $
                   (buildNary <$> termT "l" <*> vp_inf)
             <+>   (vp_inf)
             <+>   (buildNary <$> vp_inf <*> termT "c" <*> termT "l" <*> vp_inf)
             <+>   (buildNary <$> vp_inf <*> termT "l" <*> vp_inf)
             <+>   (buildNary <$> termT "c" <*> termT "y" <*> termT "l" <*> vp_inf)

nm          = memoize L_nm      $
                   (buildNary <$> termT "n" <*> termT "n" <*> termT "n" <*> termT "n")
             <+>   (buildNary <$> termT "n" <*> termT "n" <*> termT "n")
             <+>   (buildNary <$> termT "n" <*> termT "n")
             <+>   (termT "n")		       		

np          = memoize L_np      $
                   (buildNary <$> np1 <*> postadjp)
             <+>   (buildNary <$> np1 <*> relc)
             <+>   (np1)
             <+>   (termT "r")
             <+>   (buildNary <$> np1 <*> vp_en_np <*> relc)
             <+>   (t_here)
             <+>   (buildNary <$> np1 <*> vp_en_np)
             <+>   (buildNary <$> np1 <*> postadjp <*> relc)
             <+>   (termT "q")

np_np       = memoize L_np_np   (emptyT)       	

np0         = memoize L_np0     $
                   (buildNary <$> adjp <*> np0)
             <+>   (nm)
             <+>   (termT "u")

np1         = memoize L_np1     $
                   (buildNary <$> np1 <*> pp)
             <+>   (buildNary <$> termT "d" <*> np0)
             <+>   (np0)
             <+>   (termT "q")

postadjp    = memoize L_postadjp $
                   (buildNary <$> termT "s" <*> adjp <*> termT "t" <*> sdec)
             <+>    (buildNary <$> adjp <*> termT "f" <*> np <*> toinf)
             <+>    (buildNary <$> adjp <*> toinf)
             <+>    (buildNary <$> adjp <*> termT "f" <*> np)
             <+>    (adjp)		       		       		       		

pp          = memoize L_pp       $
                   (buildNary <$> termT "p" <*> np)
             <+>    (buildNary <$> termT "p" <*> vp_ing)	

pp_np       = memoize L_pp_np    $
                   (buildNary <$> termT "p" <*> vp_ing_np)
             <+>    (buildNary <$> termT "p" <*> np_np)


qadj        = memoize L_qadj     $
                   (buildNary <$> termT "h" <*> termT "a")
             <+>    (termT "w")

qadv        = memoize L_qadv     $
                   (termT "w")
             <+>    (termT "e")
             <+>    (termT "h")
             <+>    (buildNary <$> termT "p" <*> qnp)
             <+>    (buildNary <$> termT "h" <*> termT "a")

qnp         = memoize L_qnp      $
                   (termT "w")
             <+>    (buildNary <$> termT "w" <*> np)
             <+>    (buildNary <$> termT "h" <*> np)
             <+>    (buildNary <$> termT "h" <*> termT "q" <*> np)
             <+>    (buildNary <$> termT "h" <*> termT "q")
             <+>    (buildNary <$> termT "h" <*> np)
             <+>    (termT "a") <+> (termT "b") <+> (termT "c")

relc        = memoize L_relc     $
                   (buildNary <$> termT "p" <*> relnp <*> sdec)
             <+>    (buildNary <$> termT "w" <*> sdec)
             <+>    (buildNary <$> relnp <*> sdec_np)
             <+>    (buildNary <$> termT "h" <*> sdec)
             <+>    (buildNary <$> termT "s" <*> termT "t" <*> sdec)

relnp       = memoize L_relnp    $
                   (termT "t")
             <+>    (termT "a") <+> (termT "b") <+> (termT "c") <+> (termT "c")
             <+>    (buildNary <$> np <*> termT "o" <*> relnp)
             <+>    (buildNary <$> termT "w" <*> np)

s           = memoize L_s        $
                   (buildNary <$> s <*> termT "c" <*> termT "c" <*> s)
             <+>    (buildNary <$> termT "c" <*> s  <*> termT "c" <*> s)
             <+>    (sdec)
             <+>    (whq)
             <+>    (buildNary <$> s <*> termT "c" <*> s)
             <+>    (buildNary <$> s <*> termT "c" <*> add)
             <+>    (ynq)
             <+>    (buildNary <$> add <*> termT "c" <*> s)
             <+>    (imp)

sdec        = memoize L_sdec     $
                   (buildNary <$> subj <*> vp_tense)
             <+>    (buildNary <$> subj <*> termT "m" <*> vp_inf)

sdec_np     = memoize L_sdec_np  $
                   (buildNary <$> np_np <*> vp_tense)
             <+>   (buildNary <$> subj <*> vp_tense_np)
             <+>   (buildNary <$> np_np <*> termT "m" <*> vp_inf)
             <+>   (buildNary <$> subj <*> termT "m" <*> vp_inf_np)

subj        = memoize L_subj     $
                   (buildNary <$> termT "t" <*> sdec)
             <+>    (np) <+> (vp_ing) <+> (toinf)

t_here      = memoize L_t_here   $
                   (termT "h")             <+> (termT "t")

toinf       = memoize L_toinf    (buildNary <$> termT "t" <*> vp_inf)

toinf_np    = memoize L_toinf_np (buildNary <$> termT "t" <*> vp_inf_np)

vp_en       = memoize L_vp_en    $
                   (buildNary <$> vp_en <*> advp)
             <+>    (buildNary <$> termT "b" <*> comp)
             <+>    (buildNary <$> advp <*> vp_en)
             <+>    (buildNary <$> vp_en <*> pp)
             <+>    (buildNary <$> vp_en <*> toinf)
             <+>    (buildNary <$> termT "o" <*> vp_en)
             <+>    (termT "v")
             <+>    (buildNary <$> termT "v" <*> comp)
             <+>    (buildNary <$> termT "v" <*> np <*> comp)
             <+>    (buildNary <$> termT "v" <*> np <*> vp_inf)
             <+>    (buildNary <$> termT "h" <*> np <*> vp_en)

vp_en_np    = memoize L_vp_en_np  $
                   (buildNary <$> vp_en_np <*> advp)
             <+>     (buildNary <$> termT "v" <*> comp_np)
             <+>     (buildNary <$> termT "v" <*> np_np <*> comp)
             <+>     (buildNary <$> termT "v" <*> np <*> comp_np)
             <+>     (buildNary <$> termT "v" <*> np_np <*> vp_inf)
             <+>     (buildNary <$> termT "v" <*> np <*> vp_inf_np)
             <+>     (buildNary <$> termT "b" <*> comp_np)
             <+>     (buildNary <$> termT "h" <*> vp_en_np)
             <+>     (buildNary <$> vp_en_np <*> toinf)
             <+>     (buildNary <$> vp_en <*> toinf_np)
             <+>     (buildNary <$> vp_en_np <*> pp)
             <+>     (buildNary <$> vp_en <*> pp_np)
             <+>     (buildNary <$> termT "o" <*> vp_en_np)

vp_inf      = memoize L_vp_inf    $
                   (buildNary <$> vp_inf <*> toinf)
             <+>     (buildNary <$> vp_inf <*> advp)
             <+>     (termT "v")
             <+>     (buildNary <$> termT "v" <*> comp)
             <+>     (buildNary <$> advp <*> vp_inf)
             <+>     (buildNary <$> vp_inf <*> pp)
             <+>     (buildNary <$> termT "o" <*> vp_inf)
             <+>     (buildNary <$> termT "h" <*> vp_en)
             <+>     (buildNary <$> termT "b" <*> comp)
             <+>     (buildNary <$> termT "v" <*> np <*> comp)
             <+>     (buildNary <$> termT "v" <*> np <*> vp_inf)

vp_inf_np  = memoize L_vp_inf_np  $
                   (buildNary <$> vp_inf_np <*> toinf)
             <+>     (buildNary <$> termT "v" <*> comp_np)
             <+>     (buildNary <$> termT "v" <*> np_np <*> comp)
             <+>     (buildNary <$> termT "v" <*> np <*> comp_np)
             <+>     (buildNary <$> termT "v" <*> np_np <*> vp_inf)
             <+>     (buildNary <$> termT "v" <*> np <*> vp_inf_np)
             <+>     (buildNary <$> termT "b" <*> comp_np)
             <+>     (buildNary <$> termT "h" <*> vp_en_np)
             <+>     (buildNary <$> vp_inf <*> toinf_np)
             <+>     (buildNary <$> termT "o" <*> vp_inf_np)
             <+>     (buildNary <$> vp_inf_np <*> advp)
             <+>     (buildNary <$> advp <*> vp_inf_np)
             <+>     (buildNary <$> vp_inf_np <*> pp)
             <+>     (buildNary <$> vp_inf <*> pp_np)


vp_ing      = memoize L_vp_ing    $
                   (buildNary <$> termT "o" <*> vp_ing)
             <+>     (buildNary <$> termT "v" <*> np <*> comp)
             <+>     (buildNary <$> termT "h" <*> vp_en)
             <+>     (buildNary <$> termT "v" <*> np <*> vp_inf)
             <+>     (termT "v")
             <+>     (buildNary <$> termT "v" <*> comp)
             <+>     (buildNary <$> termT "b" <*> comp)
             <+>     (buildNary <$> vp_ing <*> pp)
             <+>     (buildNary <$> advp <*> vp_ing)
             <+>     (buildNary <$> vp_ing <*> advp)
             <+>     (buildNary <$> vp_ing <*> toinf)

vp_ing_np  = memoize L_vp_ing_np  $
                   (buildNary <$> termT "o" <*> vp_ing_np)
             <+>     (buildNary <$> termT "h" <*> vp_en_np)
             <+>     (buildNary <$> termT "b" <*> comp_np)
             <+>     (buildNary <$> termT "v" <*> np <*> vp_ing_np)
             <+>     (buildNary <$> termT "v" <*> np_np <*> comp)
             <+>     (buildNary <$> termT "v" <*> np <*> comp_np)
             <+>     (buildNary <$> advp <*> vp_ing_np)
             <+>     (buildNary <$> termT "v" <*> np_np <*> vp_ing)
             <+>     (buildNary <$> vp_ing_np <*> toinf)
             <+>     (buildNary <$> vp_ing <*> toinf_np)
             <+>     (buildNary <$> vp_ing_np <*> pp)
             <+>     (buildNary <$> vp_ing <*> pp_np)
             <+>     (buildNary <$> termT "v" <*> comp_np)
             <+>     (buildNary <$> vp_ing_np <*> advp)

vp_tense   = memoize L_vp_tense   $
                   (termT "v")
             <+>      (buildNary <$> termT "v" <*> comp)
             <+>      (buildNary <$> termT "v" <*> np <*> comp)
             <+>      (buildNary <$> termT "v" <*> np <*> vp_inf)
             <+>      (buildNary <$> vp_tense <*> advp)
             <+>      (buildNary <$> termT "h" <*> vp_en)
             <+>      (buildNary <$> termT "b" <*> comp)
             <+>      (buildNary <$> termT "o" <*> vp_tense)
             <+>      (buildNary <$> vp_tense <*> toinf)
             <+>      (buildNary <$> vp_tense <*> pp)
             <+>      (buildNary <$> advp <*> vp_tense)

vp_tense_np  = memoize L_vp_tense_np  $
                   (buildNary <$> vp_tense_np <*> advp)
             <+>        (buildNary <$> termT "v" <*> np_np <*> comp)
             <+>        (buildNary <$> termT "v" <*> np <*> comp_np)
             <+>        (buildNary <$> termT "v" <*> comp_np)
             <+>        (buildNary <$> termT "v" <*> np_np <*> vp_inf)
             <+>        (buildNary <$> termT "v" <*> np <*> vp_inf_np)
             <+>        (buildNary <$> termT "b" <*> comp_np)
             <+>        (buildNary <$> termT "h" <*> vp_en_np)
             <+>        (buildNary <$> advp <*> vp_tense_np)
             <+>        (buildNary <$> vp_tense_np <*> toinf)
             <+>        (buildNary <$> vp_tense <*> toinf_np)
             <+>        (buildNary <$> vp_tense_np <*> pp)
             <+>        (buildNary <$> vp_tense <*> pp_np)
             <+>        (buildNary <$> termT "o" <*> vp_tense_np)

whq        = memoize L_whq   $
                   (buildNary <$> qnp <*> ynq_np)
             <+> (buildNary <$> qadv <*> ynq)
             <+> (buildNary <$> qadj <*> termT "b" <*> subj)	

ynq        = memoize L_ynq   $
                   (buildNary <$> termT "b" <*> subj <*> comp)
             <+> (buildNary <$> termT "m" <*> subj <*> vp_inf)
             <+> (buildNary <$> termT "h" <*> subj <*> vp_en)	



ynq_np     = memoize L_ynq_np   $
                    (buildNary <$> termT "b" <*> subj <*> comp_np)
             <+>    (buildNary <$> termT "b" <*> comp)
             <+>    (buildNary <$> termT "h" <*> vp_en)
             <+>    (buildNary <$> termT "h" <*> subj <*> vp_en_np)
             <+>    (buildNary <$> termT "m" <*> vp_inf)
             <+>    (buildNary <$> termT "m" <*> subj <*> vp_inf_np)

start = s

--------- END OF tomita 3 --------


specs = [  ("T3", (start, tests)) ]

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
