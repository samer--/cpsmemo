module T_G4 where

import Parser as P
import ParserResults hiding (pp)
import qualified ParserResults as PR (pp)
import Extensions
import Prelude hiding(pred)

-------- tomita 4 -------------
adjcomp = memoize L_adjcomp     $ buildNary <$> termT "r" <*> sdec
				  <+> buildNary <$>    termT "p" <*> infinitive 
				  <+> buildNary <$>    termT "e" <*> termT "p" <*> np <*> infinitive
				  <+> buildNary <$>    termT "e" <*> termT "p" <*> np
				  <+> buildNary <$>    termT "e" <*> infinitive
				  <+>   termT "e"
				  <+> buildNary <$>    termT "p" <*> np
				  <+>   infinitive
          
adjp    = memoize L_adjp        $ buildNary <$> adjp <*> thancomp
				   <+> buildNary <$>   termT "a" <*> adjp
				   <+> buildNary <$>   detq <*> termT "a" <*> adjp
				   <+> buildNary <$>   detq <*> termT "a" <*> termT "q" <*> adjp
				   <+> buildNary <$>   detq <*> adjp
				   <+> buildNary <$>   termT "m" <*> termT "a" <*> adjp <*> ascomp
				   <+> buildNary <$>   termT "m" <*> termT "s" <*> adjp <*> ascomp
				   <+> buildNary <$>   termT "q" <*> termT "a"
				   <+> buildNary <$>   adjp <*> termT "r" <*> adjp
				   <+> buildNary <$>   termT "a" <*> adjp
				   <+> buildNary <$>   detq <*> termT "q" <*> adjp
				   <+>  termT "a"
				   <+> buildNary <$>   termT "a" <*> adjcomp
				   <+> buildNary <$>   termT "q" <*> termT "a"
				   <+> buildNary <$>   qpp <*> termT "q" <*> termT "a"
				   <+> buildNary <$>   termT "a" <*> termT "q" <*> adjp
				   <+> buildNary <$>   ddet <*> termT "a"
				   <+> buildNary <$>   termT "q" <*> adjp
				   <+> buildNary <$>   termT "a" <*> adjp
                   
advp    = memoize L_advp       $ buildNary <$> detq <*> termT "a"
				   <+> buildNary <$>  advp <*> thancomp
				   <+> buildNary <$>  qpp <*> termT "q" <*> termT "a"
				   <+> buildNary <$>  termT "q" <*> termT "a"
				   <+> buildNary <$>  detq <*> termT "q" <*> termT "a"
				   <+> buildNary <$>  termT "a" <*> advp
				   <+> buildNary <$>  termT "q" <*> termT "b"
				   <+> buildNary <$>  termT "p" <*> sdec
				   <+> buildNary <$>  termT "s" <*> sdec
				   <+> buildNary <$>  termT "s" <*> vp
				   <+> buildNary <$>  termT "q" <*> termT "c"
				   <+> termT "a"     
                   
ascomp  = memoize L_ascomp     $ buildNary <$> termT "a" <*> subj <*> bep
				  <+> buildNary <$>   termT "a" <*> subj <*> auxd <*> vp2
				  <+> buildNary <$>   termT "a" <*> sdec <*> vp2
				  <+> buildNary <$>   termT "a" <*> np
				  <+> buildNary <$>   termT "a" <*> subj <*> aux <*> bep       
                  
aux     = memoize L_aux        $ buildNary <$> modalp <*> havep
				  <+> buildNary <$>   modalp <*> bep
				  <+>  bep
				  <+>  modalp
				  <+>  havep
				  <+> buildNary <$>   modalp <*> havep <*> bep
				  <+> buildNary <$>   havep <*> bep
                  
auxd    = memoize L_auxd       $       aux
                		  <+>  dop

-- bep :: ALP Label Label
bep     = memoize L_bep        $       termT "b"
                                  <+> buildNary <$>   termT "b" <*> termT "o"  
                  
ddet    = memoize L_ddet       $ buildNary <$> termT "a" <*> termT "u"
				  <+> buildNary <$>   termT "a" <*> qpp
				  <+> buildNary <$>   termT "o" <*> termT "a" <*> termT "u"
				  <+> buildNary <$>   termT "o" <*> termT "a" <*> qpp
				  <+> buildNary <$>   termT "o" <*> termT "a" <*> termT "d" <*> qpp
				  <+> buildNary <$>   termT "a" <*> termT "d" <*> termT "u"
				  <+> buildNary <$>   termT "a" <*> termT "d" <*> qpp
				  <+> buildNary <$>   termT "o" <*> termT "a" <*> termT "d" <*> termT "u"
				  <+> buildNary <$>   termT "o" <*> termT "a"
				  <+> buildNary <$>   termT "d" <*> qpp
				  <+>  termT "a"
				  <+> buildNary <$>   termT "a" <*> termT "d"
				  <+>  termT "d"
				  <+> buildNary <$>   termT "o" <*> termT "a" <*> termT "d"
				  <+> buildNary <$>   termT "d" <*> termT "u"              
                  
detq    = memoize L_detq       $       termT "u"
				  <+> buildNary <$>   termT "a" <*> termT "u"
				  <+> buildNary <$>   ddet <*> termT "q"
				  <+> buildNary <$>   termT "d" <*> termT "q"
				  <+>  qpp
				  <+> buildNary <$>   termT "a" <*> termT "q"
                  
dop     = memoize L_dop        $       termT "o"
                  		  <+> buildNary <$>   termT "o" <*> termT "t"
                  
gerund  = memoize L_gerund     $ buildNary <$> termT "h" <*> termT "b" <*> pred
				   <+> buildNary <$>  termT "o" <*> termT "h" <*> termT "b" <*> pred
				   <+> buildNary <$>  termT "b" <*> pred
				   <+> buildNary <$>  termT "o" <*> termT "h" <*> termT "b" <*> vp
				   <+> buildNary <$>  termT "h" <*> termT "b" <*> vp
				   <+> buildNary <$>  termT "o" <*> termT "b" <*> vp
				   <+> buildNary <$>  termT "o" <*> termT "b" <*> pred
				   <+> buildNary <$>  termT "o" <*> termT "h" <*> vp
				   <+> buildNary <$>  termT "h" <*> vp
				   <+> buildNary <$>  termT "o" <*> vp
				   <+> vp
				   <+> buildNary <$>  termT "h" <*> vp
                   
                   
havep  = memoize L_havep       $ termT "h" <+> buildNary <$>  termT "h" <*> termT "o"

-- infinitive :: M Label Label
infinitive = memoize L_infinitive  
            $ buildNary <$> termT "t" <*> bep <*> bep <*> pred
 		<+> buildNary <$>        termT "t" <*> bep <*> vp
 		<+> buildNary <$>        termT "o" <*> termT "t" <*> bep <*> vp
 		<+> buildNary <$>        termT "o" <*> termT "t" <*> termT "h"  <*> bep <*> bep <*> pred
  		<+> buildNary <$>        termT "o" <*> termT "t" <*> bep <*> bep <*> pred
 		<+> buildNary <$>        termT "t" <*> termT "h" <*> bep <*> vp
 		<+> buildNary <$>        termT "o" <*> termT "t" <*> termT "h"  <*> bep <*> vp
 		<+> buildNary <$>        termT "t" <*> termT "h" <*> bep <*> bep <*> pred
 		<+> buildNary <$>        termT "o" <*> termT "t" <*> termT "h" <*> bep <*> pred
 		<+> buildNary <$>        termT "t" <*> vp
 		<+> buildNary <$>        termT "t" <*> termT "h" <*> bep <*> pred
 		<+> buildNary <$>        termT "t" <*> bep      <*> pred
 		<+> buildNary <$>        termT "o" <*> termT "t" <*> vp
 		<+> buildNary <$>        termT "o" <*> termT "t" <*> bep <*> pred
 		<+> buildNary <$>        termT "t" <*> termT "h" <*> vp
 		<+> buildNary <$>        termT "o" <*> termT "t" <*> termT "h" <*> vp
		
                     
infinitive1    = memoize L_infinitive1     $ buildNary <$> bep <*> vp
					 <+> buildNary <$>        termT "o" <*> bep <*> vp
					 <+> buildNary <$>        bep <*> bep <*> pred
					 <+> buildNary <$>        termT "o" <*> termT "h" <*> bep <*> pred
					 <+> buildNary <$>        termT "o" <*> bep <*> bep <*> pred
					 <+> buildNary <$>        termT "o" <*> termT "h" <*> bep <*> vp
					 <+> buildNary <$>        termT "h" <*> bep <*> bep <*> pred
					 <+> buildNary <$>        termT "h" <*> bep <*> vp
					 <+>       vp
					 <+> buildNary <$>        termT "o" <*> vp
					 <+> buildNary <$>        bep <*> pred
					 <+> buildNary <$>        termT "o" <*> termT "h" <*> vp
					 <+> buildNary <$>        termT "o" <*> bep <*> pred
					 <+> buildNary <$>        termT "h" <*> vp
					 <+> buildNary <$>        termT "h" <*> bep <*> pred  
                         
infinitrel    = memoize L_infinitrel       $ buildNary <$> termT "p" <*> termT "r" <*> termT "b" <*> vp
					<+> buildNary <$>         termT "p" <*> termT "r" <*> vp
					<+> buildNary <$>         termT "p" <*> termT "r" <*> infinitive
					<+> buildNary <$>         termT "p" <*> np <*> termT "b" <*> vp
					<+> buildNary <$>         termT "p" <*> np <*> vp
					<+> buildNary <$>         termT "b" <*> vp
					<+> buildNary <$>         termT "p" <*> np <*> infinitive
					<+>        vp
					<+>        infinitive
                        
modalp        = memoize L_modalp     $ termT "m" <+> buildNary <$>  termT "m" <*> termT "o"

ncomp         = memoize L_ncomp      $ adjp <+> srel <+> thancomp 
                                       <+> buildNary <$> ncomp <*> pp
                        		<+>  infinitrel <+> vp2 <+> pp
                        
nomhd         = memoize L_nomhd      $ buildNary <$> qpp <*> adjp <*> nomhd
					<+> buildNary <$>   termT "v" <*> nomhd
					<+> buildNary <$>   adjp <*> nomhd
					<+>  termT "n"
					<+> buildNary <$>   termT "n" <*> termT "n"
					<+> buildNary <$>   termT "n" <*> termT "n" <*> termT "n"                        
                         
np            = memoize L_np         $ buildNary <$> ddet <*> termT "q" <*> adjp <*> ncomp
					<+> buildNary <$>   termT "d" <*> gerund <+> gerund
					<+> buildNary <$>   termT "a" <*> qpp
					<+> buildNary <$>   termT "a" <*> qpp <*> nomhd
					<+> buildNary <$>   termT "a" <*> termT "a" <*> np
					<+> buildNary <$>   termT "a" <*> qpp <*> np
					<+> buildNary <$>   termT "a" <*> termT "a" <*> np <*> thancomp
					<+> buildNary <$>   termT "a" <*> qpp <*> termT "a" <*> np <*> thancomp
					<+> buildNary <$>   termT "s" <*> sdec
					<+> buildNary <$>   np <*> termT "r" <*> np
					<+> buildNary <$>   np <*> termT "c" <*> np
					<+>  termT "r" <+> nomhd
					<+> buildNary <$>   ddet <*> nomhd
					<+> buildNary <$>   detq <*> nomhd
					<+> buildNary <$>   termT "a" <*> ncomp
					<+> buildNary <$>   termT "a" <*> nomhd
					<+>  detq
					<+> buildNary <$>   ddet <*> adjp <*> np <*> ncomp
					<+> buildNary <$>   ddet <*> adjp <*> nomhd
					<+> buildNary <$>   detq <*> termT "q"
					<+> buildNary <$>   termT "r" <*> ncomp
					<+> buildNary <$>   np <*> pp
                        
obj      = memoize L_obj      $       np
obj1     = memoize L_obj1     $       np
obj2     = memoize L_obj2     $       np

pp       = memoize L_pp         $ buildNary <$> termT "p" <*> np <*> termT "a" <*> np
                   			<+> buildNary <$>   termT "p" <*> obj
                   
-- pred     :: M Label Label
pred     = memoize L_pred       $       vp2
				   <+> buildNary <$>   pred <*> infinitive
				   <+> buildNary <$>   pred <*> pp
				   <+>  pp
				   <+>  np
				   <+>  adjp
                   
qpp      = memoize L_qpp        $ buildNary <$> termT "q" <*> qpp
				   <+> buildNary <$>   termT "q" <*> termT "q" <*> qpp
				   <+>  termT "q"
				   <+> buildNary <$>   termT "q" <*> termT "q"
                   
scmp     = memoize L_scmp       $ buildNary <$> vp1 <*> termT "a" <*> vp1
				   <+> buildNary <$>   vp1 <*> termT "a" <*> sdec
				   <+> buildNary <$>   sdec <*> termT "c" <*> scmp
				   <+> buildNary <$>   sdec <*> termT "a" <*> vp1
				   <+> buildNary <$>   sdec <*> termT "a" <*> scmp
				   <+> buildNary <$>   vp1 <*> termT "a" <*> scmp           
                   
sdec     = memoize L_sdec       $ buildNary <$> sdec <*> advp
				   <+> buildNary <$>   sdec <*> termT "c" <*> vp3
				   <+> buildNary <$>   vp3 <*> termT "c" <*> sdec
				   <+> buildNary <$>   termT "t" <*> aux <*> bep <*> subj <*> pred
				   <+> buildNary <$>   termT "t" <*> bep <*> subj <*> pred
				   <+> buildNary <$>   sdec <*> termT "c" <*> infinitive
				   <+> buildNary <$>   termT "t" <*> aux <*> bep <*> subj
				   <+> buildNary <$>   subj <*> vp
				   <+> buildNary <$>   subj <*> auxd <*> vp
				   <+> buildNary <$>   subj <*> bep <*> pred
				   <+> buildNary <$>   termT "t" <*> bep <*> subj
				   <+> buildNary <$>   subj <*> termT "b"
				   <+> buildNary <$>   subj <*> aux <*> termT "b"
				   <+> buildNary <$>   subj <*> aux <*> bep <*> pred
                   
                   
sentence     = memoize L_sentence       $ buildNary <$> advp <*> termT "c" <*> sentence
				       <+> buildNary <$>       simp <*> termT "a" <*> sdec
				       <+> buildNary <$>       simp <*> termT "c" <*> sdec
				       <+> buildNary <$>       simp <*> termT "c" <*> simp
				       <+> buildNary <$>       pp <*> termT "c" <*> sentence
				       <+>      swhq
				       <+>      sq
				       <+> buildNary <$>       termT "c" <*> sentence
				       <+> buildNary <$>       simp <*> termT "a" <*> simp
				       <+>      sdec
				       <+> buildNary <$>       sdec <*> termT "c" <*> scmp
				       <+> buildNary <$>       simp <*> termT "a" <*> vp1
				       <+> buildNary <$>       sdec <*> termT "c" <*> sdec
				       <+> buildNary <$>       sdec <*> termT "a" <*> sdec
				       <+> buildNary <$>       sdec <*> termT "a" <*> vp1
				       <+>      simp
				       <+> buildNary <$>       simp <*> termT "c" <*> simp  
                       
simp       = memoize L_simp            $ buildNary <$> termT "b" <*> pred
				     <+> buildNary <$>        dop <*> termT "b" <*> pred
				     <+> buildNary <$>        simp <*> advp
				     <+>       vp
				     <+> buildNary <$>        dop <*> vp

sq         = memoize L_sq          $ buildNary <$> bep <*> termT "t" <*> subj
				     <+> buildNary <$>    sq <*> advp
				     <+> buildNary <$>    bep <*> termT "t" <*> subj <*> pred
				     <+> buildNary <$>    modalp <*> termT "t" <*> bep <*> subj
				     <+> buildNary <$>    modalp <*> termT "t" <*> bep <*> subj <*> pred
				     <+> buildNary <$>    modalp <*> termT "t" <*> termT "o" <*> bep <*> subj
				     <+> buildNary <$>    havep <*> termT "t" <*> bep <*> subj <*> pred
				     <+> buildNary <$>    modalp <*> termT "t" <*> termT "o" <*> bep <*> subj <*> pred
				     <+> buildNary <$>    modalp <*> termT "t" <*> havep <*> bep <*> subj
				     <+> buildNary <$>    modalp <*> termT "t" <*> havep <*> bep <*> subj <*> pred
				     <+> buildNary <$>    modalp <*> termT "t" <*> termT "o" <*> havep <*> bep <*> subj
				     <+> buildNary <$>    modalp <*> termT "t" <*> termT "o" <*> havep <*> bep <*> subj <*> pred
				     <+> buildNary <$>    havep <*> termT "t" <*> bep <*> subj
				     <+> buildNary <$>    havep <*> subj <*> bep <*> vp
				     <+> buildNary <$>    bep <*> subj <*> termT "o" <*> vp
				     <+> buildNary <$>    bep <*> subj <*> pred
				     <+> buildNary <$>    bep <*> subj <*> termT "b" <*> pred
				     <+> buildNary <$>    modalp <*> subj <*> bep <*> pred
				     <+> buildNary <$>    modalp <*> subj <*> havep <*> bep <*> pred
				     <+> buildNary <$>    havep <*> subj <*> termT "b" <*> pred
				     <+> buildNary <$>    dop <*> subj <*> vp
				     <+> buildNary <$>    modalp <*> subj <*> havep <*> bep <*> vp
				     <+> buildNary <$>    modalp <*> subj <*> vp
				     <+> buildNary <$>    modalp <*> subj <*> bep <*> vp
				     <+> buildNary <$>    modalp <*> subj <*> havep <*> vp
				     <+> buildNary <$>    havep <*> subj <*> vp
				     <+> buildNary <$>    bep <*> subj <*> vp

sq1   = memoize L_sq1        $ buildNary <$> bep <*> subj <*> vp2
				<+> buildNary <$>   bep <*> subj <*> termT "o" <*> vp
				<+> buildNary <$>   bep <*> subj
				<+> buildNary <$>   modalp <*> subj <*> havep <*> bep
				<+> buildNary <$>   modalp <*> subj <*> bep
				<+> buildNary <$>   havep <*> subj <*> termT "b"
				<+> buildNary <$>   sq1 <*> advp
				<+> buildNary <$>   modalp <*> subj <*> bep <*> vp2
				<+> buildNary <$>   dop <*> subj <*> vp2
				<+> buildNary <$>   modalp <*> subj <*> vp2
				<+> buildNary <$>   modalp <*> subj <*> havep <*> vp2
				<+> buildNary <$>   havep <*> subj <*> bep <*> vp2
				<+> buildNary <$>   modalp <*> subj <*> havep <*> bep <*> vp2
				<+> buildNary <$>   havep <*> subj <*> vp2

sq2           = memoize L_sq2      $ buildNary <$> modalp <*> termT "t" <*> termT "o" <*> havep <*> bep <*> vp
				<+> buildNary <$>   modalp <*> termT "t" <*> termT "o" <*> havep <*> bep
				<+> buildNary <$>   sq2 <*> advp
				<+> buildNary <$>   modalp <*> termT "t" <*> havep <*> bep <*> srel
				<+> buildNary <$>   modalp <*> termT "t" <*> havep <*> bep <*> pred
				<+> buildNary <$>   modalp <*> termT "t" <*> havep <*> bep <*> termT "b" <*> pred
				<+> buildNary <$>   modalp <*> termT "t" <*> havep <*> bep <*> vp
				<+> buildNary <$>   havep <*> termT "t" <*> bep <*> srel
				<+> buildNary <$>   modalp <*> termT "t" <*> termT "o" <*> havep <*> bep <*> termT "b" <*> pred
				<+> buildNary <$>   modalp <*> termT "t" <*> termT "o" <*> havep <*> bep <*> pred
				<+> buildNary <$>   modalp <*> termT "t" <*> termT "o" <*> havep <*> bep <*> srel
				<+> buildNary <$>   havep <*> termT "t" <*> bep
				<+> buildNary <$>   havep <*> termT "t" <*> bep <*> vp
				<+> buildNary <$>   havep <*> termT "t" <*> bep <*> termT "b" <*> pred
				<+> buildNary <$>   havep <*> termT "t" <*> bep <*> pred
				<+> buildNary <$>   modalp <*> termT "t" <*> termT "o" <*> bep <*> srel
				<+> buildNary <$>   bep <*> termT "t"
				<+> buildNary <$>   bep <*> termT "t" <*> vp
				<+> buildNary <$>   bep <*> termT "t" <*> termT "b" <*> pred
				<+> buildNary <$>   bep <*> termT "t" <*> pred
				<+> buildNary <$>   bep <*> termT "t" <*> srel
				<+> buildNary <$>   modalp <*> termT "t" <*> bep
				<+> buildNary <$>   modalp <*> termT "t" <*> bep <*> vp
				<+> buildNary <$>   modalp <*> termT "t" <*> termT "o" <*> bep <*> vp
				<+> buildNary <$>   modalp <*> termT "t" <*> termT "o" <*> bep <*> pred
				<+> buildNary <$>   modalp <*> termT "t" <*> termT "o" <*> bep <*> termT "b" <*> pred
				<+> buildNary <$>   modalp <*> termT "t" <*> havep <*> bep
				<+> buildNary <$>   modalp <*> termT "t" <*> bep <*> termT "b" <*> pred
				<+> buildNary <$>   modalp <*> termT "t" <*> bep <*> srel
				<+> buildNary <$>   modalp <*> termT "t" <*> bep <*> pred
				<+> buildNary <$>   modalp <*> termT "t" <*> termT "o" <*> bep  
                
srel         = memoize L_srel         $ buildNary <$> termT "p" <*> termT "r" <*> sdec
				       <+> buildNary <$>     termT "r" <*> subj <*> vp2
				       <+> buildNary <$>     termT "r" <*> subj <*> auxd <*> vp2
				       <+> buildNary <$>     termT "r" <*> subj <*> aux <*> bep
				       <+> buildNary <$>     termT "r" <*> subj <*> bep
				       <+> buildNary <$>     srel <*> pp
				       <+> buildNary <$>     termT "c" <*> srel
				       <+> buildNary <$>     termT "r" <*> auxd <*> vp
				       <+> buildNary <$>     termT "r" <*> vp
				       <+> buildNary <$>     termT "r" <*> aux <*> bep <*> pred
				       <+> buildNary <$>     termT "r" <*> bep <*> pred
				       <+> buildNary <$>     subj <*> aux <*> bep
				       <+> buildNary <$>     subj <*> auxd <*> vp2
				       <+> buildNary <$>     subj <*> bep
				       <+> buildNary <$>     subj <*> vp2
                       
start_t4        = memoize L_start_t4     $       sentence

subj         = memoize L_subj      $       np

swhq         = memoize L_swhq      $ buildNary <$> whnp <*> aux <*> bep
				       <+> buildNary <$>  whnp <*> vp
				       <+> buildNary <$>  whnp <*> auxd <*> vp
				       <+> buildNary <$>  whnp <*> bep
				       <+> buildNary <$>  whadjp <*> sq
				       <+> buildNary <$>  whadjp <*> sq1
				       <+> buildNary <$>  whnp <*> sq
				       <+> buildNary <$>  whnp <*> sq1
				       <+> buildNary <$>  whnp <*> sq2
                       
thancomp     = memoize L_thancomp     $ buildNary <$> termT "t" <*> subj <*> aux <*> bep
				       <+> buildNary <$>     termT "t" <*> subj <*> bep
				       <+> buildNary <$>     termT "t" <*> subj <*> auxd <*> vp2
				       <+> buildNary <$>     termT "t" <*> subj <*> vp2
				       <+> buildNary <$>     termT "t" <*> np
				       <+> buildNary <$>     termT "t" <*> sdec

-- vp :: M Label Label
vp           = memoize L_vp           $ buildNary <$> termT "v" <*> obj <*> adjp
				       <+> buildNary <$>     termT "v" <*> whadjp <*> infinitive
				       <+> buildNary <$>     termT "v" <*> whadjp <*> sdec
				       <+> buildNary <$>     termT "v" <*> whnp <*> infinitive
				       <+> buildNary <$>     termT "v" <*> termT "r" <*> sdec
				       <+> buildNary <$>     termT "v" <*> whnp <*> sdec
				       <+> buildNary <$>     termT "v" <*> whpp <*> infinitive
				       <+> buildNary <$>     termT "v" <*> whpp <*> sdec
				       <+> buildNary <$>     vp <*> pp
				       <+> buildNary <$>     termT "v" <*> obj <*> vp
				       <+> buildNary <$>     termT "v" <*> advp <*> obj
				       <+> buildNary <$>     termT "v" <*> obj <*> advp
				       <+> buildNary <$>     termT "v" <*> obj <*> termT "b" <*> pred
				       <+> buildNary <$>     vp <*> infinitive
				       <+> buildNary <$>     vp <*> advp
				       <+> buildNary <$>     vp <*> termT "a" <*> infinitive
				       <+> buildNary <$>     vp <*> termT "a" <*> adjp
				       <+>    termT "v"
				       <+> buildNary <$>     termT "v" <*> advp
				       <+> buildNary <$>     termT "v" <*> adjp
				       <+> buildNary <$>     termT "v" <*> infinitive
				       <+> buildNary <$>     termT "v" <*> vp
				       <+> buildNary <$>     termT "v" <*> termT "b" <*> pred
				       <+> buildNary <$>     termT "v" <*> obj
				       <+> buildNary <$>     termT "v" <*> termT "r"
				       <+> buildNary <$>     termT "v" <*> obj <*> infinitive1
				       <+> buildNary <$>     termT "v" <*> obj <*> infinitive
				       <+> buildNary <$>     termT "v" <*> infinitive
				       <+> buildNary <$>     termT "v" <*> sdec
				       <+> buildNary <$>     termT "v" <*> obj1 <*> obj2                       

vp1           = memoize L_vp1         $ buildNary <$> bep <*> pred
                       			 <+>  vp
                        
vp2           = memoize L_vp2         $ buildNary <$> vp2 <*> advp
					<+> buildNary <$>    termT "v" <*> adjp
					<+> buildNary <$>    vp2 <*> pp
					<+> buildNary <$>    termT "v" <*> termT "p" <*> obj1
					<+> buildNary <$>    termT "v" <*> obj1
					<+> buildNary <$>     termT "v" <*> infinitive1
					<+>    termT "v"
					<+> buildNary <$>     termT "v" <*> infinitive
					<+> buildNary <$>     termT "v" <*> advp

vp3           = memoize L_vp3         $       vp
					<+> buildNary <$>    termT "h" <*> vp
					<+> buildNary <$>    termT "b" <*> pred
					<+>   pred
					<+> buildNary <$>    termT "o" <*> vp3
                        
whadjp        = memoize L_whadjp      $ buildNary <$> termT "h" <*> termT "q"
					<+>   termT "h"
					<+> buildNary <$>    termT "h" <*> adjp
                        
whdet         = memoize L_whdet       $       termT "w"
                       			 <+> buildNary <$>    termT "h" <*> termT "q" 

whnp          = memoize L_whnp        $ buildNary <$> whdet <*> termT "n" <*> ncomp
					<+> buildNary <$>    whdet <*> termT "n"
					<+> buildNary <$>    whdet <*> nomhd <*> ncomp
					<+> buildNary <$>    whdet <*> ncomp
					<+> buildNary <$>    whdet <*> nomhd
                        
whpp          = memoize L_whpp        $       whdet
					<+> buildNary <$>    termT "p" <*> termT "w"                      
---
-- Labels

data Label
 = L_adjcomp
 | L_adjp
 | L_advp
 | L_ascomp
 | L_aux
 | L_auxd
 | L_bep
 | L_ddet
 | L_detq
 | L_dop
 | L_gerund
 | L_havep
 | L_infinitive
 | L_infinitive1
 | L_infinitrel
 | L_modalp
 | L_ncomp
 | L_nomhd
 | L_np
 | L_obj
 | L_obj1
 | L_obj2
 | L_pp
 | L_pred
 | L_qpp
 | L_scmp
 | L_sdec
 | L_sentence
 | L_simp
 | L_sq
 | L_sq1
 | L_sq2
 | L_srel
 | L_start_t4
 | L_subj
 | L_swhq
 | L_thancomp
 | L_vp
 | L_vp1
 | L_vp2
 | L_vp3
 | L_whadjp
 | L_whdet
 | L_whnp
 | L_whpp
   deriving (Eq,Ord,Show,Bounded,Enum)

{-
instance MemoLabel T_G4.Label where
instance BoundedMemoLabel T_G4.Label where

instance PP T_G4.Label where
	pp l          = text $ show l
-}


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

---------------------------------------
-- grammar specs

specs = [ ("T4", (start_t4, tests)) ]
