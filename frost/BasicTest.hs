module BasicTest where

import Data.Char (isDigit)
import Text.PrettyPrint.HughesPJ(text, sep)

import Data.Map    as M (keys,toAscList,(!))
import Data.IntMap as I ((!))

import ParserResults	hiding (E)
import PP
import Prelude hiding(pred) 

import System.TimeIt

data Strict a = Strict !a

time f = timeItT (seq (Strict (f ())) (return ()))

----- Highly Ambiguous Classical ---
-- Three diff forms
-- sA = right recursive
-- sB = direct left-recursive
-- sC = direct left-recursive with part-memoized

data L = A | B | C | C1 | D | D1 | E | F deriving (Show, Eq, Ord, Enum, Bounded)

instance MemoLabel L where
instance PP L where { pp l = text $ show l }

sA  :: ALP String (Tree String L) (Tree String L)
sA   = memoize A 
     $ branch3 <$> termT "a" <*> sA <*> sA 
       <+> emptyT

sB   = memoize B 
     $ branch3 <$> sB <*> sB <*> termT "a" 
       <+> emptyT

sC   = memoize C 
     $ branch2 <$> sC <*> memoize C1 (branch2 <$> sC <*> termT "a") 
       <+> emptyT

-- memo the double-D subparser
sD   = memoize D 
     $ branch2 <$> memoize D1 (branch2 <$> sD <*> sD) <*> termT "a" 
       <+> emptyT

--------------------------------------------------------------------------------
-- simple semantic example

	-- ambiguous addition
	-- need to wrap up 
	-- TODO: can use const ?  eg type ExprResult b = Const (Sum Int) b

newtype IntL t l = IntL Int deriving (Eq, Show)

instance Num (IntL t l) where
	IntL l + IntL r = IntL $ l + r
instance Semantic IntL where
instance SharedSemantic IntL where
	sharedS _ vs = vs
instance PP (IntL t l) where
	pp (IntL x)      = pp x

expr :: ALP String (IntL String L) (IntL String L)
expr = memoize BasicTest.E 
    $     (IntL . read) <$> satisfy (isDigit . head)
      <+> (+)           <$> expr <* term "+" <*> expr

test_expr = qi expr (words "1 + 2 + 3 + 4 + 5")



--------------------------------------------------------------------------------

-- hidden LR (via option)
sE = memoize E $ branch4 <$> sF <*> sE <*> sE <*> termT "a" <+> emptyT
sF = memoize F $ termT "a" <+> emptyT


-- mutual rec, differing ctxts
sG = memoize A $     branch2 <$> sG <*> termT "a" 
                 <+> branch2 <$> termT "a" <*> sH 
		 <+> sH
sH = memoize B $     branch2 <$> sH <*> termT "a" 
                 <+> emptyT

-- complex grammar, to test most things

sI = memoize A $     branch3 <$> sI <*> sI <*> termT "a"
                 <+> branch3 <$> termT "a" <*> sI <*> sI
                 <+> branch4 <$> emptyT <*> sI <*> sI <*> termT "a"
                 <+> branch4 <$> opt_x <*> sI <*> sI <*> termT "a"
                 <+> branch3 <$> sJ <*> sI <*> termT "a"
                 <+> branch3 <$> sK <*> sJ <*> termT "a"  -- lr chain in 2nd pos
		 <+> termT "a"
sJ = memoize B $     sI 
                 <+> branch2 <$> sJ <*> termT "a"
sK = memoize C $     sI 
                 <+> branch2 <$> sJ <*> termT "a"

opt_x = termT "x" <+> emptyT		-- NB not memoized

--------------------------------------------------------------------------------

{-

-- looking at cycles 
sL = memoize A $ sL *> term "a" <+> sL <+> empty 
sM = memoize B $ sM *> term "a" <+> empty <+> sM
sN = memoize C $ (sL <+> sM) *> (sL <+> sM)

sO = memoize D $ sO *> term "a" <+> sP <+> empty
sP = memoize E $ sO *> term "a" <+> sO <+> empty
-}

--------------------------------------------------------------------------------

-- forcing complex contexts

sQ = memoize A 
   $ branch3 <$> sQ <*> sR <*> termT "a" <+> emptyT
sR = memoize B
   $ branch4 <$> sR <*> sQ <*> termT "a" <*> termT "a" <+> emptyT

--------------------------------------------------------------------------------
-- monadic code

mA = memoize A 
   $ do { i <- satisfy (isDigit.head) 
        ; if i == "1" then termT "a" else branch2 <$> termT "a" <*> termT "a" }

--------------------------------------------------------------------------------
-- pruned choice

pA = termT "a" <|> termT "b" *> termT "c"

--------------------------------------------------------------------------------
-- expressions

e1 = memoize A 
   $ branch3 <$> e1 <*> termT '+' <*> e1 <+> termT '1'

--------------------------------------------------------------------------------

qt :: ALP String (Tree String L) (Tree String L) -> Int -> IO ()
qt g n
 = do 
      let inp = a_sentences n
      qi g inp
-- qi :: Show t => ALP t (Tree t L) (Tree t L) -> [t] -> IO ()
qi g inp
 = do 
      let state = runALP g inp 0
      -- let table = format format_table toEnum state 
      let table = resultsTable state 
      let table = spanTable state 
      -- po putStrLn $ table M.! A I.! 0 I.! (length inp - 1)
      po putStrLn $ toAscList table
      -- po putStrLn $ keys table

a_sentences
 = \i -> replicate i "a"

