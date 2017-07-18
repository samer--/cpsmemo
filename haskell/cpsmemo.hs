{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, RankNTypes, RecursiveDo #-}
{- cpsmemo - Nondeterministic, left recursive memoisations using delimited continuations
   (c) Samer Abdallah, 2017
 -}
module CPSMemo where

import Prelude hiding (foldl, foldr)
import Control.Monad.Cont
import Control.Monad.ST
import Control.Monad.Ref
import Control.Monad.Trans.List
import Control.Monad.Fix
import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- class (Monad m, MonadPlus n) => MonadMemoTable m n where
--   runM  :: n a -> m [a]
--   memoM :: (Ord a, Ord b) => (a -> n b) -> m (m [(a,[b])], a -> n b)

type NDC r s = ContT r (ListT (ST s)) -- nondeterministic continuation monad
type NDCK r s a b =  a -> NDC r s b   -- Kleilsi arrow of NDC monad
type Table a b = [(a, Set.Set b)]

instance MonadPlus (NDC r s) where
  mzero = ContT {runContT= \k -> mzero}
  mplus f g = ContT {runContT= \k -> runContT f k `mplus` runContT g k}

run :: NDC a s a -> ST s [a]
run m = runListT (runContT m return)

memo' :: (Ord a, Ord b) => (NDCK r s a b) -> ST s (ST s (Table a b), NDCK r s a b)
memo' f = do
  loc <- newRef Map.empty
  let sanitize (x,(s,_)) = (x,s)
  let feed x table k = do
      let update e t = lift (lift (writeRef loc (Map.insert x e t)))
      let consumer (res,conts) = do
          update (res, k:conts) table
          foldr' (mplus . k) mzero res
      let producer = do
          update (Set.empty, [k]) table
          y <- f x
          table' <- lift (lift (readRef loc))
          let Just (res,conts) = Map.lookup x table'
          if Set.member y res then mzero
          else update (Set.insert y res, conts) table' >>
               foldr' (\k -> mplus (k y)) mzero conts
      maybe producer consumer (Map.lookup x table)
  return (readRef loc >>= return . map sanitize . Map.assocs,
          \x -> readRef loc >>= callCC . feed x)

memo :: (Ord a, Ord b) => (NDCK r s a b) -> ST s (NDCK r s a b)
memo = fmap snd . memo'

-- Fast Fibonnaci function
ffib n = runST $ mdo
  fib <- memo (\n -> if n<2 then return n else 
                     liftM2 (+) (fib (n-2)) (fib (n-1)))
  run (fib n)


infixl 7 *>
infixl 6 <|>

type Parser r s a = [a] -> NDC r s [a]

(*>), (<|>) :: Parser r s a -> Parser r s a -> Parser r s a 
epsilon :: Parser r s a
term :: Eq a => a -> Parser r s a

( *> ) f g xs = f xs >>= g
( <|> ) f g xs = (f xs) `mplus` (g xs)
epsilon xs = return xs 
term x (y:ys) | x==y = return ys
term x _ = mzero

parse :: (forall s. ST s (Parser [t] s t)) -> [t] -> [[t]]
parse p xs = runST $ p >>= run . ($ xs)

-- Some grammars to play with -------------------------------

johnson = mdo
  v   <- return $ term "likes" <|> term "knows"
  pn  <- return $ term "Kim" <|> term "Sandy"
  det <- return $ term "every" <|> term "no"
  n   <- return $ term "student" <|> term "professor"
  np  <- memo $ det *> n <|> pn <|> np *> term "'s" *> n
  vp  <- memo $ v *> np <|> v *> s
  s   <- memo $ np *> vp
  return s

a = term 'a'

s :: ST s (Parser r s Char)
s = mdo
  s <- return $ a *> s *> s <|> epsilon
  return s

sm = mdo
  sm <- memo $ a *> sm *> sm <|> epsilon
  return sm

sml = mdo
  sml <- memo $ sml *> sml *> a <|> epsilon
  return sml

smml = mdo
  smml <- memo $ smml *> aux <|> epsilon
  aux  <- memo $ smml *> a
  return smml

tomita1 = mdo
  np <- memo $ term 'n' <|> term 'd' *> term 'n' <|> np *> pp
  pp <- memo $ term 'p' *> np
  vp <- memo $ term 'v' *> np <|> vp *> pp
  s  <- memo $ np *> vp
  return s

tomita_sentence i = "nvdn" ++ Prelude.concat (replicate i "pdn")

tomita2 = let t = term in mdo
  advm <- memo $ t 'a' *> advm <|> t 'a' <|> advm *> t 'c' *> advm
  adjm <- memo $ t 'j' <|> t 'j' *> adjm <|> advm *> t 'j' <|> adjm *> t 'c' *> adjm
  nm   <- memo $ t 'n' <|> t 'n' *> nm
  vc   <- memo $ t 'x' *> t 'v' <|> t 'v'
  np0  <- memo $ nm <|> adjm *> nm <|> t 'd' *> nm <|> t 't' *> adjm *> nm
  np1  <- memo $ adjm *>  np0 *> pp *> pp
             <|> adjm *> np0 *> pp
             <|> adjm *> np0
             <|> np0 *> pp
             <|> np0
             <|> np0 *> pp *> pp
  np   <- memo $ np *> t 'c' *> np
             <|> np1 *> t 't' *> s
             <|> np1 *> s
             <|> np1
  pp   <- memo $ pp *> t 'c' *> pp <|> t 'p' *> np
  s    <- memo $ np *> vp *> pp *> pp
             <|> np *> vp *> pp
             <|> np *> vp
             <|> s *> t 'c' *> s
  vp   <- memo $ vc *> np <|> vp *> t 'c' *> vp <|> vc
  dir  <- memo $ dir *> t 'c' *> dir
             <|> pp *> vp
             <|> vp
             <|> vp *> pp
  memo $ dir <|> np <|> s
