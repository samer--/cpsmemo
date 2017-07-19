{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, RankNTypes, RecursiveDo #-}
{- cpsmemo - Nondeterministic, left recursive memoisations using delimited continuations
   (c) Samer Abdallah, 2017
 -}
module CPSMemo where

import Prelude hiding (foldl, foldr)
import Control.Monad.Cont
import Control.Monad.ST
import Control.Monad.Ref
import Control.Monad.Fix
import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type NDC s n r = ContT (n r) (ST s)    -- nondeterministic continuation monad
type NDCK s n r a b = a -> NDC s n r b -- Kleilsi arrow of NDC monad ... SNRAB.
type Table a b = [(a, Set.Set b)]

instance MonadPlus n => MonadPlus (NDC s n r) where
  mzero = ContT {runContT= \_ -> return mzero}
  mplus f g = ContT {runContT= \k -> liftM2 mplus (runContT f k) (runContT g k)}

run :: MonadPlus n => NDC s n a a -> ST s (n a)
run m = runContT m (return . return)

memo' :: (MonadPlus n, Ord a, Ord b) => (NDCK s n r a b) -> ST s (ST s (Table a b), NDCK s n r a b)
memo' f = do
  loc <- newRef Map.empty
  let sanitize (x,(s,_)) = (x,s)
  let feed x table k = do
      let update e t = writeRef loc (Map.insert x e t)
      let consumer (res,conts) = do
          update (res, k:conts) table
          foldr' (mplus . k) mzero res
      let producer = do
          update (Set.empty, [k]) table
          y <- f x
          table' <- readRef loc
          let Just (res,conts) = Map.lookup x table'
          if Set.member y res then mzero
          else update (Set.insert y res, conts) table' >>
               foldr' (\k -> mplus (k y)) mzero conts
      maybe producer consumer (Map.lookup x table)
  return (readRef loc >>= return . map sanitize . Map.assocs,
          \x -> readRef loc >>= callCC . feed x)

memo :: (MonadPlus n, Ord a, Ord b) => (NDCK s n r a b) -> ST s (NDCK s n r a b)
memo = fmap snd . memo'

-- Fast Fibonnaci function
ffib n = runST $ mdo
  fib <- memo (\n -> if n<2 then return n else 
                     liftM2 (+) (fib (n-2)) (fib (n-1)))
  run (fib n)


-- Parser combinators ----------------------------------------

infixl 7 *>
infixl 6 <|>

type Parser s n r a = NDCK s n r [a] [a]
type ParserGen s n r a = ST s (Parser s n r a)
type ParserGen' s n r a = ST s (ST s (Table [a] [a]), Parser s n r a)

(*>)  f g xs = f xs >>= g
(<|>) f g xs = (f xs) `mplus` (g xs)
term x (y:ys) | x==y = return ys
term x _ = mzero
epsilon xs = return xs 

parse :: (forall s. ParserGen s [] [t] t) -> [t] -> [[t]]
parse p xs = runST $ p >>= run . ($ xs)

parse' :: (forall s. ParserGen' s [] [t] t) -> [t] -> ([[t]], Table [t] [t])
parse' gen xs = runST $ do
  (getter, p) <- gen
  result <- run (p xs)
  table <- getter
  return (result, table)

-- -- Some grammars to play with -------------------------------

johnson :: MonadPlus n => ParserGen s n r String
johnson = mdo
  v   <- return $ term "likes" <|> term "knows"
  pn  <- return $ term "Kim" <|> term "Sandy"
  det <- return $ term "every" <|> term "no"
  n   <- return $ term "student" <|> term "professor"
  np  <- memo $ det *> n <|> pn <|> np *> term "'s" *> n
  vp  <- memo $ v *> np <|> v *> s
  s   <- memo $ np *> vp
  return s

a :: MonadPlus n => Parser s n r Char
a = term 'a'

s, sm, sml, smml :: MonadPlus n => ParserGen s n r Char

s   = mfix (\s -> return $ a *> s *> s <|> epsilon)
sm  = mfix (\s -> memo   $ a *> s *> s <|> epsilon)

sml = mdo
  sml <- memo $ sml *> sml *> a <|> epsilon
  return sml

smml = mdo
  smml <- memo $ smml *> aux <|> epsilon
  aux  <- memo $ smml *> a
  return smml

-- with table getter
sm' :: MonadPlus n => ParserGen' s n r Char
sm' = mdo
  (getter, sm) <- memo' $ a *> sm *> sm <|> epsilon
  return (getter, sm)

tomita_sentence i = "nvdn" ++ Prelude.concat (replicate i "pdn")

tomita1, tomita2 :: MonadPlus n => ParserGen s n r Char
tomita1 = mdo
  np <- memo $ term 'n' <|> term 'd' *> term 'n' <|> np *> pp
  pp <- memo $ term 'p' *> np
  vp <- memo $ term 'v' *> np <|> vp *> pp
  s  <- memo $ np *> vp
  return s

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
