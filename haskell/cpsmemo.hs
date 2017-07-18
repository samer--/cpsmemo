{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RankNTypes, RecursiveDo #-}
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

fix1 f = fp where fp x = f fp x

-- class (Monad m, MonadPlus n) => MonadMemoTable m n where
--   run  :: n a -> m [a]
--   memo :: (c -> a -> n b) -> m (m [(a,[b])], c -> a -> n b)


-- mem  :: MonadMemoTable m n => (a -> n b) -> m (m [(a,[b])], a -> n b)
-- mem f = memo (\()-> f) >>= \(gt,mf) -> return (gt, mf ())
-- memrec f = memo f >>= \(g,mf) -> return (g, fix mf)
-- memrec2  (f,g) = do
--   (get_f, mf) <- memo f
--   (get_g, mg) <- memo g
--   let (fp,gp) = fix2 (mf,mg)
--   return ((get_f,get_g),(fp,gp))

{- Plan.
 - 1. Use delimitied continuation with answer type polymorphism
 - 2. Create a new type wrapper around this to be an instance of MonadPlus
 -    using an arbitrary MonadPlus for reification
 - 3. Use ST for references
 -}

-- nondeterministic continuation monad
type NDC r s = ContT r (ListT (ST s))

instance MonadPlus (NDC r s) where
  mzero = ContT {runContT= \k -> mzero}
  mplus f g = ContT {runContT= \k -> runContT f k `mplus` runContT g k}

run :: NDC a s a -> ST s [a]
run m = runListT (runContT m return)

memo :: (Ord a, Ord b) => (a -> NDC r s b) -> ST s (a -> NDC r s b)
memo f = do
  loc <- newRef Map.empty
  let update x e t = lift (lift (writeRef loc (Map.insert x e t)))
  return (\x -> do
    table <- readRef loc
    callCC (\k ->
      let handle (Just (res,conts)) = do
            update x (res,k:conts) table
            foldr' (mplus . k) mzero res
          handle Nothing = do
            update x (Set.empty,[k]) table
            y <- f x
            table' <- lift (lift (readRef loc))
            let Just (res,conts) = Map.lookup x table'
            if Set.member y res then mzero
            else update x (Set.insert y res, conts) table' >>
                 foldr' (\k -> mplus (k y)) mzero conts
      in handle (Map.lookup x table)))

-- Keilsi arrow of NDC monad
type NDCK r s a b =  a -> NDC r s b

-- type of open recursive arrow
type Open r s a b = NDCK r s a b -> NDCK r s a b

memopen :: (Ord a, Ord b) => Open r s a b -> ST s (Open r s a b)
memopen fopen = do
  loc <- newRef Map.empty
  -- let liftRef m  = lift (lift m) in
  -- let sanitize (x,(s,_)) = (x, Set.fold (:)s []) in
  let update x e t = lift (lift (writeRef loc (Map.insert x e t)))
  return (\fp x -> do
    table <- readRef loc
    callCC (\k ->
      let handle (Just (res,conts)) = do
            update x (res,k:conts) table
            foldr' (mplus . k) mzero res
          handle Nothing = do
            update x (Set.empty,[k]) table
            y <- fopen fp x
            table' <- lift (lift (readRef loc))
            let Just (res,conts) = Map.lookup x table'
            if Set.member y res then mzero
            else update x (Set.insert y res, conts) table' >>
                 foldr' (\k -> mplus (k y)) mzero conts
      in handle (Map.lookup x table)))

fib' fib n = if n<2 then return n 
             else liftM2 (+) (fib (n-2)) (fib (n-1))
-- mkfib = mfix fib'

mkfib = memopen fib' >>= return . fix 

mkfib' = mdo
  fib <- memo (\n -> if n<2 then return n else 
                     liftM2 (+) (fib (n-2)) (fib (n-1)))
  return fib

-- These two need be run using runST
test1 = mkfib >>= \f run (f 60)
test2 = mkfib' >>= \f run (f 60)


-- ( *> ) f g xs = f xs >>= g
-- ( <|> ) f g xs = (f xs) `mplus` (g xs)
-- epsilon xs = return xs 
-- term x = function | y:ys when x=y -> M.return ys
--                   | _ -> M.mzero ()

-- module type GRAMMAR = functor (MM : MONADMEMOTABLE) -> sig 
--   val grammar : ((bytes * (bytes list, bytes list) MM.table MM.m) list *
--                  (bytes -> bytes list -> bytes list MM.Nondet.m)) MM.m
--   val sentence : int -> bytes list
-- end

-- let words s = BatString.nsplit s ~by:" "

-- module MM = MemoTabT (Ref) (BatSet)

-- module TestG (G: GRAMMAR) = struct
--   (* module MM = MemoTabT (Ref) (ListC) *)
--   module GM = G (MM)
--   include Parser (MM.Nondet)
--   include MonadOps (MM)
--   include MemoTabOps (MM)
--   open MM

--   let success = function | [] -> true | _ -> false


--   let get_table (label, getter) = 
--     getter >>= fun table ->
--     return (label, table)

--   let sent i = GM.sentence i
--   let parse start input = 
--     Ref.run ( GM.grammar >>= fun (getters, get_nonterm) ->
--               run ((get_nonterm start) input) >>= fun results ->
--               mapM get_table getters >>= fun tables ->
--               return (results, tables) )

--   let profile start input = timeit (fun () -> fst (parse start input))
-- end

-- module GrammarOps (MM: MONADMEMOTABLE) = struct
--   include Parser (MM.Nondet)
--   include MonadOps (MM)
--   include MemoTabOps (MM)
--   include MM
-- end

-- module Johnson (MM : MONADMEMOTABLE) = struct
-- 	include GrammarOps (MM)

--   let success = function | [] -> true | _ -> false
--   let grammar = 
--     let v   = term "likes" <|> term "knows" in
--     let pn  = term "Kim" <|> term "Sandy" in 
--     let det = term "every" <|> term "no" in
--     let n   = term "student" <|> term "professor" in
--     memrec (fun np -> det *> n <|> pn <|> np *> term "'s" *> n) >>= fun (get_np,np) ->
--     memrec2 ((fun (vp,s) -> v *> np <|> v *> s),
--              (fun (vp,s) -> np *> vp))                          >>= fun ((get_vp,get_s),(vp,s)) -> 
--     return ([("s",get_s); ("np",get_np); ("vp",get_vp)], 
--             function | "s" -> s
--                      | "vp" -> vp)

--   let sentence = List.nth []
-- end

-- module Frost1 (MM: MONADMEMOTABLE) = struct
-- 	include GrammarOps (MM)

--   let grammar = 
--     let t = term in
--     mem (t "boy" <|> t "girl" <|> t "man" <|> t "woman") >>= fun (gn, n) ->
--     mem (t "knows" <|> t "respects" <|> t "loves") >>= fun (gv,v) ->
--     mem (t "helen" <|> t "john" <|> t "pat"      ) >>= fun (gpn, pn) ->
--     mem (t "and" <|> t "or"                      ) >>= fun (gconj, conj) ->
--     mem (t "every" <|> t "some"                  ) >>= fun (gdet, det) ->
--     mem (det *> n                                ) >>= fun (gdp, dp) ->
--     mem (pn <|> dp                               ) >>= fun (gnp', np') ->
--     memrec (fun np -> np' <|> np *> conj *> np   ) >>= fun (gnp, np) ->
--     memrec (fun vp -> v <|> vp *> conj *> vp     ) >>= fun (gvp, vp) ->
--     mem (np *> vp *> np) >>= fun (gs,s) ->
--     return (["np",gnp; "s",gs], (function "np" -> np | "s" -> s))

--   let s1 = "every boy or some girl and helen and john or pat " 
--   let s2 = s1^"knows and respects or loves "
--              ^"every boy or some girl and pat or john and helen"
--   let sentence = List.nth [words s1; words s2]
-- end

-- module FrostAmbig (MM: MONADMEMOTABLE) = struct
-- 	include GrammarOps (MM)

--   let grammar = 
--     let a = term "a" in 
--     let s = fix (fun s -> a *> s *> s <|> epsilon) in
--     memrec (fun sm -> a *> sm *> sm <|> epsilon) >>= fun (gsm, sm) ->
--     memrec (fun sml -> sml *> sml *> a <|> epsilon) >>= fun (gsml, sml) ->
--     memrec2 ((fun (smml, smml') -> smml *> smml' <|> epsilon),
--              (fun (smml, smml') -> smml *> a)) >>= fun ((gsmml,_), (smml,_)) ->
--     return (["s", return []; "sm",gsm; "sml",gsml; "smml",gsmml],
--             function "s" -> s | "sm" -> sm | "sml" -> sml | "smml" -> smml)

--   let sentence i = replicate i "a"
-- end

-- module T1 (MM: MONADMEMOTABLE) = struct
-- 	include GrammarOps (MM)
	
-- 	let sentences i = (words "n v d n") @ List.concat (replicate i (words "p d n"))
-- 	let grammar = 
-- 	  memrec2 ((fun (np,pp) -> term "n" <|> term "d" *> term "n" <|> np *> pp),
--              (fun (np,pp) -> term "p" *> np)) >>= fun ((gnp,gpp), (np,pp)) ->
-- 		memrec (fun vp -> term "v" *> np <|> vp *> pp) >>= fun (gvp, vp) ->
-- 		mem (np *> vp) >>= fun (gs, s) ->
-- 		return (["s",gs], s)
-- end

-- module Tomita2 (MM: MONADMEMOTABLE) = struct
-- 	include GrammarOps (MM)
	
--   (* polyadic memoising fixed point *)
--   let memrec_list fops =
--      mapM memo fops >>= fun memos_getters ->
--      let (getters, mfops) = List.split memos_getters in
--      let mfuns = fixlist mfops in
--      return (List.combine getters (fixlist mfops))

-- 	let sentence i = (words "n v d n") @ List.concat (replicate i (words "p d n"))

--   (* open recursive grammar - a lot of mutual recursion here *)
--   let [advm; adjm; nm; vc; np0; np1; np; pp; s; vp; dir; start] = List.map (fun i l -> List.nth l i) (iota 12)

--   let rules = 
-- 		let t = term in 
-- 		[ (* advm *) (fun g -> t "a" *> advm g <|> t "a" <|> advm g *> t "c" *> advm g)
--     ; (* adjm *) (fun g -> t "j" <|> t "j" *> adjm g <|> advm g *> t "j" <|> adjm g *> t "c" *> adjm g)
--     ; (* nm *)   (fun g -> t "n" <|> t "n" *> nm g)
--     ; (* vc *)   (fun g -> t "x" *> t "v" <|> t "v")
--     ; (* np0 *)  (fun g -> nm g <|> adjm g *> nm g <|> t "d" *> nm g <|> t "t" *> adjm g *> nm g)
--     ; (* np1 *)  (fun g -> adjm g *>  np0 g *> pp g *> pp g
--                        <|> adjm g *> np0 g *> pp g
--                        <|> adjm g *> np0 g
--                        <|> np0 g *> pp g
--                        <|> np0 g
--                        <|> np0 g *> pp g *> pp g)
--     ; (* np *)   (fun g -> np g *> t "c" *> np g
--                        <|> np1 g *> t "t" *> s g
--                        <|> np1 g *> s g
--                        <|> np1 g)
--     ; (* pp *)   (fun g -> pp g *> t "c" *> pp g <|> t "p" *> np g)
--     ; (* s *)    (fun g -> np g *> vp g *> pp g *> pp g
--                        <|> np g *> vp g *> pp g
--                        <|> np g *> vp g
--                        <|> s g *> t "c" *> s g)
--     ; (* vp *)   (fun g -> vc g *> np g <|> vp g *> t "c" *> vp g <|> vc g)
--     ; (* dir *)  (fun g -> dir g *> t "c" *> dir g
--                        <|> pp g *> vp g
--                        <|> vp g
--                        <|> vp g *> pp g)
--     ; (* start *)(fun g -> dir g <|> np g <|> s g)
--     ]

--   let grammar = memrec_list rules >>= fmap (flip List.nth 11) >>= fun (gs,s) ->
--                 return (["s",gs], fun "s" -> s)
-- end

