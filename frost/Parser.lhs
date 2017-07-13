> {-# OPTIONS_GHC -funbox-strict-fields #-}

Copyright 2007, Paul Callaghan, Richard Frost, Rahmatullah Hafiz.
All rights reserved.

Please see LICENCE file distributed with the source code.


> module Parser ( module Parser
>               , (<*), (<*>), (*>), (<$>) ) where


> import Prelude as P

> import Control.Applicative
> import Control.Monad(foldM)
> import Data.Monoid hiding ((<>))

> import Data.Array as A
> import Data.IntSet as S 
> import Data.IntMap as I

> import PP
> import Data.List as L

%---------------------------------------
P&A of relevant operators
 - other operators defined elsewhere

> infixl 2 <@>
> infixl 3 <+>, <|>


%---------------------------------------
Properties of memoize labels
 - must be enumerable

> class Enum l => MemoLabel l where
> 	-- empty class
> 	-- grammar labels must be convertible to Ints

> class (Bounded l, MemoLabel l) => BoundedMemoLabel l where
> 	-- empty class
> 	-- for some operations, the upper and lower limits are needed




%---------------------------------------
int-tagged values, used to store endpoints + results
 -- TODO: look at switching to fns rather than appended lists
 --  ANS: results not promising - abandoned (or try later)

> data Tag a = Tag !Int !a deriving Show
> instance (PP a) => PP (Tag a) where
>       -- pp (Tag a b) = pp a PR.<+> text "=>" PR.<+> pp b
>       pp (Tag a b) = pp (a,b)


%---------------------------------------
A collection of endpoint-tagged results 
-- INVARIANT: identical endpoints are merged, ie at most one tag per end pos
-- TODO: look at DESCENDING lists
-- TODO: does this need to be [s]? or should it be generalised?

> newtype E s 
>  = E { unE :: [Tag [s]] } deriving (Show)

> instance Functor E where
> 	fmap f (E ts) = E [ Tag i (fmap f x) | Tag i x <- ts ]

> instance Monoid (E a) where
> 	mempty              = E []	-- mzero
> 	mappend (E a) (E b) = E $ sorted_merge a b
> 		-- check complexity here
> 		-- might be: this is not a good representation

> sorted_merge :: [Tag [a]] -> [Tag [a]] -> [Tag [a]]
> sorted_merge [] bs = bs
> sorted_merge as [] = as
> sorted_merge as@(ia@(Tag i a):at) bs@(jb@(Tag j b):bt)
>  = case i `compare` j of
> 	LT -> ia : sorted_merge at bs
> 	EQ -> Tag i (mappend a b) : sorted_merge at bt
> 	GT -> jb : sorted_merge as bt



%---------------------------------------
Main types

---
Spans: showing extent of a match

> type Start = Int
> type End   = Int
> data Span l = Span l Start End deriving (Show,Eq)
> instance (PP l) => PP (Span l) where
>       pp (Span x s e) = pp x <> text "=>" <> pp (s,e)

---
internal types

> type Pos = Int
> type ILabel = Int

--- 
parsing results

> -- we group (lists of) results by identical endpoints
> type ParseResult a = E a

> -- this is what is stored in memo table
> -- the first slot is used to retain subnodes
> -- the second slot is the "expansion" of the relevant subnodes
> -- TODO: exploit the singleton lists of subnodes? 
> data Stored a 
>  = Stored { s_stored  :: UpResult a		-- context + subnodes
>           , s_context :: L_Context		-- for checking reuse
>           , s_results :: ParseResult a	-- full results
>           }


> -- a memotable has structure Label -> StartPosition -> Results
> type State a = IntMap {-ILabel-} (IntMap (Stored a))



> -- We pass this plus a set of curtailing NTs upwards.
> type CurtailingNTs = IntSet
> empty_cuts :: CurtailingNTs
> empty_cuts = S.empty



> -- NOTE: curtailing could be attached to all results
> -- NOTE: it only matters for zero-width results - not all results
> -- TODO: use this observation to wrap this concept better (currently ad hoc)
> --       eg have as union type? 
> type UpResult a = (CurtailingNTs, ParseResult a)


> -- the main construct in use: Ambiguous, Left-recursive Parser
> -- memo table stores one type of result, monad can yield another
> newtype ALP t stored a 
>  = ALP { unALP :: L_Context 
>                -> Pos 
>                -> StateM (Input t)
>                          (State stored) 
>                          (UpResult a) }

> -- the operation for running a parse
> runALP :: ALP t s s -> [t] -> Int -> State s
> runALP p ts i 
>  = snd $ unState (unALP p empty_context i) (mkInput ts) I.empty


> -- ALP is a functor
> instance Functor (ALP t s) where
>	fmap f (ALP g) 
>	 = ALP $ \cc pos -> fmap (\(i,r) -> (i, fmap f r)) $ g cc pos 

> -- which is applicative
> instance Applicative (ALP t s) where
>	(<*>) = alp_product

>	-- 'pure' allows the value in all subsequent positions
>	pure v = ALP $ \_ pos -> do
>	                            inp <- getR
>	                            return (empty_cuts, all_tags inp pos)
>	         where
>	             all_tags inp pos = E [ Tag j [v] 
>	                                  | j <- [ pos .. num_tokens inp ] ]

> -- and we can also define a monad for it, for finer control
> instance Monad (ALP t s) where
>	return = pure
>	m >>= k = thenALP m k





%---------------------------------------
LR context and its various operations

> -- The LR context records how many times each non-terminal has been called 
> -- on the current path through the grammar. Only this is passed downwards.
> -- TODO: investigate level-first repres, to reduce traversal?
> -- TODO: check density; leave? depends on Gr
> newtype L_Context     = L_Context [(ILabel, Int)]	
> empty_context = L_Context []

> findCount :: L_Context -> ILabel -> Int
> findCount (L_Context cs) i_name
>  = case P.lookup i_name cs of
> 	Nothing -> 0
> 	Just fc -> fc 

> canReuse :: L_Context -> L_Context -> Bool
> canReuse (L_Context current) (L_Context stored)
>  = and [ or [ cc >= sc 
>             | (cn,cc) <- current, sn == cn ]
>        | (sn,sc) <- stored ]

> 	-- for ALL entries in sc, 
>	--	see if corresponding item in cc is not less constrained
> 	-- obviously N^2, but can we use ordering info to reduce this?


-- FASTER: filter ncs by set-member test in rs, goes n^2 to nlogn + lazy
-- QUERY: does it make a difference? 
-- OR if only used in checkContext, then can rely on efficient test 
-- (though: check profile!) 

> -- TODO: document
> -- TODO: check ctxt ordering, prefer standardise on list? eg asc label order?
> pruneContext :: CurtailingNTs -> L_Context -> L_Context
> pruneContext rs _
>  | S.null rs = empty_context
> pruneContext rs (L_Context ctxt)
>  = L_Context [ nc | nc@(n,c) <- ctxt, n `S.member` rs ]





> -- TODO: is there a standard version of this?
> -- TODO: investigate order
> incContext :: ILabel -> L_Context -> L_Context 
> incContext name (L_Context lcs) 
>  = L_Context $ inc lcs
>    where
>	inc []                         = [(name,1)]
>	inc (nc@(n,c):ncs) | n == name = (n,c + 1) : ncs 
>        	           | otherwise = nc : inc ncs
>                            

%---------------------------------------

-- state-monad definition
-- we define it ourselves ONLY to avoid import of non-Haskell-98 libraries
-- includes state-reader element

> newtype StateM r s t = State { unState :: r -> s -> (t, s) }
> instance Monad (StateM r s) where
> 	return x = State (\r s -> (x,s))
> 	State m >>= k 
> 	 = State (\r s -> let (a,y) = m r s in unState (k a) r y)
> instance Functor (StateM t s) where
>	fmap f (State sf) = State $ fmap (fmap (\(t,s) -> (f t, s))) sf

> -- standard functions
> get       :: StateM r s (r,s)
> get       = State $ \r s -> ((r,s),s)
> getR      :: StateM r s r
> getR       = State $ \r s -> (r,s)
> put       :: s -> StateM r s ()
> put s      = State $ \_ _ -> ((),s)
> modify    :: (s -> s) -> StateM r s ()
> modify sf  = State $ \_ s -> ((), sf s)


%---------------------------------------
Interface for overloading result-building
 - partly intended to reduce memory leakage (through duplication)
 - reflects the parse-building primitives

> class Semantic s where	-- param
>	emptyS  :: s t l
>	tokenS  :: t -> s t l
>	branchS :: l -> [s t l] -> s t l

> class Semantic s => SharedSemantic s where
>	sharedS :: MemoLabel l => Span l -> [s t l] -> [s t l]
>	-- HACKY - hides info of whether singleton subnode created
>	-- HACKY - would like to use this info???
>	-- NOTE: this allows for filtering and pruning of sub-results
>	-- NOTE: it also allows for some processing of sub-results too


%-------------------------------------------------------------------------------
-- `orelse` combinator

> -- TODO: would be nice to use proper applicative structure here?
> --       but maybe only at top level?
> -- call it: InclusiveAlternative? 
> (<+>) :: ALP t s a -> ALP t s a -> ALP t s a
> p <+> q
>  = ALP $ \cc pos ->
>    do
> 	(cut1,m) <- unALP p cc pos
> 	(cut2,n) <- unALP q cc pos
> 	return ( {-# SCC "oe_union" #-} S.union cut1 cut2
> 	       , {-# SCC "oe_list" #-}  m `mappend` n )

%-------------------------------------------------------------------------------
-- first-past-the-post choice
-- we don't visit the second parser if the first produced anything
-- NOTE: should this be done at the endpoint level?

> (<|>) :: ALP t s a -> ALP t s a -> ALP t s a
> p <|> q
>  = ALP $ \cc pos ->
>    do
> 	left@(_,E left_vals) <- unALP p cc pos
> 	case left_vals of 
>	  [] -> unALP q cc pos
>	  _  -> return left


%-------------------------------------------------------------------------------
The applicative product operation

-- TODO: could clarify this as 
	p >> \rs -> fold <+> (return p <+> q ctxt r) ...
-- ie, as big union of simpler results
-- following is kind of deforested version
-- QUERY: done internally, can we optimise what happens with <+>?
	(cf occurrence outside in normal grammar)

> alp_product :: ALP t s (a -> b) -> ALP t s a -> ALP t s b
> alp_product p q 
>  = ALP $ \cc pos -> 
>    do
>  	(cut, E p_results) <- unALP p cc pos 

> 	let 
> 	    join_with_first (Tag end l_results) -- special case
> 	     = do 
> 		  let null_p = end == pos
> 		  let new_ctxt | null_p     = cc		-- keep
>                              | otherwise  = empty_context	-- reset
> 		  (new_cuts, q_result) <- unALP q new_ctxt end
>		  let out_cuts | null_p    = new_cuts `S.union` cut  -- keep
>                              | otherwise = cut                     -- ignore
> 		  return ( out_cuts , join_results l_results q_result )

> 	    join_with_rest (Tag end l_results) 
> 	     = do 
> 		  q_result <- unALP q empty_context end
> 		  return $ join_results l_results (snd q_result)

>	case p_results of
>	  []   -> return (cut, mempty)
>	  r:rs -> do
> 	             (cuts,first_result) <- join_with_first r 
>		     rest_results        <- mapM join_with_rest rs
>                    let merged_results = foldr1 mappend $ first_result : rest_results
>	               -- note order to get sols in right order
>	               -- TODO: consider batched merging? (ie merge in pairs)
>	               -- TODO: consider which sol we want out first?
>	               -- TODO: could supply eager/greedy as a strategy?
>
> 	             return ( cuts , merged_results )

> join_results :: [a -> b] -> ParseResult a -> ParseResult b
> join_results left_result (E rights)
>  = E [ Tag re [ f r | f <- left_result, r <- right_results ]
>      | Tag re right_results <- rights ]
>	-- Note: this isn't a functorial pattern, because E hides [] str


> -- NOTE: identical to alp_product, apart from the "let alts = ..." line
> -- TODO: merge this with product code ?
> -- NOTE: extra function parameter for alp_product may slow it down
> thenALP :: ALP t s a -> (a -> ALP t s b) -> ALP t s b
> thenALP m k
>  = ALP $ \cc pos -> 
>    do
>	(cut, E p_results) <- unALP m cc pos 

> 	let join_with_first (Tag end l_results) -- special cases
> 	     = do 
> 		  let null_p = end == pos
> 		  let new_ctxt | null_p     = cc		-- keep
>                              | otherwise  = empty_context	-- reset

> 		  let alts = foldr1 (<+>) [ k r | r <- l_results ]
> 		  (new_cuts, q_result) <- unALP alts new_ctxt end	

>		  let out_cuts | null_p    = new_cuts `S.union` cut  -- keep
>                              | otherwise = cut                     -- ignore
> 		  return ( out_cuts , q_result )

> 	    join_with_rest (Tag end l_results) 
> 	     = do 
> 		  let alts = foldr1 (<+>) [ k r | r <- l_results ]
> 		  q_result <- unALP alts empty_context end
> 		  return $ snd q_result

>	case p_results of
>	  []   -> return (cut, mempty)
>	  r:rs -> do
> 	             (cuts,first_result) <- join_with_first r 
> 	             rest_results        <- mapM join_with_rest rs
>	             let merged_results = foldr1 mappend $ first_result : rest_results
> 	             return ( cuts , merged_results )

-----------------------------------------		
-- empty and term combinators

-- PLAN: empty really is empty, and need work to relabel it
-- DISA: could mean memory leak, in certain grammars - can try to share???
-- TODO: memoise also? (via input object)

> empty :: ALP t l ()
> empty = ALP $ \cc pos -> return (empty_cuts, E [Tag pos [()]])

> emptyR :: Semantic s => ALP t l (s t a)
> emptyR = ALP $ \cc pos -> return (empty_cuts, E [Tag pos [emptyS]])


-- NB could memoise this stuff inside the Input_ type

> term :: Eq t => t -> ALP t l t
> term c 
>  = satisfy (c ==)

> -- TODO: generalise to passed input
> -- TODO: fuse into input handling, ie can precomp ALL of this
> --       OR (of course) can compute something for the token stream! 
> --       -- so satisfy becomes simple filter on categories or whatever
> satisfy :: (t -> Bool) -> ALP t l t
> satisfy test 
>  = ALP $ \cc pos ->
>    do 
>	inp <- getR
>	return (empty_cuts, case fetchT inp pos of
>                             Nothing                    -> mempty
>                             Just (tok,res) | test tok  -> res
>                                            | otherwise -> mempty  )


%-------------------------------------------------------------------------------
----Memoize----


> (<@>) :: (SharedSemantic s, PP l, MemoLabel l)
>       => l -> ALP t (s t l) (s t l) -> ALP t (s t l) (s t l)
> (<@>) = memoize		-- synonym, lower than <+>

> memoize 
>  :: (SharedSemantic s, PP l, MemoLabel l) 
>  => l -> ALP t (s t l) (s t l) -> ALP t (s t l) (s t l)
> memoize = memoize_ sharedS

> memoize_ :: MemoLabel l 
>          => (Span l -> [out] -> [out]) -> l -> ALP t out out -> ALP t out out
> memoize_ share_fn e_name f 
>  = ALP $ \context pos -> 
>    do
>       (inp,mTable) <- get
>	case lookupT i_name pos context mTable of	
>         Just res 
>              -> return res
>         Nothing 
>              | findCount context i_name > num_tokens inp - pos + 1
>              -> return (S.singleton i_name, E [] {-mempty-})
> 	     	-- empty, pruned
> 		-- NOTE: poss mem leak here, since repeatedly make singletons
> 		-- can handle if force e_name to be bounded, though store where?
> 		-- (poss: in input block...)

> 	       | otherwise
> 	       -> do
> 	            let new_down_ctxt = incContext i_name context 
> 	            (up_ctxt,results) <- {-# SCC "memo_run" #-} 
>	                                 unALP f new_down_ctxt pos
>                   let subnodes = E [ Tag e $ share_fn (Span e_name pos e) ss
>                                    | Tag e ss <- unE results ]
>                   let result   = (up_ctxt, subnodes) 
>                   let repacked = results	-- stored in packed form anyway!
> 	            let sub_ctxt = {-# SCC "memo_ctxt" #-} 
> 	                           pruneContext up_ctxt context
>                   let to_store = Stored result sub_ctxt repacked
>	            modify ({-# SCC "memo_update" #-} update_ pos to_store)
>	            return result
>    where 
>	i_name = fromEnum e_name

>	----Update----
>	-- NOTICE: merges by OVERWRITING first match
>	--         can do this since new result is more complete
>	update_ :: Start -> Stored l -> State l -> State l
>	update_ pos res 
>	 = I.insertWith (\_ prev -> I.insert pos res prev) 
>                       i_name 
>	                (I.singleton pos res)





%---------------------------------------

----Look Up----
-- saved left-rec-context  is compared with the current left-rec-context.
-- The result is only reused if, for each parser and input position, 
-- saved left-rec-context equal to the current left-rec-count in the current context.


> lookupT :: ILabel -> Int -> L_Context -> State l -> Maybe (UpResult l)
> lookupT name inp current mTable
>  = do
>	inner <- I.lookup name mTable
>	memo  <- {-# SCC "inner_lu" #-} I.lookup inp inner
>	let stored = s_stored memo
>	if S.null (fst stored)
>	  then return stored		-- reuse if nothing curtailed it
>         else if canReuse current (s_context memo) 
>	         then Just stored
>	         else Nothing




%-------------------------------------------------------------------------------
Input processing
 - we memoize certain results, to avoid recomputation etc
 - fetchT produces a token and a parse result containing only the token
 - TODO: generalise leaf building

> data Input t
>  = Input { tokens     :: ![t]
>         , num_tokens :: !Int
>         , token_arr  :: !(Array Int t)
>         , fetch      :: !(Int -> t) 
>         , fetchT     :: !(Int -> Maybe (t,ParseResult t)) }

> mkInput :: [t] -> Input t
> mkInput ts
>  = Input ts
>         (length ts)
>         arr
>         (arr A.!)
>         (term_check A.!)
>    where
> 	arr = listArray (0, length ts - 1) ts
>	term_check 
>	 = listArray (0, length ts) 
>	 $ [ Just (t, E [Tag i_1 [t]]) | (t,i_1) <- zip ts [1..] ] ++ [Nothing]


