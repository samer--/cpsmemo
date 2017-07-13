TODO: extract table into Span-indexed simpler table
NOTE: need to do cycle detection here!

> module ParserResults ( module ParserResults
>                      , module Parser
>                      , module ParserTests
>                      , PP(..)
>                      , text) where

> import Debug.Trace		-- tmp

> import Prelude as P

> import Control.Monad(foldM)
> import Control.Applicative
> import Data.Monoid

> import Data.Map      as M
> import Data.IntSet   as S (toAscList)
> import Data.IntMap   as I
> import Data.Sequence (Seq)
> import Data.Foldable as F (toList)

> import Text.PrettyPrint.HughesPJ as PR -- ((<+>), vcat, text, render,($$),nest,Doc)

> import PP
> import Parser
> import ParserTests

%-------------------------------------------------------------------------------
Create simpler tables
 - note: for some applications, spanTable may force too much computation
 - use "resultsTable" when laziness is important

> spanTable :: (Eq l, MemoLabel l) => State (s l) -> M.Map (Span l) [s l]
> spanTable st
>  = M.fromAscList [ (Span (toEnum l) s e , rs )
>                  | (l, pt)    <- I.toAscList st
>                  , (s, et)    <- I.toAscList pt
>                  , let (E ts)  = s_results et
>                  , Tag e rs   <- ts ]

> resultsTable :: (Eq l, MemoLabel l)
>              => State (s l) -> M.Map l (IntMap (IntMap [s l]))
> resultsTable st
>  = M.fromAscList [ (toEnum l, fmap (convert_endpts . s_results) pt) 
>                  | (l,pt) <- I.toAscList st ]
>    where
>	convert_endpts (E ts) = I.fromAscList [ (e,rs) | Tag e rs <- ts ]

> rawResults st =  fmap (fmap (result . s_results)) st 
> result (E r) = r

%-------------------------------------------------------------------------------
One form of tree
 - branches aren't labelled here (originally: to support nesting)
 - NB we could add some support, though it may duplicate part of memoize

> data Tree a l
>  = Empty
>  | Leaf a
>  | Branch [Tree a l] 
>  | SubNode (Span l)
>    deriving Show

> mapLabels f Empty = Empty
> mapLabels f (Leaf a) = Leaf a
> mapLabels f (Branch ts) = Branch (P.map (mapLabels f) ts)
> mapLabels f (SubNode (Span l st end)) = SubNode (Span (f l) st end)

> tree_size Empty = 0
> tree_size (Leaf a) = 1
> tree_size (Branch ts) = sum (P.map tree_size ts)
> tree_size (SubNode (Span l st end)) = end - st

> instance Semantic Tree where
>	emptyS    = Empty
>	tokenS    = Leaf
>	branchS _ = Branch
> instance SharedSemantic Tree where
>	sharedS span _ = [SubNode span]		-- TODO: see notes in main file

> instance (Show t, PP l) => PP (Tree t l) where
>       pp Empty        = text "{_}"
>       pp (Leaf x)     = text "Leaf"    PR.<+> text (show x)
>       pp (Branch ts)  = text "Branch"  PR.<+> pp_list sep ts
>       pp (SubNode (Span x s e)) 
>	                = text "SubNode" PR.<+> pp x PR.<+> text (show (s,e)) 

---
Some utility functions 

> emptyT  = const Empty <$> Parser.empty
> termT t = Leaf        <$> term t
> branch2 a b     = Branch [a,b]
> branch3 a b c   = Branch [a,b,c]
> branch4 a b c d = Branch [a,b,c,d]



%-------------------------------------------------------------------------------
For showing full result tables

> format_table :: (MemoLabel l, PP l, PP (s l))
>              => (Int -> l) -> Stored (s l) -> Doc
> format_table toEnum s@(Stored (cs,E sn) (L_Context ct) rs)
>  = vcat [ text "CUT:" PR.<+> pp (P.map (pp.toEnum) $ S.toAscList cs)
>         , text "CTX:" PR.<+> pp [ (toEnum c, j) | (c,j) <- ct ]
> 	  , text "RES:" PR.<+> format_result toEnum s
>	  ]

-- for just showing result tables

> format_result :: PP (s l) => (Int -> l) -> Stored (s l) -> Doc
> format_result _ (Stored _ _ (E rs))
>  = pp_list sep rs

<> instance PP a => PP (Seq a) where
<>	pp s = pp $ F.toList s

-- just showing the top level of the table (ie, root from 0)

> format_top :: (PP (s l), MemoLabel l) => l -> State (s l) -> Doc
> format_top root table 
>  = maybe PR.empty id 
>  $ do
>	inner  <- I.lookup (fromEnum root) table
>	result <- I.lookup 0               inner
>	return $ format_result (error "") result


> format :: (PP l, MemoLabel l) 
>        => ((Int -> l) -> Stored (s l) -> Doc) 
>        -> (Int -> l) 
>        -> State (s l)
>        -> Doc

> format inner_format toEnum t 
>  = vcat
>    [ text "Label" PR.<+> pp (toEnum s)
>      $$ 
>      nest 1 (pp_list vcat
>                 [ pp i PR.<+> text "->" PR.<+> inner_format toEnum inner 
>                 | (i,inner) <- I.toList sr
>                 , let (E results) = s_results inner
>                 , not $ P.null results
>                 ])
>    | (s,sr) <- I.toList t ]


%---------------------------------------

> {-
> stats :: MemoLabel l => M (s l) (s l) -> String -> l -> String -> Int -> IO ()
> stats grammar grammar_name e_name test_name test_num
>  = do
> 	let (output,tests) = 
> 	      case P.lookup grammar_name specs of
> 	        Nothing  -> error $ "Bad grammar name: " ++ show grammar_name
> 	        Just val -> val

> 	let ts =
> 	      case getTest tests test_name test_num of 
> 	        Nothing  -> error $ "Bad test case: " ++ show (test_name,test_num)
> 	        Just val -> val

> 	setInput ts
> 	let table = test grammar ts 0
> 	putStrLn $ show $ counts table
> 	-- trees e_name table

%---------------------------------------

NOTE: this is computed on graph containing partials
      it isn't what we really want.
      could do it after conversion to actually-required graph 

> graphStats :: (State (s l) -> Int
> graphStats 
>  = I.fold (\inner prev -> 
>            I.fold (\es prev -> 
> 	           count (s_results es) + prev) prev inner) 0
>    where
>   	count (E ets) = length [ t | Tag e ts <- ets, t <- ts ]

> data GraphStats
>  = GraphStats { choice_nodes :: Int	-- how many >1 OR branches
>               , graph_nodes  :: Int	-- how many 

> ct 1 = stats Misc.sA "sm" Misc.A "aaa" 
> ct 2 = stats Misc.sB "sml" Misc.B "aaa" 
> -}

> ---------------------------------------


> find = I.findWithDefault (error "didn't expect") 

TODO: generalise this
      however: it relies on properties of unpicking the sharing
      not sure how to do this

> -- expanded :: MemoLabel l => ([s l] -> s l) -> l -> State (s l) -> IO ()
> expanded l t
>  = po putStrLn $ find 0 (find (fromEnum l) t_)
>    where
>    	t_ = I.map (I.map (decode_Es t_ . s_results)) t		-- cyclic
> 	decode_Es t (E ets) 
> 	 = [ Tag e $ concatMap (trees t) $ F.toList ts | Tag e ts <- ets ]
>       fetch p ers = head [ r | Tag e r <- ers, e == p ]
> 	trees t l@(Leaf _)           
>	 = [l]
> 	trees t (SubNode (Span l  s e)) 
>	 = fetch e $ find s $ find (fromEnum l) t
> 	trees t (Branch ts)            
>	 = fmap Branch $ product $ fmap (trees t) ts
> 	product [] = [[]]
> 	product (ts:tss) = [ u : us | u <- ts, us <- product tss ]


