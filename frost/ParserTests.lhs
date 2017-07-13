> module ParserTests where
> import Text.PrettyPrint.HughesPJ (Doc)

-- simple naming system for test cases
-- a grammar exports a named collection of tests, 
-- each of which is either a parametric test or a list of fixed sentences

> type GrammarTests token = [(String, TestCase token)]

> data TestCase token
>  = Parametric { remark :: String, generator :: Int -> [token] }
>  | Sentences  [[token]]

> getTest :: GrammarTests token -> String -> Int -> Maybe [token]
> getTest gts s i
>  = do
>       t <- lookup s gts
>       case t of 
>	  Parametric{} -> return $ generator t i
>	  Sentences ss -> lookup i $ zip [0..] ss


-- likewise, a naming scheme for grammar tests
-- grammar files should export one of these.

> type GrammarSpec token
>  = [(String, ([token] -> Int -> Doc, GrammarTests token))]
