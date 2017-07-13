> module PP (module PP, text, (<>)) where

> import Data.IntMap (toAscList, IntMap)
> import Text.PrettyPrint.HughesPJ 

> po :: (PP a) => (String -> IO ()) -> a -> IO ()
> po act val = act $ render80 $ pp val $$ empty
> ps :: (PP a) => a -> IO ()
> ps = po putStrLn
> render80 = renderStyle (style{lineLength = 82})	-- tweaked, more space?


> class PP a where
>       pp :: a -> Doc

> instance PP Doc where
>       pp c = c

> instance PP Char where
>       pp c = text $ show c

<> instance PP String where
<>       pp c = text $ c

> instance PP Int where
>       pp i = text $ let s = show i in replicate (3 - length s) ' ' ++ s

> instance PP Integer where
>       pp i = text $ show i 

> instance PP a => PP (Maybe a) where
>       pp Nothing  = text "Nothing"
>       pp (Just x) = parens $ text "Just" <+> pp x

> instance (PP a, PP b) => PP (a,b) where
>       -- pp (a,b) = parens $ pp a <+> text "->" <+> pp b
>       pp (a,b) = pp a <+> text "->" <+> pp b

> instance PP a => PP [a] where
>	pp = pp_list sep 

> pp_list cat_or_sep []     = brackets $ text ""
> pp_list cat_or_sep (x:xs) = cat_or_sep $ (char '[' <+> pp x)
>                                        : [ comma <+> pp y | y <- xs ]
>                                        ++ [char ']']


> instance PP a => PP (IntMap a) where
>	pp = pp_list cat . toAscList
