{-# LANGUAGE RankNTypes #-}
module Main where

import System.Environment
import System.TimeIt
import CPSMemo

str2gen sel = case sel of
                "s" -> s
                "sm" -> sm
                "sml" -> sml
                "smml" -> smml

main = do
  [sel,nstr] <- getArgs
  profile (str2gen sel) (read nstr)
  -- (n:_) <- getArgs
  -- timeIt $ putStrLn (ffib (read n)>>= show)

profile :: (forall s. ParserGen s [] [Char] Char) -> Int -> IO ()
profile gen n = do
  (t,_) <- timeItT $ putStrLn $ show $ length $ parse gen $ replicate n 'a'
  putStrLn $ "Time: " ++ (show t)
