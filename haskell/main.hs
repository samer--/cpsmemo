module Main where

import System.Environment
import CPSMemo

main = do
  [sel,nstr] <- getArgs
  profile sel (read nstr)
