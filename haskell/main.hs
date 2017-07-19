module Main where

import System.Environment
import CPSMemo

str2gen sel = case sel of
                "s" -> s
                "sm" -> sm
                "sml" -> sml
                "smml" -> smml

main = do
  [sel,nstr] <- getArgs
  profile (str2gen sel) (read nstr)
