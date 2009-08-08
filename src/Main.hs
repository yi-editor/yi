-- Copyright (C) 2008 JP Bernardy
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons

-- | "Real" Frontend to the static binary.

module Main (main) where

import Yi.Boot (yiDriver)
import Yi.Config.Default (defaultConfig)

main :: IO ()
main = yiDriver defaultConfig

