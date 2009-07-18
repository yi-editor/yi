-- Copyright (C) 2008 JP Bernardy
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons

-- | "Real" Frontend to the static binary. This is merely calling the driver (see
-- Yi.Boot ~> HConf)

module Main ( main ) where

import Yi

main :: IO ()
main = driver
