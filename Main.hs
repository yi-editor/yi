-- Copyright (C) 2004, 2008 Don Stewart - http://www.cse.unsw.edu.au/~dons

--
-- | Frontend to the static binary. We have a separte frontend (rather
-- than putting main in Yi.hs) so we don't get ZCMain_main_* symbols
-- in -package yi, which lets us have multiple frontends, and load
-- them all in ghci.
--
module Main ( main ) where

import Yi

main = driver
