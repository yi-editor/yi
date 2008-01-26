-- Copyright (C) 2004, 2008 Don Stewart - http://www.cse.unsw.edu.au/~dons

--
-- | Frontend to the static binary. We have a separte frontend (rather
-- than putting main in Yi.hs) so we don't get ZCMain_main_* symbols
-- in -package yi, which lets us have multiple frontends, and load
-- them all in ghci.
--
module Main ( main ) where

import qualified Yi.Main as Yi

import Yi.Debug
import Yi.Kernel
#ifdef DYNAMIC
import Yi.Boot

{-
main :: IO ()
main = do
  initDebug ".yi-static.dbg"
  kernel <- initialize
  Yi.Boot.startYi kernel -- call Yi.main dynamically

-- TODO: also init the debug system

-}

import Control.Monad

main :: IO ()
main = do
  initDebug ".yi-static.dbg"
  kernel <- initialize

  -- Setup the debug module in the dynamic session (this needs to be done only because
  -- debug uses "global" variables.
  debugMod <- guessTarget kernel "Yi.Debug" Nothing
  setTargets kernel [debugMod]
  loadAllTargets kernel
  initDebug' <- join $ evalMono kernel "Yi.Debug.initDebug \".yi.dbg\""
  initDebug' :: IO ()

  -- Fire up Yi
  Yi.main kernel

#else
main :: IO ()
main = do
  initDebug ".yi-static.dbg"
  Yi.main Kernel
#endif
