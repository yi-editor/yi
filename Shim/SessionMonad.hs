module Shim.SessionMonad where

#if __GLASGOW_HASKELL__ >= 610
import HscTypes (Session)
#else
import GHC (Session)
#endif

class Monad m => SessionMonad m where
  getSession :: m Session 

