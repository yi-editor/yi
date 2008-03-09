module Shim.SessionMonad where

import GHC(Session)

class Monad m => SessionMonad m where
  getSession :: m Session 
