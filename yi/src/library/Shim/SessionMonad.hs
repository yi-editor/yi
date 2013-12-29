{-# LANGUAGE CPP #-}
module Shim.SessionMonad where

import HscTypes (Session)

class Monad m => SessionMonad m where
  getSession :: m Session

