-- -*- haskell -*-

{-# LANGUAGE FlexibleContexts #-}

module Yi.Editor where

import Control.Monad.State hiding (mapM_)
import {-# SOURCE #-} Yi.Config
import Yi.Monad (getsAndModify)

data Editor
data EditorM a

runEditor :: Config -> EditorM a -> Editor -> (Editor, a)

class (Monad m, MonadState Editor m) => MonadEditor m
    where askCfg :: m Config
          withEditor :: EditorM a -> m a
          withEditor f = do
              cfg <- askCfg
              getsAndModify (runEditor cfg f)
