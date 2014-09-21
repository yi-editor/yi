-- -*- haskell -*-

{-# LANGUAGE FlexibleContexts #-}

module Yi.Editor where

import Control.Monad.Reader hiding (mapM, forM_ )
import Control.Monad.State hiding (mapM_)
import {-# SOURCE #-} Yi.Config
import Yi.Monad (getsAndModify)

data Editor
newtype EditorM a = EditorM {fromEditorM :: ReaderT Config (State Editor) a}

runEditor :: Config -> EditorM a -> Editor -> (Editor, a)

class (Monad m, MonadState Editor m) => MonadEditor m where
  askCfg :: m Config

  withEditor :: EditorM a -> m a
  withEditor f = do
    cfg <- askCfg
    getsAndModify (runEditor cfg f)

  withEditor_ :: EditorM a -> m ()
  withEditor_ = withEditor . void
