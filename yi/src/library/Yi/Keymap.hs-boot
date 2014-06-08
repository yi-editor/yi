-- -*- haskell -*-

module Yi.Keymap where

import Control.Monad.Reader hiding (mapM_)

import qualified Yi.Interact as I
import Yi.Event
import {-# SOURCE #-} Yi.Editor (MonadEditor)
import {-# SOURCE #-} Yi.Buffer.Misc (BufferM)

data Action

instance Eq Action

emptyAction :: Action

type Interact ev a = I.I ev Action a

type KeymapM a = Interact Event a

type Keymap = KeymapM ()

type KeymapEndo = Keymap -> Keymap

type KeymapProcess = I.P Event Action

data KeymapSet

data Yi

newtype YiM a = YiM {runYiM :: ReaderT Yi IO a}

instance Monad YiM

instance MonadEditor YiM

withBuffer :: MonadEditor m => BufferM a -> m a


extractTopKeymap :: KeymapSet -> Keymap
