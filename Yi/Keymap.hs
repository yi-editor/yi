module Yi.Keymap where

import Control.Monad.Reader
import Data.IORef
import Yi.Event
import qualified Yi.Interact as I
import {-# source #-} Yi.Editor

-- | The type of user-bindable functions
type EditorM = ReaderT (IORef Editor) IO

type Action = EditorM ()

type Interact ev a = I.Interact ev EditorM a

type Keymap = Interact Event ()

type KeymapMod = Keymap -> Keymap

runKeymap :: Interact ev a -> [ev] -> EditorM a
runKeymap  = I.runProcess



