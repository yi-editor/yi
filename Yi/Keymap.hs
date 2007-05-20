module Yi.Keymap where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.IORef
import Yi.Event
import qualified Yi.Interact as I
import {-# source #-} Yi.Editor

-- ---------------------------------------------------------------------
-- | The type of user-bindable functions
--

write :: Action -> Interact ev ()
write x = I.write (tell [x])

type EditorM = ReaderT (IORef Editor) IO

type Action = EditorM ()

type Interact ev a = I.Interact ev (Writer [Action]) a

type Keymap = Interact Event ()

type KeymapMod = Keymap -> Keymap

runKeymap :: Interact ev () -> [ev] -> [Action]
runKeymap p evs = snd $ runWriter (I.runProcess p evs)
