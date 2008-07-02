-- -*- haskell -*-

module Yi.Keymap where

import qualified Yi.Interact as I
import Yi.Event
import Control.Monad.Reader
import Yi.Monad
import Data.Typeable

data Action

type Interact ev a = I.I ev Action a

type KeymapM a = Interact Event a

type Keymap = KeymapM ()

type KeymapEndo = Keymap -> Keymap

type KeymapProcess = I.P Event Action

