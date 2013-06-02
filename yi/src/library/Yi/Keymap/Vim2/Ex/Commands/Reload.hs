module Yi.Keymap.Vim2.Ex.Commands.Reload
    ( commands
    ) where

import Prelude ()
import Yi.Prelude

import {-# source #-} Yi.Boot (reload)
import Yi.Keymap.Vim2.Ex.Types

commands :: [ExCommandBox]
commands = [pack Reload]

data Reload = Reload

instance Show Reload where
    show _ = "reload"

instance ExCommand Reload where
    cmdIsPure _ = False
    cmdComplete _ = return Nothing
    cmdParse _ s = if s == "reload" then Just Reload else Nothing
    cmdAction _ = Right reload