module Yi.Keymap.Vim2.Ex.Commands.Nohl
    ( commands
    ) where

import Prelude ()
import Yi.Prelude

import Yi.Keymap.Vim2.Ex.Types
import Yi.Search

commands :: [ExCommandBox]
commands = [pack Nohl]

data Nohl = Nohl

instance Show Nohl where
    show _ = "nohlsearch"

instance ExCommand Nohl where
    cmdIsPure _ = True
    cmdComplete _ = return $! Just "nohlsearch"
    cmdParse _ s = if s == "nohl" || s == "nohlsearch"
                   then Just Nohl
                   else Nothing
    cmdAction _ = Left resetRegexE