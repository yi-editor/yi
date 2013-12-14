module Yi.Keymap.Vim2.Ex.Commands.Nohl
    ( parse
    ) where

import Prelude ()
import Yi.Prelude

import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import Yi.Keymap.Vim2.Ex.Commands.Common (pureExCommand)
import Yi.Search

parse :: String -> Maybe ExCommand
parse s = if s == "nohl" || s == "nohlsearch"
    then Just nohl
    else Nothing

nohl :: ExCommand
nohl = pureExCommand {
    cmdAction = EditorA resetRegexE
  , cmdShow = "nohlsearch"
  }