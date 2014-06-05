module Yi.Keymap.Vim.Ex.Commands.Nohl
    ( parse
    ) where

import Yi.Keymap
import Yi.Keymap.Vim.Ex.Types
import Yi.Keymap.Vim.Ex.Commands.Common (pureExCommand)
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
