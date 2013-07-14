module Yi.Keymap.Vim2.Ex.Commands.Reload
    ( parse
    ) where

import Prelude ()
import Yi.Prelude

import {-# source #-} Yi.Boot (reload)
import Yi.Keymap.Vim2.Ex.Types
import Yi.Keymap.Vim2.Ex.Commands.Common (impureExCommand)

parse :: String -> Maybe ExCommand
parse "reload" = Just $ impureExCommand {
    cmdShow = "reload"
  , cmdAction = Right reload
  }
parse _ = Nothing