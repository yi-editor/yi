module Yi.Keymap.Vim.Ex.Commands.Reload
    ( parse
    ) where




import {-# source #-} Yi.Boot (reload)
import Yi.Keymap
import Yi.Keymap.Vim.Ex.Types
import Yi.Keymap.Vim.Ex.Commands.Common (impureExCommand)

parse :: String -> Maybe ExCommand
parse "reload" = Just $ impureExCommand {
    cmdShow = "reload"
  , cmdAction = YiA reload
  }
parse _ = Nothing
