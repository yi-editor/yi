{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Reload
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Reload (parse) where

import Data.Text ()
import Yi.Boot.Internal (reload)
import Yi.Keymap
import Yi.Keymap.Vim.Common
import Yi.Keymap.Vim.Ex.Commands.Common (impureExCommand)
import Yi.Keymap.Vim.Ex.Types

parse :: EventString -> Maybe ExCommand
parse "reload" = Just $ impureExCommand {
    cmdShow = "reload"
  , cmdAction = YiA reload
  }
parse _ = Nothing
