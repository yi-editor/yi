{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.GotoLine
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Undo (parse) where

import           Data.Monoid
import           Yi.Buffer.Adjusted
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.Ex.Commands.Common (pureExCommand)
import           Yi.Keymap.Vim.Ex.Types

parse :: EventString -> Maybe ExCommand
parse (Ev s) | s `elem` ["u", "undo"] =  
         Just pureExCommand {
             cmdAction   = BufferA undoB
           , cmdShow     =         "undo"
           , cmdComplete = return ["undo"]
         }
parse _                               = Nothing 
