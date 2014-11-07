{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Undo
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Undo (parse) where

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
parse (Ev s) | s `elem` ["redo"] =
         Just pureExCommand {
             cmdAction   = BufferA redoB
           , cmdShow     =         "redo"
           , cmdComplete = return ["redo"]
         }
parse _                               = Nothing 
