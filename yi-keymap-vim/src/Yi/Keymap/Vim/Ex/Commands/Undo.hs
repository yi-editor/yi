{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Undo
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Undo (parse) where

import           Yi.Buffer.Adjusted               (redoB, undoB)
import           Yi.Keymap                        (Action (BufferA))
import           Yi.Keymap.Vim.Common             (EventString (Ev))
import           Yi.Keymap.Vim.Ex.Commands.Common (pureExCommand)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdComplete, cmdShow))

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
