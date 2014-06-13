module Yi.Keymap.Vim.Ex.Commands.GotoLine
    ( parse
    ) where

import Data.Char (isDigit)

import Yi.Buffer
import Yi.Keymap
import Yi.Keymap.Vim.Ex.Types
import Yi.Keymap.Vim.Ex.Commands.Common (pureExCommand)

parse :: String -> Maybe ExCommand
parse s = if and $ not (null s): fmap isDigit s
    then let l = read s in
         Just $ pureExCommand {
             cmdAction = BufferA $ gotoLn l >> firstNonSpaceB
           , cmdShow = s
         }
    else Nothing
