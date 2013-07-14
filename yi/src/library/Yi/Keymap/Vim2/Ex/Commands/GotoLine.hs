module Yi.Keymap.Vim2.Ex.Commands.GotoLine
    ( parse
    ) where

import Prelude ()
import Yi.Prelude

import Data.Char (isDigit)

import Yi.Buffer
import Yi.Editor
import Yi.Keymap.Vim2.Ex.Types
import Yi.Keymap.Vim2.Ex.Commands.Common (pureExCommand)

parse :: String -> Maybe ExCommand
parse s = if and (fmap isDigit s)
    then let l = read s in
         Just $ pureExCommand {
             cmdAction = Left . withBuffer0 $gotoLn l >> firstNonSpaceB
           , cmdShow = s
         }
    else Nothing