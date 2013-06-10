module Yi.Keymap.Vim2.Ex.Commands.GotoLine
    ( commands
    ) where

import Prelude ()
import Yi.Prelude

import Data.Char (isDigit)

import Yi.Buffer hiding (Delete)
import Yi.Editor
import Yi.Keymap.Vim2.Ex.Types

commands :: [ExCommandBox]
commands = [pack $ GotoLine undefined]

data GotoLine = GotoLine Int

instance Show GotoLine where
    show (GotoLine l) = show l

instance ExCommand GotoLine where
    cmdIsPure _ = True
    cmdComplete _ = return Nothing
    cmdParse _ s = if and (fmap isDigit s) then Just (GotoLine (read s)) else Nothing
    cmdAction (GotoLine l) = Left . withBuffer0 $gotoLn l >> firstNonSpaceB