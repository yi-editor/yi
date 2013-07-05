module Yi.Keymap.Vim2.Ex.Commands.Yi
    ( commands
    ) where

import Prelude ()
import Yi.Prelude

import qualified Text.ParserCombinators.Parsec as P

import Yi.Eval (execEditorAction)
import Yi.Keymap.Vim2.Ex.Types
import Yi.Keymap.Vim2.Ex.Commands.Common

commands :: [ExCommandBox]
commands = [pack $ Eval undefined]

data Eval = Eval {
    _cmd :: !String
}

instance Show Eval where
    show (Eval cmd) = "yi " ++ cmd

instance ExCommand Eval where
    cmdParse _ = parse $ do
        discard $ P.string "yi"
        discard $ P.many1 P.space
        cmd <- P.many1 P.anyChar
        return $! Eval cmd
    cmdAction (Eval cmd) = Right $ execEditorAction cmd