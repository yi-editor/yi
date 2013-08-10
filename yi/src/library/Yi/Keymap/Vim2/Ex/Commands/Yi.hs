module Yi.Keymap.Vim2.Ex.Commands.Yi
    ( parse
    ) where

import Prelude ()
import Yi.Prelude

import qualified Text.ParserCombinators.Parsec as P

import Yi.Eval (execEditorAction)
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import qualified Yi.Keymap.Vim2.Ex.Commands.Common as Common

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    discard $ P.string "yi"
    discard $ P.many1 P.space
    cmd <- P.many1 P.anyChar
    return $! Common.impureExCommand {
        cmdAction = YiA $ execEditorAction cmd
      , cmdShow = cmd
      }