module Yi.Keymap.Vim.Ex.Commands.Yi
    ( parse
    ) where

import Control.Monad

import qualified Text.ParserCombinators.Parsec as P

import Yi.Eval (execEditorAction)
import Yi.Keymap
import Yi.Keymap.Vim.Ex.Types
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.string "yi"
    void $ P.many1 P.space
    cmd <- P.many1 P.anyChar
    return $! Common.impureExCommand {
        cmdAction = YiA $ execEditorAction cmd
      , cmdShow = cmd
      }
