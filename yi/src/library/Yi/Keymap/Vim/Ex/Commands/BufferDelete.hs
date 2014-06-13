module Yi.Keymap.Vim.Ex.Commands.BufferDelete
    ( parse
    ) where

import Control.Monad
import Control.Applicative
import qualified Text.ParserCombinators.Parsec as P

import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Vim.Ex.Types
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.try ( P.string "bdelete") <|> P.try ( P.string "bdel") <|> P.try (P.string "bd")
    return $ Common.pureExCommand {
        cmdShow = "bdelete"
      , cmdAction = EditorA closeBufferAndWindowE
      }
