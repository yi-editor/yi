module Yi.Keymap.Vim2.Ex.Commands.BufferDelete
    ( parse
    ) where

import Prelude ()
import Yi.Prelude

import qualified Text.ParserCombinators.Parsec as P

import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import qualified Yi.Keymap.Vim2.Ex.Commands.Common as Common

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    discard $ P.try ( P.string "bdelete") <|> P.try ( P.string "bdel") <|> P.try (P.string "bd")
    return $ Common.pureExCommand {
        cmdShow = "bdelete"
      , cmdAction = EditorA closeBufferAndWindowE
      }
