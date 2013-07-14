module Yi.Keymap.Vim2.Ex.Commands.Delete
    ( parse
    ) where

import Prelude ()
import Yi.Prelude

import qualified Text.ParserCombinators.Parsec as P

import Yi.Buffer hiding (Delete)
import Yi.Editor
import Yi.Keymap.Vim2.Ex.Types
import qualified Yi.Keymap.Vim2.Ex.Commands.Common as Common

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    discard $ P.try ( P.string "delete") <|> P.string "d"
    return $ Common.pureExCommand {
        cmdShow = "delete"
      , cmdAction = Left . withBuffer0 $ do
            deleteUnitB Line Forward
            deleteN 1
      }