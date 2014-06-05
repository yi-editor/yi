module Yi.Keymap.Vim.Ex.Commands.Delete
    ( parse
    ) where

import Control.Monad
import Control.Applicative
import qualified Text.ParserCombinators.Parsec as P

import Yi.Buffer hiding (Delete)
import Yi.Keymap
import Yi.Keymap.Vim.Ex.Types
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.try ( P.string "delete") <|> P.string "d"
    return $ Common.pureExCommand {
        cmdShow = "delete"
      , cmdAction = BufferA $ do
            deleteUnitB Line Forward
            deleteN 1
      }
