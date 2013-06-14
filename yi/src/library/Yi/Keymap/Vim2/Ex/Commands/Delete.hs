module Yi.Keymap.Vim2.Ex.Commands.Delete
    ( commands
    ) where

import Prelude ()
import Yi.Prelude

import qualified Text.ParserCombinators.Parsec as P

import Yi.Buffer hiding (Delete)
import Yi.Editor
import Yi.Keymap.Vim2.Ex.Types
import Yi.Keymap.Vim2.Ex.Commands.Common

commands :: [ExCommandBox]
commands = [pack Delete]

data Delete = Delete

instance Show Delete where
    show Delete = "delete"

instance ExCommand Delete where
    cmdIsPure _ = True
    cmdParse _ = parse $ do
        discard $ P.try ( P.string "delete") <|> P.string "d"
        return Delete
    cmdAction _ = Left . withBuffer0 $ do
        deleteUnitB Line Forward
        deleteN 1
