module Yi.Keymap.Vim2.Ex.Commands.Write
    ( commands
    ) where

import Prelude ()
import Yi.Prelude

import System.FilePath

import qualified Text.ParserCombinators.Parsec as P

import Yi.File
import Yi.Keymap.Vim2.Ex.Types
import Yi.Keymap.Vim2.Ex.Commands.Common

commands :: [ExCommandBox]
commands = [ pack $ WriteAs undefined
           , pack $ Write undefined
           ]

data WriteAs = WriteAs FilePath

data Write = Write {
    _all :: !Bool
}

instance Show WriteAs where
    show (WriteAs f) = "write " ++ f

instance Show Write where
    show (Write a) = "write" ++ (if a then "all" else "")

instance ExCommand Write where
    cmdParse _ = parse $ do
        discard $ P.try ( P.string "write") <|> P.string "w"
        alls <- P.many (P.try ( P.string "all") <|> P.string "a")
        return $! Write (not (null alls))
    cmdAction (Write False) = Right viWrite
    cmdAction (Write True) = Right $ forAllBuffers fwriteBufferE

instance ExCommand WriteAs where
    cmdParse _ = parse $ do
        discard $ P.try ( P.string "write") <|> P.string "w"
        discard $ P.many1 P.space
        filename <- P.many1 P.anyChar
        return $! WriteAs filename
    cmdAction (WriteAs f) = Right $ viWriteTo f
