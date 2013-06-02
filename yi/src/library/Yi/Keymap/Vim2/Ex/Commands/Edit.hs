module Yi.Keymap.Vim2.Ex.Commands.Edit 
    ( commands
    ) where

import Prelude ()
import Yi.Prelude

import System.FilePath
import qualified Text.ParserCombinators.Parsec as P

import Yi.Editor
import Yi.File
import Yi.Keymap.Vim2.Ex.Types
import Yi.Keymap.Vim2.Ex.Commands.Common

commands :: [ExCommandBox]
commands = [pack $ Edit undefined undefined]

data Edit = Edit {
    _shouldOpenInNewTab :: !Bool
  , _filename :: !FilePath
}

instance Show Edit where
    show (Edit tab f) = (if tab then "tab" else "") ++ "edit " ++ f

instance ExCommand Edit where
    cmdIsPure _ = False
    cmdComplete (Edit tab f) = (fmap . fmap) (show . Edit tab)
                                (filenameComplete f)
    cmdParse _ = parse $ do
        tab <- P.many (P.string "tab")
        discard $ P.try ( P.string "edit") <|> P.string "e"
        discard $ P.many1 P.space
        filename <- P.many1 P.anyChar
        return $! Edit (not (null tab)) filename
    cmdAction (Edit tab f) = Right $ do
        when tab $ withEditor newTabE
        discard . editFile $ f