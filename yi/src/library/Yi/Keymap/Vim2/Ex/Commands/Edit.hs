module Yi.Keymap.Vim2.Ex.Commands.Edit
    ( parse
    ) where

import Prelude ()
import Yi.Prelude

import System.FilePath
import qualified Text.ParserCombinators.Parsec as P

import Yi.Editor
import Yi.File
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import qualified Yi.Keymap.Vim2.Ex.Commands.Common as Common

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    tab <- P.many (P.string "tab")
    discard $ P.try ( P.string "edit") <|> P.string "e"
    discard $ P.many1 P.space
    filename <- P.many1 P.anyChar
    return $! edit (not (null tab)) filename

edit :: Bool -> FilePath -> ExCommand
edit tab f = Common.impureExCommand {
    cmdShow = showEdit tab f
  , cmdAction = YiA $ do
        when tab $ withEditor newTabE
        discard . editFile $ f
  , cmdComplete = (fmap . fmap) (showEdit tab) (Common.filenameComplete f)
  }

showEdit :: Bool -> FilePath -> String
showEdit tab f = (if tab then "tab" else "") ++ "edit " ++ f
