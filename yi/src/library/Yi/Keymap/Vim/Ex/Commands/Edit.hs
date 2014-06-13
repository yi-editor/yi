module Yi.Keymap.Vim.Ex.Commands.Edit
    ( parse
    ) where

import Control.Monad
import Control.Applicative

import qualified Text.ParserCombinators.Parsec as P

import Yi.Editor
import Yi.File
import Yi.Keymap
import Yi.Keymap.Vim.Ex.Types
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    tab <- P.many (P.string "tab")
    void $ P.try ( P.string "edit") <|> P.string "e"
    void $ P.many1 P.space
    filename <- P.many1 P.anyChar
    return $! edit (not (null tab)) filename

edit :: Bool -> FilePath -> ExCommand
edit tab f = Common.impureExCommand {
    cmdShow = showEdit tab f
  , cmdAction = YiA $ do
        when tab $ withEditor newTabE
        void . editFile $ f
  , cmdComplete = (fmap . fmap) (showEdit tab) (Common.filenameComplete f)
  }

showEdit :: Bool -> FilePath -> String
showEdit tab f = (if tab then "tab" else "") ++ "edit " ++ f
