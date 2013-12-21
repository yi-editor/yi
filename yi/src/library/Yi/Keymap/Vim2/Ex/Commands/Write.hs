module Yi.Keymap.Vim2.Ex.Commands.Write
    ( parse
    ) where

import Prelude ()
import Yi.Prelude

import System.FilePath

import qualified Text.ParserCombinators.Parsec as P

import Yi.File
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import qualified Yi.Keymap.Vim2.Ex.Commands.Common as Common

parse :: String -> Maybe ExCommand
parse = Common.parse $ P.choice [parseWrite, parseWriteAs]
    where parseWrite = do
            discard $ P.try ( P.string "write") <|> P.string "w"
            alls <- P.many (P.try ( P.string "all") <|> P.string "a")
            return $! writeCmd $ not (null alls)

          parseWriteAs = do
            discard $ P.try ( P.string "write") <|> P.string "w"
            discard $ P.many1 P.space
            filename <- P.many1 P.anyChar
            return $! writeAsCmd filename

writeCmd :: Bool -> ExCommand
writeCmd allFlag = Common.impureExCommand {
    cmdShow = "write" ++ (if allFlag then "all" else "")
  , cmdAction = YiA $ if allFlag then Common.forAllBuffers fwriteBufferE else viWrite
  }

writeAsCmd :: FilePath -> ExCommand
writeAsCmd filename = Common.impureExCommand {
    cmdShow = "write " ++ filename
  , cmdAction = YiA $ viWriteTo filename
  }
