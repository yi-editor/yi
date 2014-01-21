module Yi.Keymap.Vim2.Ex.Commands.Cabal
    ( parse
    ) where

import Control.Applicative
import Control.Monad

import qualified Text.ParserCombinators.Parsec as P

import Yi.Command
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import qualified Yi.Keymap.Vim2.Ex.Commands.Common as Common
import Yi.MiniBuffer

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.try ( P.string "cabal build") <|> P.try ( P.string "cabal")
    args <- P.many (P.many1 P.space *> P.many1 P.anyChar)
    return $ Common.impureExCommand {
        cmdShow = "cabal build"
      , cmdAction = YiA $ cabalBuildE $ CommandArguments args
      }
