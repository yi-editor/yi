module Yi.Keymap.Vim.Ex.Commands.Tag
    ( parse
    ) where

import Control.Monad

import qualified Text.ParserCombinators.Parsec as P

import Yi.Keymap
import Yi.Keymap.Vim.Tag
import Yi.Keymap.Vim.Ex.Types
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import Yi.Tag

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.string "ta"
    void . P.optionMaybe $ P.string "g"
    t <- P.optionMaybe $ do
        void $ P.many1 P.space
        P.many1 P.anyChar
    case t of
        Nothing -> P.eof >> return (tag Nothing)
        t -> return $! tag t

tag :: Maybe Tag -> ExCommand
tag Nothing = Common.impureExCommand {
    cmdShow = "tag"
  , cmdAction = YiA $ unpopTag
  , cmdComplete = return ["tag"]
  }
tag (Just t) = Common.impureExCommand {
    cmdShow = "tag " ++ t
  , cmdAction = YiA $ gotoTag t
  , cmdComplete = (fmap . fmap) ("tag "++) $ completeVimTag t
  }
