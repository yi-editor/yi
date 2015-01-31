{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Tag
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Tag (parse) where

import Control.Applicative ( Alternative((<|>)), (<$>) )
import Control.Monad ( void )
import Data.Monoid ( (<>) )
import qualified Data.Text as T ( pack )
import qualified Text.ParserCombinators.Parsec as P
    ( GenParser, optionMaybe, many1, eof, string, space, anyChar )
import Yi.Keymap ( Action(YiA) )
import Yi.Keymap.Vim.Common ( EventString )
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
    ( parse, impureExCommand )
import Yi.Keymap.Vim.Ex.Types
    ( ExCommand(cmdAction, cmdComplete, cmdShow) )
import Yi.Keymap.Vim.Tag
    ( gotoTag, nextTag, unpopTag, completeVimTag )
import Yi.Tag ( Tag(Tag) )

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.string "t"
    parseTag <|> parseNext

parseTag :: P.GenParser Char () ExCommand
parseTag = do
  void $ P.string "a"
  void . P.optionMaybe $ P.string "g"
  t <- P.optionMaybe $ do
      void $ P.many1 P.space
      P.many1 P.anyChar
  case t of
    Nothing -> P.eof >> return (tag Nothing)
    Just t' -> return $! tag (Just (Tag (T.pack t')))

parseNext :: P.GenParser Char () ExCommand
parseNext = do
  void $ P.string "next"
  return next

tag :: Maybe Tag -> ExCommand
tag Nothing = Common.impureExCommand {
    cmdShow = "tag"
  , cmdAction = YiA unpopTag
  , cmdComplete = return ["tag"]
  }
tag (Just (Tag t)) = Common.impureExCommand {
    cmdShow = "tag " <> t
  , cmdAction = YiA $ gotoTag (Tag t) 0 Nothing
  , cmdComplete = map ("tag " <>) <$> completeVimTag t
  }

next :: ExCommand
next = Common.impureExCommand {
    cmdShow = "tnext"
  , cmdAction = YiA nextTag
  , cmdComplete = return ["tnext"]
  }
