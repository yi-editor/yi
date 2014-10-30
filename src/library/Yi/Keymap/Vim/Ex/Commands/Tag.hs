{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Tag
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Tag (parse) where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import qualified Data.Text as T
import qualified Text.ParserCombinators.Parsec as P
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import           Yi.Keymap.Vim.Ex.Types
import           Yi.Keymap.Vim.Tag
import           Yi.Tag

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
  , cmdComplete = (fmap . fmap) (mappend "tag ") $ completeVimTag t
  }

next :: ExCommand
next = Common.impureExCommand {
    cmdShow = "tnext"
  , cmdAction = YiA nextTag
  , cmdComplete = return ["tnext"]
  }
