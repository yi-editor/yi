{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Tag
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Tag (parse) where

import           Control.Applicative              (Alternative ((<|>)))
import           Control.Monad                    (void)
import qualified Data.Attoparsec.Text             as P (Parser, anyChar, endOfInput,
                                                        many1, option,
                                                        space, string)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T (pack)
import           Yi.Keymap                        (Action (YiA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (impureExCommand, parse)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdComplete, cmdShow))
import           Yi.Keymap.Vim.Tag                (completeVimTag, gotoTag, nextTag, unpopTag)
import           Yi.Tag                           (Tag (Tag))

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.string "t"
    parseTag <|> parseNext

parseTag :: P.Parser ExCommand
parseTag = do
  void $ P.string "a"
  void . P.option Nothing $ Just <$> P.string "g"
  t <- P.option Nothing $ Just <$> do
      void $ P.many1 P.space
      P.many1 P.anyChar
  case t of
    Nothing -> P.endOfInput >> return (tag Nothing)
    Just t' -> return $! tag (Just (Tag (T.pack t')))

parseNext :: P.Parser ExCommand
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
