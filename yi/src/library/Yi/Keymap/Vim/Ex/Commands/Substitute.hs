{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Substitute
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Substitute (parse) where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import qualified Data.Text as T
import qualified Text.ParserCombinators.Parsec as P
import           Yi.Buffer hiding (Delete)
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import           Yi.Keymap.Vim.Ex.Types
import qualified Yi.Rope as R
import           Yi.Search

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    percents <- P.many (P.char '%')
    void $ P.try (P.string "substitute") <|> P.string "s"
    delimiter <- P.oneOf "!@#$%^&*()[]{}<>/.,~';:?-="
    from <- R.fromString <$> P.many (P.noneOf [delimiter])
    void $ P.char delimiter
    to <- R.fromString <$> P.many (P.noneOf [delimiter])
    void $ P.char delimiter
    flagChars <- P.many (P.oneOf "gi")
    return $! substitute from to delimiter
        ('g' `elem` flagChars)
        ('i' `elem` flagChars)
        (not $ null percents)

substitute :: R.YiString -> R.YiString -> Char -> Bool -> Bool -> Bool -> ExCommand
substitute from to delimiter global caseInsensitive allLines = Common.pureExCommand {
    cmdShow = (if allLines then "%" else "")
              <>       "substitute"
              <>       (delimiter `T.cons` R.toText from)
              <>       (delimiter `T.cons` R.toText to)
              `T.snoc` delimiter
              <>       (if caseInsensitive then "i" else "")
              <>       (if global then "g" else "")
  , cmdAction = BufferA $ do
        let regex = makeSimpleSearch from
            replace = do
                region <- regionOfB Line
                void $ searchAndRepRegion0 regex to global region

        if allLines
        then withEveryLineB replace
        else replace

        moveToSol
  }
