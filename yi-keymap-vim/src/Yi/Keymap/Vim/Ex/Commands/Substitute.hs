{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Substitute
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Substitute (parse) where

import           Control.Applicative              (Alternative)
import           Control.Monad                    (void)
import qualified Data.Attoparsec.Text             as P (char, inClass, many', match,
                                                        satisfy, string, option,
                                                        (<?>), Parser)
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T (Text, cons, snoc)
import           Lens.Micro.Platform              (over, _2)
import           Yi.Buffer
import           Yi.Keymap                        (Action (EditorA))
import           Yi.Keymap.Vim.Common             (EventString, Substitution(..))
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (parse, pureExCommand, parseRange)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))
import qualified Yi.Rope                          as R (fromString, toText)
import           Yi.Keymap.Vim.Substitution

-- | Skip one or no occurrences of a given parser.
skipOptional :: Alternative f => f a -> f ()
skipOptional p = P.option () (() <$ p)
{-# SPECIALIZE skipOptional :: P.Parser a -> P.Parser () #-}

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    (rangeText, rangeB) <- over _2 (fromMaybe $ regionOfB Line) <$> P.match Common.parseRange
    P.char 's' *> 
      skipOptional (P.string "ub" *> skipOptional (P.string "stitute"))
      P.<?> "substitute"
    delimiter <- P.satisfy (`elem` ("!@#$%^&*()[]{}<>/.,~';:?-=" :: String))
    from <- R.fromString <$> P.many' (P.satisfy (/= delimiter))
    void $ P.char delimiter
    to <- R.fromString <$> P.many' (P.satisfy (/= delimiter))
    flagChars <- P.option "" $
      P.char delimiter *> P.many' (P.satisfy $ P.inClass "gic")
    return $! substitute
        (Substitution
            from
            to
            ('g' `elem` flagChars)
            ('i' `elem` flagChars)
            ('c' `elem` flagChars))
        delimiter
        rangeText
        rangeB

substitute :: Substitution -> Char -> T.Text -> BufferM Region -> ExCommand
substitute s@(Substitution from to global caseInsensitive confirm) delimiter regionText regionB = Common.pureExCommand
  { cmdShow = regionText
              <>       "s"
              <>       (delimiter `T.cons` R.toText from)
              <>       (delimiter `T.cons` R.toText to)
              `T.snoc` delimiter
              <>       (if confirm then "c" else "")
              <>       (if caseInsensitive then "i" else "")
              <>       (if global then "g" else "")
  , cmdAction = EditorA $ substituteE s regionB
  }
