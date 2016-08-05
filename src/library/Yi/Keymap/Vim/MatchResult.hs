{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.MatchResult
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.MatchResult where

import           Control.Applicative (Alternative ((<|>), empty))

data MatchResult a = NoMatch
                   | PartialMatch
                   | WholeMatch a
                   deriving Functor

instance Applicative MatchResult where
    pure = WholeMatch
    WholeMatch f <*> WholeMatch x = WholeMatch (f x)
    _ <*> _ = NoMatch

instance Alternative MatchResult where
    empty = NoMatch
    WholeMatch x <|> _ = WholeMatch x
    _ <|> WholeMatch x = WholeMatch x
    PartialMatch <|> _ = PartialMatch
    _ <|> PartialMatch = PartialMatch
    _ <|> _ = NoMatch

instance Show (MatchResult a) where
    show (WholeMatch _) = "WholeMatch"
    show PartialMatch = "PartialMatch"
    show NoMatch = "NoMatch"

matchFromBool :: Bool -> MatchResult ()
matchFromBool b = if b then WholeMatch () else NoMatch

matchFromMaybe :: Maybe a -> MatchResult a
matchFromMaybe Nothing = NoMatch
matchFromMaybe (Just a) = WholeMatch a