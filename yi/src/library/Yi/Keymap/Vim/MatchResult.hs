{-# LANGUAGE DeriveFunctor #-}
module Yi.Keymap.Vim.MatchResult where

import Control.Applicative
import Data.List (isPrefixOf)

data MatchResult a = NoMatch
                   | PartialMatch
                   | WholeMatch a
                   deriving Functor

-- like Data.List.lookup, but with MatchResult instead of Maybe
lookupBestMatch :: String -> [(String, a)] -> MatchResult a
lookupBestMatch key = foldl go NoMatch
    where go m (k, x) = m <|> fmap (const x) (key `matchesString` k)

matchesString :: String -> String -> MatchResult ()
matchesString got expected | expected == got = WholeMatch ()
                           | got `isPrefixOf` expected = PartialMatch
                           | otherwise = NoMatch

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
