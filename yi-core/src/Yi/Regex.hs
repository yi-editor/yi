{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

-- Copyright (c) Jean-Philippe Bernardy 2008
module Yi.Regex
  ( SearchOption(..), makeSearchOptsM
  , SearchExp(..), searchString, searchRegex, emptySearch
  , emptyRegex
  , regexEscapeString
  , reversePattern
  , module Text.Regex.TDFA
  ) where

import Data.Bifunctor (first)
import Data.Binary
import GHC.Generics (Generic)
import Yi.Buffer.Basic (Direction(..))

import Text.Regex.TDFA ( Regex, CompOption(..), caseSensitive, multiline
                       , defaultCompOpt, defaultExecOpt, makeRegexOptsM
                       , matchOnceText, makeRegex, RegexLike(matchAll)
                       , AllTextSubmatches(..), (=~))
import Text.Regex.TDFA.Pattern (Pattern(..), DoPa(..), showPattern)
import Text.Regex.TDFA.ReadRegex(parseRegex)
import Text.Regex.TDFA.TDFA(patternToRegex)

-- input string, regexexp, backward regex.
data SearchExp = SearchExp
    { seInput        :: String
    , seCompiled     :: Regex
    , seBackCompiled :: Regex
    , seOptions      :: [SearchOption]
    }

searchString :: SearchExp -> String
searchString = seInput

searchRegex :: Direction -> SearchExp -> Regex
searchRegex Forward = seCompiled
searchRegex Backward = seBackCompiled

--
-- What would be interesting would be to implement our own general
-- mechanism to allow users to supply a regex function of any kind, and
-- search with that. This removes the restriction on strings be valid
-- under regex(3).
--

data SearchOption
    = IgnoreCase   -- ^ Compile for matching that ignores char case
    | NoNewLine    -- ^ Compile for newline-insensitive matching
    | QuoteRegex   -- ^ Treat the input not as a regex but as a literal string to search for.
    deriving (Eq, Generic)

instance Binary SearchOption

searchOpt :: SearchOption -> CompOption -> CompOption
searchOpt IgnoreCase = \o->o{caseSensitive = False}
searchOpt NoNewLine = \o->o{multiline = False}
searchOpt QuoteRegex = id

makeSearchOptsM :: [SearchOption] -> String -> Either String SearchExp
makeSearchOptsM opts re = (\p->SearchExp { seInput        = re
                                         , seCompiled     = compile p
                                         , seBackCompiled = compile $ reversePattern p
                                         , seOptions      = opts
                                         }) <$> pattern
    where searchOpts = foldr ((.) . searchOpt) id
          compile source = patternToRegex source (searchOpts opts defaultCompOpt) defaultExecOpt
          pattern = if QuoteRegex `elem` opts
                          then Right (literalPattern re)
                          else first show (parseRegex re)

instance Binary SearchExp where
  get = do re   <- get
           opts <- get
           return $ case makeSearchOptsM opts re of
                      Left err -> error err
                      Right se -> se
  put (SearchExp { seInput   = re,
                   seOptions = opts, .. }) = do put re
                                                put opts

-- | Return an escaped (for parseRegex use) version of the string.
regexEscapeString :: String -> String
regexEscapeString source = showPattern . literalPattern' $ source

-- | Return a pattern that matches its argument.
literalPattern :: (Num t) => String -> (Pattern, (t, DoPa))
literalPattern source = (literalPattern' source, (0,DoPa 0))

literalPattern' :: String -> Pattern
literalPattern' = PConcat . map (PChar (DoPa 0))

-- | Reverse a pattern. Note that the submatches will be reversed as well.
reversePattern :: (Pattern, (t, DoPa)) -> (Pattern, (t, DoPa))
reversePattern (pattern,rest) = (rev pattern, rest)
    where rev (PConcat l)      = PConcat (reverse (map rev l))
          rev (PCarat  dp)     = PDollar dp
          rev (PDollar dp)     = PCarat  dp
          rev (PEscape dp '<') = PEscape dp '>'
          rev (PEscape dp '>') = PEscape dp '<'
          rev (PGroup a x)     = PGroup a (rev x)
          rev (POr l)          = POr (map rev l)
          rev (PQuest x)       = PQuest (rev x)
          rev (PPlus x)        = PPlus (rev x)
          rev (PStar b x)      = PStar b (rev x)
          rev (PBound i m x)   = PBound i m (rev x)
          rev (PNonCapture x)  = PNonCapture (rev x)
          rev (PNonEmpty x)    = PNonEmpty (rev x)
          rev x = x

{-
Chris K Commentary:

I have one DIRE WARNING and one suggestion.

The DIRE WARNING is against using the reversed Pattern to find captured subexpressions.
It will work perfectly to find the longest match but give nonsense for captures.  In
particular matching text "abc" with "(.)*" forward returns the 1st capture as "c".
Searching "cba" with the reverse of "(.)*", which is identical, returns the 1st capture as "a".

Enough changes to the matching engine could allow for the reversed search on the
reversed text to return the same captures as the the forward search on the forward
text.  Rather than that tricky complexity, if you need the substring captures you
can use the reversed pattern to find a whole match and then run the forward pattern
on that substring.

The one suggestion is that the DoPa are irrelevant to the matching — they are there to
allow a person to understand how the output of each stage of the regex-tdfa code relates
to the input pattern.

-}

emptySearch :: SearchExp
emptySearch = SearchExp "" emptyRegex emptyRegex []


-- | The regular expression that matches nothing.
emptyRegex :: Regex
Just emptyRegex = makeRegexOptsM defaultCompOpt defaultExecOpt "[[:empty:]]"
