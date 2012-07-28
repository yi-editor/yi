{-# LANGUAGE FlexibleContexts, TemplateHaskell, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- uniplate uses incomplete patterns
-- Copyright (c) Jean-Philippe Bernardy 2008
module Yi.Regex 
  (
   SearchOption(..), makeSearchOptsM,
   SearchExp(..), searchString, searchRegex, emptySearch,
   emptyRegex,
   regexEscapeString,
   module Text.Regex.TDFA,
   )
where

import Data.Binary
import Data.DeriveTH
import Data.Generics.Uniplate
import Text.Regex.TDFA
import Text.Regex.TDFA.Pattern
import Text.Regex.TDFA.Common
import Control.Applicative
import Text.Regex.TDFA.ReadRegex(parseRegex)
import Text.Regex.TDFA.TDFA(patternToRegex)
import Yi.Buffer.Basic (Direction(..))

-- input string, regexexp, backward regex.
data SearchExp = SearchExp { seInput        :: String
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
    deriving Eq

$(derive makeBinary ''SearchOption)

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
    where searchOpts = foldr (.) id . map searchOpt
          compile source = patternToRegex source (searchOpts opts defaultCompOpt) defaultExecOpt
          pattern = if QuoteRegex `elem` opts 
                          then Right (literalPattern re) 
                          else mapLeft show (parseRegex re)

instance Binary SearchExp where
  get = do re   <- get
           opts <- get
           return $ case makeSearchOptsM opts re of
                      Left err -> error err
                      Right se -> se
  put (SearchExp { seInput   = re,
                   seOptions = opts, .. }) = do put re
                                                put opts

mapLeft :: (t1 -> a) -> Either t1 t -> Either a t
mapLeft _ (Right a) = Right a
mapLeft f (Left a) = Left (f a)

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
reversePattern (pattern,(gi,DoPa maxDoPa)) = (transform (rev) pattern, (gi,DoPa maxDoPa))
    where rev (PConcat l) = PConcat (reverse l)
          rev (PCarat  x) = PDollar x
          rev (PDollar x) = PCarat  x
          rev (PEscape {getDoPa = dp, getPatternChar = '<'}) = PEscape {getDoPa = dp, getPatternChar = '>'}
          rev (PEscape {getDoPa = dp, getPatternChar = '>'}) = PEscape {getDoPa = dp, getPatternChar = '<'}
          rev x           = x

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

-- Cannot use Derive because we have to handle list arguments specially (POr, PConcat)
instance Uniplate Pattern where
    uniplate = \pat -> 
      case pat of
          PGroup x p -> ([p], \[z] ->PGroup x z)
          POr ps -> (ps, POr)
          PConcat ps -> (ps, PConcat)
          PQuest p ->([p], \[z] -> PQuest z)
          PPlus p ->([p], \[z] -> PPlus z)
          PStar x p -> ([p], \[z] ->PStar x z)
          PBound w x p -> ([p], \[z] ->PBound w x z)
          PNonCapture p ->([p], \[z] -> PNonCapture z)
          PNonEmpty p ->([p], \[z] -> PNonEmpty z)
          p ->([],\[]->p)

emptySearch :: SearchExp
emptySearch = SearchExp "" emptyRegex emptyRegex []


-- | The regular expression that matches nothing.
emptyRegex :: Regex
Just emptyRegex = makeRegexOptsM defaultCompOpt defaultExecOpt "[[:empty:]]"

