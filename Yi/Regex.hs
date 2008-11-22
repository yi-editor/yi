{-# LANGUAGE FlexibleContexts #-}
-- Copyright (c) Jean-Philippe Bernardy 2008
module Yi.Regex 
  (
   SearchF(..), makeSearchOptsM,
   SearchExp, searchString, searchRegex, emptySearch,
   emptyRegex,
   module Text.Regex.TDFA,
   )
where

import Text.Regex.TDFA
import Text.Regex.TDFA.Pattern
import Text.Regex.TDFA.Common
import Control.Monad
import Control.Applicative
import Text.Regex.TDFA.ReadRegex(parseRegex)
import Text.Regex.TDFA.TDFA(patternToDFA)

type SearchExp = (String, Regex)

searchString :: SearchExp -> String
searchString = fst

searchRegex :: SearchExp -> Regex
searchRegex = snd

--
-- What would be interesting would be to implement our own general
-- mechanism to allow users to supply a regex function of any kind, and
-- search with that. This removes the restriction on strings be valid
-- under regex(3).
--

data SearchF = IgnoreCase   -- ^ Compile for matching that ignores char case
             | NoNewLine    -- ^ Compile for newline-insensitive matching
             | QuoteRegex   -- ^ Treat the input not as a regex but as a literal string to search for.
    deriving Eq

searchOpt :: SearchF -> CompOption -> CompOption
searchOpt IgnoreCase = \o->o{caseSensitive = False}
searchOpt NoNewLine = \o->o{multiline = False}
searchOpt QuoteRegex = id

makeSearchOptsM' :: (Functor m, Monad m, RegexMaker Regex CompOption ExecOption source) => [SearchF] -> source -> m (source, Regex)
makeSearchOptsM' opts re = (\r->(re,r)) <$> makeRegexOptsM (searchOpts opts defaultCompOpt) defaultExecOpt re
    where searchOpts = foldr (.) id . map searchOpt

makeSearchOptsM :: [SearchF] -> String -> Either String (String, Regex)
makeSearchOptsM opts re = (\r->(re,r)) <$> compilePattern (searchOpts opts defaultCompOpt) defaultExecOpt <$> pattern
    where searchOpts = foldr (.) id . map searchOpt
          pattern = if QuoteRegex `elem` opts 
                          then Right (literalPattern re) 
                          else mapLeft show (parseRegex re)


mapLeft f (Right a) = Right a
mapLeft f (Left a) = Left (f a)

-- | Return a pattern that matches its argument.
literalPattern source = (PConcat $ map (PChar (DoPa 0)) $ source, (0,DoPa 0))

compilePattern  :: CompOption -- ^ Flags (summed together)
         -> ExecOption -- ^ Flags (summed together)
         -> (Pattern, (GroupIndex, DoPa))    -- ^ The pattern to compile
         -> Regex -- ^ Returns: the compiled regular expression
compilePattern compOpt execOpt source =
      let (dfa,i,tags,groups) = patternToDFA compOpt source
      in Regex dfa i tags groups compOpt execOpt


emptySearch :: SearchExp
emptySearch = ("", emptyRegex)


-- | The regular expression that matches nothing.
emptyRegex :: Regex
Just emptyRegex = makeRegexOptsM defaultCompOpt defaultExecOpt "[[:empty:]]"


