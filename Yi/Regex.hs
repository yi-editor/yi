module Yi.Regex 
  (
   searchOpt,
   searchOpts,
   SearchF(..), makeSearchOptsM,
   SearchExp, searchString, searchForward,
   emptyRegex,
   module Text.Regex.TDFA,
   )
where

import Text.Regex.TDFA

type SearchExp = (String, Regex)


searchString = fst
searchForward = snd

--
-- What would be interesting would be to implement our own general
-- mechanism to allow users to supply a regex function of any kind, and
-- search with that. This removes the restriction on strings be valid
-- under regex(3).
--

data SearchF = IgnoreCase   -- ^ Compile for matching that ignores char case
             | NoNewLine    -- ^ Compile for newline-insensitive matching
    deriving Eq

searchOpt IgnoreCase = \o->o{caseSensitive = False}
searchOpt NoNewLine = \o->o{multiline = False}


searchOpts =  foldr (.) id . map searchOpt

makeSearchOptsM opts re = fmap (\r->(re,r)) $ makeRegexOptsM (searchOpts opts defaultCompOpt) defaultExecOpt re

emptyRegex :: Regex
Just emptyRegex = makeRegexOptsM defaultCompOpt defaultExecOpt "[]"


