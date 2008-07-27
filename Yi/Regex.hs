module Yi.Regex 
  (
   SearchF(..), makeSearchOptsM,
   SearchExp, searchString, searchRegex, emptySearch,
   emptyRegex,
   module Text.Regex.TDFA,
   )
where

import Text.Regex.TDFA
import Control.Monad

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
    deriving Eq

searchOpt IgnoreCase = \o->o{caseSensitive = False}
searchOpt NoNewLine = \o->o{multiline = False}


makeSearchOptsM :: (Monad m, RegexMaker Regex CompOption ExecOption source) => [SearchF] -> source -> m (source, Regex)
makeSearchOptsM opts re = liftM (\r->(re,r)) $ makeRegexOptsM (searchOpts opts defaultCompOpt) defaultExecOpt re
    where searchOpts =  foldr (.) id . map searchOpt

emptySearch :: SearchExp
emptySearch = ("", emptyRegex)

-- | The regular expression that matches nothing.
emptyRegex :: Regex
Just emptyRegex = makeRegexOptsM defaultCompOpt defaultExecOpt "[[:empty:]]"


