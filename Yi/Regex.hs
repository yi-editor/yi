{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
-- Copyright (c) Jean-Philippe Bernardy 2008
module Yi.Regex 
  (
   SearchF(..), makeSearchOptsM,
   SearchExp(..), searchString, searchRegex, emptySearch,
   emptyRegex,
   module Text.Regex.TDFA,
   )
where

import Data.Generics.Uniplate
import Text.Regex.TDFA
import Text.Regex.TDFA.Pattern
import Text.Regex.TDFA.Common
import Control.Applicative
import Text.Regex.TDFA.ReadRegex(parseRegex)
import Text.Regex.TDFA.TDFA(patternToDFA)
import Yi.Buffer.Basic (Direction(..))

-- input string, regexexp, backward regex.
data SearchExp = SearchExp { seInput :: String, seCompiled :: Regex, seBackCompiled :: Regex}

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

data SearchF = IgnoreCase   -- ^ Compile for matching that ignores char case
             | NoNewLine    -- ^ Compile for newline-insensitive matching
             | QuoteRegex   -- ^ Treat the input not as a regex but as a literal string to search for.
    deriving Eq

searchOpt :: SearchF -> CompOption -> CompOption
searchOpt IgnoreCase = \o->o{caseSensitive = False}
searchOpt NoNewLine = \o->o{multiline = False}
searchOpt QuoteRegex = id

makeSearchOptsM :: [SearchF] -> String -> Either String SearchExp
makeSearchOptsM opts re = (\p->SearchExp re (compile p) (compile $ reversePattern p)) <$> pattern
    where searchOpts = foldr (.) id . map searchOpt
          compile = compilePattern (searchOpts opts defaultCompOpt) defaultExecOpt
          pattern = if QuoteRegex `elem` opts 
                          then Right (literalPattern re) 
                          else mapLeft show (parseRegex re)

mapLeft :: (t1 -> a) -> Either t1 t -> Either a t
mapLeft _ (Right a) = Right a
mapLeft f (Left a) = Left (f a)

-- | Return a pattern that matches its argument.
literalPattern :: (Num t) => String -> (Pattern, (t, DoPa))
literalPattern source = (PConcat $ map (PChar (DoPa 0)) $ source, (0,DoPa 0))

compilePattern  :: CompOption -- ^ Flags (summed together)
         -> ExecOption -- ^ Flags (summed together)
         -> (Pattern, (GroupIndex, DoPa))    -- ^ The pattern to compile
         -> Regex -- ^ Returns: the compiled regular expression
compilePattern compOpt execOpt source =
      let (dfa,i,tags,groups) = patternToDFA compOpt source
      in Regex dfa i tags groups compOpt execOpt


-- reversePattern :: (Pattern,(gi,DoPa max)) -> (Pattern,(gi,DoPa max))
reversePattern (pattern,(gi,DoPa maxDoPa)) = (transform (rev) pattern, (gi,DoPa maxDoPa))
    where rev (PConcat l) = PConcat (reverse l)
          rev (PCarat  x) = PDollar x
          rev (PDollar x) = PCarat  x
          rev x           = x
          fixDoPa (PCarat p) = PCarat (f p)
          fixDoPa (PDollar p) = PDollar (f p)
          fixDoPa (PAny p x) = PAny (f p) x
          fixDoPa (PDot p) = PDot (f p)
          fixDoPa (PAnyNot p x) = PAnyNot (f p) x
          fixDoPa (PEscape p x) = PEscape (f p) x
          fixDoPa (PChar p x) = PChar (f p) x
          fixDoPa x = x
          f (DoPa x) = DoPa (maxDoPa + 1 - x)

-- Cannot use Derive because we have to handle list arguments specially.
instance Uniplate Pattern where
    uniplate = \p -> 
      case p of
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
emptySearch = SearchExp "" emptyRegex emptyRegex


-- | The regular expression that matches nothing.
emptyRegex :: Regex
Just emptyRegex = makeRegexOptsM defaultCompOpt defaultExecOpt "[[:empty:]]"

