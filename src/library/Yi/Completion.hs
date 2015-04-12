{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Completion
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Collection of functions for completion and matching.

module Yi.Completion
  ( completeInList, completeInList'
  , completeInListCustomShow
  , commonPrefix
  , prefixMatch, infixMatch
  , subsequenceMatch
  , containsMatch', containsMatch, containsMatchCaseInsensitive
  , mkIsPrefixOf
  )
where

import           Control.Applicative ((<$>))
import           Data.Function       (on)
import           Data.List           (find, nub)
import           Data.Maybe          (catMaybes)
import           Data.Monoid         ((<>))
import qualified Data.Text           as T (Text, breakOn, isPrefixOf, length, null, tails, toCaseFold)
import           Yi.Editor           (EditorM, printMsg, printMsgs)
import           Yi.String           (commonTPrefix', showT)
import           Yi.Utils            (commonPrefix)


-------------------------------------------
-- General completion

-- | Like usual 'T.isPrefixOf' but user can specify case sensitivity.
-- See 'T.toCaseFold' for exotic unicode gotchas.
mkIsPrefixOf :: Bool -- ^ Is case-sensitive?
             -> T.Text
             -> T.Text
             -> Bool
mkIsPrefixOf True = T.isPrefixOf
mkIsPrefixOf False = T.isPrefixOf `on` T.toCaseFold

-- | Prefix matching function, for use with 'completeInList'
prefixMatch :: T.Text -> T.Text -> Maybe T.Text
prefixMatch prefix s = if prefix `T.isPrefixOf` s then Just s else Nothing

-- | Infix matching function, for use with 'completeInList'
infixMatch :: T.Text -> T.Text -> Maybe T.Text
infixMatch needle haystack = case T.breakOn needle haystack of
  (_, t) -> if T.null t then Nothing else Just t

-- | Example: "abc" matches "a1b2c"
subsequenceMatch :: String -> String -> Bool
subsequenceMatch needle haystack = go needle haystack
  where go (n:ns) (h:hs) | n == h = go ns hs
        go (n:ns) (h:hs) | n /= h = go (n:ns) hs
        go [] _ = True
        go _ [] = False
        go _ _  = False

-- | TODO: this is a terrible function, isn't this just
-- case-insensitive infix? – Fūzetsu
containsMatch' :: Bool -> T.Text -> T.Text -> Maybe T.Text
containsMatch' caseSensitive pattern str =
  const str <$> find (pattern `tstPrefix`) (T.tails str)
  where
    tstPrefix = mkIsPrefixOf caseSensitive

containsMatch :: T.Text -> T.Text -> Maybe T.Text
containsMatch = containsMatch' True

containsMatchCaseInsensitive :: T.Text -> T.Text -> Maybe T.Text
containsMatchCaseInsensitive = containsMatch' False


-- | Complete a string given a user input string, a matching function
-- and a list of possibilites. Matching function should return the
-- part of the string that matches the user string.
completeInList :: T.Text -- ^ Input to match on
               -> (T.Text -> Maybe T.Text) -- ^ matcher function
               -> [T.Text] -- ^ items to match against
               -> EditorM T.Text
completeInList = completeInListCustomShow id

-- | Same as 'completeInList', but maps @showFunction@ on possible
-- matches when printing
completeInListCustomShow :: (T.Text -> T.Text) -- ^ Show function
                         -> T.Text -- ^ Input to match on
                         -> (T.Text -> Maybe T.Text) -- ^ matcher function
                         -> [T.Text] -- ^ items to match against
                         -> EditorM T.Text
completeInListCustomShow showFunction s match possibilities
    | null filtered = printMsg "No match" >> return s
    | prefix /= s = return prefix
    | isSingleton filtered = printMsg "Sole completion" >> return s
    | prefix `elem` filtered =
        printMsg ("Complete, but not unique: " <> showT filtered) >> return s
    | otherwise = printMsgs (map showFunction filtered)
                  >> return (bestMatch filtered s)
    where
      prefix   = commonTPrefix' filtered
      filtered = filterMatches match possibilities

completeInList' :: T.Text
                -> (T.Text -> Maybe T.Text)
                -> [T.Text]
                -> EditorM T.Text
completeInList' s match l = case filtered of
  [] -> printMsg "No match" >> return s
  [x] | s == x    -> printMsg "Sole completion" >> return s
      | otherwise -> return x
  _ -> printMsgs filtered >> return (bestMatch filtered s)
  where
    filtered = filterMatches match l

-- | This function attempts to provide a better tab completion result in
-- cases where more than one file matches our prefix. Consider directory with
-- following files: @["Main.hs", "Main.hi", "Main.o", "Test.py", "Foo.hs"]@.
--
-- After inserting @Mai@ into the minibuffer and attempting to complete, the
-- possible matches will be filtered in 'completeInList'' to
-- @["Main.hs", "Main.hi", "Main.o"]@ however because of multiple matches,
-- the buffer will not be updated to say @Main.@ but will instead stay at @Mai@.
--
-- This is extremely tedious when trying to complete filenames in directories
-- with many files so here we try to catch common prefixes of filtered files and
-- if the result is longer than what we have, we use it instead.
bestMatch :: [T.Text] -> T.Text -> T.Text
bestMatch fs s = let p = commonTPrefix' fs
                 in if T.length p > T.length s then p else s

filterMatches :: Eq a => (b -> Maybe a) -> [b] -> [a]
filterMatches match = nub . catMaybes . fmap match

-- Not really necessary but a bit faster than @(length l) == 1@
isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False
