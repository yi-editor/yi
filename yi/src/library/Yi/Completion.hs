-- Copyright (C) 2008 JP Bernardy


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

import Yi.Editor
import Yi.Utils
import Data.List
import Data.Maybe
import Data.Char (toLower)

-------------------------------------------
-- General completion

mkIsPrefixOf :: Bool -> String -> String -> Bool
mkIsPrefixOf caseSensitive = if caseSensitive
                             then isPrefixOf
                             else isPrefixOfIC
  where isPrefixOfIC x y = map toLower x `isPrefixOf` map toLower y

-- | Prefix matching function, for use with 'completeInList'
prefixMatch :: String -> String -> Maybe String
prefixMatch prefix s = if prefix `isPrefixOf` s then Just s else Nothing

-- | Infix matching function, for use with 'completeInList'
infixMatch :: String -> String -> Maybe String
infixMatch needle haystack = fmap (`drop` haystack) $ findIndex (needle `isPrefixOf`) (tails haystack)

{-# ANN subsequenceMatch "HLint: ignore Eta reduce" #-}
-- | Example: "abc" matches "a1b2c"
subsequenceMatch :: String -> String -> Bool
subsequenceMatch needle haystack = go needle haystack
  where go (n:ns) (h:hs) | n == h = go ns hs
        go (n:ns) (h:hs) | n /= h = go (n:ns) hs
        go [] _ = True
        go _ [] = False
        go _ _  = False -- NOTE: to satisfy broken GHC analyzer, which doesn't know that n==h or n/=h. After all one can make Eq so that it doesn't work ;->.

containsMatch' :: Bool -> String -> String -> Maybe String
containsMatch' caseSensitive pattern str = fmap (const str) $ find (pattern `tstPrefix`) (tails str)
  where tstPrefix = mkIsPrefixOf caseSensitive

containsMatch :: String -> String -> Maybe String
containsMatch = containsMatch' True

containsMatchCaseInsensitive :: String -> String -> Maybe String
containsMatchCaseInsensitive = containsMatch' False


-- | Complete a string given a user input string, a matching function
-- and a list of possibilites.  Matching function should return the
-- part of the string that matches the user string.
completeInList :: String -> (String -> Maybe String) -> [String] -> EditorM String
completeInList = completeInListCustomShow id

-- | Same as 'completeInList', but maps @showFunction@ on possible matches when printing
completeInListCustomShow :: (String -> String) -> String -> (String -> Maybe String) ->
                            [String] -> EditorM String
completeInListCustomShow showFunction s match possibilities
    | null filtered = printMsg "No match" >> return s
    | prefix /= s = return prefix
    | isSingleton filtered = printMsg "Sole completion" >> return s
    | prefix `elem` filtered = printMsg ("Complete, but not unique: " ++ show filtered) >> return s
    | otherwise = printMsgs (map showFunction filtered)
                  >> return (bestMatch filtered s)
    where
      prefix   = commonPrefix filtered
      filtered = filterMatches match possibilities

completeInList' :: String -> (String -> Maybe String) -> [String] -> EditorM String
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
bestMatch :: [String] -> String -> String
bestMatch fs s = let p = commonPrefix fs
                 in if length p > length s then p else s

filterMatches :: Eq a => (b -> Maybe a) -> [b] -> [a]
filterMatches match = nub . catMaybes . fmap match

-- Not really necessary but a bit faster than @(length l) == 1@
isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False
