-- Copyright (C) 2008 JP Bernardy


module Yi.Completion 
  ( completeInList, completeInList'
  , commonPrefix
  , prefixMatch, infixMatch
  , containsMatch', containsMatch, containsMatchCaseInsensitive
  , mkIsPrefixOf
  )
where

import Prelude ()
import Yi.Prelude hiding (elem, find)
import Yi.Editor
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
infixMatch needle haystack = fmap (\n -> drop n haystack) $ findIndex (needle `isPrefixOf`) (tails haystack)

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
completeInList s match l
    | null filtered = printMsg "No match" >> return s
    | prefix /= s = return prefix
    | isSingleton filtered = printMsg "Sole completion" >> return s
    | prefix `elem` filtered = printMsg ("Complete, but not unique: " ++ show filtered) >> return s
    | otherwise = printMsgs filtered >> return s
    where
    prefix   = commonPrefix filtered
    -- filtered = nub $ catMaybes $ fmap match l
    filtered = filterMatches match l

completeInList' :: String -> (String -> Maybe String) -> [String] -> EditorM String
completeInList' s match l
    | null filtered = printMsg "No match" >> return s
    | isSingleton filtered && s == (head filtered) = printMsg "Sole completion" >> return s
    | isSingleton filtered                         = return $ head filtered
    | otherwise = printMsgs filtered >> return s
    where
    filtered = filterMatches match l

filterMatches :: Eq a => (b -> Maybe a) -> [b] -> [a]
filterMatches match = nub . catMaybes . fmap match

-- Not really necessary but a bit faster than @(length l) == 1@
isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

