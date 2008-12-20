-- Copyright (C) 2008 JP Bernardy


module Yi.Completion 
  ( completeInList
  , commonPrefix
  , prefixMatch, infixMatch
  )
where

import Prelude ()
import Yi.Prelude hiding (elem)
import Yi.Editor
import Data.List
import Data.Maybe

-------------------------------------------
-- General completion


-- | Prefix matching function, for use with 'completeInList'
prefixMatch :: String -> String -> Maybe String
prefixMatch prefix s = if prefix `isPrefixOf` s then Just s else Nothing

-- | Infix matching function, for use with 'completeInList'
infixMatch :: String -> String -> Maybe String
infixMatch needle haystack = fmap (\n -> drop n haystack) $ findIndex (needle `isPrefixOf`) (tails haystack)


-- | Complete a string given a user input string, a matching function
-- and a list of possibilites.  Matching function should return the
-- part of the string that matches the user string.
completeInList :: String -> (String -> Maybe String) -> [String] -> EditorM String
completeInList s match l
    | null filtered = printMsg "No match" >> return s
    | prefix /= s = return prefix
    | isSingleton filtered = printMsg "Sole completion" >> return s
    | prefix `elem` filtered = printMsg ("Complete, but not unique: " ++ show filtered) >> return s
    | otherwise = printMsg ("Matches: " ++ show filtered) >> return s
    where
    prefix   = commonPrefix filtered
    filtered = nub $ catMaybes $ fmap match l

    -- Not really necessary but a bit faster than @(length l) == 1@
    isSingleton :: [a] -> Bool
    isSingleton [_] = True
    isSingleton _   = False



