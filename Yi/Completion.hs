-- Copyright (C) 2008 JP Bernardy


module Yi.Completion (completeInList) where

import Yi.Core
import Data.List

-------------------------------------------
-- General completion
-------------------------------------------
commonPrefix :: [String] -> String
commonPrefix [] = []
commonPrefix strings
    | any null strings = []
    | all (== prefix) heads = prefix : commonPrefix tailz
    | otherwise = []
    where
          (heads, tailz) = unzip [(h,t) | (h:t) <- strings]
          prefix = head heads
-- for an alternative implementation see GHC's InteractiveUI module.



completeInList :: String -> (String -> Bool) -> [ String ] -> YiM String
completeInList s condition l
    | null filtered = msgE "No match" >> return s
    | prefix /= s = return prefix
    | isSingleton filtered = msgE "Sole completion" >> return s
    | prefix `elem` filtered = msgE ("Complete, but not unique: " ++ show filtered) >> return s
    | otherwise = msgE ("Matches: " ++ show filtered) >> return s
    where
    prefix   = commonPrefix filtered
    filtered = nub $ filter condition l

    -- Not really necessary but a bit faster than @(length l) == 1@
    isSingleton :: [ a ] -> Bool
    isSingleton [_] = True
    isSingleton _   = False



