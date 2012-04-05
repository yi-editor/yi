{-# LANGUAGE TemplateHaskell #-}

module Data.Trie where

-- Trie module. Partly taken from http://www.haskell.org/haskellwiki/Haskell_Quiz/Word_Search/Solution_Sjanssen

import Data.Binary
import Data.DeriveTH
import qualified Data.Map as Map
import Control.Monad

data Trie = Trie Bool (Map.Map Char Trie) deriving (Show)

-- | A blank Trie
empty :: Trie
empty = Trie False Map.empty

-- | Insert a new string into the trie.
insert :: String -> Trie -> Trie
insert []     (Trie _ m) = Trie True m
insert (x:xs) (Trie b m) = Trie b $ Map.alter (maybe (Just $ fromString xs) (Just . insert xs)) x m

fromString :: String -> Trie
fromString = foldr (\x xs -> Trie False (Map.singleton x xs)) (Trie True Map.empty)

-- | Take a list of String and compress it into a Trie
fromList :: [String] -> Trie
fromList = foldr insert empty

-- | Take a trie and expand it into the strings that it represents
toList :: Trie -> [String]
toList (Trie b m) =
    if b then "":expand
    else expand
    where expand = [ char:word | (char, trie) <- Map.toList m,
                                 word <- toList trie ]

-- | Takes a trie and a prefix and returns the sub-trie that
-- of words with that prefix
lookupPrefix :: (MonadPlus m) => String -> Trie -> m Trie
lookupPrefix [] trie = return trie
lookupPrefix (x:xs) (Trie _ m) = liftMaybe (Map.lookup x m) >>= lookupPrefix xs

liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe Nothing = mzero
liftMaybe (Just x) = return x

-- | Finds the longest certain path down the trie starting at a the root
-- Invariant Assumption: All paths have at least one 'true' node below them
forcedNext :: Trie -> String
forcedNext (Trie _ m) =
    if length ls == 1 then
        let (char, trie) = head ls in
        char:forcedNext trie
    else []
    where ls = Map.toList m

-- | Helper function, finds all the suffixes of a given prefix
possibleSuffixes :: String -> Trie -> [String]
possibleSuffixes prefix fulltrie =
    lookupPrefix prefix fulltrie >>= toList

-- | Helper function, finds the longest certain path down the trie starting at a given word
certainSuffix :: String -> Trie -> String
certainSuffix prefix fulltrie =
    lookupPrefix prefix fulltrie >>= forcedNext

$(derive makeBinary ''Trie)
