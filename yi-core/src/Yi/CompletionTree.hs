{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      :  Yi.CompletionTree
License     :  GPL-2
Maintainer  :  yi-devel@googlegroups.com
Stability   :  experimental
Portability :  portable

Little helper for completion interfaces.

Intended to be imported qualified:

>import qualified Yi.CompletionTree as CT
-}
module Yi.CompletionTree (
  -- * CompletionTree type
  CompletionTree (CompletionTree),
  -- * Lists
  fromList, toList,
  -- * Modification
  complete, update,
  -- * Debugging
  pretty,
  -- ** Lens
  unCompletionTree
  ) where

import           Control.Arrow       (first)
import           Data.Function       (on)
import           Data.List           (partition, maximumBy, intercalate)
import qualified Data.Map.Strict     as M
import           Data.Map.Strict     (Map)
import           Data.Maybe          (isJust, fromJust, listToMaybe, catMaybes)
import qualified Data.ListLike       as LL
import           Data.ListLike       (ListLike)
import           Lens.Micro.Platform (over, Lens', _2, (.~), (&))
import           Data.Binary         (Binary)

-- | A CompletionTree is a map of partial completions.
--
-- Example:
--
-- fromList ["put","putStr","putStrLn","print","abc"]
--
-- Gives the following tree:
--
--            / \
--          "p" "abc"
--         /  \
--      "ut"  "rint"
--      /  \
--   "Str"  ""
--   /  \
-- "Ln"  ""
--
-- (The empty strings are needed to denote the end of a word)
-- (A CompletionTree is not limited to a binary tree)
newtype CompletionTree a = CompletionTree {_unCompletionTree :: (Map a (CompletionTree a))}
  deriving (Monoid, Eq, Binary)

unCompletionTree :: Lens' (CompletionTree a) (Map a (CompletionTree a))
unCompletionTree f ct = (\unCompletionTree' -> ct {_unCompletionTree = unCompletionTree'}) <$>
                        f (_unCompletionTree ct)

instance (Ord a, Show a, ListLike a i) => Show (CompletionTree a) where
  show ct = "fromList " ++ show (toList ct)

-- | This function converts a list of completable elements to a CompletionTree
-- It finds elements that share a common prefix and groups them.
--
-- prop> fromList . toList = id
fromList :: (Ord a, ListLike a i, Eq i) => [a] -> CompletionTree a
fromList [] = mempty
fromList (x:xs)
  | x == mempty = over unCompletionTree (M.insert mempty mempty) (fromList xs)
  | otherwise = case maximumBy' (compare `on` childrenIn xs) (tail $ LL.inits x) of
      Nothing -> over unCompletionTree (M.insert x mempty) (fromList xs)
      Just parent -> case first (x:) $ partition (parent `LL.isPrefixOf`) xs of
        ([_],rest) -> over unCompletionTree (M.insert parent mempty) $ fromList rest
        (hasParent, rest) -> over unCompletionTree (M.insert parent (fromList $
           map (fromJust . LL.stripPrefix parent) hasParent)) $ fromList rest
      -- A parent is the prefix and the children are the items with the parent as prefix
      where childrenIn list parent = length $ filter (parent `LL.isPrefixOf`) list

-- | The largest element of a non-empty structure with respect to the
-- given comparison function, Nothing if there are multiple 'largest' elements.
maximumBy' :: Eq a => (a -> a -> Ordering) -> [a] -> Maybe a
maximumBy' cmp l | atleast 2 (== max') l = Nothing
                 | otherwise = Just max'
  where  max' = maximumBy cmp l
         -- This short-circuits if the condition is met n times before the end of the list.
         atleast :: Int -> (a -> Bool) -> [a] -> Bool
         atleast 0 _ _ = True
         atleast _ _ [] = False
         atleast n cmp' (x:xs) | cmp' x = atleast (n - 1) cmp' xs
                               | otherwise = atleast n cmp' xs

-- | Complete as much as possible without guessing.
--
-- Examples:
--
-- >>> complete $ fromList ["put","putStrLn","putStr"]
-- ("put", fromList ["","Str","StrLn"])
--
-- >>> complete $ fromList ["put","putStr","putStrLn","abc"]
-- ("", fromList ["put","putStr","putStrLn","abc"])
complete :: (Eq i, Ord a, ListLike a i) => CompletionTree a -> (a, CompletionTree a)
complete (CompletionTree ct)
  | M.size ct == 1 = if snd (M.elemAt 0 ct) == mempty
                       then M.elemAt 0 ct & _2 .~ fromList [mempty]
                       else M.elemAt 0 ct
  | otherwise = (mempty,CompletionTree ct)

-- | Update the CompletionTree with new information.
-- An empty list means that there is no completion left.
-- A [mempty] means that the end of a word is reached.
--
-- Examples:
--
-- >>> update (fromList ["put","putStr"]) "p"
-- fromList ["ut","utStr"]
--
-- >>> update (fromList ["put","putStr"]) "put"
-- fromList ["","Str"]
--
-- >>> update (fromList ["put","putStr"]) "putS"
-- fromList ["tr"]
--
-- >>> update (fromList ["put"]) "find"
-- fromList []
--
-- >>> update (fromList ["put"]) "put"
-- fromList [""]
update :: (Ord a, ListLike a i, Eq i) => CompletionTree a -> a -> CompletionTree a
update (CompletionTree ct) p
  -- p is empty, this case just doesn't make sense:
  | mempty == p = error "Can't update a CompletionTree with a mempty"
  -- p is a key in the map ct that doesn't have children:
  -- (This means the end of a word is reached)
  | isJust one && mempty == fromJust one = CompletionTree $ M.singleton mempty mempty
  -- p is a key in the map ct with children:
  | isJust one = fromJust one
  -- a substring of p is a key in ct:
  | isJust remaining = uncurry update $ fromJust remaining
  -- p is a substring of a key in ct:
  | otherwise = CompletionTree $ M.mapKeys fromJust
                               $ M.filterWithKey (const . isJust)
                               $ M.mapKeys (LL.stripPrefix p) ct
  where
    one = M.lookup p ct
    remaining = listToMaybe . catMaybes $
      map (\p' -> (,fromJust $ LL.stripPrefix p' p) <$> M.lookup p' ct) (tail $ LL.inits p)

-- | Converts a CompletionTree to a list of completions.
--
-- prop> toList . fromList = sort . nub
--
-- Examples:
--
-- >>> toList mempty
-- []
--
-- >>> toList (fromList ["a"])
-- ["a"]
--
-- >>> toList (fromList ["a","a","a"])
-- ["a"]
--
-- >>> toList (fromList ["z","x","y"])
-- ["x","y","z"]
toList :: (Ord a, ListLike a i) => CompletionTree a -> [a]
toList ct
  | mempty == ct = []
  | otherwise = toList' ct
  where
    toList' (CompletionTree ct')
      | M.null ct' = [mempty]
      | otherwise = concat $ M.elems $ M.mapWithKey (\k v -> map (k `LL.append`) $ toList' v) ct'

-- TODO: make this function display a tree and rename to showTree
-- | For debugging purposes.
--
-- Example:
--
-- >>> putStrLn $ pretty $ fromList ["put", "putStr", "putStrLn"]
-- ["put"[""|"Str"[""|"Ln"]]]
pretty :: Show a => CompletionTree a -> String
pretty (CompletionTree ct)
  | M.null ct = ""
  | otherwise = "[" ++ intercalate "|" (M.elems (M.mapWithKey (\k v -> shows k (pretty v)) ct)) ++ "]"
