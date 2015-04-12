-- | This is a little helper for completion interfaces.

module Yi.Keymap.Completion (
        CompletionTree(CT),
        stepTree, obvious, mergeTrees, listToTree, complete)
   where

import Control.Arrow (first)
import Data.List     (find, groupBy, intercalate, sortBy)

-- inside a completion tree, the a's must be unique on each level

data CompletionTree a = CT [(a,CompletionTree a)]

instance (Show a) => Show (CompletionTree a) where
  show = show'

show' :: (Show a) => CompletionTree a -> String
show' (CT []) = []
show' (CT [(a,st)]) = shows a $ show' st
show' (CT trees) = "[" ++ intercalate "|" (map (\(x,y)->shows x $ show' y) trees) ++ "]"

compareBy :: (Ord b) => (a->b)->a->a->Ordering
compareBy f a b = compare (f a) (f b)

listToTree :: [a] -> CompletionTree a
listToTree = foldr (\a b->CT [(a,b)]) (CT [])

stepTree :: Eq a => CompletionTree a->a->Maybe ([a],CompletionTree a)
stepTree (CT completions) letter = Just $ obvious $ CT $ filter
                                   ((letter==).fst) completions

obvious :: CompletionTree a -> ([a],CompletionTree a)
obvious (CT [(letter,moretrees)]) = first ((:) letter) $
                                             obvious moretrees
obvious remainingchoice           = ([],remainingchoice)

mergeTrees :: Ord a => [CompletionTree a] -> CompletionTree a
mergeTrees a = mergeTrees' (map sort' a)
  where sort' = CT . sortBy (compareBy fst) . (\(CT l)->l)

mergeTrees':: Ord a => [CompletionTree a] -> CompletionTree a
mergeTrees' trees = CT $
                    map (\x->(fst $ head x,mergeTrees $ map snd x)) $
                    groupBy (((EQ==).).compareBy fst)  $
                    sortBy (compareBy fst) $
                    concatMap (\(CT x)->x) trees

complete :: Eq a => CompletionTree a -> [a] -> ([a],CompletionTree a)
complete tree    [] = ([],tree)
complete (CT []) _  = ([],CT [])
complete (CT level) (a:ta) = first ((:) a) $
    case match of
       Just m -> complete (snd m) ta
       Nothing -> ([],CT [])
     where match = find ((a==).fst) level


--alternatives :: CompletionTree a->[[a]]
