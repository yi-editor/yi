-- 
-- Copyright (c) B.Zapf July 2005
-- 
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
-- 

-- | This is a little helper for completion interfaces.

module Yi.Keymap.Completion ( 
        CompletionTree(CT),
        stepTree, obvious, mergeTrees, listToTree, complete)
   where

import Data.List

-- inside a completion tree, the a's must be unique on each level

data CompletionTree a = CT [(a,CompletionTree a)]

instance (Show a) => Show (CompletionTree a) where
  show x = show' x

show' :: (Show a) => CompletionTree a -> String
show' (CT []) = []
show' (CT [(a,st)]) = shows a $ show' st
show' (CT trees) = "["++
                   (concat $ 
                    intersperse "|" $ 
                    map (\(x,y)->shows x $ show' y) trees)
                   ++"]"

compareBy :: (Ord b) => (a->b)->a->a->Ordering
compareBy f a b = compare (f a) (f b)

listToTree :: [a] -> CompletionTree a
listToTree = foldr (\a b->CT [(a,b)]) (CT []) 

stepTree :: Eq a => CompletionTree a->a->Maybe ([a],CompletionTree a)
stepTree (CT completions) letter = Just $ obvious $ CT $ filter
                                   ((letter==).fst) completions 

obvious :: CompletionTree a -> ([a],CompletionTree a)
obvious (CT [(letter,moretrees)]) = ((\(x,y)->(letter:x,y)) $ 
                                             obvious moretrees) 
obvious remainingchoice           = ([],remainingchoice)

mergeTrees :: Ord a => [CompletionTree a] -> CompletionTree a 
mergeTrees a = mergeTrees' (map sort' a) 
  where sort' = CT.(sortBy (compareBy fst).(\(CT l)->l))

mergeTrees':: Ord a => [CompletionTree a] -> CompletionTree a 
mergeTrees' trees = CT $
                    map (\x->(fst $ head x,mergeTrees $ map snd x)) $
                    groupBy (((EQ==).).compareBy fst)  $ 
                    sortBy (compareBy fst) $ 
                    concat $ 
                    map (\(CT x)->x) trees

complete :: Eq a => CompletionTree a -> [a] -> ([a],CompletionTree a)
complete tree    [] = ([],tree)
complete (CT []) _  = ([],CT [])
complete (CT level) (a:ta) = (\(x,y)->(a:x,y)) $
    case match of 
       Just m -> complete (snd m) ta
       Nothing -> ([],CT [])
     where match = find ((a==).fst) level


--alternatives :: CompletionTree a->[[a]]

