{-# OPTIONS -fglasgow-exts #-}
module Yi.Syntax.Fractal (parse, getStrokes) where

import Yi.Syntax.Alex (Tok(..), tokBegin)
import qualified Yi.IncrementalParse as P
import qualified Data.Tree as S
import Control.Applicative
import Yi.Prelude 
import Prelude ()
import Data.List (splitAt)

data Tree a = Node a (Tree a) (Tree a)
            | Leaf
              deriving Show

factor :: Int
factor = 2

toTree ::  [a] -> Tree a
toTree = tt factor
    where tt _ [] = Leaf
          tt level (x:xs) = let (l,r) = splitAt level xs
                            in Node x (toTree l) (tt (level * factor) r)

fromTree :: Tree a -> [a]
fromTree (Node a l r) = a : fromTree l ++ fromTree r
fromTree Leaf = []


shape :: Show a => Tree a -> [S.Tree String]
shape Leaf = [] -- [S.Node "o"[]]
shape (Node x l r) = [S.Node (show x) (shape l ++ shape r)]

parse = parse' initSize maxBound
    where initSize = 1
          parse' :: Int -> Int -> P.P (Tok a) (Tree (Tok a))
          parse' leftSize maxSize
             | maxSize <= 0 = pure Leaf
             | otherwise 
               =  (Node <$> P.symbol (const True)
                        <*> parse' (initSize * factor) (min leftSize (maxSize - 1))
                        <*> parse' (leftSize * factor) (maxSize - leftSize - 1))
               <|> (P.eof *> pure Leaf) 
              -- NOTE: eof here is important for performance (otherwise the
              -- parser would have to keep this case until the very end of input
              -- is reached.

getStrokes begin end t = getStrokes' begin end t []
    where getStrokes' :: Int -> Int -> Tree (Tok token) -> Endom [Tok token]
          getStrokes' _ _ Leaf = id
          getStrokes' _     end (Node t _ _) 
              | end < tokBegin t = id
          getStrokes' begin end (Node s lc Leaf) 
              = (s :) . getStrokes' begin end lc
          getStrokes' begin end (Node s _ rc@(Node t _ _))
              | tokBegin t < begin = (s :) . getStrokes' begin end rc
          getStrokes' begin end (Node s lc rc)
              = (s :) . getStrokes' begin end lc . getStrokes' begin end rc
