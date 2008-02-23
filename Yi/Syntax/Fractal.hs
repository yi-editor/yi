{-# OPTIONS -fglasgow-exts #-}
module Yi.Syntax.Fractal (mkHighlighter) where

import Yi.Syntax
import Yi.Syntax.Alex (AlexState)
import qualified Yi.IncrementalParse as P
import Data.Maybe
import qualified Data.Tree as S
import Control.Applicative
import Data.List

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

parse :: Int -> Int -> P.P a (Tree a)
parse leftSize maxSize
   | maxSize <= 0 = pure Leaf
   | otherwise 
     =  (Node <$> P.symbol (const True)
              <*> parse factor              (min leftSize (maxSize - 1))
              <*> parse (leftSize * factor) (maxSize - leftSize - 1))
     <|> (P.eof *> pure Leaf) 
    -- NOTE: eof here is important for performance (otherwise the
    -- parser would have to keep this case until the very end of input
    -- is reached.
         
type Cache = P.IResult Stroke (Tree Stroke)

mkHighlighter :: forall lexState. lexState
              -> (AlexState lexState -> Maybe (Stroke, AlexState lexState))
              -> Highlighter (Cache)
mkHighlighter initState alexScanToken = 
  Yi.Syntax.SynHL { hlStartState   = P.Leaf Leaf 0 (P.run (parse 1 100000000))
                  -- FIXME: max int
                  , hlRun          = run
                  , hlGetStrokes   = getS
                  }
      where run source dirty cache = fst3 $ P.upd fst3 (tokenSource source) dirty cache
            getS begin end cache = getStrokes begin end (P.getValue cache)
            tokenSource source ofs = unfoldr alexScanToken (ofs,source ofs,initState)
            -- FIXME: Starting re-parsing with initState is wrong!
            fst3 (x,_,_) = x

getStrokes :: Int -> Int -> Tree Stroke -> [Stroke]
getStrokes _ _ Leaf = []
getStrokes _     end (Node (i,_,_) _ _) 
    | end < i = []
getStrokes begin end (Node s lc Leaf) 
    = s : getStrokes begin end lc
getStrokes begin end (Node s _ rc@(Node (i,_,_) _ _))
    | i < begin = s : getStrokes begin end rc
getStrokes begin end (Node s lc rc)
    = s : getStrokes begin end lc ++ getStrokes begin end rc


