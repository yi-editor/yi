{-# OPTIONS -fglasgow-exts #-}
module Yi.Syntax.Fractal (mkHighlighter) where

import Yi.Syntax
import Yi.Syntax.Alex (AlexState, AlexInput)
import qualified Yi.IncrementalParse as P
import Data.Maybe
import qualified Data.Tree as S
import Control.Applicative
import Data.List
import Yi.Prelude

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
         
type T token = (Int,token,Int)
type Cache lexState token = P.IResult lexState (T token) (Tree (T token))

mkHighlighter :: forall lexState token. lexState
              -> (AlexState lexState -> Maybe (T token, AlexState lexState))
              -> (T token -> Stroke)
              -> Highlighter (Cache lexState token)
mkHighlighter initState alexScanToken tokenToStroke = 
  Yi.Syntax.SynHL { hlStartState   = P.Leaf Leaf (P.InputState initState 0) 
                                            (P.run (parse 1 1000000000))
                  -- FIXME: max int
                  , hlRun          = run
                  , hlGetStrokes   = getS
                  }
      where run source dirty cache = fst3 $ P.upd initState (tokenSource source) dirty cache
            getS begin end cache = fmap tokenToStroke $ getStrokes begin end (P.getValue cache) []
            tokenSource :: (Int -> AlexInput) -> P.InputState lexState -> [(P.InputState lexState, T token)]
            tokenSource source (P.InputState lexState ofs) 
                = unfoldLexer alexScanToken (ofs,source ofs,lexState)
            fst3 (x,_,_) = x

            getStrokes :: Int -> Int -> Tree (T token) -> Endom [T token]
            getStrokes _ _ Leaf = id
            getStrokes _     end (Node (i,_,_) _ _) 
                | end < i = id
            getStrokes begin end (Node s lc Leaf) 
                = (s :) . getStrokes begin end lc
            getStrokes begin end (Node s _ rc@(Node (i,_,_) _ _))
                | i < begin = (s :) . getStrokes begin end rc
            getStrokes begin end (Node s lc rc)
                = (s :) . getStrokes begin end lc . getStrokes begin end rc


unfoldLexer :: (AlexState lexState -> Maybe (token, AlexState lexState)) 
             -> AlexState lexState -> [(P.InputState lexState, token)]
unfoldLexer f b@(ofs,_,lexState) = case f b of
             Nothing -> []
             Just (t, b') -> (P.InputState lexState ofs, t) : unfoldLexer f b'



