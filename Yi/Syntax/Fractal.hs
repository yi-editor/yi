{-# OPTIONS -fglasgow-exts #-}
module Yi.Syntax.Fractal (mkHighlighter) where

import Yi.Syntax
import Yi.Syntax.Alex (AlexState(..), startPosn, AlexInput)
import qualified Yi.IncrementalParse as P
import qualified Data.Tree as S
import Control.Applicative
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
              -> ((AlexState lexState, AlexInput) -> Maybe (T token, (AlexState lexState, AlexInput)))
              -> (T token -> Stroke)
              -> Highlighter (Cache lexState token)
mkHighlighter initState alexScanToken tokenToStroke = 
  Yi.Syntax.SynHL { hlStartState   = P.Leaf Leaf (AlexState 0 initState 0 startPosn) 
                                            (P.run (parse 1 1000000000))
                  -- FIXME: max int
                  , hlRun          = run
                  , hlGetStrokes   = getS
                  }
      where run source dirty cache = fst3 $ P.upd initState (tokenSource source) dirty cache
            getS begin end cache = fmap tokenToStroke $ getStrokes begin end (P.getValue cache) []
            tokenSource :: (Int -> AlexInput) -> AlexState lexState -> [(AlexState lexState, T token)]
            tokenSource source st
                = unfoldLexer alexScanToken (st, source (startOffset st))
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


unfoldLexer :: ((AlexState lexState, input) -> Maybe (token, (AlexState lexState, input)))
             -> (AlexState lexState, input) -> [(AlexState lexState, token)]
unfoldLexer f b = case f b of
             Nothing -> []
             Just (t, b') -> (fst b, t) : unfoldLexer f b'



