{-# OPTIONS -fglasgow-exts #-}
module Yi.Syntax.Fractal where

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

factor = 2

toTree ::  Int -> [a] -> Tree a
toTree _ [] = Leaf
toTree level (x:xs) = let (l,r) = splitAt level xs -- should be lazy (?)
                      in Node x (toTree' l) (toTree (level * factor) r)

fromTree :: Tree a -> [a]
fromTree (Node a l r) = a : fromTree l ++ fromTree r
fromTree Leaf = []

toTree' = toTree factor

shape :: Show a => Tree a -> [S.Tree String]
shape Leaf = [] -- [S.Node "o"[]]
shape (Node x l r) = [S.Node (show x) (shape l ++ shape r)]

sz :: S.Tree a -> Int
sz (S.Node a xs) = 1 + sum (map sz xs)

trans :: (S.Tree a -> b) -> (S.Tree a -> S.Tree b)
trans f n@(S.Node x xs) = S.Node (f n) (map (trans f) xs)

ev f (S.Node x xs) = S.Node (f x) (map (ev f) xs)

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
         

--getNextItem :: Int -> P s s
getNextItem sz
    | sz <= 0 = empty
    | otherwise = P.symbol (const True)

-- test1 = tt factor 30 <* P.eof

-- main = putStrLn $ S.drawForest $ shape $ snd $ fromJust $ unP test1 [1..100]
-- tree = P.runPolish test1 [1..100]
-- main = putStrLn $ S.drawForest  $ shape $ tree

type Cache = P.IResult Stroke (Tree Stroke)

mkHighlighter :: forall s. s
              -> (AlexState s -> Maybe (Stroke, AlexState s))
              -> Highlighter (Cache)
mkHighlighter initState alexScanToken = 
  Yi.Syntax.SynHL { hlStartState   = P.Leaf Leaf 0 (P.run (parse 1 100000000))
                  -- FIXME: max int
                  , hlRun          = run
                  , hlGetStrokes   = getStrokes
                  }
      where run source dirty cache = fst3 $ P.upd fst3 (alexScanTokens . source) dirty cache
            getStrokes begin end cache = fromTree (P.getValue cache)
            alexScanTokens inp = unfoldr alexScanToken (0,inp,initState)



fst3 (x,_,_) = x
