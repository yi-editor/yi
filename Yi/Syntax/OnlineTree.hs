module Yi.Syntax.OnlineTree (Tree(..), parse, dropToIndex) where
import Prelude ()
import Yi.Prelude
import Yi.IncrementalParse
import Control.Applicative
import Data.Traversable
import Data.Foldable
import Yi.Lexer.Alex
import Yi.Buffer.Basic (Point)


data Tree a = Node a (Tree a) (Tree a)
            | Leaf
              deriving Show

instance Traversable Tree where
    traverse f (Node x l r) = Node <$> f x <*> traverse f l <*> traverse f r
    traverse _ Leaf = pure Leaf

instance Foldable Tree where
    foldMap = foldMapDefault

instance Functor Tree where
    fmap = fmapDefault

case_ :: (Maybe s -> Bool) -> P s a -> P s a -> P s a
case_ f true false = (testNext f *> true) <|> (testNext (not . f) *> false)

symbolBefore :: Point -> Maybe (Tok t) -> Bool
symbolBefore _ Nothing = False
symbolBefore p (Just x) = tokBegin x <= p

factor :: Int
factor = 2  

initialLeftSize :: Size
initialLeftSize = 2 

parse = parse' initialLeftSize 0 maxBound

-- Invariant: all symbols are after the l
-- | Parse all the symbols starting in the interval [lb, rb[
parse' :: Size -> Point -> Point -> P (Tok t) (Tree (Tok t))
parse' leftSize lB rB
   | rB <= lB = pure Leaf
   | otherwise = case_ (symbolBefore rB)
       (Node <$> symbol (const True)
             <*> parse' initialLeftSize      lB   midB
             <*> parse' (leftSize * fromIntegral factor)  midB rB)
       (pure Leaf) 
  where midB = min rB (lB +~ leftSize)
    -- NOTE: eof here is important for performance (otherwise the
    -- parser would have to keep this case until the very end of input
    -- is reached.

toEndo Leaf = id
toEndo (Node x l r) = (x :) . toEndo l . toEndo r

dropToIndex :: Point -> Tree t -> [t]
dropToIndex index t = dropHelp initialLeftSize t index []

dropHelp :: Size -> Tree a -> Point -> [a] -> [a]
dropHelp _leftsize Leaf _n = id
dropHelp leftsize (Node x l r) index
    | fromIntegral index <  leftsize = (x :) . dropHelp initialLeftSize     l index . toEndo  r
    -- the head is speculatively put in the result; but it does not matter, since we
    -- add only O(log n) elements this way.
    | otherwise         = (x :) . dropHelp (leftsize * fromIntegral factor) r  (index -~ leftsize)


