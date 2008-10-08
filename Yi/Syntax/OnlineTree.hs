module Yi.Syntax.OnlineTree (Tree(..), parse, dropToIndex, dropBut) where
import Prelude hiding (foldl)
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
    traverse f Leaf = pure Leaf

instance Foldable Tree where
    foldMap = foldMapDefault

instance Functor Tree where
    fmap = fmapDefault

case_ f true false = ((lookNext f) *> true) <|> (lookNext (not . f) *> false)

symbolBefore :: Point -> Maybe (Tok t) -> Bool
symbolBefore _ Nothing = False
symbolBefore p (Just x) = tokBegin x <= p

factor = 2
initialLeftSize = 2

parse = parse' initialLeftSize 0 maxBound

-- Invariant: all symbols are after the l
-- | Parse all the symbols starting in the interval [lb, rb[
parse' leftSize lB rB
   | rB <= lB = pure Leaf
   | otherwise = case_ (symbolBefore rB)
       (Node <$> symbol (const True)
             <*> parse' factor               lB   midB
             <*> parse' (leftSize * factor)  midB rB)
       (pure Leaf) 
  where midB = min rB (lB + leftSize)
    -- NOTE: eof here is important for performance (otherwise the
    -- parser would have to keep this case until the very end of input
    -- is reached.

toEndo Leaf = id
toEndo (Node x l r) = (x :) . toEndo l . toEndo r

toReverseList = foldl (flip (:)) []

type E a = a -> a

dropBut amount t = drop' initialLeftSize id t amount []
  where
    -- drop' :: Int -> E [a] -> Tree a -> Int -> E [a]
    drop' leftsize prec Leaf n = prec
    drop' leftsize prec t@(Node x l r) index
        | index <= 0 = prec . toEndo t
        | index <  leftsize = drop' initialLeftSize     (x :)         l (index)            . toEndo r
        | otherwise         = drop' (leftsize * factor) (last prec l) r (index - leftsize)
    last :: E [a] -> Tree a -> [a] -> [a]
    last prec t = case toReverseList t of
        (x:xs) -> (x :)
        _ -> prec

dropToIndex index t = dropHelp initialLeftSize t index []

-- dropHelp :: Int -> Tree a -> Int -> [a] -> [a]
dropHelp leftsize Leaf n = id
dropHelp leftsize t@(Node x l r) index
    | index <  leftsize = (x :) . dropHelp initialLeftSize     l index . toEndo  r
    -- the head is speculatively put in the result; but it does not matter, since we
    -- add only O(log n) elements this way.
    | otherwise         = (x :) . dropHelp (leftsize * factor) r  (index - leftsize)


