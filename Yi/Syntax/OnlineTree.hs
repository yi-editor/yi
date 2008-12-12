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

parse :: P (Tok t) (Tree (Tok t))
parse = parse' initialLeftSize 0 maxBound

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

-- | Parse all the symbols starting in the interval [lb, rb[
parse'' :: Size -> Point -> Point -> P (Tok t) x -> P (Tok t) (Tree x)
parse'' leftSize lB rB p
   | rB <= lB = pure Leaf
   | otherwise = case_ (symbolBefore rB)
       (Node <$> p
             <*> parse'' initialLeftSize                   lB   midB  p
             <*> parse'' (leftSize * fromIntegral factor)  midB rB    p)
       (pure Leaf) 
  where midB = min rB (lB +~ leftSize)


toEndo :: Tree a -> [a] -> [a]
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


type E a = a -> a

-- As above, but ensure we also put the element "just before" into result as well.
dropButHelp :: Size -> Tree a -> Point -> E [a] -> E [a]
dropButHelp _leftsize Leaf _n previous =  -- we may have forgotten to insert the previous element here, so we add it.
       previous
dropButHelp leftsize (Node x l r) index _previous
    -- Note that we do not know if the head is to the left or to the right of the middle.
    -- So, the head is speculatively put in the result; but it does not matter, since we
    -- add only O(log n) elements this way.

    -- go to the left
    -- here the previous element was the head, so we do not need to do add any other previous element.
    | fromIntegral index <  leftsize = (x :) . dropButHelp initialLeftSize     l index id . toEndo  r
    -- go to the right.
    -- we also put the last element of the left branch /if it exists/. If it does not exist,
    -- we know we have kept the "just previous element": it was the head.
    | otherwise         = (x :) . dropButHelp (leftsize * fromIntegral factor) r  (index -~ leftsize) (lastElem l)
  where lastElem t = case toReverseList t of
             [] -> id
             (y:_) -> (y :)

        toReverseList :: Tree a -> [a]
        toReverseList = foldl (flip (:)) []

