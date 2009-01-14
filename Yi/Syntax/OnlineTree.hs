module Yi.Syntax.OnlineTree (Tree(..), manyToks, dropToIndex, 
                             dropToIndexBad, tokAtOrBefore, 
                             manyT, sepByT, MaybeOneMore(..), TreeAtPos) where
import Prelude ()
import Yi.Prelude
import Yi.IncrementalParse
import Control.Applicative
import Data.Traversable
import Data.Foldable
import Data.List (takeWhile, reverse)
import Data.Maybe (listToMaybe)
import Yi.Lexer.Alex
import Yi.Buffer.Basic (Point)
import Yi.Region

data TreeAtPos a = TreeAtPos Size (Tree a)
    deriving Show

data MaybeOneMore f x = None | OneMore x (f x)
    deriving Show

data Tree a = Node a (Tree a) (Tree a)
            | Leaf
              deriving Show

instance Traversable f => Traversable (MaybeOneMore f) where
    traverse f None = pure None
    traverse f (OneMore x xs) = OneMore <$> f x <*> traverse f xs

instance Traversable f => Foldable (MaybeOneMore f) where foldMap = foldMapDefault
instance Traversable f => Functor (MaybeOneMore f) where fmap = fmapDefault

instance Traversable TreeAtPos where
    traverse f (TreeAtPos s t) = TreeAtPos s <$> traverse f t

instance Foldable TreeAtPos where foldMap = foldMapDefault
instance Functor TreeAtPos where fmap = fmapDefault

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
-- TODO: increase to a reasonable value. Since files are often > 1024 bytes, this
-- seems a reasonable start.

manyToks :: P (Tok t) (Tree (Tok t))
manyToks = parse' initialLeftSize 0 maxBound
    where
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

manyT :: P (Tok t) x -> P (Tok t) (TreeAtPos x)
manyT p = lookNext >>= \x -> case x of
    Nothing -> pure (TreeAtPos 0 Leaf)
    Just t -> let b = tokBegin t 
               in (TreeAtPos 0) <$> parse'' initialLeftSize b maxBound p


sepByT :: P (Tok t) x -> P (Tok t) y -> P (Tok t) (MaybeOneMore TreeAtPos x)
sepByT p q = pure None <|> OneMore <$> p <*> manyT (q *> p)


      
-- | Parse all the elements starting in the interval [lb, rb[
parse'' :: Size -> Point -> Point -> P (Tok t) x -> P (Tok t) (Tree x)
parse'' leftSize lB rB p
   | rB <= lB = pure Leaf
   | otherwise = case_ (symbolBefore rB)
       ((Node <$> p -- FIXME: we'd like a cut here (?)
              <*> parse'' initialLeftSize                   lB   midB  p
              <*> parse'' (leftSize * fromIntegral factor)  midB rB    p)
        <|> pure Leaf -- This creates a branch which cannot be discarded without parsing some of the future.
       )
       (pure Leaf) 
  where midB = min rB (lB +~ leftSize)

       


toEndo :: Tree a -> E [a]
toEndo Leaf = id
toEndo (Node x l r) = (x :) . toEndo l . toEndo r

dropToIndexBad :: Point -> Tree t -> [t]
dropToIndexBad index t = dropHelp initialLeftSize t index []

dropHelp :: Size -> Tree a -> Point -> [a] -> [a]
dropHelp _leftsize Leaf _n = id
dropHelp leftsize (Node x l r) index
    | fromIntegral index <  leftsize = (x :) . dropHelp initialLeftSize     l index . toEndo  r
    -- the head is speculatively put in the result; but it does not matter, since we
-- add only O(log n) elements this way.
    | otherwise         = (x :) . dropHelp (leftsize * fromIntegral factor) r  (index -~ leftsize)


type E a = a -> a

dropToIndex index (TreeAtPos begin t) = dropButHelp initialLeftSize t (index -~ begin)

-- As above, but ensure we also put the element "just before" into result as well.

-- We need this because we constructed the tree in such a way that the elements
-- begin after a given bound. So, if we drop stuff before, we might drop a node
-- that indeed starts before the bound, but ends after it. So, we must keep one
-- extra element before.

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


tokAtOrBefore :: Point -> Tree (Tok a) -> Maybe (Tok a)
tokAtOrBefore p res = listToMaybe $ reverse $ toksInRegion (mkRegion 0 (p+1)) res

-- TODO: improve
toksInRegion :: Region -> Tree (Tok a) -> [Tok a]
toksInRegion reg = takeWhile (\t -> tokBegin t <= regionEnd   reg) . dropToIndexBad (regionStart reg)
