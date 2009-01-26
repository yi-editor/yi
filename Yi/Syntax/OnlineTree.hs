{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeFamilies #-}
module Yi.Syntax.OnlineTree (Tree(..), manyToks, dropToIndex, dropToIndex',
                             dropToIndexBad, tokAtOrBefore, 
                             manyT, sepByT, MaybeOneMore(..), TreeAtPos) where
import Prelude ()
import Yi.Prelude hiding (trace)
-- import Debug.Trace
import Yi.IncrementalParse
import Control.Applicative
import Data.Traversable
import Data.Foldable
import Data.List (takeWhile, reverse)
import Data.Maybe (listToMaybe)
import Yi.Lexer.Alex
import Yi.Buffer.Basic (Point)
import Yi.Region
import Yi.Syntax.Tree
-- #ifdef TESTING
import Test.QuickCheck 
import Data.List (isSuffixOf, dropWhile, length, drop, take, zipWith, scanl)
import Yi.Buffer.Basic
instance Arbitrary Point where
    arbitrary = Point  <$> arbitrary
-- #endif


data TreeAtPos a = TreeAtPos Point (Tree a)
    deriving Show

data MaybeOneMore f x = None | OneMore x (f x)
    deriving Show

data Tree a = Node a (Tree a) (Tree a)
            | Leaf
              deriving Show

shapeSig Leaf = "."
shapeSig (Node _ l r) = "(" ++ shapeSig l ++ shapeSig r ++ ")"

instance Traversable f => Traversable (MaybeOneMore f) where
    traverse _ None = pure None
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

instance SubTree (TreeAtPos (Tok a)) where
    type Element (TreeAtPos (Tok a)) = Tok a
    foldMapToksAfter begin f t = foldMap f $ dropToIndex begin tokEnd t

case_ :: (s -> Bool) -> P s a -> P s a -> P s a
case_ f true false = Look false (\s -> if f s then true else false)

symbolBefore :: Point -> (Tok t) -> Bool
symbolBefore p x = tokBegin x <= p



factor :: Int
factor = 2  

initialLeftSize :: Size
initialLeftSize = 1
-- TODO: increase to a reasonable value. Since files are often > 1024 bytes, this
-- seems a reasonable start.

manyToks = TreeAtPos 0 <$> manyToks' -- FIXME: read the current position!

manyToks' :: P (Tok t) (Tree (Tok t))
manyToks' = parse' initialLeftSize 0 maxBound
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

manyT :: Show t =>P (Tok t) x -> P (Tok t) (TreeAtPos x)
manyT p = lookNext >>= \x -> case x of
    Nothing -> pure (TreeAtPos 0 Leaf)
    Just t -> let b = tokBegin t 
               in (TreeAtPos b) <$> parse'' initialLeftSize b maxBound p


sepByT :: Show t =>P (Tok t) x -> P (Tok t) y -> P (Tok t) (MaybeOneMore TreeAtPos x)
sepByT p q = pure None <|> OneMore <$> p <*> manyT (q *> p)


      
-- | Parse all the elements starting in the interval [lb, rb[
parse'' :: Show t =>Size -> Point -> Point -> P (Tok t) x -> P (Tok t) (Tree x)
parse'' leftSize lB rB p 
          | rB <= lB = pure Leaf
          | otherwise = 
  case_ symbolIn
       ((Node <$> p
              <*> parse'' initialLeftSize                   lB   midB  p
              <*> parse'' (leftSize * fromIntegral factor)  midB rB    p)
        <|> pure Leaf -- This creates a branch which cannot be discarded without parsing some of the future.
       )
       (pure Leaf) 
  where midB = min rB (lB +~ leftSize)
        symbolIn x = -- trace (show leftSize ++ "|" ++ show lB ++ "<" ++ show x ++ "<" ++ show rB ++ "=" ++ show result) $ 
                     result
            where result = (lB <= tokBegin x) && (tokBegin x < rB)

       


toEndo :: Tree a -> E [a]
toEndo Leaf = id
toEndo (Node x l r) = (x :) . toEndo l . toEndo r

dropToIndexBad :: Point -> Tree t -> [t]
dropToIndexBad index t = -- trace ("tt=" ++ shapeSig t ++ " index=" ++ show index) $ 
                         dropHelp initialLeftSize t index []

dropHelp :: Size -> Tree a -> Point -> [a] -> [a]
dropHelp _leftsize Leaf _n = id
dropHelp leftsize (Node x l r) index
    | fromIntegral index <  leftsize = (x :) . dropHelp initialLeftSize     l index . toEndo  r
    -- the head is speculatively put in the result; but it does not matter, since we
-- add only O(log n) elements this way.
    | otherwise         = (x :) . dropHelp (leftsize * fromIntegral factor) r  (index -~ leftsize)


type E a = a -> a

dropToIndex' _index elementEnd None = []
dropToIndex' index  elementEnd (OneMore x xs) = x : dropToIndex index elementEnd xs

dropToIndex :: Point ->(t -> Point) -> TreeAtPos t -> [t]
dropToIndex index elementEnd (TreeAtPos begin t) = --trace ("t=" ++ shapeSig t ++ " index=" ++ show index ++ " begin = " ++ show begin ) $
  dropTo elementEnd t (index - begin) 

-- | As dropHelp, but ensure we also put the element "just before" into result as well.

-- We need this because we constructed the tree in such a way that the elements
-- begin after a given bound. So, if we drop stuff before, we might drop a node
-- that indeed starts before the bound, but ends after it. So, we must keep one
-- extra element before.

xSz :: forall a. Show a => (a -> Point) ->Tree a -> Point -> Int
xSz elEnd tree index0 = extraSz initialLeftSize tree index0 id
  where
     extraSz _leftsize Leaf index previous = length extra
         where extra =  (takeWhile ((index0 <) . elEnd) (previous []))
     extraSz leftsize (Node x l r) index previous
         | fromIntegral index <  leftsize = -- trace (params ++ "left") $
                       extraSz initialLeftSize                  l index               ((x:) . previous) 
         | otherwise = extraSz (leftsize * fromIntegral factor) r (index -~ leftsize) (toReverseList l . (x:) . previous)
       where lastElem t = case toReverseList t [] of
                  [] -> id
                  (y:_) -> (y :)
             params = "x="++show x ++ " ls=" ++ show leftsize ++ " ix=" ++ show index ++ " -->"


dropTo :: forall a. (a -> Point) ->Tree a -> Point -> [a]
dropTo elEnd tree index0 = dropButHelp initialLeftSize  tree index0 id []
   where dropButHelp :: Size -> Tree a -> Point -> E [a] ->E [a]
         dropButHelp _leftsize Leaf index previous =  -- we may have forgotten to insert the previous element here, so we add it.
         
                -- trace ("adding previous : " ++ show (length extra)) $
                (reverse extra ++) -- FIXME: take head
             where extra =  takeWhile ((index0 <) . elEnd) (previous [])
         dropButHelp leftsize (Node x l r) index previous
             -- Note that we do not know if the head is to the left or to the right of the middle.
             -- So, the head is speculatively put in the result; but it does not matter, since we
             -- add only O(log n) elements this way.
         
             -- go to the left
             -- here the previous element was the head, so we do not need to do add any other previous element.
             | fromIntegral index <  leftsize = -- trace (params ++ "left") $
                           dropButHelp initialLeftSize                  l index               ((x:) . previous)     . toEndo  r
             -- go to the right.
             -- we also put the last element of the left branch /if it exists/. If it does not exist,
             -- we know we have kept the "just previous element": it was the head.
             | otherwise = -- trace (params ++ "right") $
                           dropButHelp (leftsize * fromIntegral factor) r (index -~ leftsize) (toReverseList l . (x:) . previous)
           where lastElem t = case toReverseList t [] of
                      [] -> id
                      (y:_) -> (y :)
                 -- params = "x="++show x ++ " ls=" ++ show leftsize ++ " ix=" ++ show index ++ " -->"

toReverseList :: Tree a -> E [a]
toReverseList Leaf = id
toReverseList (Node a l r) = toReverseList r . toReverseList l . (a:)


-- debug.
type DebugEl = (Point,Point)
toTree' :: Size -> Point -> Point -> [DebugEl] -> (Tree DebugEl, [DebugEl])
toTree' _ _ _ [] = (Leaf, [])
toTree' leftSize lB rB (n@(x,_):xs) 
    | lB <= x && x < rB =
                  let
                      (l,xs')  = toTree' initialLeftSize     lB midB              xs
                      (r,xs'') = toTree' (leftSize * fromIntegral factor) midB rB xs'
                      midB = min rB (lB +~ leftSize)
                  in (Node n l r, xs'')
    | otherwise = (Leaf, n:xs)

toTree = fst . toTree' initialLeftSize 0 maxBound -- where maxBound stands for infinity.

checkTree :: (a -> Point) -> (Tree a) -> Bool
checkTree start root = ct initialLeftSize 0 maxBound root
    where ct leftSize lB rB (Node x l r) = lB <= start x && start x < rB
                                  && ct initialLeftSize lB midB l
                                  && ct (leftSize * fromIntegral factor) midB rB r
              where midB = min rB (lB +~ leftSize)
          ct _ _ _ Leaf = True

both f (x,y) = (f x, f y)

-- | Generate a list of @n@ non-overlapping things
anyList 0 = return [(0,1)]
anyList n = do h <-choose (1,100)
               t <- anyList (n-1)
               return $ (0,h) : fmap (both (+(h+1))) t
    
shrinkList l = concat [[drop (n+1) l, take n l] | n <- shrinkIntegral n]
    where n = length l


prop_dropBut' = 
       forAll (choose (1::Int,100)) $ \n -> 
       forAllShrink (anyList n) shrinkList $ \l0 ->let m = snd $ last l0; l = fmap (both Point) l0 in
       forAllShrink (choose (0,m)) shrinkIntegral $ \i0 ->let i = Point i0 in
                                         
       dropWhile ((<= i) . snd) l `isSuffixOf` dropTo snd (toTree l) i

prop_notToManyExtra = 
       forAll (anyList 100) $ \l0 ->let m = snd $ last l0; l = fmap (both Point) l0 in
       forAllShrink (choose (0,m)) shrinkIntegral $ \i0 ->let i = Point i0 in
       60 > xSz snd (toTree l) i


testTree = toTree [(542,551),(552,553)]
test = dropTo snd testTree 1

qc = quickCheck prop_dropBut'
qc2 = quickCheck prop_notToManyExtra

testTree2 = toTree [(0,4),(5,9),(10,17),(18,28),(29,32),(33,36),(37,46),(47,52),(53,57),(58,66),(67,74),(75,83),(84,89),(90,96),(97,107),(108,113),(114,119),(120,125),(126,130),(131,135),(136,139),(140,146),(147,150),(151,152),(153,154),(155,162),(163,173),(174,183),(184,193),(194,203),(204,208),(209,214),(215,221),(222,231),(232,234),(235,244),(245,250),(251,254),(255,256),(257,264),(265,274),(275,283),(284,292),(293,296),(297,307),(308,318),(319,321),(322,323),(324,330),(331,335),(336,344),(345,353),(354,357),(358,368),(369,370),(371,377),(378,379),(380,386),(387,390),(391,401),(402,412),(413,418),(419,429),(430,434),(435,440),(441,444),(445,451),(452,453),(454,464),(465,467),(468,471),(472,482),(483,485),(486,490),(491,494),(495,500),(501,503),(504,505),(506,511),(512,517),(518,527),(528,529),(530,532),(533,535),(536,537),(538,541),(542,550),(551,552),(553,560),(561,565),(566,568),(569,576),(577,587),(588,590),(591,601),(602,610),(611,614),(615,622),(623,628),(629,633),(634,635)]


test2 = dropTo snd (testTree2) 382 