{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeFamilies, CPP, NoMonomorphismRestriction #-}
module Yi.Syntax.OnlineTree (Tree(..), manyToks, 
                             tokAtOrBefore, dropToBut',
                             manyT, sepByT, MaybeOneMore(..), TreeAtPos) where
import Prelude ()
import Yi.Prelude -- hiding (trace)
-- import Debug.Trace
import Yi.IncrementalParse
import Control.Applicative
import Data.Traversable
import Data.Foldable
import Data.List (takeWhile, reverse, iterate)
import Data.Maybe (listToMaybe, maybeToList)
import Yi.Lexer.Alex
import Yi.Buffer.Basic (Point)
import Yi.Region
import Yi.Syntax.Tree
import Data.Monoid

#ifdef TESTING
import Test.QuickCheck 
import Parser.Incremental
#endif

import Data.List (isSuffixOf, dropWhile, length, drop, take, zipWith, zip, zip3, scanl)
import Yi.Buffer.Basic

#ifdef TESTING
instance Arbitrary Point where
    arbitrary = Point  <$> arbitrary
#endif

data TreeAtPos a = TreeAtPos Point [Tree a]
    deriving Show

data MaybeOneMore f x = None | OneMore x (f x)
    deriving Show

data Tree a = Bin (Tree a) (Tree a)
            | Leaf a
            | Tip
              deriving Show    


-- shapeSig Leaf = "."
-- shapeSig (Node _ l r) = "(" ++ shapeSig l ++ shapeSig r ++ ")"

instance Traversable f => Traversable (MaybeOneMore f) where
    traverse _ None = pure None
    traverse f (OneMore x xs) = OneMore <$> f x <*> traverse f xs

instance Traversable f => Foldable (MaybeOneMore f) where foldMap = foldMapDefault
instance Traversable f => Functor (MaybeOneMore f) where fmap = fmapDefault

instance Traversable TreeAtPos where
    traverse f (TreeAtPos s t) = TreeAtPos s <$> traverse (traverse f) t

instance Foldable TreeAtPos where foldMap = foldMapDefault
instance Functor TreeAtPos where fmap = fmapDefault

instance Traversable Tree where
    traverse f (Bin l r) = Bin <$> traverse f l <*> traverse f r
    traverse f (Leaf a) = Leaf <$> f a
    traverse _ Tip = pure Tip



instance Foldable Tree where 
    foldMap f Tip = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Bin l r) = foldMap f l <> foldMap f r
    -- foldMap = foldMapDefault

instance Functor Tree where
    fmap = fmapDefault

instance SubTree a =>SubTree (TreeAtPos a) where
    type Element (TreeAtPos a) = Element a
    foldMapToksAfter begin f t = foldMapToksAfter begin f (dropToBut begin t)
    foldMapToks f = foldMap (foldMapToks f)


case_ :: (s -> Bool) -> P s a -> P s a -> P s a
case_ f true false = Look false (\s -> if f s then true else false)

symbolBefore :: Point -> (Tok t) -> Bool
symbolBefore p x = tokBegin x <= p

symbolAt :: Point -> (Tok t) -> Bool
symbolAt p x = tokBegin x == p

manyToks = manyToks' tokBegin

-- TODO: scrap in favour of the below.
manyToks' :: (a -> Point) -> P a (TreeAtPos a)
manyToks' tokBegin = curPos >>= \p -> TreeAtPos p <$> topLvl p initialSize
    where 
        topLvl start size = lookNext >>= \s ->case s of
            Nothing -> pure []
            Just _ -> (:) <$> subTree start size <*> topLvl (start +~ size) (2 * size)

        subTree start 1 = case_ (\s ->tokBegin s == start)
              (Leaf <$> symbol (const True))
              (pure Tip)
        subTree start size = case_ (\s -> tokBegin s < end)
              (Bin <$> subTree startL subSize <*> subTree startR subSize)
              (pure Tip)
            where startL = start 
                  startR = startL +~ subSize
                  end = start +~ size
                  subSize = size `div` 2

        curPos = tB <$> lookNext
            where tB Nothing = maxBound
                  tB (Just x) = tokBegin x


manyT' :: (a -> Point) -> P a b -> P a (TreeAtPos b)
manyT' tokBegin element = curPos >>= \p -> TreeAtPos p <$> topLvl p initialSize
    where 
        topLvl start size = case_ (\s ->tokBegin s >= start)
                  ((:) <$> subTree start size <*> topLvl (start +~ size) (2 * size))
                  (pure [])

        subTree start 1 = case_ (\s ->tokBegin s == start)
              (Leaf <$> element  <|> pure Tip)
              (pure Tip)
        subTree start size = case_ (\s ->tokBegin s >= start && tokBegin s < end)
               (Bin <$> subTree startL subSize <*> subTree startR subSize)
               (pure Tip)
            where startL = start 
                  startR = startL +~ subSize
                  end = start +~ size
                  subSize = size `div` 2

        curPos = tB <$> lookNext
            where tB Nothing = maxBound
                  tB (Just x) = tokBegin x




curPos :: P (Tok t) Point
curPos = tB <$> lookNext
    where tB Nothing = maxBound
          tB (Just x) = tokBegin x

manyT0 :: P (Tok t) x -> P (Tok t) (TreeAtPos x)
manyT0 p = lookNext >>= \x -> case x of
    Nothing -> pure (TreeAtPos maxBound [])
    Just t -> fmap snd . toTree (tokBegin t) fst <$> many ((,) <$> curPos <*> p)

manyT = manyT' tokBegin

sepByT :: Show t =>P (Tok t) x -> P (Tok t) y -> P (Tok t) (MaybeOneMore TreeAtPos x)
sepByT p q = pure None <|> OneMore <$> p <*> manyT0 (q *> p)


initialSize = 1
-- TODO: increase to a reasonable value. Since files are often > 4096 bytes, this
-- seems a reasonable start.


sizes :: [Size]
sizes = take 30 $ iterate (*2) initialSize -- beyond 32, it overflows and makes toTree choke.

starts :: [Point]
starts = scanl (+~) 0 sizes

toTree :: forall a. Point -> (a ->Point) -> [a] -> TreeAtPos a
toTree point0 startPoint input = TreeAtPos point0 (snd $ mapAccumL toTree' input (zip sizes starts'))
    where 
        startOf x = startPoint x
        toTree' :: [a] ->(Size,Point) -> ([a], Tree a)
        toTree' [] _ = ([], Tip)
        toTree' input (0, _) = (input, Tip)
        toTree' input@(x:xs) (1, start) = if startOf x == start then (xs,Leaf x) else (input,Tip)
        toTree' input@(x:_) (size, start)
            | p >= end = (input, Tip)
            | otherwise = let (xs' ,l) = toTree' input  (subSize, startL)
                              (xs'',r) = toTree' xs'    (subSize, startR)
                          in -- trace ("Constructing at " ++ show start) 
                             (xs'', Bin l r)
            where p = startOf x
                  startL = start 
                  startR = startL +~ subSize
                  end = start +~ size
                  subSize = size `div` 2
        starts' = (point0 +) <$> starts

dropTo = selectTo False
takeTo = selectTo True

dropToBut :: Point ->TreeAtPos a ->[a]
dropToBut index t = (maybeToList $ getLast $ takeTo (Last . Just) index t)
                            ++ appEndo (dropTo (Endo . (:)) index t) []

dropToBut' _index None = []
dropToBut' index  (OneMore x xs) = x : dropToBut index xs


selectTo :: Monoid m =>Bool -> (a -> m) -> Point ->TreeAtPos a -> m 
selectTo toTheLeft f index (TreeAtPos point0 input) = -- trace ("selectTo: start=" ++ show point0) $
           mconcat $ fmap help $ zip3 sizes starts' input
    where help (size,start,t)
            | index <= start = before f t
            | end <= index = after f t
            | otherwise = case t of
                 Bin l r -> help (subSize, startL, l) <> help (subSize, startR, r)
                 _ -> mempty
             where
                   startL = start 
                   startR = startL +~ subSize
                   end = start +~ size
                   subSize = size `div` 2
          -- help0 x@(size,start,t) = trace ("help0 start=" ++ show start ++ " index=" ++ show index) $ help x
          starts' = preFilter $ fmap (point0 +) starts
          (preFilter,before,after) = if toTheLeft then (takeWhile (< index),\_ _ ->mempty,foldMap)
                                                  else (id,                 foldMap,\_ _ ->mempty)


#ifdef TESTING
both f (x,y) = (f x, f y)

-- | Generate a list of @n@ non-overlapping things
anyList 0 = return [(0,1)]
anyList n = do h <-choose (1,100)
               t <- anyList (n-1)
               return $ (0,h) : fmap (both (+(h+1))) t
    
shrinkList l = concat [[drop (n+1) l, take n l] | n <- shrinkIntegral n]
    where n = length l

prop_dropTo = 
       forAll (choose (1::Int,100)) $ \n -> 
       forAllShrink (anyList n) shrinkList $ \l0 ->not (null l0) ==>
              let m = snd $ last l0; l = fmap (both Point) l0 in
                  forAllShrink (choose (0,m)) shrinkIntegral $ \i0 ->let i = Point i0 in
                                                     
                  dropWhile ((< i) . fst) l == dropTo box i (toTree 0 fst l)

prop_dropToBut = 
       forAll (choose (1::Int,100)) $ \n -> 
       forAllShrink (anyList n) shrinkList $ \l0 ->not (null l0) ==>
              let m = snd $ last l0; l = fmap (both Point) l0 in
                  forAllShrink (choose (0,m)) shrinkIntegral $ \i0 ->let i = Point i0 in
                                                     
                  dropWhile ((<= i) . snd) l `isSuffixOf` dropToBut i (toTree 0 fst l)


prop_parse = 
       forAll (choose (1::Int,100)) $ \n -> 
       forAllShrink (anyList n) shrinkList $ \l0 ->not (null l0) ==>
              let m = snd $ last l0; l = fmap (both Point) l0 in
                  forAllShrink (choose (0,m)) shrinkIntegral $ \i0 ->let i = Point i0 in
                                                     
                  dropWhile ((<= i) . snd) l `isSuffixOf` (dropToBut i $ fst $ run (mkProcess $ manyT' fst (symbol $ const True) <* eof) $ l)



qc = quickCheck prop_parse

box x = [x]


t1 = toTree 0 fst $ [(408,420),(421,499)] ++ undefined
t2 = dropTo box 440 t1

t3 = dropToBut 5 $ fst $ run (mkProcess $ manyT' id (symbol $ const True) <* eof) $ [1..10]

#endif

