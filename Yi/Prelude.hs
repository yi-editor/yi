{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Yi.Prelude 
    (
(++),
(=<<),
Double,
Char,
Either(..),
Endom,
Eq(..),
Fractional(..),
Functor(..),
IO,
Integer,
Integral(..),
Bounded(..),
Enum(..),
Maybe(..),
Monad(..),
Num(..),
Ord(..),
Read(..),
Real(..),
RealFrac(..),
ReaderT(..),
SemiNum(..),
String,
every,
fromIntegral,
fst,
fst3,
groupBy',
head,
init,
io,
last,
lookup,
mapAdjust',
mapAlter',
module Control.Applicative,
module Control.Category,
module Data.Accessor,
module Data.Bool,
module Data.Foldable,
module Data.Function,
module Data.Int,
module Data.Traversable,
module Text.Show,
module Yi.Debug,
module Yi.Monad,
nubSet,
null,
print,
putStrLn,
replicate,
read,
seq,
singleton,
snd,
snd3,
tail,
trd3,
undefined,
unlines,
when,
writeFile -- because Data.Derive uses it.
    ) where

import Yi.Debug
import Yi.Monad
import Data.Accessor
import qualified Data.Accessor.Basic
import Text.Show
import Data.Bool
import Data.Foldable
import Data.Function hiding ((.), id)
import Data.Int
import Control.Category
import Control.Monad.Reader
import Control.Applicative
import Control.Category
import Data.Traversable 
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map
    
type Endom a = a -> a

io :: MonadIO m => IO a -> m a
io = liftIO

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

trd3 :: (a,b,c) -> c
trd3 (_,_,x) = x

class SemiNum absolute relative | absolute -> relative where
    (+~) :: absolute -> relative -> absolute
    (-~) :: absolute -> relative -> absolute
    (~-) :: absolute -> absolute -> relative

singleton :: a -> [a]
singleton x = [x]

-- TODO: move somewhere else.
-- | As 'Prelude.nub', but with O(n*log(n)) behaviour.
nubSet :: (Ord a) => [a] -> [a]
nubSet xss = f Set.empty xss where
       f _ [] = []
       f s (x:xs) = if x `Set.member` s then f s xs else x : f (Set.insert x s) xs

-- | As Map.adjust, but the combining function is applied strictly.
mapAdjust' :: (Ord k) => (a -> a) -> k -> Map.Map k a -> Map.Map k a
mapAdjust' f = Map.alter f' where
    f' Nothing = Nothing
    f' (Just x) = let x' = f x in x' `seq` Just x'
    -- This works because Map is structure-strict, and alter needs to force f' to compute
    -- the structure.


-- | As Map.alter, but the newly inserted element is forced with the map.
mapAlter' :: Ord k => (Maybe a -> Maybe a) -> k -> Map.Map k a -> Map.Map k a
mapAlter' f = Map.alter f' where
    f' arg = case f arg of 
        Nothing -> Nothing
        Just x -> x `seq` Just x
    -- This works because Map is structure-strict, and alter needs to force f' to compute
    -- the structure.



-- | Alternative to groupBy.
--
-- > groupBy' (\a b -> abs (a - b) <= 1) [1,2,3] = [[1,2,3]]
--
-- whereas
--
-- > groupBy (\a b -> abs (a - b) <= 1) [1,2,3] = [[1,2],[3]]
--
-- TODO: Check in ghc 6.12 release if groupBy == groupBy'.
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' p l = s1 : groupBy' p s2 where
    (s1, s2) = chain p l

chain :: (a -> a -> Bool) -> [a] -> ([a],[a])
chain _ [] = ([], [])
chain _ [e] = ([e], [])
chain q (e1 : es@(e2 : _))
    | q e1 e2 = let (s1, s2) = chain q es in (e1 : s1, s2)
    | otherwise = ([e1], es)

----------------------
-- Accessors support

instance Category Data.Accessor.Basic.T where
    id = Data.Accessor.Basic.self
    (.) = (<.)

-- | Lift an accessor to a traversable structure. (This can be seen as a
-- generalization of fmap)
every :: Traversable t => Accessor whole part -> Accessor (t whole) (t part)
every a = accessor (fmap (getVal a)) (\parts wholes -> zipWithT (setVal a) parts wholes)

-- | zipWith, generalized to Traversable structures.
zipWithT :: Traversable t => (a -> b -> c) -> t a -> t b -> t c
zipWithT f ta tb = result
  where step []     _ = Yi.Debug.error "zipT: non matching structures!"
        step (b:bs) a = (bs,f a b)
        ([], result) = mapAccumL step (toList tb) ta
