{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Yi.Prelude
    (
(<>),
(++), -- consider scrapping this and replacing it by the above
(=<<),
(<=<),
($!),
Double,
Binary,
Char,
Either(..),
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
Typeable,
commonPrefix,
void,
dummyPut,
dummyGet,
findPL,
focusA,
fromIntegral,
fst,
fst3,
groupBy',
list,
head,
init,
io,
last,
lookup,
mapAdjust',
mapAlter',
mapFromFoldable,
module Control.Applicative,
module Control.Category,
module Data.Accessor,
module Data.Accessor.Monad.MTL.State, putA, getA, modA,
module Data.Bool,
module Data.Foldable,
module Data.Function,
module Data.Int,
module Data.Rope,
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
swapFocus,
tail,
trd3,
undefined,
unlines,
when,
writeFile -- because Data.Derive uses it.
    ) where

import Prelude hiding (any, all)
import Yi.Debug
import Yi.Monad
import Text.Show
import Data.Bool
import Data.Binary
import Data.Foldable
import Data.Default
import Data.Function hiding ((.), id)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable(Hashable)
import Data.Int
import Data.Rope (Rope)
import Control.Category
import Control.Monad.Reader
import Control.Applicative hiding((<$))
import Data.Traversable
import Data.Typeable
import Data.Monoid (Monoid, mappend)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Monad.State.Class as CMSC
import qualified Data.Accessor.Basic as Accessor
import Data.Accessor ((<.), accessor, getVal, setVal, Accessor,(^.),(^:),(^=))
import qualified Data.Accessor.Monad.MTL.State as Accessor.MTL
import Data.Accessor.Monad.MTL.State ((%:), (%=))
import qualified Data.List.PointedList as PL

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

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


-- 'list' is the canonical list destructor as 'either' or 'maybe'.
list :: b -> (a -> [a] -> b) -> [a] -> b
list nil _    []     = nil
list _   cons (x:xs) = cons x xs

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


-- | Generalisation of 'Map.fromList' to arbitrary foldables.
mapFromFoldable :: (Foldable t, Ord k) => t (k, a) -> Map.Map k a
mapFromFoldable = foldMap (uncurry Map.singleton)

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


-- | Return the longest common prefix of a set of lists.
--
-- > P(xs) === all (isPrefixOf (commonPrefix xs)) xs
-- > length s > length (commonPrefix xs) --> not (all (isPrefixOf s) xs)
commonPrefix :: Eq a => [[a]] -> [a]
commonPrefix [] = []
commonPrefix strings
    | any null strings = []
    | all (== prefix) heads = prefix : commonPrefix tailz
    | otherwise = []
    where
          (heads, tailz) = unzip [(h,t) | (h:t) <- strings]
          prefix = head heads
-- for an alternative implementation see GHC's InteractiveUI module.

---------------------- PointedList stuff
-- | Finds the first element satisfying the predicate, and returns a zipper pointing at it.
findPL :: (a -> Bool) -> [a] -> Maybe (PL.PointedList a)
findPL p xs = go [] xs where
  go _  [] = Nothing
  go ls (f:rs) | p f    = Just (PL.PointedList ls f rs)
               | otherwise = go (f:ls) rs

focusA :: Accessor (PL.PointedList a) a
focusA = accessor getter setter where
  getter   (PL.PointedList _ x _) = x
  setter y (PL.PointedList x _ z) = PL.PointedList x y z

-- | Given a function which moves the focus from index A to index B, return a function which swaps the elements at indexes A and B and then moves the focus. See Yi.Editor.swapWinWithFirstE for an example.
swapFocus :: (PL.PointedList a -> PL.PointedList a) -> (PL.PointedList a -> PL.PointedList a)
swapFocus moveFocus xs = focusA ^= (xs ^. focusA) $ moveFocus $ focusA ^= (moveFocus xs ^. focusA) $ xs
----------------------
-- Acessor stuff

putA :: CMSC.MonadState r m => Accessor.T r a -> a -> m ()
putA = Accessor.MTL.set

getA :: CMSC.MonadState r m => Accessor.T r a -> m a
getA = Accessor.MTL.get

modA :: CMSC.MonadState r m => Accessor.T r a -> (a -> a) -> m ()
modA = Accessor.MTL.modify

-- | Write nothing. Use with 'dummyGet'
dummyPut :: a -> Put
dummyPut _ = return ()

-- | Read nothing, and return 'def'. Use with 'dummyPut'.
dummyGet :: Default a => Get a
dummyGet = return def

----------------- Orphan 'Binary' instances
instance (Eq k, Hashable k, Binary k, Binary v) => Binary (HashMap.HashMap k v) where
    put x = put (HashMap.toList x)
    get = HashMap.fromList <$> get

