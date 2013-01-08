{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, Rank2Types #-}

module Yi.Prelude 
    (
id,
(.),
(<>),
(++), -- consider scrapping this and replacing it by the above
(=<<),
($!),
Double,
Binary,
Char,
Either(..),
Endom,
Eq(..),
Fractional(..),
Functor(..),
IO,
Initializable(..),
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
discard,
dummyPut,
dummyGet,
every,
findPL,
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

Lens',
ALens',
Getting,
Setting,
(^.),
view,
set,
use,
to,
(.=),
(%=),
(%~),
mapped,
makeLensesWithSuffix,
makeLenses,

-- deprecated
(^:),
(^=),
getVal,
setVal,
putA, getA, modA,
Accessor,
accessor,
fromSetGet,
mapDefault,

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
import Data.Function hiding ((.), id)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable(Hashable)
import Data.Int
import Data.Rope (Rope)
import Control.Lens hiding (Accessor, focus, (^=))
import Control.Monad.Reader
import Control.Applicative hiding((<$))
import Data.Traversable 
import Data.Typeable
import Data.Monoid (Monoid, mappend)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Monad.State.Class as CMSC
import Data.List.PointedList (PointedList(PointedList), focus)

type Accessor s a = Lens' s a
{-# DEPRECATED Accessor "use 'Lens'' instead" #-}

accessor :: (s -> a) -> (b -> s -> t) -> IndexPreservingLens s t a b
accessor getter setter = lens getter (flip setter)
{-# DEPRECATED accessor "use 'lens' or something even better" #-}

fromSetGet :: (b -> s -> t) -> (s -> a) -> IndexPreservingLens s t a b
fromSetGet setter getter = lens getter (flip setter)
{-# DEPRECATED fromSetGet "use 'lens' or something even better" #-}

type Endom a = a -> a


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

discard :: Functor f => f a -> f ()
discard = fmap (const ())

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

-- | Lift an accessor to a traversable structure. (This can be seen as a
-- generalization of fmap)
every :: Traversable t => Lens' whole part -> Lens' (t whole) (t part)
every a = lens (fmap (view a)) (flip (zipWithT (set a)))

-- | zipWith, generalized to Traversable structures.
zipWithT :: Traversable t => (a -> b -> c) -> t a -> t b -> t c
zipWithT f ta tb = result
  where step []     _ = Yi.Debug.error "zipT: non matching structures!"
        step (b:bs) a = (bs,f a b)
        ([], result) = mapAccumL step (toList tb) ta

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
findPL :: (a -> Bool) -> [a] -> Maybe (PointedList a)
findPL p xs = go [] xs where
  go _  [] = Nothing
  go ls (f:rs) | p f    = Just (PointedList ls f rs)
               | otherwise = go (f:ls) rs

-- | Given a function which moves the focus from index A to index B, return a function which swaps the elements at indexes A and B and then moves the focus. See Yi.Editor.swapWinWithFirstE for an example.
swapFocus :: (PointedList a -> PointedList a) -> (PointedList a -> PointedList a)
swapFocus moveFocus xs = xs & focus .~ (moveFocus xs^.focus)
                            & moveFocus
                            & focus .~ xs^.focus

----------------------
-- Acessor stuff

makeLensesWithSuffix s =
  makeLensesWith (defaultRules & lensField .~ Just . (++s))

putA :: CMSC.MonadState s m => ASetter s s a b -> b -> m ()
putA = (.=)
{-# DEPRECATED putA "use '.=' instead" #-}

getA :: CMSC.MonadState s m => Getting a s t a b -> m a
getA = use
{-# DEPRECATED getA "use 'use' instead" #-}

modA :: CMSC.MonadState s m => ASetter s s a b -> (a -> b) -> m ()
modA = (%=)
{-# DEPRECATED modA "use '%=' instead" #-}

getVal :: MonadReader s m => Getting a s t a b -> m a
getVal = view
{-# DEPRECATED getVal "use 'view' or '^.' instead" #-}

infixr 5 ^=, ^:

(^:) :: ASetter s t a b -> (a -> b) -> s -> t
(^:) = (%~)
{-# DEPRECATED (^:) "use '%~' instead" #-}

(^=) :: ASetter s t a b -> b -> s -> t
(^=) = (.~)
{-# DEPRECATED (^=) "use '.~' or 'set' instead" #-}

setVal :: ASetter s t a b -> b -> s -> t
setVal = (.~)
{-# DEPRECATED setVal "use '.~' or 'set' instead" #-}

mapDefault :: Ord key => elem -> key -> Accessor (Map.Map key elem) elem
mapDefault deflt key =
  fromSetGet (Map.insert key) (Map.findWithDefault deflt key)

-------------------- Initializable typeclass
-- | The default value. If a function tries to get a copy of the state, but the state
--   hasn't yet been created, 'initial' will be called to supply *some* value. The value
--   of initial will probably be something like Nothing,  \[\], \"\", or 'Data.Sequence.empty' - compare 
--   the 'mempty' of "Data.Monoid".
class Initializable a where
    initial :: a

instance Initializable (Maybe a) where
    initial = Nothing

-- | Write nothing. Use with 'dummyGet'
dummyPut :: a -> Put
dummyPut _ = return ()

-- | Read nothing, and return 'initial'. Use with 'dummyPut'.
dummyGet :: Initializable a => Get a
dummyGet = return initial

----------------- Orphan 'Binary' instances
instance (Eq k, Hashable k, Binary k, Binary v) => Binary (HashMap.HashMap k v) where
    put x = put (HashMap.toList x)
    get = HashMap.fromList <$> get

