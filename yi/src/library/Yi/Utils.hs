{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
-- Unique code from Yi.Prelude

module Yi.Utils where

import Data.Binary
import Data.Char (toLower)
import Data.Foldable hiding (all,any)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable(Hashable)
import Control.Monad.Base
import Control.Applicative
import Control.Lens hiding (cons)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List.PointedList as PL
import qualified Language.Haskell.TH.Syntax as THS

io :: MonadBase IO m => IO a -> m a
io = liftBase

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

{-# ANN nubSet "HLint: ignore Eta reduce" #-}
-- TODO: move somewhere else.
-- | As 'Prelude.nub', but with O(n*log(n)) behaviour.
nubSet :: (Ord a) => [a] -> [a]
nubSet xss = f Set.empty xss
  where
      f _ [] = []
      f s (x:xs) = if x `Set.member` s then f s xs else x : f (Set.insert x s) xs

-- | As Map.adjust, but the combining function is applied strictly.
mapAdjust' :: (Ord k) => (a -> a) -> k -> Map.Map k a -> Map.Map k a
mapAdjust' f = Map.alter f' where
    f' Nothing = Nothing
    f' (Just x) = let x' = f x in x' `seq` Just x'
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

{-# ANN findPL "HLint: ignore Eta reduce" #-}
---------------------- PointedList stuff
-- | Finds the first element satisfying the predicate, and returns a zipper pointing at it.
findPL :: (a -> Bool) -> [a] -> Maybe (PL.PointedList a)
findPL p xs = go [] xs where
  go _  [] = Nothing
  go ls (f:rs) | p f    = Just (PL.PointedList ls f rs)
               | otherwise = go (f:ls) rs

{-# ANN swapFocus "HLint: ignore Redundant bracket" #-}
-- | Given a function which moves the focus from index A to index B, return a function which swaps the elements at indexes A and B and then moves the focus. See Yi.Editor.swapWinWithFirstE for an example.
swapFocus :: (PL.PointedList a -> PL.PointedList a) -> (PL.PointedList a -> PL.PointedList a)
swapFocus moveFocus xs =
    let xs' = moveFocus xs
        f1  = view PL.focus xs
        f2  = view PL.focus xs'
    in set PL.focus f1 . moveFocus . set PL.focus f2 $ xs

----------------- Orphan 'Binary' instances
instance (Eq k, Hashable k, Binary k, Binary v) => Binary (HashMap.HashMap k v) where
    put x = put (HashMap.toList x)
    get = HashMap.fromList <$> get

makeClassyWithSuffix :: String -> THS.Name -> THS.Q [THS.Dec]
makeClassyWithSuffix s = makeLensesWith (classyRules
  & lensField .~ (\_ n -> addSuffix n s)
  & lensClass .~ classy)
  where
    classy :: THS.Name -> Maybe (THS.Name, THS.Name)
    classy n = case THS.nameBase n of
      x:xs -> Just (THS.mkName ("Has" ++ x:xs),
                    THS.mkName (toLower x : xs ++ s))
      []   -> Nothing

addSuffix :: THS.Name -> String -> [DefName]
addSuffix n s = [TopName $ THS.mkName $ THS.nameBase n ++ s]

makeLensesWithSuffix :: String -> THS.Name -> THS.Q [THS.Dec]
makeLensesWithSuffix s =
  makeLensesWith (fieldRules & lensField .~ (\_ n -> addSuffix n s))
