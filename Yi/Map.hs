{-# OPTIONS -cpp #-}
-- 
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- 
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
-- 

--
-- | Compatibility code between Data.FiniteMap and Data.Map We use the
-- function names from Data.Map.
--
module Yi.Map (
#if __GLASGOW_HASKELL__ >= 604
        module Data.Map,
        addList,
#else
        Map,
        empty,
        insert,
        insertWith,
        delete,
        update,
        lookup,
        toList,
        fromList,
        addList,
        size,
        elems,
        singleton,
        member,
        keys,
        assocs,

        mapWithKey,
        filterWithKey,
        foldWithKey,
#endif
  ) where

import Prelude hiding (lookup)

#if __GLASGOW_HASKELL__ >= 604
import Data.Map

addList :: (Ord k) => [(k,a)] -> Map k a -> Map k a
addList l m = union (fromList l) m

#else
--
-- compatibility code for deprecated FiniteMap
--
import Prelude hiding (lookup)
import qualified Data.FiniteMap as FM

type Map = FM.FiniteMap

instance Functor (Map k) where
  fmap = FM.mapFM . const

empty  :: Map k a
empty  = FM.emptyFM

singleton :: k -> a -> Map k a
singleton = FM.unitFM

insert :: Ord k => k -> a -> Map k a -> Map k a
insert = \k e m -> FM.addToFM m k e

-- | This function is pure evil. Avoid it if possible.
insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f k e m = FM.addToFM_C (flip f) m k e

delete :: Ord k => k -> Map k a -> Map k a
delete = flip FM.delFromFM

update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f k m = case lookup k m of
  Nothing -> m
  Just v  -> case f v of
    Nothing -> delete k m
    Just v' -> insert k v' m

lookup :: Ord k => k -> Map k a -> Maybe a
lookup = flip FM.lookupFM

fromList :: Ord k => [(k,a)] -> Map k a
fromList = FM.listToFM

toList :: Map k a -> [(k,a)]
toList = FM.fmToList

size :: Map k a -> Int
size = FM.sizeFM

elems :: Map k a -> [a]
elems = FM.eltsFM

member :: Ord k => k -> Map k a -> Bool
member = FM.elemFM

keys  :: Map k a -> [k]
keys = FM.keysFM

assocs :: Map k a -> [(k, a)]
assocs = FM.fmToList

addList :: (Ord k) => [(k, a)] -> Map k a -> Map k a
addList = flip FM.addListToFM

-- delListFromFM = \fm keys -> foldl delete fm keys

-- Posted by Gracjan Polak on haskell-cafe@
-- note that we want the mapping the other way around.
--
-- deleteList list map = foldl (flip Data.Map.delete) map list
-- insertList asclist map = union map (Data.Map.fromList asclist)
--

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey = FM.mapFM

-- map f m == mapFM (const f)

filterWithKey :: Ord k => (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey = FM.filterFM

foldWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldWithKey = FM.foldFM

#endif
