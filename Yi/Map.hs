
{-# OPTIONS -cpp #-}
-- 
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
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
-- Compatibility code between Data.FiniteMap and Data.Map
-- We use the function names from Data.Map.
--
module Yi.Map (
#if __GLASGOW_HASKELL__ >= 604
        module Data.Map,
#else
        Map,
        empty, insert, delete, lookup, fromList, size, elems
#endif
  ) where

import Prelude hiding (lookup)
#if __GLASGOW_HASKELL__ >= 604
import Data.Map
#else
--
-- compatibility code for deprecated FiniteMap
--
import Prelude hiding (lookup)
import qualified Data.FiniteMap as FM

type Map k a = FM.FiniteMap k a

empty  :: Map k a
empty  = FM.emptyFM

insert :: Ord k => k -> a -> Map k a -> Map k a
insert = \k e m -> FM.addToFM m k e

delete :: Ord k => k -> Map k a -> Map k a
delete = flip FM.delFromFM

lookup :: Ord k => k -> Map k a -> Maybe a
lookup = flip FM.lookupFM

fromList :: Ord k => [(k,a)] -> Map k a
fromList = FM.listToFM

size :: Map k a -> Int
size = FM.sizeFM

elems :: Map k a -> [a]
elems = FM.eltsFM

#endif
