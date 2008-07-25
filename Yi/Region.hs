{-# LANGUAGE DeriveDataTypeable #-}
-- Copyright (C) 2008 JP Bernardy

-- | This module defines the Region ADT

module Yi.Region
  (
   Region
  , emptyRegion
  , mkRegion, mkRegion'
  , regionStart
  , regionEnd
  , regionSize
  , regionDirection
  , inRegion, nearRegion
  , fmapRegion
  , intersectRegion
  , unionRegion
  )
where
import Yi.Buffer.Basic
import Data.Typeable
import Yi.Prelude
import Prelude ()

-- | The region data type. 
--The region is semi open: it includes the start but not the end bound. This allows simpler region-manipulation algorithms.
-- Invariant : regionStart r <= regionEnd r
data Region = Region {regionDirection :: !Direction,
                      regionStart, regionEnd :: !Point} 
                 deriving (Typeable)

instance Show Region where
    show r = show (regionStart r) ++ 
             (case regionDirection r of
               Forward -> " -> " 
               Backward -> " <- " 
             ) ++ 
             show (regionEnd r)

fmapRegion :: (Point -> Point) -> Region -> Region
fmapRegion f (Region d x y) = Region d (f x) (f y)

regionSize :: Region -> Size
regionSize r = regionEnd r ~- regionStart r

mixDirections :: Direction -> Direction -> Direction
mixDirections Backward Backward = Backward
mixDirections _ _ = Forward

-- | Take the intersection of two regions
intersectRegion :: Region -> Region -> Region
intersectRegion (Region _ x1 y1) (Region _ x2 y2) = ordRegion (max x1 x2) (min y1 y2)

-- | Take the union of two regions (including what is between them)
unionRegion :: Region -> Region -> Region
unionRegion (Region _ x1 y1) (Region _ x2 y2) = mkRegion (min x1 x2) (max y1 y2)


-- | Create a region from ordered bounds. If 2nd argument is greater than
-- 1st, then the region will be empty.
ordRegion :: Point -> Point -> Region
ordRegion x y = if x < y then Region Forward x y else emptyRegion

-- | Construct a region from its bounds, emacs style:
-- the right bound is excluded
mkRegion :: Point -> Point -> Region
mkRegion x y = if x <= y then Region Forward x y else Region Backward y x

mkRegion' :: Direction -> Point -> Point -> Region
mkRegion' d x y = if x <= y then Region d x y else Region d y x

-- | The empty region
emptyRegion :: Region
emptyRegion = Region Forward 0 0 

-- | True if the given point is inside the given region.
inRegion :: Point -> Region -> Bool
p `inRegion` (Region _ start stop) = start <= p && p < stop

-- | True if the given point is inside the given region or at the end of it.
nearRegion :: Point -> Region -> Bool
p `nearRegion` (Region _ start stop) = start <= p && p <= stop




