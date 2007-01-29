-- This module defines the Region ADT

module Yi.Region (Region, mkRegion, mkVimRegion, regionStart, regionEnd) where

import Yi.Buffer (Point)

-- | The region data type. 
--The region is semi open: it includes the start but not the end bound. This allows simpler region-manipulation algorithms.
-- Invariant : regionStart r <= regionEnd r
data Region = Region {regionStart, regionEnd :: !Point} 

-- | Construct a region from its bounds, vim style:
-- the right bound in included.
mkVimRegion :: Point -> Point -> Region
mkVimRegion x y = if x < y then Region x (y+1) else Region y (x+1)


-- | Construct a region from its bounds, emacs style:
-- the right bound is excluded
mkRegion :: Point -> Point -> Region
mkRegion x y = if x < y then Region x y else Region y x


