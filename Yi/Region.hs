-- This module defines the Region ADT

module Yi.Region (Region, mkRegion, mkVimRegion, regionStart, regionEnd) where

import Yi.Buffer (Point)

-- | The region data type.
-- Invariant 
data Region = Region {regionStart, regionEnd :: !Point} 

-- | Construct a region from its bounds, vim style:
-- the right bound in included.
mkVimRegion :: Point -> Point -> Region
mkVimRegion x y = if x < y then Region x y else Region y x


-- | Construct a region from its bounds, emacs style:
-- the right bound is excluded
mkRegion :: Point -> Point -> Region
mkRegion x y = if x < y then Region x (y-1) else Region y (x-1)


