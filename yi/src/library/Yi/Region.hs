{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Copyright (C) 2008 JP Bernardy

-- | This module defines the Region ADT

module Yi.Region
  (
   Region
  , emptyRegion
  , regionIsEmpty
  , mkRegion, mkRegion', mkSizeRegion
  , regionStart
  , regionEnd
  , regionSize
  , regionDirection
  , inRegion, nearRegion
  , includedRegion
  , fmapRegion
  , intersectRegion
  , unionRegion
  , regionFirst, regionLast, regionsOverlap
  )
where
import Yi.Buffer.Basic
import Yi.Utils
import Data.Typeable
import Data.Binary
#if __GLASGOW_HASKELL__ < 708
import Data.DeriveTH
#else
import GHC.Generics (Generic)
#endif

#ifdef TESTING
import Test.QuickCheck
#endif

-- | The region data type.
--The region is semi open: it includes the start but not the end bound. This allows simpler region-manipulation algorithms.
-- Invariant : regionStart r <= regionEnd r
data Region = Region {regionDirection :: !Direction,
                      regionStart, regionEnd :: !Point}
                 deriving (Typeable)

#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''Region)
#else
deriving instance Generic Region
instance Binary Region
#endif

#ifdef TESTING
instance Arbitrary Region where
    arbitrary = sized $ \size -> do
        x0 :: Int <- arbitrary
        return $ mkRegion (fromIntegral x0) (fromIntegral (x0 + size))
#endif


instance Show Region where
    show r = show (regionStart r) ++
             (case regionDirection r of
               Forward -> " -> "
               Backward -> " <- "
             ) ++
             show (regionEnd r)

regionFirst :: Region -> Point
regionFirst (Region Forward p _) = p
regionFirst (Region Backward _ p) = p

regionLast :: Region -> Point
regionLast (Region Forward _ p) = p
regionLast (Region Backward p _) = p


fmapRegion :: (Point -> Point) -> Region -> Region
fmapRegion f (Region d x y) = Region d (f x) (f y)

regionSize :: Region -> Size
regionSize r = regionEnd r ~- regionStart r

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

mkSizeRegion :: Point -> Size -> Region
mkSizeRegion x s = mkRegion x (x +~ s)

-- | The empty region
emptyRegion :: Region
emptyRegion = Region Forward 0 0

-- | True if the given point is inside the given region.
inRegion :: Point -> Region -> Bool
p `inRegion` (Region _ start stop) = start <= p && p < stop

-- | True if the given point is inside the given region or at the end of it.
nearRegion :: Point -> Region -> Bool
p `nearRegion` (Region _ start stop) = start <= p && p <= stop

-- | Returns if a region (1st arg) is  included in another (2nd arg)
includedRegion :: Region -> Region -> Bool
r0 `includedRegion` r = regionStart r <= regionStart r0 && regionEnd r0 <= regionEnd r

regionIsEmpty :: Region -> Bool
regionIsEmpty (Region _ start stop) = start >= stop

regionsOverlap :: Bool -> Region -> Region -> Bool
regionsOverlap border (Region _ x1 y1) (Region _ x2 y2) =
    cmp x2 y1 y2 || cmp x2 x1 y2 ||
    cmp x1 y2 y1 || cmp x1 x2 y1
  where
    cmp a b c = a <= b && if border then b <=c  else b < c
