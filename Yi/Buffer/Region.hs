{-# LANGUAGE DeriveDataTypeable #-}
-- Copyright (C) 2008 JP Bernardy

-- * This module defines the Region ADT

module Yi.Buffer.Region 
  ( Region
  , emptyRegion
  , mkRegion
  , mkVimRegion
  , regionStart
  , regionEnd
  , swapRegionsB
  , deleteRegionB
  , replaceRegionB
  , readRegionB
  , mapRegionB
  , inRegion
  , modifyRegionB
  , winRegion
  , fmapRegion
  , intersectRegion
  , unionRegion
  )
where
import Data.Typeable
import Yi.Window
import Yi.Buffer
import Control.Applicative
import Yi.Prelude
import Prelude ()

-- | The region data type. 
--The region is semi open: it includes the start but not the end bound. This allows simpler region-manipulation algorithms.
-- Invariant : regionStart r <= regionEnd r
data Region = Region {regionDirection :: !Direction,
                      regionStart, regionEnd :: !Point} 
                 deriving (Typeable, Show)
fmapRegion :: (Point -> Point) -> Region -> Region
fmapRegion f (Region d x y) = Region d (f x) (f y)

mixDirections Backward Backward = Backward
mixDirections _ _ = Forward

-- | Take the intersection of two regions
intersectRegion :: Region -> Region -> Region
intersectRegion (Region _ x1 y1) (Region _ x2 y2) = ordRegion (max x1 x2) (min y1 y2)

-- | Take the union of two regions (including what is between them)
unionRegion :: Region -> Region -> Region
unionRegion (Region _ x1 y1) (Region _ x2 y2) = mkRegion (min x1 x2) (max y1 y2)

winRegion :: Window -> Region
winRegion w = mkRegion (tospnt w) (bospnt w)

-- | Construct a region from its bounds, vim style:
-- the right bound in included.
-- FIXME: this does not handle UTF8 correctly.
mkVimRegion :: Point -> Point -> Region
mkVimRegion x y = if x <= y then Region Forward x (y+1) else Region Backward y (x+1)

-- | Create a region from ordered bounds. If 2nd argument is greater than
-- 1st, then the region will be empty.
ordRegion :: Point -> Point -> Region
ordRegion x y = if x < y then Region Forward x y else emptyRegion

-- | Construct a region from its bounds, emacs style:
-- the right bound is excluded
mkRegion :: Point -> Point -> Region
mkRegion x y = if x <= y then Region Forward x y else Region Backward y x

-- | The empty region
emptyRegion :: Region
emptyRegion = Region Forward 0 0 

-- | Delete an arbitrary part of the buffer
deleteRegionB :: Region -> BufferM ()
deleteRegionB r = deleteNBytes (regionDirection r) (regionEnd r ~- regionStart r) (regionStart r)

-- | Read an arbitrary part of the buffer
readRegionB :: Region -> BufferM String
readRegionB r = nelemsB' (regionEnd r ~- i) i
    where i = regionStart r

replaceRegionB :: Region -> String -> BufferM ()
replaceRegionB r s = do
  deleteRegionB r
  insertNAt s (regionStart r)

mapRegionB :: Region -> (Char -> Char) -> BufferM ()
mapRegionB r f = do
  text <- readRegionB r
  replaceRegionB r (fmap f text)

-- | Swap the content of two Regions
swapRegionsB :: Region -> Region -> BufferM ()  
swapRegionsB r r'
    | regionStart r > regionStart r' = swapRegionsB r' r
    | otherwise = do w0 <- readRegionB r
                     w1 <- readRegionB r'
                     replaceRegionB r' w0
                     replaceRegionB r  w1

-- | True if the given point is inside the given region.
inRegion :: Point -> Region -> Bool
p `inRegion` (Region _ start stop) = start <= p && p < stop




-- | Modifies the given region according to the given
-- string transformation function
modifyRegionB :: (String -> String)
                 -- ^ The string modification function
              -> Region
                 -- ^ The region to modify
              -> BufferM ()
modifyRegionB transform region = replaceRegionB region =<< transform <$> readRegionB region


