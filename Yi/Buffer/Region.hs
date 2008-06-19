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
data Region = Region {regionStart, regionEnd :: !Point} 
                 deriving (Typeable, Show)
fmapRegion :: (Point -> Point) -> Region -> Region
fmapRegion f (Region x y) = Region (f x) (f y)

intersectRegion :: Region -> Region -> Region
intersectRegion (Region x1 y1) (Region x2 y2) = ordRegion (max x1 x2) (min y1 y2)

winRegion :: Window -> Region
winRegion w = mkRegion (tospnt w) (bospnt w)

-- | Construct a region from its bounds, vim style:
-- the right bound in included.
-- FIXME: this does not handle UTF8 correctly.
mkVimRegion :: Point -> Point -> Region
mkVimRegion x y = if x < y then Region x (y+1) else Region y (x+1)

-- | Create a region from ordered bounds. If 2nd argument is greater than
-- 1st, then the region will be empty.
ordRegion :: Point -> Point -> Region
ordRegion x y = if x < y then Region x y else emptyRegion

-- | Construct a region from its bounds, emacs style:
-- the right bound is excluded
mkRegion :: Point -> Point -> Region
mkRegion x y = if x < y then Region x y else Region y x

-- | The empty region
emptyRegion :: Region
emptyRegion = Region 0 0 

-- | Delete an arbitrary part of the buffer
deleteRegionB :: Region -> BufferM ()
deleteRegionB r = deleteNBytes (regionEnd r ~- regionStart r) (regionStart r)

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
p `inRegion` (Region start stop) = start <= p && p < stop




-- | Modifies the given region according to the given
-- string transformation function
modifyRegionB :: (String -> String)
                 -- ^ The string modification function
              -> Region
                 -- ^ The region to modify
              -> BufferM ()
modifyRegionB transform region = replaceRegionB region =<< transform <$> readRegionB region


