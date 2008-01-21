-- This module defines the Region ADT

module Yi.Buffer.Region (Region, mkRegion, mkVimRegion, regionStart, regionEnd,
                         swapRegionsB, deleteRegionB, replaceRegionB, readRegionB, mapRegionB,
                         getSelectRegionB) where

import Yi.Buffer

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


-- | Delete an arbitrary part of the buffer
deleteRegionB :: Region -> BufferM ()
deleteRegionB r = deleteNAt (regionEnd r - regionStart r) (regionStart r)

-- | Read an arbitrary part of the buffer
readRegionB :: Region -> BufferM String
readRegionB r = nelemsB (regionEnd r - i) i
    where i = regionStart r

replaceRegionB :: Region -> String -> BufferM ()
replaceRegionB r s = savingPointB $ do
  deleteRegionB r
  insertNAt s (regionStart r)

mapRegionB :: Region -> (Char -> Char) -> BufferM ()
mapRegionB r f = do
  text <- readRegionB r
  replaceRegionB r (map f text)

-- | swap the content of two Regions
swapRegionsB :: Region -> Region -> BufferM ()  
swapRegionsB r r'
    | regionStart r > regionStart r' = swapRegionsB r' r
    | otherwise = do w0 <- readRegionB r
                     w1 <- readRegionB r'
                     replaceRegionB r' w0
                     replaceRegionB r  w1
                     
-- | Get the current region boundaries
getSelectRegionB :: BufferM Region
getSelectRegionB = do 
  m <- getMarkPointB =<< getSelectionMarkB
  p <- pointB
  return $ mkRegion m p
