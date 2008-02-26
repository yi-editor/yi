-- This module defines the Region ADT

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
  )
where

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

-- | The empty region
emptyRegion :: Region
emptyRegion = Region 0 0 

-- | Delete an arbitrary part of the buffer
deleteRegionB :: Region -> BufferM ()
deleteRegionB r = deleteNAt (regionEnd r - regionStart r) (regionStart r)

-- | Read an arbitrary part of the buffer
readRegionB :: Region -> BufferM String
readRegionB r = nelemsB (regionEnd r - i) i
    where i = regionStart r

replaceRegionB :: Region -> String -> BufferM ()
replaceRegionB r s = do
  deleteRegionB r
  insertNAt s (regionStart r)

mapRegionB :: Region -> (Char -> Char) -> BufferM ()
mapRegionB r f = do
  text <- readRegionB r
  replaceRegionB r (map f text)

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
              -> BufferM Int
                 -- ^ The returned buffer action, the 'Int' contained
                 -- is the difference in length of the two regions.
modifyRegionB transform region =
  do text <- readRegionB region
     let newText = transform text
     replaceRegionB region $ newText
     return $ (length newText) - (length text)

