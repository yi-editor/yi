{-# LANGUAGE DeriveDataTypeable #-}
-- Copyright (C) 2008 JP Bernardy

-- | This module defines buffer operation on regions

module Yi.Buffer.Region 
  (
   module Yi.Region
  , swapRegionsB
  , deleteRegionB
  , replaceRegionB
  , replaceRegionClever
  , readRegionB
  , mapRegionB
  , modifyRegionB
  , modifyRegionClever
  , winRegionB
  , inclusiveRegionB
  , blockifyRegion
  )
where
import Data.Algorithm.Diff
import Yi.Region
import Yi.Buffer.Misc
import Yi.Prelude
import Prelude ()
import Data.List (length, sort)

import Control.Monad.RWS.Strict (ask)

winRegionB :: BufferM Region
winRegionB = do
    w <- ask
    Just ms <- getMarks w
    tospnt <- getMarkPointB (fromMark ms)
    bospnt <- getMarkPointB (toMark ms)
    return $ mkRegion tospnt bospnt

-- | Delete an arbitrary part of the buffer
deleteRegionB :: Region -> BufferM ()
deleteRegionB r = deleteNAt (regionDirection r) (fromIntegral (regionEnd r ~- regionStart r)) (regionStart r)

-- | Read an arbitrary part of the buffer
readRegionB :: Region -> BufferM String
readRegionB r = nelemsB (fromIntegral (regionEnd r - i)) i
    where i = regionStart r

-- | Replace a region with a given string.
replaceRegionB :: Region -> String -> BufferM ()
replaceRegionB r s = do
  deleteRegionB r
  insertNAt s (regionStart r)

-- | As 'replaceRegionB', but do a minimal edition instead of deleting the whole
-- region and inserting it back.
replaceRegionClever :: Region -> String -> BufferM ()
replaceRegionClever region text' = savingExcursionB $ do
    text <- readRegionB region
    let diffs = getGroupedDiff text text'
    moveTo (regionStart region)
    forM_ diffs $ \(d,str) -> do
        case d of
            F -> deleteN $ length str
            B -> rightN $ length str
            S -> insertN str

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

-- Transform a replace into a modify.
replToMod :: (Region -> a -> BufferM b) -> (String -> a) -> Region -> BufferM b
replToMod replace transform region = replace region =<< transform <$> readRegionB region

-- | Modifies the given region according to the given
-- string transformation function
modifyRegionB :: (String -> String)
                 -- ^ The string modification function
              -> Region
                 -- ^ The region to modify
              -> BufferM ()
modifyRegionB = replToMod replaceRegionB

    
-- | As 'modifyRegionB', but do a minimal edition instead of deleting the whole
-- region and inserting it back.
modifyRegionClever :: (String -> String) -> Region -> BufferM ()
modifyRegionClever =  replToMod replaceRegionClever

-- | Extend the right bound of a region to include it.
inclusiveRegionB :: Region -> BufferM Region
inclusiveRegionB r =
          if regionStart r <= regionEnd r
              then mkRegion (regionStart r) <$> pointAfter (regionEnd r)
              else mkRegion <$> pointAfter (regionStart r) <*> pure (regionEnd r)
    where pointAfter p = pointAt $ do 
                           moveTo p
                           rightB

-- | See a region as a block/rectangular region,
-- since regions are represented by two point, this returns
-- a list of small regions form this block region.
blockifyRegion :: Region -> BufferM [Region]
blockifyRegion r = savingPointB $ do
  [lowCol,highCol] <- sort <$> mapM colOf [regionStart r, regionEnd r]
  startLine <- lineOf $ regionStart r
  endLine   <- lineOf $ regionEnd r
  when (startLine > endLine) $ fail "blockifyRegion: impossible"
  mapM (\line -> mkRegion <$> pointOfLineColB line lowCol <*> pointOfLineColB line (1 + highCol))
       [startLine..endLine]
