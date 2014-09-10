{-# LANGUAGE CPP #-}
-- Copyright (C) 2008 JP Bernardy

-- | This module defines buffer operation on regions

module Yi.Buffer.Region
  (
   module Yi.Region
  , swapRegionsB
  , deleteRegionB
  , replaceRegionB
  , replaceRegionB'
  , replaceRegionClever
  , readRegionB
  , readRegionB'
  , mapRegionB
  , modifyRegionB
  , modifyRegionClever
  , winRegionB
  , inclusiveRegionB
  , blockifyRegion
  , joinLinesB
  , concatLinesB
  )
where

import Control.Applicative
import Control.Monad
import Control.Lens hiding (transform)
import Data.Algorithm.Diff
import Data.Char (isSpace)
import Data.List (sort)
import Yi.OldRope (Rope)

import Yi.Buffer.Misc
import Yi.Region
import Yi.String (lines')
import Yi.Window (winRegion)
import Yi.Utils

winRegionB :: BufferM Region
winRegionB = askWindow winRegion

-- | Delete an arbitrary part of the buffer
deleteRegionB :: Region -> BufferM ()
deleteRegionB r = deleteNAt (regionDirection r) (fromIntegral (regionEnd r ~- regionStart r)) (regionStart r)

-- | Read an arbitrary part of the buffer
readRegionB :: Region -> BufferM String
readRegionB r = nelemsB (fromIntegral (regionEnd r - i)) i
    where i = regionStart r

readRegionB' :: Region -> BufferM Rope
readRegionB' r = nelemsB' (fromIntegral (regionEnd r - i)) i
    where i = regionStart r

-- | Replace a region with a given string.
replaceRegionB :: Region -> String -> BufferM ()
replaceRegionB r s = do
  deleteRegionB r
  insertNAt s (regionStart r)

-- | Replace a region with a given rope.
replaceRegionB' :: Region -> Rope -> BufferM ()
replaceRegionB' r s = do
  deleteRegionB r
  insertNAt' s (regionStart r)

-- TODO: reimplement 'getGroupedDiff' for Ropes, so we can use 'replaceRegionClever' on large regions.

-- | As 'replaceRegionB', but do a minimal edition instead of deleting the whole
-- region and inserting it back.
replaceRegionClever :: Region -> String -> BufferM ()
replaceRegionClever region text' = savingExcursionB $ do
    text <- readRegionB region
    let diffs = getGroupedDiff text text'
    moveTo (regionStart region)
    forM_ diffs $ \d -> case d of
            First str -> deleteN $ length str
            Both str _ -> rightN $ length str
            Second str -> insertN str

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
              then mkRegion (regionStart r) <$> pointAfterCursorB (regionEnd r)
              else mkRegion <$> pointAfterCursorB (regionStart r) <*> pure (regionEnd r)

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

skippingNull :: ([a] -> [b]) -> [a] -> [b]
skippingNull _ [] = []
skippingNull f xs = f xs

joinLinesB :: Region -> BufferM ()
joinLinesB =
  savingPointB .
    modifyRegionClever (over _init $
       concat . over _tail (fmap $ skippingNull ((' ':) . dropWhile isSpace)) . lines')

concatLinesB :: Region -> BufferM ()
concatLinesB = savingPointB . modifyRegionClever (over _init $ filter (/= '\n'))
