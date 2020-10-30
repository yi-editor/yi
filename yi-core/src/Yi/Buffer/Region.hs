{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Buffer.Region
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines buffer operation on regions

module Yi.Buffer.Region
  ( module Yi.Region
  , swapRegionsB
  , deleteRegionB
  , replaceRegionB
  , readRegionB
  , mapRegionB
  , modifyRegionB
  , winRegionB
  , inclusiveRegionB
  , blockifyRegion
  , joinLinesB
  , concatLinesB
  , linesOfRegionB
  ) where

import           Control.Monad       (when)
import           Data.Char           (isSpace)
import           Data.List           (sort)
import           Yi.Buffer.Misc
import           Yi.Region
import           Yi.Rope             (YiString)
import qualified Yi.Rope             as R (YiString, cons, dropWhile, filter
                                          , lines, lines', map, null, length)
import           Yi.String           (overInit)
import           Yi.Utils            (SemiNum ((~-)))
import           Yi.Window           (winRegion)



winRegionB :: BufferM Region
winRegionB = askWindow winRegion

-- | Delete an arbitrary part of the buffer
deleteRegionB :: Region -> BufferM ()
deleteRegionB r = deleteNAt (regionDirection r) (fromIntegral (regionEnd r ~- regionStart r)) (regionStart r)

readRegionB :: Region -> BufferM YiString
readRegionB r = nelemsB (fromIntegral (regionEnd r - i)) i
    where i = regionStart r

-- | Replace a region with a given rope.
replaceRegionB :: Region -> YiString -> BufferM ()
replaceRegionB r s = do
  deleteRegionB r
  insertNAt s $ regionStart r

-- | Map the given function over the characters in the region.
mapRegionB :: Region -> (Char -> Char) -> BufferM ()
mapRegionB r f = do
  text <- readRegionB r
  replaceRegionB r (R.map f text)

-- | Swap the content of two Regions
swapRegionsB :: Region -> Region -> BufferM ()
swapRegionsB r r'
    | regionStart r > regionStart r' = swapRegionsB r' r
    | otherwise = do w0 <- readRegionB r
                     w1 <- readRegionB r'
                     replaceRegionB r' w0
                     replaceRegionB r  w1

-- | Modifies the given region according to the given
-- string transformation function
modifyRegionB :: (R.YiString -> R.YiString)
                 -- ^ The string modification function
              -> Region
                 -- ^ The region to modify
              -> BufferM ()
modifyRegionB f region = f <$> readRegionB region >>= replaceRegionB region

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
  (lowCol, highCol) <- curry sortTuple <$> colOf (regionStart r) <*> colOf (regionEnd r)
  startLine <- lineOf $ regionStart r
  endLine   <- lineOf $ regionEnd r
  when (startLine > endLine) $ fail "blockifyRegion: impossible"
  mapM (\line -> mkRegion <$> pointOfLineColB line lowCol
                          <*> pointOfLineColB line (1 + highCol))
       [startLine..endLine]
  where
    sortTuple (a,b) = if a < b then (a,b) else (b,a) 

-- | Joins lines in the region with a single space, skipping any empty
-- lines.
joinLinesB :: Region -> BufferM ()
joinLinesB = savingPointB . modifyRegionB g'
  where
    g' = overInit $ mconcat . pad . R.lines

    pad :: [R.YiString] -> [R.YiString]
    pad [] = []
    pad (x:xs) = x : fmap (skip (R.cons ' ' . R.dropWhile isSpace)) xs

    skip g x = if R.null x then x else g x

-- | Concatenates lines in the region preserving the trailing newline
-- if any.
concatLinesB :: Region -> BufferM ()
concatLinesB = savingPointB . modifyRegionB (overInit $ R.filter (/= '\n'))

-- | Gets the lines of a region (as a region), preserving newlines. Thus the
-- resulting list of regions is a partition of the original region.
--
-- The direction of the region is preserved and all smaller regions will
-- retain that direction.
--
-- Note that regions should never be empty, so it would be odd for this to
-- return an empty list...
linesOfRegionB :: Region -> BufferM [Region]
linesOfRegionB region = do
    let start = regionStart region
        direction = regionDirection region
    ls <- R.lines' <$> readRegionB region
    return $ case ls of
        [] -> []
        (l:ls') -> let initialRegion = mkRegion' direction start (start + fromIntegral (R.length l))
                   in scanl nextRegion initialRegion ls'

-- | Given some text and the previous region, finds the next region
-- (used for implementing linesOfRegionB, not generally useful)
nextRegion :: Region -> R.YiString -> Region
nextRegion r l = mkRegion' (regionDirection r) (regionEnd r) (regionEnd r + len)
    where len = fromIntegral $ R.length l
