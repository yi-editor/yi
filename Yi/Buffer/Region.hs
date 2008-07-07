{-# LANGUAGE DeriveDataTypeable #-}
-- Copyright (C) 2008 JP Bernardy

-- * This module defines buffer operation on regions

module Yi.Buffer.Region 
  (
   module Yi.Region
  , swapRegionsB
  , deleteRegionB
  , replaceRegionB
  , readRegionB
  , mapRegionB
  , modifyRegionB
  , winRegionB
  , inclusiveRegionB
  )
where
import Yi.Region
import Yi.Window
import Yi.Buffer
import Control.Applicative
import Yi.Prelude
import Prelude ()

import Control.Monad.RWS.Strict (ask)

winRegionB :: BufferM Region
winRegionB = do
    w <- ask
    tospnt <- getMarkPointB (fromMark w)
    bospnt <- getMarkPointB (toMark w)
    return $ mkRegion tospnt bospnt

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

-- | Modifies the given region according to the given
-- string transformation function
modifyRegionB :: (String -> String)
                 -- ^ The string modification function
              -> Region
                 -- ^ The region to modify
              -> BufferM ()
modifyRegionB transform region = replaceRegionB region =<< transform <$> readRegionB region


-- | Extend the right bound of a region to include it.
inclusiveRegionB :: Region -> BufferM Region
inclusiveRegionB r =
          if regionStart r <= regionEnd r
              then mkRegion (regionStart r) <$> pointAfter (regionEnd r)
              else mkRegion <$> pointAfter (regionStart r) <*> pure (regionEnd r)
    where pointAfter p = savingPointB $ do 
                           moveTo p
                           rightB
                           pointB
