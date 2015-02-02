{-# LANGUAGE CPP #-}

-- | A normalized API to many buffer operations.

-- The idea is that most operations should be parametric in both
--  * the textual units they work on
--  * the direction towards which they operate (if applicable)

module Yi.Buffer.Normal ( TextUnit(Character, Line, VLine, Document, GenUnit)
                        , isAnySep
                        , isWordChar
                        , leftBoundaryUnit
                        , outsideUnit
                        , unitDelimited
                        , unitEmacsParagraph
                        , unitParagraph
                        , unitSentence
                        , unitSep
                        , unitSepThisLine
                        , unitViWORD
                        , unitViWORDAnyBnd
                        , unitViWORDOnLine
                        , unitViWord
                        , unitViWordAnyBnd
                        , unitViWordOnLine
                        , unitWord
                         -- TextUnit is exported abstract intentionally:
                         -- we'd like to move more units to the GenUnit format.
                        , atBoundaryB
                        , deleteB
                        , doIfCharB
                        , doUntilB_
                        , genMaybeMoveB
                        , genMoveB
                        , maybeMoveB
                        , moveB
                        , numberOfB
                        , readPrevUnitB
                        , readUnitB
                        , regionOfB
                        , regionOfNonEmptyB
                        , regionOfPartB
                        , regionOfPartNonEmptyAtB
                        , regionOfPartNonEmptyB
                        , transformB
                        , transposeB
                        , untilB
                        , untilB_
                        , whileB
                        , BoundarySide(..)
                        , checkPeekB
                        , genAtBoundaryB
                        , genEnclosingUnit
                        , genUnitBoundary
                        , RegionStyle(..)
                        , convertRegionToStyleB
                        , extendRegionToBoundaries
                        , getRegionStyle
                        , mkRegionOfStyleB
                        , putRegionStyle
                        , unitWiseRegion
                        ) where

import           Data.List          (sort)
import           Yi.Buffer.Basic    (Direction (Backward, Forward), Point)
import           Yi.Buffer.Misc     (BufferM, getBufferDyn, moveTo, pointB, putBufferDyn, savingPointB)
import           Yi.Buffer.Region   (Region (..), inclusiveRegionB, mkRegion, mkRegion')
import           Yi.Buffer.TextUnit
import           Yi.Types           (RegionStyle (..))

getRegionStyle :: BufferM RegionStyle
getRegionStyle = getBufferDyn
putRegionStyle :: RegionStyle -> BufferM ()
putRegionStyle = putBufferDyn

convertRegionToStyleB :: Region -> RegionStyle -> BufferM Region
convertRegionToStyleB r = mkRegionOfStyleB (regionStart r) (regionEnd r)

mkRegionOfStyleB :: Point -> Point -> RegionStyle -> BufferM Region
mkRegionOfStyleB start' stop' regionStyle =
   let [start, stop] = sort [start', stop']
       region = mkRegion start stop in
   case regionStyle of
     LineWise  -> inclusiveRegionB =<< unitWiseRegion Line region
     Inclusive -> inclusiveRegionB region
     Exclusive -> return region
     Block     -> return region

unitWiseRegion :: TextUnit -> Region -> BufferM Region
unitWiseRegion unit = extendRegionToBoundaries unit InsideBound OutsideBound

-- | Extend the given region to boundaries of the text unit.
-- For instance one can extend the selection to complete lines, or
-- paragraphs.
extendRegionToBoundaries :: TextUnit -> BoundarySide -> BoundarySide -> Region -> BufferM Region
extendRegionToBoundaries unit bs1 bs2 region = savingPointB $ do
  moveTo $ regionStart region
  genMaybeMoveB unit (Backward, bs1) Backward
  start <- pointB
  moveTo $ regionEnd region
  genMaybeMoveB unit (Forward, bs2) Forward
  stop <- pointB
  return $ mkRegion' (regionDirection region) start stop
