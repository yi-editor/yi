{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
--
-- Copyright (C) 2008 JP Bernardy
--

-- | A normalized API to many buffer operations.

-- The idea is that most operations should be parametric in both
--  * the textual units they work on
--  * the direction towards which they operate (if applicable)

module Yi.Buffer.Normal (TextUnit(Character, Line, VLine, Document, GenUnit),
                         outsideUnit,
                         leftBoundaryUnit,
                         unitWord,
                         unitViWord,
                         unitViWORD,
                         unitViWordAnyBnd,
                         unitViWORDAnyBnd,
                         unitViWordOnLine,
                         unitViWORDOnLine,
                         unitDelimited,
                         unitSentence, unitEmacsParagraph, unitParagraph,
                         isAnySep, unitSep, unitSepThisLine, isWordChar,
                         -- TextUnit is exported abstract intentionally:
                         -- we'd like to move more units to the GenUnit format.
                         moveB, maybeMoveB,
                         transformB, transposeB,
                         regionOfB, regionOfNonEmptyB, regionOfPartB,
                         regionOfPartNonEmptyB, regionOfPartNonEmptyAtB,
                         readPrevUnitB, readUnitB,
                         untilB, doUntilB_, untilB_, whileB, doIfCharB,
                         atBoundaryB,
                         numberOfB,
                         deleteB, genMaybeMoveB,
                         genMoveB, BoundarySide(..), genAtBoundaryB,
                         genEnclosingUnit, genUnitBoundary,
                         checkPeekB
                         , RegionStyle(..)
                         , mkRegionOfStyleB
                         , convertRegionToStyleB
                         , unitWiseRegion
                         , extendRegionToBoundaries
                         , regionStyleA
                         ) where

import Prelude(length, subtract)
import Yi.Prelude

import Data.Binary
import Data.DeriveTH
import Data.List (sort)

import Yi.Buffer.Basic
import Yi.Buffer.Misc
import Yi.Buffer.Region
import Yi.Buffer.TextUnit
import Yi.Dynamic

-- Region styles are relative to the buffer contents.
-- They likely should be considered a TextUnit.
data RegionStyle = LineWise
                 | Inclusive
                 | Exclusive
                 | Block
  deriving (Eq, Typeable, Show)

$(derive makeBinary ''RegionStyle)

-- TODO: put in the buffer state proper.
instance Initializable RegionStyle where
  initial = Inclusive

instance YiVariable RegionStyle

regionStyleA :: Accessor FBuffer RegionStyle
regionStyleA = bufferDynamicValueA

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
