{-# LANGUAGE DeriveDataTypeable #-}
--
-- Copyright (C) 2008 JP Bernardy
--

-- | A normalized API to many buffer operations.

-- The idea is that most operations should be parametric in both
--  * the textual units they work on
--  * the direction towards which they operate (if applicable)

module Yi.Buffer.Normal (TextUnit(Character, Word, Line, ViWord, ViWORD, VLine,
                                  Delimited, Document),
                         unitSentence, unitEmacsParagraph, unitParagraph,
                         -- TextUnit is exported abstract intentionally:
                         -- we'd like to move more units to the GenUnit format.
                         moveB, maybeMoveB,
                         transformB, transposeB,
                         peekB, regionOfB, regionOfPartB, readUnitB,
                         untilB, doUntilB_, untilB_, whileB,
                         atBoundaryB,
                         numberOfB,
                         deleteB, genMaybeMoveB,
                         genMoveB, BoundarySide(..), genAtBoundaryB,
                         checkPeekB
                         ) where

import Yi.Buffer
import Yi.Buffer.Region
import Data.Char
import Control.Applicative
import Control.Monad
import Data.Typeable

-- | Designate a given "unit" of text.
data TextUnit = Character
              | Word
              | ViWord -- ^ a word as in use in Vim
              | ViWORD -- ^ a WORD as in use in Vim
              | Line  -- ^ a line of text (between newlines)
              | VLine -- ^ a "vertical" line of text (area of text between to characters at the same column number)
              | Delimited Char Char
              | Document
              | GenUnit {genEnclosingUnit :: TextUnit,
                         genUnitBoundary :: Direction -> BufferM Bool}
   -- (haddock, stay away) | Page | Searched
                deriving Typeable

isWordChar :: Char -> Bool
isWordChar x = isAlphaNum x || x == '_'

isNl :: Char -> Bool
isNl = (== '\n')

-- | Tells if a char can ends a sentence ('.', '!', '?').
isEndOfSentence :: Char -> Bool
isEndOfSentence = (`elem` ".!?")

-- | Verifies that the list matches all the predicates, pairwise.
-- If the list is "too small", then return 'False'.
checks :: [a -> Bool] -> [a] -> Bool
checks [] _ = True
checks _ [] = False
checks (p:ps) (x:xs) = p x && checks ps xs

-- | read some characters in the specified direction, for boundary testing purposes
peekB :: Direction -> Int -> Int -> BufferM String
peekB dir siz ofs = savingPointB $
  do moveN dirOfs
     mayReverse dir <$> (nelemsB siz =<< pointB)
  where
  dirOfs = case dir of
             Forward  -> ofs
             Backward -> 0 - siz - ofs

checkPeekB :: Int -> [Char -> Bool] -> Direction -> BufferM Bool
checkPeekB offset conds dir = checks conds <$> peekB dir (length conds) offset

-- | Is the point at a @Unit@ boundary in the specified @Direction@?
atBoundary :: TextUnit -> Direction -> BufferM Bool
atBoundary Document Backward = (== 0) <$> pointB
atBoundary Document Forward  = (>=)   <$> pointB <*> sizeB
atBoundary Character _ = return True
atBoundary VLine _ = return True -- a fallacy; this needs a little refactoring.
atBoundary Word direction =
    checkPeekB (-1) [isWordChar, not . isWordChar] direction
atBoundary ViWORD direction =
    checkPeekB (-1) [not . isSpace, isSpace] direction
atBoundary ViWord direction = do
    ~cs@[c1,c2] <- peekB direction 2 (-1)
    return (length cs /= 2 || (not (isSpace c1) && (charType c1 /= charType c2)))
        where charType c | isSpace    c = 1::Int
                         | isWordChar c = 2
                         | otherwise    = 3
atBoundary Line direction = checkPeekB 0 [isNl] direction
atBoundary (Delimited c _) Backward = checkPeekB 0 [(== c)] Backward
atBoundary (Delimited _ c) Forward  = (== c) <$> readB
atBoundary (GenUnit _ atBound) dir = atBound dir

enclosingUnit :: TextUnit -> TextUnit
enclosingUnit (GenUnit enclosing _) = enclosing
enclosingUnit _ = Document 

atBoundaryB :: TextUnit -> Direction -> BufferM Bool
atBoundaryB Document d = atBoundary Document d
atBoundaryB u d = (||) <$> atBoundary u d <*> atBoundaryB (enclosingUnit u) d

-- | Paragraph to implement emacs-like forward-paragraph/backward-paragraph
unitEmacsParagraph :: TextUnit
unitEmacsParagraph = GenUnit Document $ checkPeekB (-2) [not . isNl, isNl, isNl]

-- | Paragraph that begins and ends in the paragraph, not the empty lines surrounding it.
unitParagraph :: TextUnit
unitParagraph = GenUnit Document $ checkPeekB (-1) [not . isNl, isNl, isNl]

unitSentence :: TextUnit
unitSentence = GenUnit unitEmacsParagraph $ \dir -> checkPeekB (if dir == Forward then -1 else 0) (mayReverse dir [isEndOfSentence, isSpace]) dir

-- | @genAtBoundaryB u d s@ returns whether the point is at a given boundary @(d,s)@ .
-- Boundary @(d,s)@ , taking Word as example, means:
--      Word 
--     ^^  ^^
--     12  34
-- 1: (Backward,Outside)
-- 2: (Backward,Inside)
-- 3: (Forward,Inside)
-- 4: (Forward,Outside)
--
-- rules:
-- genAtBoundaryB u Backward InsideBound  = atBoundaryB u Backward
-- genAtBoundaryB u Forward  OutsideBound = atBoundaryB u Forward
genAtBoundaryB :: TextUnit -> Direction -> BoundarySide -> BufferM Bool
genAtBoundaryB u d s = withOffset (off u d s) $ atBoundaryB u d
    where withOffset 0 f = f
          withOffset ofs f = savingPointB (((ofs +) <$> pointB) >>= moveTo >> f)
          off _    Backward  InsideBound = 0
          off _    Backward OutsideBound = 1
          off _    Forward   InsideBound = 1
          off _    Forward  OutsideBound = 0


numberOfB :: TextUnit -> TextUnit -> BufferM Int
numberOfB unit containingUnit = savingPointB $ do
                   maybeMoveB containingUnit Backward
                   start <- pointB
                   moveB containingUnit Forward
                   end <- pointB
                   moveTo start
                   length <$> untilB ((>= end) <$> pointB) (moveB unit Forward)

whileB :: BufferM Bool -> BufferM a -> BufferM [a]
whileB cond f = untilB (not <$> cond) f

-- | Repeat an action until the condition is fulfilled or the cursor stops moving.
-- The Action may be performed zero times.
untilB :: BufferM Bool -> BufferM a -> BufferM [a]
untilB cond f = do
  stop <- cond
  if stop then return [] else doUntilB cond f

-- | Repeat an action until the condition is fulfilled or the cursor stops moving.
-- The Action is performed at least once.
doUntilB :: BufferM Bool -> BufferM a -> BufferM [a]
doUntilB cond f = loop
      where loop = do
              p <- pointB
              x <- f
              p' <- pointB
              stop <- cond
              (x:) <$> if (p /= p' && not stop) 
                then loop
                else return []

doUntilB_ :: BufferM Bool -> BufferM a -> BufferM ()
doUntilB_ cond f = doUntilB cond f >> return () -- maybe do an optimized version?

untilB_ :: BufferM Bool -> BufferM a -> BufferM ()
untilB_ cond f = untilB cond f >> return () -- maybe do an optimized version?


-- | Boundary side
data BoundarySide = InsideBound | OutsideBound

-- | Generic move operation
-- Warning: moving To the (OutsideBound, Backward) bound of Document is impossible (offset -1!)
-- @genMoveB u b d@: move in direction d until encountering boundary b or unit u. See 'genAtBoundaryB' for boundary explanation.
genMoveB :: TextUnit -> (Direction, BoundarySide) -> Direction -> BufferM ()
genMoveB Document (Forward,InsideBound) Forward = moveTo =<< (subtract 1) <$> sizeB
genMoveB Document _                     Forward = moveTo =<< sizeB
genMoveB Document _ Backward = moveTo 0 -- impossible to go outside beginning of doc.
genMoveB Character _ Forward  = rightB
genMoveB Character _ Backward = leftB
genMoveB VLine     _ Forward  = 
  do ofs <- lineMoveRel 1
     when (ofs < 1) (maybeMoveB Line Forward)
genMoveB VLine _ Backward = lineUp
genMoveB unit (boundDir, boundSide) moveDir = 
  doUntilB_ (genAtBoundaryB unit boundDir boundSide) (moveB Character moveDir)
    
-- | Generic maybe move operation.
-- As genMoveB, but don't move if we are at boundary already.
genMaybeMoveB :: TextUnit -> (Direction, BoundarySide) -> Direction -> BufferM ()
genMaybeMoveB Document boundSpec moveDir = genMoveB Document boundSpec moveDir 
   -- optimized casex for Document
genMaybeMoveB unit (boundDir, boundSide) moveDir =
  untilB_ (genAtBoundaryB unit boundDir boundSide) (moveB Character moveDir)


-- | Move to the next unit boundary
moveB :: TextUnit -> Direction -> BufferM ()
moveB u d = genMoveB u (d, case d of Forward -> OutsideBound; Backward -> InsideBound) d


-- | As 'moveB', unless the point is at a unit boundary

-- So for example here moveToEol = maybeMoveB Line Forward;
-- in that it will move to the end of current line and nowhere if we
-- are already at the end of the current line. Similarly for moveToSol.

maybeMoveB :: TextUnit -> Direction -> BufferM ()
maybeMoveB u d = genMaybeMoveB u (d, case d of Forward -> OutsideBound; Backward -> InsideBound) d

transposeB :: TextUnit -> Direction -> BufferM ()
transposeB unit direction = do
  moveB unit (reverseDir direction)
  w0 <- pointB
  moveB unit direction
  w0' <- pointB
  moveB unit direction
  w1' <- pointB
  moveB unit (reverseDir direction)
  w1 <- pointB
  swapRegionsB (mkRegion w0 w0') (mkRegion w1 w1')
  moveTo w1'

transformB :: (String -> String) -> TextUnit -> Direction -> BufferM ()
transformB f unit direction = do
  p <- pointB
  moveB unit direction
  q <- pointB
  let r = mkRegion p q
  replaceRegionB r =<< f <$> readRegionB r

-- | delete between point and next unit boundary, return the deleted region
deleteB :: TextUnit -> Direction -> BufferM ()
deleteB unit dir = deleteRegionB =<< regionOfPartNonEmptyB unit dir

indexAfterB :: BufferM a -> BufferM Point
indexAfterB f = savingPointB (f >> pointB)

-- | Region of the whole textunit where the current point is
regionOfB :: TextUnit -> BufferM Region
regionOfB unit = mkRegion
                 <$> indexAfterB (maybeMoveB unit Backward)
                 <*> indexAfterB (maybeMoveB unit Forward)

-- | Region between the point and the next boundary.
-- The region is empty if the point is at the boundary.
regionOfPartB :: TextUnit -> Direction -> BufferM Region
regionOfPartB unit dir = savingPointB $ do
         b <- pointB
         maybeMoveB unit dir
         e <- pointB
         return $ mkRegion b e

-- | Non empty region between the point and the next boundary,
-- In fact the region can be empty if we are at the end of file.
regionOfPartNonEmptyB :: TextUnit -> Direction -> BufferM Region
regionOfPartNonEmptyB unit dir = savingPointB $ do
         b <- pointB
         moveB unit dir
         e <- pointB
         return $ mkRegion b e


readUnitB :: TextUnit -> BufferM String
readUnitB = readRegionB <=< regionOfB


