{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE MultiWayIf #-}

-- |
-- Module      :  Yi.Buffer.HighLevel
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- High level operations on buffers.

module Yi.Buffer.HighLevel
    ( atEof
    , atEol
    , atLastLine
    , atSol
    , bdeleteB
    , bdeleteLineB
    , bkillWordB
    , botB
    , bufInfoB
    , BufferFileInfo (..)
    , capitaliseWordB
    , deleteBlankLinesB
    , deleteHorizontalSpaceB
    , deleteRegionWithStyleB
    , deleteToEol
    , deleteTrailingSpaceB
    , downFromTosB
    , downScreenB
    , downScreensB
    , exchangePointAndMarkB
    , fillParagraph
    , findMatchingPairB
    , firstNonSpaceB
    , flipRectangleB
    , getBookmarkB
    , getLineAndCol
    , getLineAndColOfPoint
    , getNextLineB
    , getNextNonBlankLineB
    , getRawestSelectRegionB
    , getSelectionMarkPointB
    , getSelectRegionB
    , gotoCharacterB
    , hasWhiteSpaceBefore
    , incrementNextNumberByB
    , insertRopeWithStyleB
    , isCurrentLineAllWhiteSpaceB
    , isCurrentLineEmptyB
    , isNumberB
    , killWordB
    , lastNonSpaceB
    , leftEdgesOfRegionB
    , leftOnEol
    , linePrefixSelectionB
    , lineStreamB
    , lowercaseWordB
    , middleB
    , modifyExtendedSelectionB
    , moveNonspaceOrSol
    , movePercentageFileB
    , moveToEol
    , moveToSol
    , moveXorEol
    , moveXorSol
    , nextCInLineExc
    , nextCInLineInc
    , nextNParagraphs
    , nextWordB
    , prevCInLineExc
    , prevCInLineInc
    , prevNParagraphs
    , prevWordB
    , readCurrentWordB
    , readLnB
    , readPrevWordB
    , readRegionRopeWithStyleB
    , replaceBufferContent
    , revertB
    , rightEdgesOfRegionB
    , scrollB
    , scrollCursorToBottomB
    , scrollCursorToTopB
    , scrollScreensB
    , scrollToCursorB
    , scrollToLineAboveWindowB
    , scrollToLineBelowWindowB
    , setSelectionMarkPointB
    , setSelectRegionB
    , shapeOfBlockRegionB
    , sortLines
    , snapInsB
    , snapScreenB
    , splitBlockRegionToContiguousSubRegionsB
    , swapB
    , switchCaseChar
    , test3CharB
    , testHexB
    , toggleCommentB
    , topB
    , unLineCommentSelectionB
    , upFromBosB
    , uppercaseWordB
    , upScreenB
    , upScreensB
    , vimScrollB
    , vimScrollByB
    ) where

import           Control.Applicative
import           Control.Lens hiding ((-~), (+~), re, transform)
import           Control.Monad
import           Control.Monad.RWS.Strict (ask)
import           Control.Monad.State hiding (forM, forM_, sequence_)
import           Data.Char (isDigit, isHexDigit, isOctDigit,
                            toUpper, isUpper, toLower, isSpace)
import           Data.List (sort, intersperse)
import           Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import           Data.Monoid
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.Tuple (swap)
import           Numeric (showOct, showHex, readOct, readHex)
import           Yi.Buffer.Basic
import           Yi.Buffer.Misc
import           Yi.Buffer.Normal
import           Yi.Buffer.Region
import           Yi.Config.Misc (ScrollStyle(SingleLine))
import           Yi.Rope (YiString)
import qualified Yi.Rope as R
import           Yi.String
import           Yi.Utils
import           Yi.Window

-- ---------------------------------------------------------------------
-- Movement operations

-- | Move point to start of line
moveToSol :: BufferM ()
moveToSol = maybeMoveB Line Backward

-- | Move point to end of line
moveToEol :: BufferM ()
moveToEol = maybeMoveB Line Forward

-- | Move cursor to origin
topB :: BufferM ()
topB = moveTo 0

-- | Move cursor to end of buffer
botB :: BufferM ()
botB = moveTo =<< sizeB

-- | Move left if on eol, but not on blank line
leftOnEol :: BufferM ()
-- @savingPrefCol@ is needed, because deep down @leftB@ contains @forgetPrefCol@
-- which messes up vertical cursor motion in Vim normal mode
leftOnEol = savingPrefCol $ do
        eol <- atEol
        sol <- atSol
        when (eol && not sol) leftB

-- | Move @x@ chars back, or to the sol, whichever is less
moveXorSol :: Int -> BufferM ()
moveXorSol x = replicateM_ x $ do c <- atSol; unless c leftB

-- | Move @x@ chars forward, or to the eol, whichever is less
moveXorEol :: Int -> BufferM ()
moveXorEol x = replicateM_ x $ do c <- atEol; unless c rightB

-- | Move to first char of next word forwards
nextWordB :: BufferM ()
nextWordB = moveB unitWord Forward

-- | Move to first char of next word backwards
prevWordB :: BufferM ()
prevWordB = moveB unitWord Backward

-- * Char-based movement actions.

gotoCharacterB :: Char -> Direction -> RegionStyle -> Bool -> BufferM ()
gotoCharacterB c dir style stopAtLineBreaks = do
    start <- pointB
    let predicate = if stopAtLineBreaks then (`elem` [c, '\n']) else (== c)
        (move, moveBack) = if dir == Forward then (rightB, leftB) else (leftB, rightB)
    doUntilB_ (predicate <$> readB) move
    b <- readB
    if stopAtLineBreaks && b == '\n'
    then moveTo start
    else when (style == Exclusive && b == c) moveBack

-- | Move to the next occurence of @c@
nextCInc :: Char -> BufferM ()
nextCInc c = gotoCharacterB c Forward Inclusive False

nextCInLineInc :: Char -> BufferM ()
nextCInLineInc c = gotoCharacterB c Forward Inclusive True

-- | Move to the character before the next occurence of @c@
nextCExc :: Char -> BufferM ()
nextCExc c = gotoCharacterB c Forward Exclusive False

nextCInLineExc :: Char -> BufferM ()
nextCInLineExc c = gotoCharacterB c Forward Exclusive True

-- | Move to the previous occurence of @c@
prevCInc :: Char -> BufferM ()
prevCInc c = gotoCharacterB c Backward Inclusive False

prevCInLineInc :: Char -> BufferM ()
prevCInLineInc c = gotoCharacterB c Backward Inclusive True

-- | Move to the character after the previous occurence of @c@
prevCExc :: Char -> BufferM ()
prevCExc c = gotoCharacterB c Backward Exclusive False

prevCInLineExc :: Char -> BufferM ()
prevCInLineExc c = gotoCharacterB c Backward Exclusive True

-- | Move to first non-space character in this line
firstNonSpaceB :: BufferM ()
firstNonSpaceB = do
  moveToSol
  untilB_ ((||) <$> atEol <*> ((not . isSpace) <$> readB)) rightB

-- | Move to the last non-space character in this line
lastNonSpaceB :: BufferM ()
lastNonSpaceB = do
  moveToEol
  untilB_ ((||) <$> atSol <*> ((not . isSpace) <$> readB)) leftB

-- | Go to the first non space character in the line;
-- if already there, then go to the beginning of the line.
moveNonspaceOrSol :: BufferM ()
moveNonspaceOrSol = do
  prev <- readPreviousOfLnB
  if R.all isSpace prev then moveToSol else firstNonSpaceB

-- | True if current line consists of just a newline (no whitespace)
isCurrentLineEmptyB :: BufferM Bool
isCurrentLineEmptyB = savingPointB $ moveToSol >> atEol

-- | Note: Returns False if line doesn't have any characters besides a newline
isCurrentLineAllWhiteSpaceB :: BufferM Bool
isCurrentLineAllWhiteSpaceB = savingPointB $ do
    isEmpty <- isCurrentLineEmptyB
    if isEmpty
    then return False
    else do
        let go = do
                  eol <- atEol
                  if eol
                  then return True
                  else do
                      c <- readB
                      if isSpace c
                      then rightB >> go
                      else return False
        moveToSol
        go

------------

-- | Move down next @n@ paragraphs
nextNParagraphs :: Int -> BufferM ()
nextNParagraphs n = replicateM_ n $ moveB unitEmacsParagraph Forward

-- | Move up prev @n@ paragraphs
prevNParagraphs :: Int -> BufferM ()
prevNParagraphs n = replicateM_ n $ moveB unitEmacsParagraph Backward

-- ! Examples:
-- @goUnmatchedB Backward '(' ')'@
-- Move to the previous unmatched '('
-- @goUnmatchedB Forward '{' '}'@
-- Move to the next unmatched '}'
goUnmatchedB :: Direction -> Char -> Char -> BufferM ()
goUnmatchedB dir cStart' cStop' = stepB >> readB >>= go (0::Int)
  where go opened c | c == cStop && opened == 0 = return ()
                    | c == cStop                = stepB >> readB >>= go (opened-1)
                    | c == cStart               = stepB >> readB >>= go (opened+1)
                    | otherwise                 = stepB >> readB >>= go opened
        (stepB, cStart, cStop) | dir == Forward = (rightB, cStart', cStop')
                               | otherwise      = (leftB, cStop', cStart')

-----------------------------------------------------------------------
-- Queries

-- | Return true if the current point is the start of a line
atSol :: BufferM Bool
atSol = atBoundaryB Line Backward

-- | Return true if the current point is the end of a line
atEol :: BufferM Bool
atEol = atBoundaryB Line Forward

-- | True if point at start of file
atSof :: BufferM Bool
atSof = atBoundaryB Document Backward

-- | True if point at end of file
atEof :: BufferM Bool
atEof = atBoundaryB Document Forward

-- | True if point at the last line
atLastLine :: BufferM Bool
atLastLine = savingPointB $ do
    moveToEol
    (==) <$> sizeB <*> pointB

-- | Get the current line and column number
getLineAndCol :: BufferM (Int, Int)
getLineAndCol = (,) <$> curLn <*> curCol

getLineAndColOfPoint :: Point -> BufferM (Int, Int)
getLineAndColOfPoint p = savingPointB $ moveTo p >> getLineAndCol

-- | Read the line the point is on
readLnB :: BufferM YiString
readLnB = readUnitB Line

readCharB :: BufferM (Maybe Char)
readCharB = fmap R.head (readUnitB Character)

-- | Read from point to end of line
readRestOfLnB :: BufferM YiString
readRestOfLnB = readRegionB =<< regionOfPartB Line Forward

-- | Read from point to beginning of line
readPreviousOfLnB :: BufferM YiString
readPreviousOfLnB = readRegionB =<< regionOfPartB Line Backward

hasWhiteSpaceBefore :: BufferM Bool
hasWhiteSpaceBefore = liftM isSpace (prevPointB >>= readAtB)

-- | Get the previous point, unless at the beginning of the file
prevPointB :: BufferM Point
prevPointB = do
  sof <- atSof
  if sof then pointB
         else do p <- pointB
                 return $ Point (fromPoint p - 1)

-- | Get the next point, unless at the end of the file
nextPointB :: BufferM Point
nextPointB = do
  eof <- atEof
  if eof then pointB
         else do p <- pointB
                 return $ Point (fromPoint p + 1)

-- | Reads in word at point.
readCurrentWordB :: BufferM YiString
readCurrentWordB = readUnitB unitWord

-- | Reads in word before point.
readPrevWordB :: BufferM YiString
readPrevWordB = readPrevUnitB unitViWordOnLine

-------------------------
-- Deletes

-- | Delete one character backward
bdeleteB :: BufferM ()
bdeleteB = deleteB Character Backward

-- | Delete forward whitespace or non-whitespace depending on
-- the character under point.
killWordB :: BufferM ()
killWordB = deleteB unitWord Forward

-- | Delete backward whitespace or non-whitespace depending on
-- the character before point.
bkillWordB :: BufferM ()
bkillWordB = deleteB unitWord Backward

-- | Delete backward to the sof or the new line character
bdeleteLineB :: BufferM ()
bdeleteLineB = atSol >>= \sol -> if sol then bdeleteB else deleteB Line Backward


-- UnivArgument is in Yi.Keymap.Emacs.Utils but we can't import it due
-- to cyclic imports.
-- | emacs' @delete-horizontal-space@ with the optional argument.
deleteHorizontalSpaceB :: Maybe Int -> BufferM ()
deleteHorizontalSpaceB u = do
  c <- curCol
  reg <- regionOfB Line
  text <- readRegionB reg
  let r = deleteSpaces c text
  modifyRegionB (const r) reg
  -- If we only deleted before point, move back that many characters
  -- too or it feels weird.
  case u of
    Just _ -> moveToColB (c - (R.length text - R.length r))
    Nothing -> return ()
  where
    deleteSpaces :: Int -> R.YiString -> R.YiString
    deleteSpaces c l =
      let (f, b) = R.splitAt c l
      in R.dropWhileEnd isSpace f <> case u of
        Nothing -> R.dropWhile isSpace b
        Just _ -> b

----------------------------------------
-- Transform operations

-- | capitalise the word under the cursor
uppercaseWordB :: BufferM ()
uppercaseWordB = transformB (R.withText T.toUpper) unitWord Forward

-- | lowerise word under the cursor
lowercaseWordB :: BufferM ()
lowercaseWordB = transformB (R.withText T.toLower) unitWord Forward

-- | capitalise the first letter of this word
capitaliseWordB :: BufferM ()
capitaliseWordB = transformB capitalizeFirst unitWord Forward

-- | switch the case of the letter under the cursor
switchCaseCharB :: BufferM ()
switchCaseCharB =
  transformB (R.withText $ T.map switchCaseChar) Character Forward

switchCaseChar :: Char -> Char
switchCaseChar c = if isUpper c then toLower c else toUpper c

-- | Delete to the end of line, excluding it.
deleteToEol :: BufferM ()
deleteToEol = deleteRegionB =<< regionOfPartB Line Forward

-- | Delete whole line moving to the next line
deleteLineForward :: BufferM ()
deleteLineForward =
  do moveToSol   -- Move to the start of the line
     deleteToEol -- Delete the rest of the line not including the newline char
     deleteN 1   -- Delete the newline character


-- | Transpose two characters, (the Emacs C-t action)
swapB :: BufferM ()
swapB = do eol <- atEol
           when eol leftB
           transposeB Character Forward

-- | Delete trailing whitespace from all lines
deleteTrailingSpaceB :: BufferM ()
deleteTrailingSpaceB =
  regionOfB Document >>= modifyRegionB (tru . mapLines stripEnd)
  where
    -- Strips the space from the end of each line, preserving
    -- newlines.
    stripEnd :: R.YiString -> R.YiString
    stripEnd x = case R.last x of
      Nothing -> x
      Just '\n' -> (`R.snoc` '\n') $ R.dropWhileEnd isSpace x
      _ -> R.dropWhileEnd isSpace x

    -- | Cut off trailing newlines, making sure to preserve one.
    tru :: R.YiString -> R.YiString
    tru x = if R.length x == 0
            then x
            else (`R.snoc` '\n') $ R.dropWhileEnd (== '\n') x

-- ----------------------------------------------------
-- | Marks

-- | Set the current buffer selection mark
setSelectionMarkPointB :: Point -> BufferM ()
setSelectionMarkPointB p = (.= p) . markPointA =<< selMark <$> askMarks

-- | Get the current buffer selection mark
getSelectionMarkPointB :: BufferM Point
getSelectionMarkPointB = use . markPointA =<< selMark <$> askMarks

-- | Exchange point & mark.
exchangePointAndMarkB :: BufferM ()
exchangePointAndMarkB = do m <- getSelectionMarkPointB
                           p <- pointB
                           setSelectionMarkPointB p
                           moveTo m

getBookmarkB :: String -> BufferM Mark
getBookmarkB = getMarkB . Just


-- ---------------------------------------------------------------------
-- Buffer operations

data BufferFileInfo =
    BufferFileInfo { bufInfoFileName :: FilePath
                   , bufInfoSize     :: Int
                   , bufInfoLineNo   :: Int
                   , bufInfoColNo    :: Int
                   , bufInfoCharNo   :: Point
                   , bufInfoPercent  :: T.Text
                   , bufInfoModified :: Bool
                   }

-- | File info, size in chars, line no, col num, char num, percent
bufInfoB :: BufferM BufferFileInfo
bufInfoB = do
    s <- sizeB
    p <- pointB
    m <- gets isUnchangedBuffer
    l <- curLn
    c <- curCol
    nm <- gets identString
    let bufInfo = BufferFileInfo { bufInfoFileName = T.unpack nm
                                 , bufInfoSize     = fromIntegral s
                                 , bufInfoLineNo   = l
                                 , bufInfoColNo    = c
                                 , bufInfoCharNo   = p
                                 , bufInfoPercent  = getPercent p s
                                 , bufInfoModified = not m
                                 }
    return bufInfo

-----------------------------
-- Window-related operations

upScreensB :: Int -> BufferM ()
upScreensB = scrollScreensB . negate

downScreensB :: Int -> BufferM ()
downScreensB = scrollScreensB

-- | Scroll up 1 screen
upScreenB :: BufferM ()
upScreenB = scrollScreensB (-1)

-- | Scroll down 1 screen
downScreenB :: BufferM ()
downScreenB = scrollScreensB 1

-- | Scroll by n screens (negative for up)
scrollScreensB :: Int -> BufferM ()
scrollScreensB n = do
    h <- askWindow height
    scrollB $ n * max 0 (h - 3) -- subtract some amount to get some overlap (emacs-like).

-- | Scroll according to function passed. The function takes the
-- | Window height in lines, its result is passed to scrollB
-- | (negative for up)
scrollByB :: (Int -> Int) -> Int -> BufferM ()
scrollByB f n = do h <- askWindow height
                   scrollB $ n * f h

-- | Same as scrollB, but also moves the cursor
vimScrollB :: Int -> BufferM ()
vimScrollB n = do scrollB n
                  void $ lineMoveRel n

-- | Same as scrollByB, but also moves the cursor
vimScrollByB :: (Int -> Int) -> Int -> BufferM ()
vimScrollByB f n = do h <- askWindow height
                      vimScrollB $ n * f h

-- | Move to middle line in screen
scrollToCursorB :: BufferM ()
scrollToCursorB = do
    MarkSet f i _ <- markLines
    h <- askWindow height
    let m = f + (h `div` 2)
    scrollB $ i - m

-- | Move cursor to the top of the screen
scrollCursorToTopB :: BufferM ()
scrollCursorToTopB = do
    MarkSet f i _ <- markLines
    scrollB $ i - f

-- | Move cursor to the bottom of the screen
scrollCursorToBottomB :: BufferM ()
scrollCursorToBottomB = do
    MarkSet _ i _ <- markLines
    r <- winRegionB
    t <- lineOf (regionEnd r - 1)
    scrollB $ i - t

-- | Scroll by n lines.
scrollB :: Int -> BufferM ()
scrollB n = do
  MarkSet fr _ _ <- askMarks
  savingPointB $ do
    moveTo =<< use (markPointA fr)
    void $ gotoLnFrom n
    (markPointA fr .=) =<< pointB
  w <- askWindow wkey
  (%=) pointFollowsWindowA (\old w' -> ((w == w') || old w'))


-- Scroll line above window to the bottom.
scrollToLineAboveWindowB :: BufferM ()
scrollToLineAboveWindowB = do downFromTosB 0
                              replicateM_ 1 lineUp
                              scrollCursorToBottomB

-- Scroll line below window to the top.
scrollToLineBelowWindowB :: BufferM ()
scrollToLineBelowWindowB = do upFromBosB 0
                              replicateM_ 1 lineDown
                              scrollCursorToTopB

-- | Move the point to inside the viewable region
snapInsB :: BufferM ()
snapInsB = do
    movePoint <- use pointFollowsWindowA
    w <- askWindow wkey
    when (movePoint w) $ do
        r <- winRegionB
        p <- pointB
        moveTo $ max (regionStart r) $ min (regionEnd r) p

-- | return index of Sol on line @n@ above current line
indexOfSolAbove :: Int -> BufferM Point
indexOfSolAbove n = pointAt $ gotoLnFrom (negate n)

data  RelPosition = Above | Below | Within
  deriving (Show)

-- | return relative position of the point @p@
-- relative to the region defined by the points @rs@ and @re@
pointScreenRelPosition :: Point -> Point -> Point -> RelPosition
pointScreenRelPosition p rs re
  | rs > p && p > re = Within
  | p < rs = Above
  | p > re = Below
pointScreenRelPosition _ _ _ = Within -- just to disable the non-exhaustive pattern match warning

-- | Move the visible region to include the point
snapScreenB :: Maybe ScrollStyle ->BufferM Bool
snapScreenB style = do
    movePoint <- use pointFollowsWindowA
    w <- askWindow wkey
    if movePoint w then return False else do
        inWin <- pointInWindowB =<< pointB
        if inWin then return False else do
            h <- askWindow actualLines
            r <- winRegionB
            p <- pointB
            let gap = case style of
                        Just SingleLine -> case pointScreenRelPosition p (regionStart r) (regionEnd r) of
                                             Above  -> 0
                                             Below  -> h - 1
                                             Within -> 0 -- Impossible but handle it anyway
                        _               -> h `div` 2
            i <- indexOfSolAbove gap
            f <- fromMark <$> askMarks
            markPointA f .= i
            return True


-- | Move to @n@ lines down from top of screen
downFromTosB :: Int -> BufferM ()
downFromTosB n = do
  moveTo =<< use . markPointA =<< fromMark <$> askMarks
  replicateM_ n lineDown

-- | Move to @n@ lines up from the bottom of the screen
upFromBosB :: Int -> BufferM ()
upFromBosB n = do
  r <- winRegionB
  moveTo (regionEnd r - 1)
  moveToSol
  replicateM_ n lineUp

-- | Move to middle line in screen
middleB :: BufferM ()
middleB = do
  w <- ask
  f <- fromMark <$> askMarks
  moveTo =<< use (markPointA f)
  replicateM_ (height w `div` 2) lineDown

pointInWindowB :: Point -> BufferM Bool
pointInWindowB p = nearRegion p <$> winRegionB

-----------------------------
-- Region-related operations

-- | Return the region between point and mark
getRawestSelectRegionB :: BufferM Region
getRawestSelectRegionB = do
  m <- getSelectionMarkPointB
  p <- pointB
  return $ mkRegion p m

-- | Return the empty region if the selection is not visible.
getRawSelectRegionB :: BufferM Region
getRawSelectRegionB = do
  s <- use highlightSelectionA
  if s then getRawestSelectRegionB else do
     p <- pointB
     return $ mkRegion p p

-- | Get the current region boundaries. Extended to the current selection unit.
getSelectRegionB :: BufferM Region
getSelectRegionB = do
  regionStyle <- getRegionStyle
  r <- getRawSelectRegionB
  convertRegionToStyleB r regionStyle

-- | Select the given region: set the selection mark at the 'regionStart'
-- and the current point at the 'regionEnd'.
setSelectRegionB :: Region -> BufferM ()
setSelectRegionB region = do
  assign highlightSelectionA True
  setSelectionMarkPointB $ regionStart region
  moveTo $ regionEnd region

-- | Extend the selection mark using the given region.
extendSelectRegionB :: Region -> BufferM ()
extendSelectRegionB region = (setSelectRegionB . unionRegion region) =<< getSelectRegionB

------------------------------------------
-- Some line related movements/operations

deleteBlankLinesB :: BufferM ()
deleteBlankLinesB = do
  isThisBlank <- isBlank <$> readLnB
  when isThisBlank $ do
    p <- pointB
    -- go up to the 1st blank line in the group
    void $ whileB (R.null <$> getNextLineB Backward) lineUp
    q <- pointB
    -- delete the whole blank region.
    deleteRegionB $ mkRegion p q

-- | Get a (lazy) stream of lines in the buffer, starting at the /next/ line
-- in the given direction.
lineStreamB :: Direction -> BufferM [YiString]
lineStreamB dir = fmap rev . R.lines <$> (streamB dir =<< pointB)
  where
    rev = case dir of
      Forward -> id
      Backward -> R.reverse

-- | Get the next line of text in the given direction. This returns
-- simply 'Nothing' if there no such line.
getMaybeNextLineB :: Direction -> BufferM (Maybe YiString)
getMaybeNextLineB dir = listToMaybe <$> lineStreamB dir

-- | The same as 'getMaybeNextLineB' but avoids the use of the 'Maybe'
-- type in the return by returning the empty string if there is no
-- next line.
getNextLineB :: Direction -> BufferM YiString
getNextLineB dir = fromMaybe R.empty <$> getMaybeNextLineB dir

-- | Get closest line to the current line (not including the current
-- line) in the given direction which satisfies the given condition.
-- Returns 'Nothing' if there is no line which satisfies the
-- condition.
getNextLineWhichB :: Direction -> (YiString -> Bool) -> BufferM (Maybe YiString)
getNextLineWhichB dir cond = listToMaybe . filter cond <$> lineStreamB dir

-- | Returns the closest line to the current line which is non-blank,
-- in the given direction. Returns the empty string if there is no
-- such line (for example if we are on the top line already).
getNextNonBlankLineB :: Direction -> BufferM YiString
getNextNonBlankLineB dir =
  fromMaybe R.empty <$> getNextLineWhichB dir (not . R.null)

------------------------------------------------
-- Some more utility functions involving
-- regions (generally that which is selected)

-- | Uses a string modifying function to modify the current selection
-- Currently unsets the mark such that we have no selection, arguably
-- we could instead work out where the new positions should be
-- and move the mark and point accordingly.
modifySelectionB :: (R.YiString -> R.YiString) -> BufferM ()
modifySelectionB = modifyExtendedSelectionB Character

modifyExtendedSelectionB :: TextUnit -> (R.YiString -> R.YiString) -> BufferM ()
modifyExtendedSelectionB unit transform
    = modifyRegionB transform =<< unitWiseRegion unit =<< getSelectRegionB

-- | Prefix each line in the selection using the given string.
linePrefixSelectionB :: R.YiString -- ^ The string that starts a line comment
                     ->  BufferM ()
linePrefixSelectionB s =
  modifyExtendedSelectionB Line . overInit $ mapLines (s <>)

-- | Uncomments the selection using the given line comment
-- starting string. This only works for the comments which
-- begin at the start of the line.
unLineCommentSelectionB :: R.YiString -- ^ The string which begins a
                                      -- line comment
                        -> R.YiString -- ^ A potentially shorter
                                      -- string that begins a comment
                        -> BufferM ()
unLineCommentSelectionB s1 s2 =
  modifyExtendedSelectionB Line $ mapLines unCommentLine
  where
  (l1, l2) = (R.length s1, R.length s2)

  unCommentLine :: R.YiString -> R.YiString
  unCommentLine line = case (R.splitAt l1 line, R.splitAt l2 line) of
    ((f, s) , (f', s')) | s1 == f   -> s
                        | s2 == f'  -> s'
                        | otherwise -> line

-- | Just like 'toggleCommentSelectionB' but automatically inserts a
-- whitespace suffix to the inserted comment string. In fact:
toggleCommentB :: R.YiString -> BufferM ()
toggleCommentB c = toggleCommentSelectionB (c `R.snoc` ' ') c

-- | Toggle line comments in the selection by adding or removing a
-- prefix to each line.
toggleCommentSelectionB :: R.YiString -> R.YiString -> BufferM ()
toggleCommentSelectionB insPrefix delPrefix = do
  l <- readUnitB Line
  if delPrefix == R.take (R.length delPrefix) l
    then unLineCommentSelectionB insPrefix delPrefix
    else linePrefixSelectionB insPrefix

-- | Justifies all the lines of the selection to be the same as
-- the top line.
-- NOTE: if the selection begins part way along a line, the other
-- lines will be justified only with respect to the part of the indentation
-- which is selected.
justifySelectionWithTopB :: BufferM ()
justifySelectionWithTopB =
  modifySelectionB justifyLines
  where

  justifyLines :: R.YiString -> R.YiString
  justifyLines input =
    case R.lines input of
      []           -> ""
      [ one ]      -> one
      (top : _)    -> mapLines justifyLine input
        where
          -- The indentation of the top line.
          topIndent = R.takeWhile isSpace top

          -- Justify a single line by removing its current indentation
          -- and replacing it with that of the top line. Note that
          -- this will work even if the indentation contains tab
          -- characters.
          justifyLine :: R.YiString -> R.YiString
          justifyLine "" = ""
          justifyLine l  = topIndent <> R.dropWhile isSpace l

-- | Replace the contents of the buffer with some string
replaceBufferContent :: YiString -> BufferM ()
replaceBufferContent newvalue = do
  r <- regionOfB Document
  replaceRegionB r newvalue

-- | Fill the text in the region so it fits nicely 80 columns.
fillRegion :: Region -> BufferM ()
fillRegion = modifyRegionB (R.unlines . fillText 80)

fillParagraph :: BufferM ()
fillParagraph = fillRegion =<< regionOfB unitParagraph

-- | Sort the lines of the region.
sortLines :: BufferM ()
sortLines = modifyExtendedSelectionB Line (onLines sort)

-- | Helper function: revert the buffer contents to its on-disk version
revertB :: YiString -> UTCTime -> BufferM ()
revertB s now = do
    r <- regionOfB Document
    replaceRegionB r s
    markSavedB now

-- get lengths of parts covered by block region
--
-- Consider block region starting at 'o' and ending at 'z':
--
--    start
--      |
--     \|/
-- def foo(bar):
--     baz
--
--     ab
--     xyz0
--      /|\
--       |
--     finish
--
-- shapeOfBlockRegionB returns (regionStart, [2, 2, 0, 1, 2])
-- TODO: accept stickToEol flag
shapeOfBlockRegionB :: Region -> BufferM (Point, [Int])
shapeOfBlockRegionB reg = savingPointB $ do
    (l0, c0) <- getLineAndColOfPoint $ regionStart reg
    (l1, c1) <- getLineAndColOfPoint $ regionEnd reg
    let (left, top, bottom, right) = (min c0 c1, min l0 l1, max l0 l1, max c0 c1)
    lengths <- forM [top .. bottom] $ \l -> do
        void $ gotoLn l
        moveToColB left
        currentLeft <- curCol
        if currentLeft /= left
        then return 0
        else do
            moveToColB right
            rightAtEol <- atEol
            leftOnEol
            currentRight <- curCol
            return $ if currentRight == 0 && rightAtEol
                     then 0
                     else currentRight - currentLeft + 1
    startingPoint <- pointOfLineColB top left
    return (startingPoint, lengths)

leftEdgesOfRegionB :: RegionStyle -> Region -> BufferM [Point]
leftEdgesOfRegionB Block reg = savingPointB $ do
    (l0, _) <- getLineAndColOfPoint $ regionStart reg
    (l1, _) <- getLineAndColOfPoint $ regionEnd reg
    moveTo $ regionStart reg
    fmap catMaybes $ forM [0 .. abs (l0 - l1)] $ \i -> savingPointB $ do
        void $ lineMoveRel i
        p <- pointB
        eol <- atEol
        return (if not eol then Just p else Nothing)
leftEdgesOfRegionB LineWise reg = savingPointB $ do
    lastSol <- do
        moveTo $ regionEnd reg
        moveToSol
        pointB
    let  go acc p = do moveTo p
                       moveToSol
                       edge <- pointB
                       if edge >= lastSol
                       then return $ reverse (edge:acc)
                       else do
                           void $ lineMoveRel 1
                           go (edge:acc) =<< pointB
    go [] (regionStart reg)
leftEdgesOfRegionB _ r = return [regionStart r]

rightEdgesOfRegionB :: RegionStyle -> Region -> BufferM [Point]
rightEdgesOfRegionB Block reg = savingPointB $ do
    (l0, _) <- getLineAndColOfPoint $ regionStart reg
    (l1, _) <- getLineAndColOfPoint $ regionEnd reg
    moveTo $ 1 + regionEnd reg
    fmap (reverse . catMaybes) $ forM [0 .. abs (l0 - l1)] $ \i -> savingPointB $ do
        void $ lineMoveRel $ -i
        p <- pointB
        eol <- atEol
        return (if not eol then Just p else Nothing)
rightEdgesOfRegionB LineWise reg = savingPointB $ do
    lastEol <- do
        moveTo $ regionEnd reg
        moveToEol
        pointB
    let  go acc p = do moveTo p
                       moveToEol
                       edge <- pointB
                       if edge >= lastEol
                       then return $ reverse (edge:acc)
                       else do
                           void $ lineMoveRel 1
                           go (edge:acc) =<< pointB
    go [] (regionStart reg)
rightEdgesOfRegionB _ reg = savingPointB $ do
    moveTo $ regionEnd reg
    leftOnEol
    fmap return pointB

splitBlockRegionToContiguousSubRegionsB :: Region -> BufferM [Region]
splitBlockRegionToContiguousSubRegionsB reg = savingPointB $ do
    (start, lengths) <- shapeOfBlockRegionB reg
    moveTo start
    forM lengths $ \l -> do
        p0 <- pointB
        moveXorEol l
        p1 <- pointB
        let subRegion = mkRegion p0 p1
        moveTo p0
        void $ lineMoveRel 1
        return subRegion

deleteRegionWithStyleB :: Region -> RegionStyle -> BufferM Point
deleteRegionWithStyleB reg Block = savingPointB $ do
    (start, lengths) <- shapeOfBlockRegionB reg
    moveTo start
    forM_ (zip [1..] lengths) $ \(i, l) -> do
        deleteN l
        moveTo start
        lineMoveRel i
    return start

deleteRegionWithStyleB reg style = savingPointB $ do
    effectiveRegion <- convertRegionToStyleB reg style
    deleteRegionB effectiveRegion
    return $! regionStart effectiveRegion

readRegionRopeWithStyleB :: Region -> RegionStyle -> BufferM YiString
readRegionRopeWithStyleB reg Block = savingPointB $ do
    (start, lengths) <- shapeOfBlockRegionB reg
    moveTo start
    chunks <- forM lengths $ \l ->
        if l == 0
        then lineMoveRel 1 >> return mempty
        else do
            p <- pointB
            r <- readRegionB $ mkRegion p (p +~ Size l)
            void $ lineMoveRel 1
            return r
    return $ R.intersperse '\n' chunks
readRegionRopeWithStyleB reg style = readRegionB =<< convertRegionToStyleB reg style

insertRopeWithStyleB :: YiString -> RegionStyle -> BufferM ()
insertRopeWithStyleB rope Block = savingPointB $ do
  let ls = R.lines rope
      advanceLine = atLastLine >>= \case
        False -> void $ lineMoveRel 1
        True -> do
          col <- curCol
          moveToEol
          newlineB
          insertN $ R.replicateChar col ' '

  sequence_ $ intersperse advanceLine $ fmap (savingPointB . insertN) ls
insertRopeWithStyleB rope LineWise = do
    moveToSol
    savingPointB $ insertN rope
insertRopeWithStyleB rope _ = insertN rope

-- consider the following buffer content
--
-- 123456789
-- qwertyuio
-- asdfgh
--
-- The following examples use characters from that buffer as points.
-- h' denotes the newline after h
--
-- 1 r -> 4 q
-- 9 q -> 1 o
-- q h -> y a
-- a o -> h' q
-- o a -> q h'
-- 1 a -> 1 a
--
-- property: fmap swap (flipRectangleB a b) = flipRectangleB b a
flipRectangleB :: Point -> Point -> BufferM (Point, Point)
flipRectangleB p0 p1 = savingPointB $ do
    (_, c0) <- getLineAndColOfPoint p0
    (_, c1) <- getLineAndColOfPoint p1
    case compare c0 c1 of
        EQ -> return (p0, p1)
        GT -> fmap swap $ flipRectangleB p1 p0
        LT -> do
            -- now we know that c0 < c1
            moveTo p0
            moveXorEol $ c1 - c0
            flippedP0 <- pointB
            return (flippedP0, p1 -~ Size (c1 - c0))

movePercentageFileB :: Int -> BufferM ()
movePercentageFileB i = do
    let f :: Double
        f = case fromIntegral i / 100.0 of
               x | x > 1.0 -> 1.0
                 | x < 0.0 -> 0.0 -- Impossible?
                 | otherwise -> x
    lineCount <- lineCountB
    void $ gotoLn $ floor (fromIntegral lineCount * f)
    firstNonSpaceB

findMatchingPairB :: BufferM ()
findMatchingPairB = do
    let go dir a b = goUnmatchedB dir a b >> return True
        goToMatch = do
          c <- readB
          case c of '(' -> go Forward  '(' ')'
                    ')' -> go Backward '(' ')'
                    '{' -> go Forward  '{' '}'
                    '}' -> go Backward '{' '}'
                    '[' -> go Forward  '[' ']'
                    ']' -> go Backward '[' ']'
                    _   -> otherChar
        otherChar = do eof <- atEof
                       eol <- atEol
                       if eof || eol
                           then return False
                           else rightB >> goToMatch
    p <- pointB
    foundMatch <- goToMatch
    unless foundMatch $ moveTo p

-- Vim numbers

-- | Increase (or decrease if negative) next number on line by n.
incrementNextNumberByB :: Int -> BufferM ()
incrementNextNumberByB n = do
    start <- pointB
    untilB_ (not <$> isNumberB) (moveXorSol 1)
    untilB_ (isNumberB) (moveXorEol 1)
    begin <- pointB
    beginIsEol <- atEol
    untilB_ (not <$> isNumberB) (moveXorEol 1)
    end <- pointB
    if beginIsEol then moveTo start
    else do modifyRegionB (increment n) (mkRegion begin end)
            moveXorSol 1

-- | Increment number in string by n.
increment :: Int -> R.YiString -> R.YiString
increment n l = R.fromString $ go (R.toString l)
  where
    go ('0':'x':xs) = ((\ys -> '0':'x':ys) . (flip showHex "") . (+ n) . (fst . head . readHex)) xs
    go ('0':'o':xs) = ((\ys -> '0':'o':ys) . (flip showOct "") . (+ n) . (fst . head . readOct)) xs
    go s            = (show . (+ n) . (\x -> read x :: Int)) s

-- | Is character under cursor a number.
isNumberB :: BufferM Bool
isNumberB = do
    eol <- atEol
    sol <- atSol
    if sol then isDigit <$> readB
    else if eol then return False
         else test3CharB

-- | Used by isNumber to test if current character under cursor is a number.
test3CharB :: BufferM Bool
test3CharB = do
    moveXorSol 1
    previous <- readB
    moveXorEol 2
    next <- readB
    moveXorSol 1
    current <- readB
    if | previous == '0' && current == 'o' && isOctDigit next -> return True  -- octal format
       | previous == '0' && current == 'x' && isHexDigit next -> return True  -- hex format
       |                    current == '-' && isDigit next    -> return True  -- negative numbers
       |                    isDigit current                   -> return True  -- all decimal digits
       |                    isHexDigit current                -> testHexB     -- ['a'..'f'] for hex
       | otherwise                                            -> return False

-- | Characters ['a'..'f'] are part of a hex number only if preceded by 0x.
-- Test if the current occurence of ['a'..'f'] is part of a hex number.
testHexB :: BufferM Bool
testHexB = savingPointB $ do
    untilB_ (not . isHexDigit <$> readB) (moveXorSol 1)
    leftChar <- readB
    moveXorSol 1
    leftToLeftChar <- readB
    if leftChar == 'x' && leftToLeftChar == '0'
    then return True
    else return False
