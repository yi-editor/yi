{-# LANGUAGE DeriveDataTypeable #-}
-- Copyright (C) 2008 JP Bernardy
module Yi.Buffer.HighLevel where

import Control.Monad.RWS.Strict (ask)
import Control.Monad.State
import Data.Char
import Data.List (isPrefixOf, sort, lines, drop, filter, length, takeWhile, dropWhile)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time (UTCTime)
import Prelude (FilePath, map)
import Yi.Prelude

import Yi.Buffer.Basic
import Yi.Buffer.Misc
import Yi.Buffer.Implementation (newLine)
import Yi.Buffer.Normal
import Yi.Buffer.Region
import Yi.String
import Yi.Window
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8

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
leftOnEol = do
        eol <- atEol
        sol <- atSol
        when (eol && not sol) leftB

-- | Move @x@ chars back, or to the sol, whichever is less
moveXorSol :: Int -> BufferM ()
moveXorSol x = replicateM_ x $ do c <- atSol; when (not c) leftB

-- | Move @x@ chars forward, or to the eol, whichever is less
moveXorEol :: Int -> BufferM ()
moveXorEol x = replicateM_ x $ do c <- atEol; when (not c) rightB

-- | Move to first char of next word forwards
nextWordB :: BufferM ()
nextWordB = moveB unitWord Forward

-- | Move to first char of next word backwards
prevWordB :: BufferM ()
prevWordB = moveB unitWord Backward

-- * Char-based movement actions.

-- | Move to the next occurence of @c@
nextCInc :: Char -> BufferM ()
nextCInc c = doUntilB_ ((c ==) <$> readB) rightB

-- | Move to the character before the next occurence of @c@
nextCExc :: Char -> BufferM ()
nextCExc c = nextCInc c >> leftB

-- | Move to the previous occurence of @c@
prevCInc :: Char -> BufferM ()
prevCInc c = doUntilB_ ((c ==) <$> readB) leftB

-- | Move to the character after the previous occurence of @c@
prevCExc :: Char -> BufferM ()
prevCExc c = prevCInc c >> rightB

-- | Move to first non-space character in this line
firstNonSpaceB :: BufferM ()
firstNonSpaceB = do moveToSol
                    untilB_ ((||) <$> atEol <*> ((not . isSpace) <$> readB)) rightB

-- | Move to the last non-space character in this line
lastNonSpaceB :: BufferM ()
lastNonSpaceB = do moveToEol
                   untilB_ ((||) <$> atSol <*> ((not . isSpace) <$> readB)) leftB

-- | Go to the first non space character in the line;
-- if already there, then go to the beginning of the line.
moveNonspaceOrSol :: BufferM ()
moveNonspaceOrSol = do prev <- readPreviousOfLnB
                       if and . map (isSpace) $ prev then moveToSol else firstNonSpaceB

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

-- | Get the current line and column number
getLineAndCol :: BufferM (Int, Int)
getLineAndCol = (,) <$> curLn <*> curCol

-- | Read the line the point is on
readLnB :: BufferM String
readLnB = readUnitB Line

readCharB :: BufferM (Maybe Char)
readCharB = fmap listToMaybe (readUnitB Character)

-- | Read from point to end of line
readRestOfLnB :: BufferM String
readRestOfLnB = readRegionB =<< regionOfPartB Line Forward

-- | Read from point to beginning of line
readPreviousOfLnB :: BufferM String
readPreviousOfLnB = readRegionB =<< regionOfPartB Line Backward

--------------------------
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


----------------------------------------
-- Transform operations

-- | capitalise the word under the cursor
uppercaseWordB :: BufferM ()
uppercaseWordB = transformB (fmap toUpper) unitWord Forward

-- | lowerise word under the cursor
lowercaseWordB :: BufferM ()
lowercaseWordB = transformB (fmap toLower) unitWord Forward

-- | capitalise the first letter of this word
capitaliseWordB :: BufferM ()
capitaliseWordB = transformB capitalizeFirst unitWord Forward


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

-- ----------------------------------------------------
-- | Marks

-- | Set the current buffer selection mark
setSelectionMarkPointB :: Point -> BufferM ()
setSelectionMarkPointB p = flip setMarkPointB p =<< selMark <$> askMarks

-- | Get the current buffer selection mark
getSelectionMarkPointB :: BufferM Point
getSelectionMarkPointB = getMarkPointB =<< selMark <$> askMarks

-- | Exchange point & mark.
exchangePointAndMarkB :: BufferM ()
exchangePointAndMarkB = do m <- getSelectionMarkPointB
                           p <- pointB
                           setSelectionMarkPointB p
                           moveTo m

getBookmarkB :: String -> BufferM Mark
getBookmarkB nm = getMarkB (Just nm)


-- ---------------------------------------------------------------------
-- Buffer operations

data BufferFileInfo =
    BufferFileInfo { bufInfoFileName :: FilePath
                   , bufInfoSize     :: Int
                   , bufInfoLineNo   :: Int
                   , bufInfoColNo    :: Int
                   , bufInfoCharNo   :: Point
                   , bufInfoPercent  :: String
                   , bufInfoModified :: Bool
                   }

-- | File info, size in chars, line no, col num, char num, percent
bufInfoB :: BufferM BufferFileInfo
bufInfoB = do
    s <- sizeB
    p <- pointB
    m <- isUnchangedB
    l <- curLn
    c <- curCol
    nm <- gets identString
    let bufInfo = BufferFileInfo { bufInfoFileName = nm
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

-- | Scroll down by one screen.
scrollScreenDownB :: BufferM ()
scrollScreenDownB = do
    p <- pointAt $ do moveTo =<< getMarkPointB =<< toMark <$> askMarks
                      gotoLnFrom (-1) -- move back a little so we have some overlap
    t <- fromMark <$> askMarks
    setMarkPointB t p
    moveTo p

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
    scrollB $ n * (h - 1)

-- | Scroll according to function passed. The function takes the
-- | Window height in lines, its result is passed to scrollB
-- | (negative for up)
scrollByB :: (Int -> Int) -> Int -> BufferM ()
scrollByB f n = do h <- askWindow height
                   scrollB $ n * (f h)

-- | Same as scrollB, but also moves the cursor
vimScrollB :: Int -> BufferM ()
vimScrollB n = do scrollB n
                  lineMoveRel n
                  return ()

-- | Same as scrollByB, but also moves the cursor
vimScrollByB :: (Int -> Int) -> Int -> BufferM ()
vimScrollByB f n = do h <- askWindow height
                      vimScrollB $ n * (f h)

-- | Move to middle line in screen
scrollToCursorB :: BufferM ()
scrollToCursorB = do
    MarkSet f i _ t <- markLines
    let m = (f + t) `div` 2
    scrollB $ i - m

-- | Move cursor to the top of the screen
scrollCursorToTopB :: BufferM ()
scrollCursorToTopB = do
    MarkSet f i _ _ <- markLines
    scrollB $ i - f

-- | Move cursor to the bottom of the screen
scrollCursorToBottomB :: BufferM ()
scrollCursorToBottomB = do
    MarkSet _ i _ t <- markLines
    scrollB $ i - t

-- | Scroll by n lines.
scrollB :: Int -> BufferM ()
scrollB n = do putA pointDriveA False
               MarkSet fr _ _ _ <- askMarks
               savingPointB $ do
                   moveTo =<< getMarkPointB fr
                   gotoLnFrom n
                   setMarkPointB fr =<< pointB

-- | Move to @n@ lines down from top of screen
downFromTosB :: Int -> BufferM ()
downFromTosB n = do
  moveTo =<< getMarkPointB =<< fromMark <$> askMarks
  replicateM_ n lineDown

-- | Move to @n@ lines up from the bottom of the screen
upFromBosB :: Int -> BufferM ()
upFromBosB n = do
  moveTo =<< getMarkPointB =<< toMark <$> askMarks
  moveToSol
  replicateM_ n lineUp

-- | Move to middle line in screen
middleB :: BufferM ()
middleB = do
  w <- ask
  f <- fromMark <$> askMarks
  moveTo =<< getMarkPointB f
  replicateM_ (height w `div` 2) lineDown

pointInWindowB :: Point -> BufferM Bool
pointInWindowB p = nearRegion p <$> winRegionB
 --    trace ("pointInWindowB " ++ show fromP ++ " " ++ show toP ++ " " ++ show p) $ (return $ fromP <= p && p <= toP)

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
  s <- getA highlightSelectionA
  if s then getRawestSelectRegionB else do
     p <- pointB
     return $ mkRegion p p

-- | Get the current region boundaries. Extended to the current selection unit.
getSelectRegionB :: BufferM Region
getSelectRegionB = do
  regionStyle <- getA regionStyleA
  r <- getRawSelectRegionB
  mkRegionOfStyleB (regionStart r) (regionEnd r) regionStyle

-- | Select the given region: set the selection mark at the 'regionStart'
-- and the current point at the 'regionEnd'.
setSelectRegionB :: Region -> BufferM ()
setSelectRegionB region = do
  setSelectionMarkPointB $ regionStart region
  moveTo $ regionEnd region

-- | Extend the selection mark using the given region.
extendSelectRegionB :: Region -> BufferM ()
extendSelectRegionB region = (setSelectRegionB . unionRegion region) =<< getSelectRegionB

------------------------------------------
-- Some line related movements/operations

deleteBlankLinesB :: BufferM ()
deleteBlankLinesB =
  do isThisBlank <- isBlank <$> readLnB
     when isThisBlank $ do
       p <- pointB
       -- go up to the 1st blank line in the group
       whileB (isBlank <$> getNextLineB Backward) lineUp
       q <- pointB
       -- delete the whole blank region.
       deleteRegionB $ mkRegion p q

-- | Get a (lazy) stream of lines in the buffer, starting at the /next/ line
-- in the given direction.
lineStreamB :: Direction -> BufferM [String]
lineStreamB dir = fmap (LazyUTF8.toString . rev) . drop 1 . LB.split newLine <$> (streamB dir =<< pointB)
    where rev = case dir of
                  Forward -> id
                  Backward -> LB.reverse

{-
  | Get the next line of text in the given direction. This returns simply 'Nothing' if there
  is no such line.
-}
getMaybeNextLineB :: Direction -> BufferM (Maybe String)
getMaybeNextLineB dir = listToMaybe <$> lineStreamB dir

{-
  | The same as 'getMaybeNextLineB' but avoids the use of the 'Maybe'
  type in the return by returning the empty string if there is no next line.
-}
getNextLineB :: Direction -> BufferM String
getNextLineB dir = fromMaybe "" <$> getMaybeNextLineB dir

{-
  | Get closest line to the current line (not including the current line) in the given direction
  which satisfies the given condition. Returns 'Nothing' if there is
  no line which satisfies the condition.
-}
getNextLineWhichB :: Direction -> (String -> Bool) -> BufferM (Maybe String)
getNextLineWhichB dir cond = listToMaybe . filter cond <$> lineStreamB dir

{-
  | Returns the closest line to the current line which is non-blank, in the given direction.
  Returns the empty string if there is no such line (for example if
  we are on the top line already).
-}
getNextNonBlankLineB :: Direction -> BufferM String
getNextNonBlankLineB dir = fromMaybe "" <$> getNextLineWhichB dir (not . isBlank)


------------------------------------------------
-- Some more utility functions involving
-- regions (generally that which is selected)

-- | Uses a string modifying function to modify the current selection
-- Currently unsets the mark such that we have no selection, arguably
-- we could instead work out where the new positions should be
-- and move the mark and point accordingly.
modifySelectionB :: (String -> String) -> BufferM ()
modifySelectionB = modifyExtendedSelectionB Character


modifyExtendedSelectionB :: TextUnit -> (String -> String) -> BufferM ()
modifyExtendedSelectionB unit transform
    = modifyRegionB transform =<< unitWiseRegion unit =<< getSelectRegionB


-- | Search and Replace all within the current region.
-- Note the region is the final argument since we might perform
-- the same search and replace over multiple regions however we are
-- unlikely to perform several search and replaces over the same region
-- since the first such may change the bounds of the region.
searchReplaceRegionB ::
                       String -- ^ The String to search for
                    -> String -- ^ The String to replace it with
                    -> Region -- ^ The region to perform this over
                       -- The int contained is the difference
                       -- in lengths between the region before and
                       -- after the substitution.
                    -> BufferM ()
searchReplaceRegionB from to =
  modifyRegionB $ substituteInList from to


-- | Peform a search and replace on the selection
searchReplaceSelectionB ::
                           String -- ^ The String to search for
                        -> String
                           -- ^ The String to replace it with
                        -> BufferM ()
searchReplaceSelectionB from to =
  modifySelectionB $ substituteInList from to

replaceString :: String -> String -> BufferM ()
replaceString a b = do end <- sizeB
                       let region = mkRegion 0 end
                       searchReplaceRegionB a b region

-- | Prefix each line in the selection using
-- the given string.
linePrefixSelectionB ::
                         String -- ^ The string that starts a line comment
                      -> BufferM ()
                         -- The returned buffer action
linePrefixSelectionB s =
  modifyExtendedSelectionB Line $ skippingLast $ mapLines (s++)
  where skippingLast f xs = f (init xs) ++ [last xs]

-- | Uncomments the selection using the given line comment
-- starting string. This only works for the comments which
-- begin at the start of the line.
unLineCommentSelectionB :: String -- ^ The string which begins a line comment
                        -> String -- ^ A potentially shorter string that begins a comment
                        -> BufferM ()
unLineCommentSelectionB s1 s2 =
  modifyExtendedSelectionB Line $ mapLines unCommentLine
  where
  unCommentLine :: String -> String
  unCommentLine line
    | isPrefixOf s1 line = drop (length s1) line
    | isPrefixOf s2 line = drop (length s2) line
    | otherwise         = line

-- | Toggle line comments in the selection by adding or removing a prefix to each
-- line.
toggleCommentSelectionB :: String -> String -> BufferM ()
toggleCommentSelectionB insPrefix delPrefix = do
  l <- readUnitB Line
  if (delPrefix `isPrefixOf` l)
    then unLineCommentSelectionB insPrefix delPrefix
    else linePrefixSelectionB insPrefix

-- | Performs as search and replace on the given string.
substituteInList :: Eq a => [ a ] -> [ a ] -> [ a ] -> [ a ]
substituteInList _from _to []       = []
substituteInList from to lst@(h : rest)
  | isPrefixOf from lst =
    to ++ (substituteInList from to $ drop (length from) lst)
  | otherwise            =
    h : (substituteInList from to rest)

-- | Justifies all the lines of the selection to be the same as
-- the top line.
-- NOTE: if the selection begins part way along a line, the other
-- lines will be justified only with respect to the part of the indentation
-- which is selected.
justifySelectionWithTopB :: BufferM ()
justifySelectionWithTopB =
  modifySelectionB justifyLines
  where
  justifyLines :: String -> String
  justifyLines input =
    case lines input of
      []           -> ""
      [ one ]      -> one
      (top : _)    -> mapLines justifyLine input
                      where
                      -- The indentation of the top line.
                      topIndent = takeWhile isSpace top

                      -- Justify a single line by removing its current
                      -- indentation and replacing it with that of the top
                      -- line. Note that this will work even if the indentation
                      -- contains tab characters.
                      justifyLine :: String -> String
                      justifyLine "" = ""
                      justifyLine l  = topIndent ++ (dropWhile isSpace l)


replaceBufferContent :: String -> BufferM ()
replaceBufferContent newvalue = do
  r <- regionOfB Document
  replaceRegionB r newvalue

-- | Fill the text in the region so it fits nicely 80 columns.
fillRegion :: Region -> BufferM ()
fillRegion = modifyRegionClever (unlines' . fillText 80)

fillParagraph :: BufferM ()
fillParagraph = fillRegion =<< regionOfB unitParagraph

-- | Sort the lines of the region.
sortLines :: BufferM ()
sortLines = modifyRegionB (onLines sort) =<< unitWiseRegion Line =<< getSelectRegionB


revertB :: String -> UTCTime -> BufferM ()
revertB s now = do
    savingPointB $ replaceBufferContent s
    markSavedB now
