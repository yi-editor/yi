{-# LANGUAGE DeriveDataTypeable #-}
-- Copyright (C) 2008 JP Bernardy
module Yi.Buffer.HighLevel where

import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.Dynamic
import Data.List
  ( isPrefixOf
  , intercalate
  )

import Data.Maybe
  ( fromMaybe )

import Yi.Buffer
import Yi.Buffer.Normal
import Yi.Buffer.Region
import Yi.String
import Yi.Window
import Yi.Dynamic

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

-- | Move @x@ chars back, or to the sol, whichever is less
moveXorSol :: Int -> BufferM ()
moveXorSol x = replicateM_ x $ do c <- atSol; when (not c) leftB

-- | Move @x@ chars forward, or to the eol, whichever is less
moveXorEol :: Int -> BufferM ()
moveXorEol x = replicateM_ x $ do c <- atEol; when (not c) rightB

-- | Move to first char of next word forwards
nextWordB :: BufferM ()
nextWordB = moveB Word Forward

-- | Move to first char of next word backwards
prevWordB :: BufferM ()
prevWordB = moveB Word Backward

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





------------

-- | Move down next @n@ paragraphs
nextNParagraphs :: Int -> BufferM ()
nextNParagraphs n = replicateM_ n $ moveB Paragraph Forward

-- | Move up prev @n@ paragraphs
prevNParagraphs :: Int -> BufferM ()
prevNParagraphs n = replicateM_ n $ moveB Paragraph Backward


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
getLineAndCol = do
  lineNo <- curLn
  colNo  <- offsetFromSol
  return (lineNo, colNo)

-- | Read the line the point is on
readLnB :: BufferM String
readLnB = readUnitB Line

-- | Read from point to end of line
readRestOfLnB :: BufferM String
readRestOfLnB = readRegionB =<< regionOfPartB Line Forward

--------------------------
-- Deletes

-- | Delete one character backward
bdeleteB :: BufferM ()
bdeleteB = deleteB Character Backward

-- | Delete forward whitespace or non-whitespace depending on
-- the character under point.
killWordB :: BufferM ()
killWordB = deleteB Word Forward

-- | Delete backward whitespace or non-whitespace depending on
-- the character before point.
bkillWordB :: BufferM ()
bkillWordB = deleteB Word Backward


----------------------------------------
-- Transform operations

-- | capitalise the word under the cursor
uppercaseWordB :: BufferM ()
uppercaseWordB = transformB (map toUpper) Word Forward

-- | lowerise word under the cursor
lowercaseWordB :: BufferM ()
lowercaseWordB = transformB (map toLower) Word Forward

-- | capitalise the first letter of this word
capitaliseWordB :: BufferM ()
capitaliseWordB = transformB capitalizeFirst Word Forward


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

-- | Set the current buffer mark
setSelectionMarkPointB :: Int -> BufferM ()
setSelectionMarkPointB pos = do m <- getSelectionMarkB; setMarkPointB m pos

-- | Get the current buffer mark
getSelectionMarkPointB :: BufferM Int
getSelectionMarkPointB = do m <- getSelectionMarkB; getMarkPointB m

-- | Exchange point & mark.
-- Maybe this is better put in Emacs\/Mg common file
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
                   , bufInfoCharNo   :: Int
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
    c <- offsetFromSol
    nm <- gets name
    let bufInfo = BufferFileInfo { bufInfoFileName = nm
                                 , bufInfoSize     = s
                                 , bufInfoLineNo   = l
                                 , bufInfoColNo    = c
                                 , bufInfoCharNo   = p
                                 , bufInfoPercent  = getPercent p s
                                 , bufInfoModified = not m
                                 }
    return bufInfo

-----------------------------
-- Window-related operations

-- | Scroll up 1 screen
upScreenB :: BufferM ()
upScreenB = upScreensB 1

-- | Scroll down 1 screen
downScreenB :: BufferM ()
downScreenB = downScreensB 1

-- | Scroll up n screens
upScreensB :: Int -> BufferM ()
upScreensB = moveScreenB Forward

-- | Scroll down n screens
downScreensB :: Int -> BufferM ()
downScreensB = moveScreenB Backward

moveScreenB :: Direction -> Int -> BufferM ()
moveScreenB dir n = do h <- askWindow height
                       case dir of
                         Forward -> gotoLnFrom (- (n * (h - 1)))
                         Backward -> gotoLnFrom $ n * (h - 1)
                       moveToSol

-- | Move to @n@ lines down from top of screen
downFromTosB :: Int -> BufferM ()
downFromTosB n = do
  moveTo =<< askWindow tospnt
  replicateM_ n lineDown

-- | Move to @n@ lines up from the bottom of the screen
upFromBosB :: Int -> BufferM ()
upFromBosB n = do
  moveTo =<< askWindow bospnt
  moveToSol
  replicateM_ n lineUp

-- | Move to middle line in screen
middleB :: BufferM ()
middleB = do
  w <- askWindow id
  moveTo (tospnt w)
  replicateM_ (height w `div` 2) lineDown

-- | Extend the given region to boundaries of the text unit.
-- For instance one can extend the selection to to complete lines, or
-- paragraphs.
extendRegionToBoundaries :: TextUnit -> BoundarySide -> BoundarySide -> Region -> BufferM Region
extendRegionToBoundaries unit bs1 bs2 region = savingPointB $ do
  moveTo $ regionStart region
  genMaybeMoveB unit (Backward, bs1) Backward
  start <- pointB
  moveTo $ regionEnd region
  genMoveB unit (Forward, bs2) Forward
  stop <- pointB
  return $ mkRegion start stop

unitWiseRegion :: TextUnit -> Region -> BufferM Region
unitWiseRegion unit = extendRegionToBoundaries unit InsideBound OutsideBound


-- TODO: either decide this is evil and contain it to Vim, or embrace it and move it to the
-- Buffer record.
newtype SelectionStyle = SelectionStyle TextUnit
  deriving (Typeable)

instance Initializable SelectionStyle where
  initial = SelectionStyle Character

-- | Get the current region boundaries
getSelectRegionB :: BufferM Region
getSelectRegionB = do
  m <- getMarkPointB =<< getSelectionMarkB
  p <- pointB
  let region = mkRegion m p
  SelectionStyle unit <- getDynamicB
  unitWiseRegion unit region


------------------------------------------
-- Some line related movements/operations

deleteBlankLinesB :: BufferM ()
deleteBlankLinesB =
  do isBlank <- isBlankLineB
     if isBlank
        then moveUpToLastBlankB >> deleteLines
        else return ()
  where
  deleteLines :: BufferM ()
  deleteLines =
    do b1 <- isBlankLineB 
       b2 <- atEof
       if b1 && (not b2)
         then do deleteLineForward 
                 deleteLines
         else return ()

  moveUpToLastBlankB :: BufferM ()
  moveUpToLastBlankB = moveUpToLastLineWhichB $ all isSpace
                           

{-
  TODO: Needs a little more thought this one:
  I think it would be best done by defining a
    getLineAt :: Int -> BufferM (Maybe String)
    and/or
    getLineRelAt :: Int -> BufferM (Maybe String)
moveRelToLastLineWhichB :: Int -> (String -> Bool) -> BufferM ()
moveRelToLastLineWhichB n 
-}


{-
  Moves us up to the last which satisfies the given condition.
  Note that we do not move beyond that last line, so after calling
  this function the point will be on a line which currently satisfies
  the condition. For the other behaviour, that is moving up until
  the current line does not satisfy the condition use 'moveUpUntil'
-}
moveUpToLastLineWhichB :: (String -> Bool) -> BufferM ()
moveUpToLastLineWhichB cond =
  do mLine <- getMaybePreviousLineB
     case mLine of
       Just s  -> if cond s
                     then lineUp >> (moveUpToLastLineWhichB cond)
                     else return ()
       Nothing -> return ()


{-|
  Moves the given amount (which may be negative to go upwards)
  of lines until the target of the move satisfies the given
  condition.
  If the move is not possible, for example we are moving -10
  but are on the 3rd line, then we will move to the top line
  and return 'False'.
  In general returning 'False' indicates that no line was found
  to satisfy the condition, of course if the magnaitude of the
  move is greater than one then we may have skipped over a line
  which satisfies the condition.
-}
moveRelUntil :: Int -> (String -> Bool) -> BufferM Bool
moveRelUntil n cond =
  do li <- readLnB
     if cond li
        then return True
        else do ofs <- lineMoveRel n
                if ofs /= n
                   then return False
                   else lineUp >> (moveUpUntil cond)


{-|
  Moves up until the given condition is true. After this action is returned
  the current line *will* satisfy the condition, unless all the lines above
  the original point do not satisfy the condition in which case the point 
  will be on the top line. We return a bool, this is true if the condition 
  stopped the upwards movement rather than the top of the file.
-}
moveUpUntil :: (String -> Bool) -> BufferM Bool
moveUpUntil = moveRelUntil (-1)

{-|
  The same as 'moveUpUntil' only we move downwards.
-}
moveDownUntil :: (String -> Bool) -> BufferM Bool
moveDownUntil = moveRelUntil 1

{-
  Returns true if the current line is a blank line.
-}
isBlankLineB :: BufferM Bool
isBlankLineB =
  do li <- readLnB
     return $ all isSpace li

{-
  Get the previous line of text. This returns simply 'Nothing' if there
  is no previous line of text.
-}
getMaybePreviousLineB :: BufferM (Maybe String)
getMaybePreviousLineB =
  savingExcursionB $ do ofs <- lineMoveRel (-1)
                        if ofs /= (-1)
                           then return Nothing
                           else liftM Just readLnB

{-
  The same as 'getMaybePreviousLineB' but avoids the use of the 'Maybe'
  type in the return by returning the empty string if there is no previous line.
-}
getPreviousLineB :: BufferM String
getPreviousLineB = liftM (fromMaybe "") getMaybePreviousLineB


{-
  Get closest line above the current line (not including the current line
  which satisfies the given condition. Returns 'Nothing' if there is
  no line above the current one which satisfies the condition.
-}
getPreviousLineWhichB :: (String -> Bool) -> BufferM (Maybe String)
getPreviousLineWhichB cond =
  do savingExcursionB getPLine
  where
  -- Recusively get the previous line of text which statisfies 'cond'
  getPLine :: BufferM (Maybe String)
  getPLine = do ofs <- lineMoveRel (-1)
                li  <- readLnB
                -- if we didn't move then we are on the top line
                -- and hence there is no line above the current line
                -- which satisfies the given condition.
                if (ofs == 0)
                  then return Nothing
                          -- If we did move up one line and the given
                          -- line satisfies the condition then we of
                          -- course return that, otherwise we recursively
                          -- attempt to find one.
                  else if cond li
                         then return $ Just li
                         else getPLine 

{-
  Returns the closest line above the current line which is non-blank.
  Returns the empty string if there is no such line (for example if
  we are on the top line already).
-}
getPreviousNonBlankLineB :: BufferM String
getPreviousNonBlankLineB =
  liftM (fromMaybe "") 
        (getPreviousLineWhichB $ any (not . isSpace))


------------------------------------------------
-- Some more utility functions involving
-- regions (generally that which is selected)

-- | Uses a string modifying function to modify the current selection
-- Currently unsets the mark such that we have no selection, arguably
-- we could insteady work out where the new positions should be
-- and move the mark and point accordingly.
modifySelectionB :: (String -> String) -> BufferM ()
modifySelectionB transform =
  do selRegion <- getSelectRegionB
     modifyRegionB transform selRegion
     unsetMarkB


-- | A helper function for creating functions suitable for
-- 'modifySelectionB' and 'modifyRegionB'.
-- To be used when the desired function should map across
-- the lines of a region.
modifyLines :: (String -> String) -> String -> String
modifyLines transform input
  -- Note the simple definition "unlines (map transform $ lines input)"
  -- only works if there is a newline character at the end of the input
  -- Because 'lines' eats up the newline character but 'unlines' adds
  -- one.
  | last input == '\n' = unlines newLines
  -- For other inputs if we use 'unlines' then a new line is inserted
  -- at the end, so instead of 'unlines' we use 'intercalate.
  | otherwise          = intercalate "\n" newLines
  where
  newLines = map transform $ lines input

-- | Search and Replace all within the current region.
-- Note the region is the final argument since we might perform
-- the same search and replace over multiple regions however we are
-- unlikely to perform several search and replaces over the same region
-- since the first such may change the bounds of the region.
searchReplaceRegionB :: -- | The String to search for
                       String
                       -- | The String to replace it with
                    -> String
                       -- | The region to perform this over
                    -> Region
                       -- | The returned buffer action
                    -> BufferM ()
searchReplaceRegionB from to =
  modifyRegionB $ substituteInList from to


-- | Peform a search and replace on the selection
searchReplaceSelectionB :: -- | The String to search for
                           String
                           -- | The String to replace it with
                        -> String
                           -- | The returned buffer action
                        -> BufferM ()
searchReplaceSelectionB from to =
  modifySelectionB $ substituteInList from to


-- | Comments the selection using line comments that begin
-- with the given string.
lineCommentSelectionB :: -- The string that starts a line comment
                         String
                         -- The returned buffer action
                      -> BufferM ()
lineCommentSelectionB s =
  modifySelectionB $ modifyLines (s ++)

-- | Comments the region using haskell line comments
haskellCommentSelectionB :: BufferM ()
haskellCommentSelectionB = lineCommentSelectionB "-- "

-- | Comments the region using latex line comments
latexCommentSelectionB :: BufferM ()
latexCommentSelectionB = lineCommentSelectionB "% "

-- | Uncomments the selection using the given line comment
-- starting string. This only works for the comments which
-- begin at the start of the line.
unLineCommentSelectionB :: -- | The string which begins a line comment
                           String
                           -- | The returned buffer action
                        -> BufferM ()
unLineCommentSelectionB s =
  modifySelectionB $ modifyLines unCommentLine
  where
  unCommentLine :: String -> String
  unCommentLine line
    | isPrefixOf s line = drop (length s) line
    | otherwise         = line

-- | uncomments a region of haskell line commented code
haskellUnCommentSelectionB :: BufferM ()
haskellUnCommentSelectionB = unLineCommentSelectionB "-- "

-- | uncomments a region of latex line commented code
latexUnCommentSelectionB :: BufferM ()
latexUnCommentSelectionB = unLineCommentSelectionB "% "


-- Performs as search and replace on the given string.
substituteInList :: Eq a => [ a ] -> [ a ] -> [ a ] -> [ a ]
substituteInList _from _to []       = []
substituteInList from to list@(h : rest)
  | isPrefixOf from list = 
    to ++ (substituteInList from to $ drop (length from) list)
  | otherwise            =
    h : (substituteInList from to rest)

-- | Increases the indentation on the region by the given amount
increaseIndentSelectionB :: Int -> BufferM ()
increaseIndentSelectionB i =
  lineCommentSelectionB $ replicate i ' '

-- | Decreases the indentation on the region by the given amount
decreaseIndentSelectionB :: Int -> BufferM ()
decreaseIndentSelectionB i =
  unLineCommentSelectionB $ replicate i ' '


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
      (top : _)    -> modifyLines justifyLine input
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
