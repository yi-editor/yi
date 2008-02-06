{-# LANGUAGE DeriveDataTypeable #-}

-- Handles indentation in the keymaps. Includes:
--  * (TODO) Auto-indentation to the previous lines indentation
--  * Tab-expansion
--  * Shifting of the indentation for a region of text

module Yi.Indent where

import Control.Monad

import Yi.Buffer
import Yi.Buffer.HighLevel
-- import Yi.Debug
import Yi.Dynamic

import Yi.Buffer.Region

import Data.Char
import Data.Typeable
import Data.List

{- Currently duplicates some of Vim's indent settings. Allowing a buffer to
 - specify settings that are more dynamic, perhaps via closures, could be
 - useful.
 -}
data IndentSettings = IndentSettings {  expandTabs :: Bool
                                      , tabSize :: Int
                                      , shiftWidth :: Int
                                     }
                      deriving (Eq, Show, Typeable)

{- The default indent settings should likely be initializable from a global
 - preference.
 - Currently the initial settings reflect what's usually used in Yi's sources.
 - One aspect that is common in Yi that is not covered is that the first
 - indent is usually 4 spaces instead of 2.
 -}
instance Initializable IndentSettings where
    initial = IndentSettings True 2 2

{- Inserts either a \t or the number of spaces specified by tabSize in the
 - IndentSettings
 -}
insertTabB :: BufferM ()
insertTabB = do
  indentSettings <- indentSettingsB
  insertN $ if expandTabs indentSettings
    then replicate (tabSize indentSettings) ' '
    else ['\t']

indentSettingsB :: BufferM IndentSettings
indentSettingsB = getDynamicB


fetchPreviousIndentsB :: BufferM [Int]
fetchPreviousIndentsB = do
  p0 <- pointB
  lineUp
  p1 <- pointB
  l <- readLnB
  i <- indentOfB l
  if p0 == p1 || i == 0 then return [0] else do
    is <- fetchPreviousIndentsB
    return (i:is)

cycleIndentsB :: [Int] -> BufferM ()
cycleIndentsB indents = do
  l <- readLnB
  curIndent <- indentOfB l
  let (below, above) = span (< curIndent) $ indents
  -- msgEditor $ show (below, above)
  indentToB $ last (above ++ below)

autoIndentB :: BufferM ()
autoIndentB = do
  is  <- savingExcursionB fetchPreviousIndentsB
  pl  <- getPreviousLineB
  pli <- indentOfB pl
  let i       = lastOpenBracket pl
      indents = pli+2 : i : is
  cycleIndentsB $ sort $ nub indents


-- Returns the position of the last opening bracket on the
-- line which is not closed on the same line.
-- Note that if we have unmatched parentheses such as "( ]"
-- then we may not get the correct answer, but in that case
-- then arguably we don't really care if we get the correct
-- answer (at least if we get it wrong the user may notice
-- their error).
lastOpenBracket :: String -> Int
lastOpenBracket s =
  getOpen 0 $ reverse s
  where
  -- We get the last open bracket by counting through
  -- the reversed line, when we see a closed bracket we
  -- add one to the count. When we see an opening bracket
  -- decrease the count. If we see a close bracket when the
  -- count is 0 we return the remaining length of the
  -- (reversed) string as the indentation.
  -- Note then that this assumes that the parentheses are
  -- well matched, although it arguably does the correct thing
  -- in the case that they are not.
  getOpen :: Int -> String -> Int
  -- If we don't see an unmatched open bracket then we just
  -- have to return 0.
  getOpen _ [] = 0
  getOpen i (c : rest)
      -- If it is opening and we have no closing to match
      -- then we return the length of the rest of the line
      -- as the indentation.
    | isOpening c && i == 0 = length rest
      -- If i is not zero then we have matched one of the
      -- closing parentheses and we can decrease the nesting count.
    | isOpening c           = getOpen (i - 1) rest
      -- If the character is a closing bracket then we must increase
      -- the nesting count
    | isClosing c           = getOpen (i + 1) rest
      -- If it is just a normal character forget about it and move on.
    | otherwise             = getOpen i rest
            
  isOpening :: Char -> Bool
  isOpening '(' = True
  isOpening '[' = True
  isOpening '{' = True
  isOpening _   = False

  isClosing :: Char -> Bool
  isClosing ')' = True
  isClosing ']' = True
  isClosing '}' = True
  isClosing _   = False

indentOfB :: String -> BufferM Int
indentOfB = spacingOfB . takeWhile isSpace

spacingOfB :: String -> BufferM Int
spacingOfB text = do
  indentSettings <- indentSettingsB
  let spacingOfChar '\t' = tabSize indentSettings
      spacingOfChar _ = 1
  return $ sum $ map spacingOfChar text

indentToB :: Int -> BufferM ()
indentToB level = do
  l   <- readLnB
  cur <- offsetFromSol
  moveToSol
  deleteToEol
  let (curIndent, restOfLine) = span isSpace l
      origOffsetFromIndent = cur - (length curIndent)
      newFromEol = if origOffsetFromIndent <= 0
                      then length restOfLine
                      else (length restOfLine - origOffsetFromIndent)
  insertN (replicate level ' ' ++ restOfLine)
  leftN newFromEol

-- | Indent as much as the previous line
indentAsPreviousB :: BufferM ()
indentAsPreviousB =
  do pl  <- getPreviousNonBlankLineB
     pli <- indentOfB pl
     indentToB pli


-- | shiftIndent num
-- |  shifts right (or left if num is negative) num times, filling in tabs if
-- |  expandTabs is set in the buffers IndentSettings
-- TODO: This uses mkVimRegion
shiftIndentOfLine :: Int -> BufferM ()
shiftIndentOfLine numOfShifts = do
  moveToSol
  sol <- pointB
  firstNonSpaceB
  -- ptOfNonSpace <- pointB
  isAtSol <- atSol
  when (not isAtSol) leftB
  ptOfLastSpace <- pointB
  -- msgEditor ("ptOfLastSpace= " ++ (show ptOfLastSpace) ++ "-" ++ (show sol) ++ "=" ++ (show (ptOfLastSpace - sol)))
  indentSettings <- indentSettingsB
  let countSpace '\t' = tabSize indentSettings
      countSpace _ = 1 -- we'll assume nothing but tabs and spaces
  cnt <- if isAtSol then return 0
                    else readRegionB (mkVimRegion sol ptOfLastSpace) >>= return . sum . map countSpace
  if not isAtSol then deleteRegionB (mkVimRegion sol ptOfLastSpace)
                 else return ()

  let newcount = cnt + ((shiftWidth indentSettings) * numOfShifts)
  if (newcount <= 0)
     then return ()
     else do
       let tabs   = replicate (newcount `div` (tabSize indentSettings)) '\t'
           spaces = replicate (newcount `mod` (tabSize indentSettings)) ' '
       moveToSol
       insertN $ if expandTabs indentSettings then replicate newcount ' '
                               else tabs ++ spaces

       firstNonSpaceB

shiftIndentOfSelection :: Int -> BufferM ()
shiftIndentOfSelection shiftCount = do
  mark <- getSelectionMarkPointB
  (row2,_) <- getLineAndCol
  moveTo mark
  (row1,_) <- getLineAndCol
  let step = if (row2 > row1)
             then lineDown
             else lineUp
      numOfLines = 1 + (abs (row2 - row1))
  replicateM_ numOfLines (shiftIndentOfLine shiftCount >> step)

