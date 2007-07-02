-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--

--
-- Handles indentation in the keymaps. Includes:
--  * (TODO) Auto-indentation to the previous lines indentation
--  * Tab-expansion
--  * Shifting of the indentation for a region of text

module Yi.Indent where

import Control.Monad

import Yi.Buffer
import Yi.Core
-- import Yi.Debug
import Yi.Dynamic

import Yi.CharMove
import Yi.Region

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
indentSettingsB = do
    getDynamicB
  
savingExcursionB :: BufferM a -> BufferM a
savingExcursionB f = do
    p <- pointB
    res <- f
    moveTo p
    return res

getPreviousLineB :: BufferM String
getPreviousLineB = 
  savingExcursionB $ do
    lineUp
    readLnB

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
  -- msgE $ show (below, above)
  indentToB $ last (above ++ below)

autoIndentB :: BufferM ()
autoIndentB = do
  is <- savingExcursionB fetchPreviousIndentsB
  pl <- getPreviousLineB
  pli <- indentOfB pl
  cycleIndentsB $ sort $ nub $ pli+2 : is

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
  l <- readLnB
  moveToSol
  deleteToEol
  insertN (replicate level ' ' ++ dropWhile isSpace l)

-- | shiftIndent num 
-- |  shifts right (or left if num is negative) num times, filling in tabs if
-- |  expandTabs is set in the buffers IndentSettings
-- TODO: This uses mkVimRegion
shiftIndentOfLine :: Int -> Action
shiftIndentOfLine numOfShifts = withBuffer $ do
  moveToSol
  sol <- pointB
  firstNonSpaceB
  -- ptOfNonSpace <- pointB
  isAtSol <- atSol
  when (not isAtSol) leftB
  ptOfLastSpace <- pointB
  -- msgE ("ptOfLastSpace= " ++ (show ptOfLastSpace) ++ "-" ++ (show sol) ++ "=" ++ (show (ptOfLastSpace - sol)))
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

shiftIndentOfSelection :: Int -> Action
shiftIndentOfSelection shiftCount = do
  mark <- withBuffer getSelectionMarkPointB
  (row2,_) <- withBuffer getLineAndCol
  withBuffer $ moveTo mark
  (row1,_) <- withBuffer getLineAndCol
  let step = if (row2 > row1) 
             then (withBuffer lineDown)
             else (withBuffer lineUp)
      numOfLines = 1 + (abs (row2 - row1))
  replicateM_ numOfLines (shiftIndentOfLine shiftCount >> step)

