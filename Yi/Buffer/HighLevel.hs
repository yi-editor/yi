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

module Yi.Buffer.HighLevel where

import Yi.Buffer
import Yi.Buffer.Normal
import Control.Monad.State

-- ---------------------------------------------------------------------
-- Movement operations

-- | Delete to the end of line, excluding it.
deleteToEol :: BufferM ()
deleteToEol = do
    p <- pointB
    moveToEol
    q <- pointB
    deleteNAt (q-p) p

-- | Move cursor to origin
topB :: BufferM ()
topB = moveTo 0

-- | Move cursor to end of buffer
botB :: BufferM ()
botB = moveTo =<< sizeB

-- | Get the current line and column number
getLineAndCol :: BufferM (Int, Int)
getLineAndCol = do
  lineNo <- curLn
  colNo  <- offsetFromSol
  return (lineNo, colNo)

-- | Transpose two characters, (the Emacs C-t action)
swapB :: BufferM ()
swapB = do eol <- atEol
           when eol leftB
           c <- readB
           deleteB
           leftB
           insertN [c]
           rightB

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


-- Fold over a range is probably useful too..

-- !!!This is a very bad implementation; delete; apply; and insert the result.
-- | Map a char function over a range of the buffer.
mapRangeB :: Int -> Int -> (Char -> Char) -> BufferM ()
mapRangeB from to fn
    | from < 0  = return ()
    | otherwise = do
            eof <- sizeB
            when (to < eof) $ do
                let loop j | j <= 0    = return ()
                           | otherwise = do
                                readB >>= return . fn >>= writeB
                                rightB
                                loop (j-1)
                loop (max 0 (to - from))
            moveTo from
