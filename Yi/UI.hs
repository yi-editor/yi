{-# OPTIONS -#include "YiUtils.h" #-}
--
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
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
-- Derived from: riot/UI.hs
--
--      Copyright (c) Tuomo Valkonen 2004.
--
-- Released under the same license.
--

-- | This module defines a user interface implemented using ncurses.

module Yi.UI (

        -- * UI initialisation
        start, end, suspend,

        -- * Input
        getKey,

        -- * Drawing
        refresh,
        screenSize,

        -- * UI type, abstract.
        UI,

        module Yi.Vty   -- UIs need to export the symbolic key names


  )   where

import Yi.Buffer        ( Buffer( ptrToLnsB
                                , getMarkB  ) )
import Yi.Editor
import Yi.Window
import Yi.Style
import Yi.Vty

import qualified Data.ByteString.Char8 as BS

import Data.List

import Control.Monad                ( ap )
import System.Posix.Signals         ( raiseSignal, sigTSTP )


-- Just really to allow me to give some signatures

import Foreign.C.String     ( CString )
------------------------------------------------------------------------

newtype UI = UI Vty --{ vty :: Vty }

-- | how to initialise the ui
start :: IO UI
start = do
  v <- mkVty
  return (UI v)

-- | Clean up and go home
end :: UI -> IO ()
end (UI vty) = Yi.Vty.shutdown vty 

-- | Suspend the program
suspend :: IO ()
suspend = raiseSignal sigTSTP

-- | Find the current screen height and width.
screenSize :: UI -> IO (Int, Int)
screenSize (UI vty) = return swap `ap` Yi.Vty.getSize vty
    where swap (x,y) = (y,x)
--
-- | Read a key. UIs need to define a method for getting events.
--

getKey :: UI -> IO () -> IO Char
getKey (UI vty) doRefresh = do 
  event <- getEvent vty
  case event of 
    (EvResize _ _) -> doRefresh >> getKey (UI vty) refresh
    _ -> return (eventToChar event)

--
-- | Redraw the entire terminal from the UI state
-- Optimised.
--
-- It is crucial that redraw doesn't modify the editor state (of course
-- it shouldn't). Just slipping in a modifyEditor_ there  will kill
-- your redraw speed, as every redraw will trigger another redraw...
-- So don't be tempted.
--
-- Two points remain: horizontal scrolling, and tab handling.
--
redraw :: IO ()
redraw = withEditor $ \e ->
    case ui e             of { (UI vty)  ->
    case getWindows e     of { ws  ->
    case cmdline e        of { cl  ->
    case cmdlinefocus e   of { cmdfoc ->
    case uistyle e        of { sty ->
    case getWindowOf e    of { w   ->
    case getWindowIndOf e of { Nothing -> return () ; (Just i) -> do
                                              
    wImages <- mapM (drawWindow e w sty) ws
    Yi.Vty.update vty pic {pImage = concat wImages ++ [withStyle (window sty) (cl ++ repeat ' ')],
                           pCursor = if cmdfoc 
                                     then NoCursor 
                                     else case w of
                                         -- calculate origin of focused window
                                         -- sum of heights of windows above this one on screen.
                                         -- needs to be shifted 'x' by the width of the tabs on this line
                                         Just w' -> let (y,x) = cursor w' in
                                             Cursor x (y + sum [ height (ws !! k) | k <- [0 .. (i-1)] ])
                                         Nothing -> NoCursor}

    }}}}}}}

-- ---------------------------------------------------------------------
-- PRIVATE:

--
-- | Draw a screen to the screen
--
-- Ok. Now, how do we deal with line wrapping? The lns we get from ptrs
-- will have to be broken up, dropping some off the end. The cursor
-- will have to be recalculated too.
--
drawWindow :: Editor
           -> Maybe Window
           -> UIStyle
           -> Window
           -> IO Pic

drawWindow e mwin sty win =
    -- so t is the current point at the top of the screen.
    -- pnt is where the current cursor is.
    case win of { Window { bufkey = u
                         , mode   = m
                         , height = h
                         , width  = w
                         , tospnt = t 
                         , pnt    = point} ->
    case window sty   of { wsty ->
    case selected sty of { selsty ->
    case eof    sty   of { eofsty -> do
    case findBufferWith e u of { b -> do
    let modeLines = case m of 
                      Nothing -> []
                      Just text -> [withStyle (modeStyle sty) text]
        modeStyle = case mwin of
               Just win' | win' == win -> modeline_focused
               _                       -> modeline

        off = length modeLines
        h' = h - off
        filler = repeat (windowfill e)
    lns <- ptrToLnsB b t h' w

    -- draw each buffer line
    -- ToDo, horizontal scrolling. determine how many screen widths to
    -- drop off the string (i.e. add to the ptr..)
    --
    -- This `len' doesn't take tabs into account. Leading to curses draw
    -- errors on lines with tabs in them. Solution, find a len that
    -- includes tab widths
    --
    -- `len' is number of chars in the line -- not the screen width.
    -- so `w' is wrong in the presence of tabs.
    --
    -- need a function that takes the desired width, and tells us how
    -- many real chars to take.
    --

    {-
      That was the first attempt, now what we'll do is split each
      line in to three parts, the bit before the selection, the
      bit within the selection and the bit after the selection,
      at most one line will have all there parts
      (and then only if the selection forms a middle part of
      one line), most will either be all selection, if the
      selection covers multiple lines, or completely before or
      completely after the selection. 
    -}
    markPoint <- getMarkB b
    let startSelect = min markPoint point
        stopSelect  = (max markPoint point) + 1

        -- The integer argument is the current point at the start of
        -- the line we wish to draw
        drawLines :: Int -> [(CString, Int)] -> Pic
        drawLines _ []                    = []
        drawLines sol ((ptr,len) : rest) =
            -- @todo{Make sure these can't *all* be zero
            -- Notice for example that some conditions imply others, eg
            -- stopSelect < eol implies startSelect < eol, so
            -- startSelect > sol && stopSelect < eol implies that
            -- the selection starts and ends on this line.

            -- I think that the inSel conditions can be slightly optimised.
            let eol        = sol + len
                byteString = BS.packCString ptr
                beforeSel
                    | startSelect > sol         = min len (startSelect - sol)
                    | otherwise                 = 0
                inSel    
                    -- selection starts and ends on this line
                    | startSelect >= sol &&
                      stopSelect  <= eol         = stopSelect - startSelect

                    -- selection is entirely before this line
                    | stopSelect  < sol         = 0

                    -- selection is entirely after this line
                    | startSelect > eol         = 0

                    -- this line is entirely within the selection
                    | startSelect < sol &&
                      stopSelect  > eol         = len

                    -- selection begins before this line, ends during it
                    | startSelect < sol &&
                      stopSelect  > sol         = stopSelect - sol

                    -- selection begins on this line, ends after it
                    | startSelect >= sol &&
                      stopSelect  > eol         = eol - startSelect

                    -- selection outside this line (not really needed)
                    | otherwise                 = 0
                afterSel = len - (beforeSel + inSel)
                --     | stopSelect < eol          = min len (eol - stopSelect)
                --     | otherwise                 = 0
                (beforeSelPtrB,
                 afterStartPtrB) = BS.splitAt beforeSel byteString
                (inSelPtrB,
                 afterSelPtrB)   = BS.splitAt inSel afterStartPtrB
            in (withStyle wsty   (map renderChar $ BS.unpack $ beforeSelPtrB) ++
                withStyle selsty (map renderChar $ BS.unpack $ inSelPtrB) ++
                withStyle wsty   ((map renderChar $ take afterSel $ BS.unpack $ afterSelPtrB) ++ filler))
              : drawLines (sol + len) rest
    return (take h' (drawLines t lns ++ repeat (withStyle eofsty filler)) ++ modeLines)

    }}}}}

renderChar :: Char -> Char
renderChar '\n' = ' ' -- Till we find a better rendering.
renderChar c = c

-- TODO: The above will actually require a bit of work, in order to properly
-- render all the non-printable chars (<32)

withStyle :: Style -> String -> [(Char, Int)]
withStyle sty str = zip str (repeat (fromAttr $ styleToAttr sty))

-- | redraw and refresh the screen
refresh :: IO ()
refresh = redraw




