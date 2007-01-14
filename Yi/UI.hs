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

        module Yi.Event   -- UIs need to export the symbolic key names


  )   where

import Yi.Buffer
import Yi.Editor
import Yi.Window as Window
import Yi.Style
import Yi.Vty hiding (def, black, red, green, yellow, blue, magenta, cyan, white)
import Yi.Event
--import Yi.Debug

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Control.Monad                ( ap )
import System.Posix.Signals         ( raiseSignal, sigTSTP )


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
getKey :: UI -> IO () -> IO Yi.Event.Event
getKey (UI vty) doRefresh = do 
  event <- getEvent vty
  case event of 
    (EvResize _ _) -> doRefresh >> getKey (UI vty) doRefresh
    _ -> do -- logPutStrLn $ show $ fromVtyEvent event
            return (fromVtyEvent event)
 where fromVtyEvent (EvKey k mods) = Event k mods
       fromVtyEvent _ = error "fromVtyEvent: unsupported event encountered."

--
-- | Redraw the entire terminal from the UI state
--
-- It is crucial that redraw doesn't modify the editor state (of course
-- it shouldn't). Just slipping in a modifyEditor_ there  will kill
-- your redraw speed, as every redraw will trigger another redraw...
-- So don't be tempted.
--
-- Two points remain: horizontal scrolling, and tab handling.
--
refresh :: IO ()
refresh = refreshEditor $ \e ->
    case ui e             of { (UI vty)  ->
    case getWindows e     of { ws  ->
    case cmdline e        of { cl  ->
    case cmdlinefocus e   of { cmdfoc ->
    case uistyle e        of { sty -> do
    case getWindowOf e    of { w   ->
    case getWindowIndOf e of { Nothing -> return e ; (Just i) -> do
    ws' <- mapM (drawWindow e w sty) ws
    let wImages = map picture ws'
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
    return e { windows = M.fromList [(key w', w') | w' <- ws']}
    }}}}}}}

lines' :: [(Char,a)] -> [[(Char,a)]]
lines' [] =  []
lines' s  =  let (l, s') = break ((== '\n') . fst) s in case s' of
                                                          [] -> [l]
                                                          ((_,x):s'') -> (l++[(' ',x)]) : lines' s''

wrapLine :: Int -> [x] -> [[x]]
wrapLine _ [] = []
wrapLine n l = let (x,rest) = splitAt n l in x : wrapLine n rest

-- ---------------------------------------------------------------------
-- PRIVATE:

showPoint :: Editor -> Window -> IO Window 
showPoint e w = do
  let b = findBufferWith e (bufkey w)          
  ln <- curLn b
  let gap = min (ln-1) ((height w) `div` 2)
      topln = ln - gap
  i <- indexOfSolAbove b gap
  return w {toslineno = topln,
            tospnt = i}

-- | redraw a window (only called when scrolling is needed)
doDrawWindow :: Editor -> Maybe Window -> UIStyle -> Window -> IO Window
doDrawWindow e mwin sty win = do
    let b = findBufferWith e (bufkey win)
        w = width win
        h = height win
        m = mode win
        off = if m then 1 else 0
        h' = h - off
        wsty = styleToAttr (window sty)
        selsty = styleToAttr (selected sty)
        eofsty = eof sty
    markPoint <- getMarkB b
    point <- pointB b
    bufData <- nelemsB b (w*h') (tospnt win) -- read enough chars from the buffer.        
    let (rendered,bos,cur) = drawText h' w (tospnt win) point markPoint selsty wsty (bufData ++ " ")
                             -- we always add one character which can be used to mark the end of file
                                                                                                 
    modeLine <- if m then updateModeLine win b else return Nothing
    let modeLines = map (withStyle (modeStyle sty)) $ maybeToList $ modeLine
        modeStyle = case mwin of
               Just win'' | win'' == win -> modeline_focused
               _                         -> modeline        
        filler = take w (windowfill e : repeat (windowfill e))
    
    return win { picture = take h' (rendered ++ repeat (withStyle eofsty filler)) ++ modeLines,
                 cursor = cur,
                 bospnt = bos }
    
-- | Draw a window
-- TODO: horizontal scrolling.
drawWindow :: Editor
           -> Maybe Window
           -> UIStyle
           -> Window
           -> IO Window

drawWindow e mwin sty win = do
    let b = findBufferWith e (bufkey win)
    point <- pointB b
    (if tospnt win <= point && point <= bospnt win then return win else showPoint e win) >>= doDrawWindow e mwin sty   


-- | Renders text in a rectangle.
-- Also returns a finite map from buffer offsets to their position on the screen.
drawText :: Int -> Int -> Point -> Point -> Point -> Attr -> Attr -> String -> (Pic, Point, (Int,Int))
drawText h w topPoint point markPoint selsty wsty bufData = (rendered, bottomPoint, pntpos)
  where [startSelect, stopSelect] = sort [markPoint,point]
        annBufData = zip bufData [topPoint..]  -- remember the point of each char
        -- TODO: render non-graphic chars (^G and the like)
        lns0 = take h $ concatMap (wrapLine w) $ lines' $ annBufData
        lns = map fillLine $ lns0 -- fill lines with blanks, so the selection looks ok.
        bottomPoint = case lns0 of 
                        [] -> topPoint 
                        _ -> snd $ last $ last $ lns0
        pntpos = case [(y,x) | (y,l) <- zip [0..] lns0, (x,(_char,p)) <- zip [0..] l, p == point] of
                   [] -> (0,0)
                   (pp:_) -> pp

        rendered = map (map colorChar) lns
        colorChar (c, x) = (c,pointStyle x)
        pointStyle x = if startSelect < x && x <= stopSelect then selsty else wsty
        fillLine [] = []
        fillLine l = take w (l ++ repeat (' ',snd $ last l))

-- TODO: The above will actually require a bit of work, in order to properly
-- render all the non-printable chars (<32)

withStyle :: Style -> String -> [(Char, Attr)]
withStyle sty str = zip str (repeat (styleToAttr sty))

