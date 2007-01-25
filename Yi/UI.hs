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
        start, end, suspend, main,

        -- * Window manipulation
        newWindow, enlargeWindow, shrinkWindow, 
        doResizeAll, deleteWindow, deleteWindow',
        hasRoomForExtraWindow,

        -- * UI type, abstract.
        UI,

        module Yi.Event   -- UIs need to export the symbolic key names


  )   where

import Prelude hiding (error)

import Yi.Buffer
import Yi.Editor
import Yi.Window as Window
import Yi.Style
import Yi.Vty hiding (def, black, red, green, yellow, blue, magenta, cyan, white)
import Yi.Event
import Yi.Debug

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Data.IORef
import System.Exit
import System.Posix.Signals         ( raiseSignal, sigTSTP )

------------------------------------------------------------------------

data UI = UI { 
              vty :: Vty                     -- ^ Vty
             ,scrsize :: !(IORef (Int,Int))  -- ^ screen size
             ,uiThread :: ThreadId
             }

-- | Initialise the ui
start :: IO UI
start = do
  v <- mkVty
  (x,y) <- Yi.Vty.getSize v
  s <- newIORef (y,x)
  -- fork input-reading thread. important to block *thread* on getKey
  -- otherwise all threads will block waiting for input
  ch <- newChan
  forkIO $ getcLoop v s ch 
  modifyEditor_ $ \e -> return $ e { input = ch }
  t <- myThreadId
  return $ UI v s t
 where
        -- | Action to read characters into a channel
        getcLoop v s ch = repeatM_ $ getKey v s >>= writeChan ch

        -- | Read a key. UIs need to define a method for getting events.
        getKey v sz = do 
          event <- getEvent v
          case event of 
            (EvResize x y) -> writeIORef sz (y,x) >> doResizeAll >> getKey v sz
            _ -> return (fromVtyEvent event)


main = do
  refreshLoop
 where
        --
        -- | When the editor state isn't being modified, refresh, then wait for
        -- it to be modified again. 
        --
        refreshLoop :: IO ()
        refreshLoop = 
            readEditor editorModified >>= \mvar -> repeatM_ $
                    takeMVar mvar >> handleJust ioErrors (logError . show) refresh

-- | Clean up and go home
end :: UI -> IO ()
end i = do  
  Yi.Vty.shutdown (vty i)
  throwTo (uiThread i) (ExitException ExitSuccess)

-- | Suspend the program
suspend :: IO ()
suspend = raiseSignal sigTSTP

-- | Find the current screen height and width.
screenSize :: UI -> IO (Int, Int)
screenSize = readIORef . scrsize


fromVtyEvent :: Yi.Vty.Event -> Yi.Event.Event
fromVtyEvent (EvKey k mods) = Event (fromVtyKey k) (map fromVtyMod mods)
fromVtyEvent _ = error "fromVtyEvent: unsupported event encountered."


fromVtyKey :: Yi.Vty.Key -> Yi.Event.Key
fromVtyKey (Yi.Vty.KEsc     ) = Yi.Event.KEsc      
fromVtyKey (Yi.Vty.KFun x   ) = Yi.Event.KFun x    
fromVtyKey (Yi.Vty.KPrtScr  ) = Yi.Event.KPrtScr   
fromVtyKey (Yi.Vty.KPause   ) = Yi.Event.KPause    
fromVtyKey (Yi.Vty.KASCII c ) = Yi.Event.KASCII c  
fromVtyKey (Yi.Vty.KBS      ) = Yi.Event.KBS       
fromVtyKey (Yi.Vty.KIns     ) = Yi.Event.KIns      
fromVtyKey (Yi.Vty.KHome    ) = Yi.Event.KHome     
fromVtyKey (Yi.Vty.KPageUp  ) = Yi.Event.KPageUp   
fromVtyKey (Yi.Vty.KDel     ) = Yi.Event.KDel      
fromVtyKey (Yi.Vty.KEnd     ) = Yi.Event.KEnd      
fromVtyKey (Yi.Vty.KPageDown) = Yi.Event.KPageDown 
fromVtyKey (Yi.Vty.KNP5     ) = Yi.Event.KNP5      
fromVtyKey (Yi.Vty.KUp      ) = Yi.Event.KUp       
fromVtyKey (Yi.Vty.KMenu    ) = Yi.Event.KMenu     
fromVtyKey (Yi.Vty.KLeft    ) = Yi.Event.KLeft     
fromVtyKey (Yi.Vty.KDown    ) = Yi.Event.KDown     
fromVtyKey (Yi.Vty.KRight   ) = Yi.Event.KRight    
fromVtyKey (Yi.Vty.KEnter   ) = Yi.Event.KEnter    

fromVtyMod :: Yi.Vty.Modifier -> Yi.Event.Modifier
fromVtyMod Yi.Vty.MShift = Yi.Event.MShift
fromVtyMod Yi.Vty.MCtrl  = Yi.Event.MCtrl
fromVtyMod Yi.Vty.MMeta  = Yi.Event.MMeta
fromVtyMod Yi.Vty.MAlt   = Yi.Event.MMeta

--
-- | Redraw the entire terminal from the UI state
--
-- It is crucial that redraw doesn't modify the editor state (of course
-- it shouldn't). Just slipping in a modifyEditor_ there will kill
-- your redraw speed, as every redraw will trigger another redraw...
-- So don't be tempted.
--
-- Two points remain: horizontal scrolling, and tab handling.
--
refresh :: IO ()
refresh = do 
  updateWindows
  withEditor $ \e -> do
    let ws = getWindows e
        wImages = map picture ws
    Yi.Vty.update (vty $ ui e) 
      pic {pImage = concat wImages ++ [withStyle (window $ uistyle e) (cmdline e ++ repeat ' ')],
           pCursor = if cmdlinefocus e
                     then NoCursor 
                     else case getWindowOf e of
                            -- calculate origin of focused window
                            -- sum of heights of windows above this one on screen.
                            -- needs to be shifted 'x' by the width of the tabs on this line
                            Just w -> let (y,x) = cursor w in
                                      Cursor x (y + (sum $ map height $ takeWhile (/= w) $ ws))
                            Nothing -> NoCursor}

updateWindows :: IO ()
updateWindows = refreshEditor $ \e -> do
                  ws <- mapM (\w -> drawWindow e (Just w == getWindowOf e) (uistyle e) w) (getWindows e)
                  return $ e { windows = M.fromList [(key w, w) | w <- ws]}

showPoint :: Editor -> Window -> IO Window 
showPoint e w = do
  let b = findBufferWith e (bufkey w)          
  ln <- curLn b
  let gap = min (ln-1) ((height w) `div` 2)
      topln = ln - gap
  i <- indexOfSolAbove b gap
  return w {toslineno = topln,
            tospnt = i}

-- | redraw a window
doDrawWindow :: Editor -> Bool -> UIStyle -> Window -> IO Window
doDrawWindow e focused sty win = do
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

    --pointData <- nelemsB b 5 point/logPutStrLn $ "doDrawWindow point=" ++ show point ++ " after: " ++ show pointData

    let (rendered,bos,cur) = drawText h' w (tospnt win) point markPoint selsty wsty (bufData ++ " ")
                             -- we always add one character which can be used to position the cursor at the end of file
                                                                                                 
    modeLine <- if m then updateModeLine win b else return Nothing
    let modeLines = map (withStyle (modeStyle sty)) $ maybeToList $ modeLine
        modeStyle = if focused then modeline_focused else modeline        
        filler = take w (windowfill e : repeat ' ')
    
    return win { picture = take h' (rendered ++ repeat (withStyle eofsty filler)) ++ modeLines,
                 cursor = cur,
                 bospnt = bos }
    
-- | Draw a window
-- TODO: horizontal scrolling.
drawWindow :: Editor
           -> Bool
           -> UIStyle
           -> Window
           -> IO Window

drawWindow e focused sty win = do
    let b = findBufferWith e (bufkey win)
    point <- pointB b
    (if tospnt win <= point && point <= bospnt win then return win else showPoint e win) >>= doDrawWindow e focused sty   


-- | Renders text in a rectangle.
-- Also returns a finite map from buffer offsets to their position on the screen.
drawText :: Int -> Int -> Point -> Point -> Point -> Attr -> Attr -> String -> (Pic, Point, (Int,Int))
drawText h w topPoint point markPoint selsty wsty bufData 
    | h == 0 || w == 0 = ([[]], topPoint, (0,0))
    | otherwise        = (rendered, bottomPoint, pntpos)
  where [startSelect, stopSelect] = sort [markPoint,point]
        annBufData = zip bufData [topPoint..]  -- remember the point of each char
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

        -- | Cut a string in lines separated by a '\n' char. 
        -- note that we add a blank character so the cursor can be positioned there.
        lines' :: [(Char,a)] -> [[(Char,a)]]
        lines' [] =  []
        lines' s  =  let (l, s') = break ((== '\n') . fst) s in case s' of
                                                                  [] -> [l]
                                                                  ((_,x):s'') -> (l++[(' ',x)]) : lines' s''

        wrapLine :: Int -> [x] -> [[x]]
        wrapLine _ [] = []
        wrapLine n l = let (x,rest) = splitAt n l in x : wrapLine n rest


-- TODO: The above will actually require a bit of work, in order to properly
-- render all the non-printable chars (<32)

withStyle :: Style -> String -> [(Char, Attr)]
withStyle sty str = zip str (repeat (styleToAttr sty))


------------------------------------------------------------------------
-- | Window manipulation

-- | Create a new window onto this buffer.
-- Top of screen of other windows needs to get adjusted
-- As does their modeslines.
--
newWindow :: FBuffer -> IO Window
newWindow b = modifyEditor $ \e -> do
    (h,w) <- readIORef $ scrsize $ ui $ e
    let wls   = M.elems $ windows e
        (y,r) = getY h (1 + (length wls))   -- should be h-1..
    let wls'  = resizeAll wls y w
    let wls'' = turnOnML wls'
    win  <- emptyWindow b (y+r,w)
    let [win'] = (if null wls then id else turnOnML) [win]
    let e' = e { windows = M.fromList $ mkAssoc (win':wls'') }
    return (e', win')

-- ---------------------------------------------------------------------
-- | Grow the given window, and pick another to shrink
-- grow and shrink compliment each other, they could be refactored.
--
enlargeWindow :: Maybe Window -> IO ()
enlargeWindow Nothing = return ()
enlargeWindow (Just win) = modifyEditor_ $ \e -> do
    let wls      = (M.elems . windows) e
    (maxy,x) <- readIORef $ scrsize $ ui e

    -- can't resize if only window on screen, or if no room left
    if length wls == 1 || height win >= maxy - (2*length wls-1)
        then return e else do
    case elemIndex win wls of
        Nothing -> error "Editor.Window: window not found"
        Just i  -> -- else pick next window up to shrink
            case getWinWithHeight wls i 1 (> 2) of
                Nothing -> return e    -- give up
                Just winnext -> do {
    ;let win'     = resize (height win + 1)     x win
    ;let winnext' = resize (height winnext -1)  x winnext
    ;return $ e { windows = (M.insert (key winnext') winnext' $
                              M.insert (key win') win' $ windows e) }
    }

-- | shrink given window (just grow another)
shrinkWindow :: Maybe Window -> IO ()
shrinkWindow Nothing = return ()
shrinkWindow (Just win) = modifyEditor_ $ \e -> do
    let wls      = (M.elems . windows) e
    (maxy,x) <- readIORef $ scrsize $ ui e
    -- can't resize if only window on screen, or if no room left
    if length wls == 1 || height win <= 3 -- rem..
        then return e else do
    case elemIndex win wls of
        Nothing -> error "Editor.Window: window not found"
        Just i  -> -- else pick a window that could be grown
            case getWinWithHeight wls i 1 (< (maxy - (2 * length wls))) of
                Nothing -> return e    -- give up
                Just winnext -> do {
    ;let win'     = resize (height win - 1)      x win
    ;let winnext' = resize (height winnext + 1)  x winnext
    ;return $ e { windows = (M.insert (key winnext') winnext' $
                              M.insert (key win') win' $ windows e) }
    }

-- | find a window, starting at offset @i + n@, whose height satisifies pred
--
getWinWithHeight :: [Window] -> Int -> Int -> (Int -> Bool) -> Maybe Window
getWinWithHeight wls i n p
   | n > length wls = Nothing
   | otherwise
   = let w = wls !! ((abs (i - n)) `mod` (length wls))
     in if p (height w)
                then Just w
                else getWinWithHeight wls i (n+1) p

--
-- | Delete a window. Note that the buffer that was connected to this
-- window is still open.
--
deleteWindow :: (Maybe Window) -> IO ()
deleteWindow Nothing    = return ()
deleteWindow (Just win) = modifyEditor_ $ \e -> deleteWindow' e win

-- internal, non-thread safe
deleteWindow' :: Editor -> Window -> IO Editor
deleteWindow' e win = do
    (y0,x) <- readIORef $ scrsize $ ui $ e

    let ws    = M.delete (key win) (windows e) -- delete window
        wls   = M.elems ws
        (y,r) = getY (y0 - 1) (length wls) -- why -1?

    let wls'  = resizeAll wls y x -- now resize

    -- now switch focus to a random window
    case wls' of
        []       -> return e { windows = M.empty }
        (win':xs) -> do
            let fm = M.fromList $ mkAssoc wls'
            let win'' = resize (y+r) x win'
            let win''' = if xs == [] then win'' { mode = False } else win''
            let e' = e { windows = M.insert (key win''') win''' fm }
            setWindow' e' win'''

------------------------------------------------------------------------

-- | Update height of windows in window set
resizeAll :: [Window] -> Int -> Int -> [Window]
resizeAll wls y x = flip map wls (\w -> resize y x w)

-- | Reset the heights and widths of all the windows
doResizeAll :: IO ()
doResizeAll = modifyEditor_ $ \e -> do
    let i = ui e
    (h,w) <- readIORef $ scrsize i
    let wls   = M.elems (windows e)
        (y,r) = getY h (length wls) -- why -1?

    let wls' = map (doresize w y) (init wls)
        wls'' = let win = last wls
                in (doresize w (y+r-1) win : wls')

    return e { windows = M.fromList $ mkAssoc wls'' }

    where doresize x y win = resize y x win

-- | Turn on modelines of all windows
turnOnML :: [Window] -> [Window]
turnOnML = map $ \w -> w { mode = True }

-- | Has the frame enough room for an extra window.
hasRoomForExtraWindow :: IO Bool
hasRoomForExtraWindow = do
    i     <- sizeWindows
    (y,_) <- screenSize =<< readEditor ui -- bah
    let (sy,r) = getY y i
    return $ sy + r <= 4  -- min window size

-- | calculate window heights, given all the windows and current height
-- doesn't take into account modelines
getY :: Int -> Int -> (Int,Int)
getY h 0 = (h, 0)
getY h 1 = (h, 0)
getY h l = h `quotRem` l
