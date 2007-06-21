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

module Yi.Vty.UI (start) where

import Prelude hiding (error, concatMap, sum, mapM, sequence)

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad (liftM)
import Control.Monad.Reader (ask)
import Control.Monad.State (runState, State, gets, modify, get, put)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Arrow (second)
import Data.Char (ord,chr)
import Data.Foldable
import Data.IORef
import Data.List (partition, sort)
import Data.Maybe
import Data.Traversable
import System.Exit
import System.Posix.Signals         ( raiseSignal, sigTSTP )
import Yi.Buffer
import Yi.FastBuffer
import Yi.Debug
import Yi.Editor
import Yi.Event
import Yi.Monad
import Yi.Style
import Yi.Vty hiding (def, black, red, green, yellow, blue, magenta, cyan, white)
import Yi.WindowSet as WS
import qualified Data.ByteString.Char8 as B
import qualified Yi.CommonUI as Common
import Yi.CommonUI (Window (..), pointInWindow)

------------------------------------------------------------------------

data Rendered = 
    Rendered {
              picture :: !Image           -- ^ the picture currently displayed.
             ,cursor  :: !(Maybe (Int,Int)) -- ^ cursor point on the above
             }




data UI = UI { 
              vty       :: Vty                     -- ^ Vty
             ,scrsize   :: !(IORef (Int,Int))  -- ^ screen size
             ,uiThread  :: ThreadId
             ,cmdline   :: IORef String
             ,uiRefresh :: MVar ()
             ,windows   :: MVar (WS.WindowSet Common.Window)
             }

mkUI ui = Common.UI 
  {
   Common.main                  = main ui,
   Common.end                   = end ui,
   Common.suspend               = suspend               ui,
   Common.refreshAll            = return (),
   Common.scheduleRefresh       = scheduleRefresh       ui,
   Common.prepareAction         = prepareAction         ui,
   Common.setCmdLine            = setCmdLine            ui
  }


-- | Initialise the ui
start :: Chan Yi.Event.Event -> MVar (WS.WindowSet Common.Window) -> EditorM Common.UI
start ch ws0 = do
  editor <- ask
  liftIO $ do 
          v <- mkVty
          (x0,y0) <- Yi.Vty.getSize v
          sz <- newIORef (y0,x0)
          -- fork input-reading thread. important to block *thread* on getKey
          -- otherwise all threads will block waiting for input
          t <- myThreadId
          cmd <- newIORef ""
          tuiRefresh <- newEmptyMVar
          let result = UI v sz t cmd tuiRefresh ws0
              -- | Action to read characters into a channel
              getcLoop = repeatM_ $ getKey >>= writeChan ch

              -- | Read a key. UIs need to define a method for getting events.
              getKey = do 
                event <- getEvent v
                case event of 
                  (EvResize x y) -> do logPutStrLn $ "UI: EvResize: " ++ show (x,y)
                                       writeIORef sz (y,x) >> refresh result editor >> getKey
                  _ -> return (fromVtyEvent event)
          forkIO $ getcLoop
          return (mkUI result)
        

main :: UI -> IORef Editor -> IO ()
main ui editor = do
  let
      -- | When the editor state isn't being modified, refresh, then wait for
      -- it to be modified again. 
      refreshLoop :: IO ()
      refreshLoop = repeatM_ $ do 
                      takeMVar (uiRefresh ui)
                      handleJust ioErrors (\except -> do 
                                             logPutStrLn "refresh crashed with IO Error"
                                             logError $ show $ except)
                                     (refresh ui editor)
  scheduleRefresh' ui
  logPutStrLn "refreshLoop started"
  refreshLoop
  

-- | Clean up and go home
end :: UI -> IO ()
end i = do  
  Yi.Vty.shutdown (vty i)
  throwTo (uiThread i) (ExitException ExitSuccess)

-- | Suspend the program
suspend :: UI -> EditorM ()
suspend _ = liftIO $ raiseSignal sigTSTP

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

-- | Redraw the entire terminal from the UI.
-- Among others, this re-computes the heights and widths of all the windows.

-- Two points remain: horizontal scrolling, and tab handling.
refresh :: UI -> IORef Editor -> IO ()
refresh ui eRef = modifyMVar_ (windows ui) $ \ws0 -> do
  e <- readRef eRef                 
  logPutStrLn "refreshing screen."
  (yss,xss) <- readRef (scrsize ui)
  let ws1 = computeHeights yss ws0
  cmd <- readRef (cmdline ui)
  zzz <- mapM (scrollAndRenderWindow e (uistyle e) xss) (WS.withFocus ws1)

  let startXs = scanrT (+) 0 (fmap height ws1)
      wImages = fmap picture $ fmap snd $ zzz
     
  WS.debug "Drawing: " ws1
  logPutStrLn $ "startXs: " ++ show startXs
  Yi.Vty.update (vty $ ui) 
      pic {pImage = vertcat (toList wImages) <-> withStyle (window $ uistyle e) (take xss $ cmd ++ repeat ' '),
           pCursor = let Just (y,x) = cursor (snd $ WS.current zzz) in
                     Cursor x (y + WS.current startXs)}
  
  return (fmap fst zzz)

scanrT (+*+) k t = fst $ runState (mapM f t) k
    where f x = do s <- get
                   let s' = s +*+ x
                   put s'
                   return s
           

-- | Scrolls the window to show the point if needed
scrollAndRenderWindow :: Editor -> UIStyle -> Int -> (Window, Bool) -> IO (Window, Rendered)
scrollAndRenderWindow e sty width (win,hasFocus) = do
    let b = findBufferWith e (bufkey win)
    (point, []) <- runBuffer b pointB
    win' <- (if not hasFocus || pointInWindow point win then return win else showPoint e win)
    (rendered, bos) <- drawWindow e sty hasFocus width win'
    return (win' {bospnt = bos}, rendered)

-- | return index of Sol on line @n@ above current line
indexOfSolAbove :: Int -> BufferM Int
indexOfSolAbove n = do
    p <- pointB
    moveToSol
    loop n
    q <- pointB
    moveTo p
    return q

    where loop 0 = return ()
          loop i = lineUp >> loop (i-1)

showPoint :: Editor -> Window -> IO Window 
showPoint e w = do
  logPutStrLn $ "showPoint " ++ show w
  let b = findBufferWith e (bufkey w)          
  (result, []) <- runBuffer b $ 
            do ln <- curLn
               let gap = min (ln-1) (height w `div` 2)
               i <- indexOfSolAbove gap
               return w {tospnt = i}
  return result

-- | Draw a window
-- TODO: horizontal scrolling.
drawWindow :: Editor -> UIStyle -> Bool -> Int -> Window -> IO (Rendered, Int)
drawWindow e sty focused w win = do
    let b = findBufferWith e (bufkey win)
        m = not (isMini win)
        off = if m then 1 else 0
        h' = height win - off
        wsty = styleToAttr (window sty)
        selsty = styleToAttr (selected sty)
        eofsty = eof sty
    (markPoint, []) <- runBuffer b (getMarkPointB =<< getSelectionMarkB)
    (point, []) <- runBuffer b pointB
    bufData <- nelemsBIH (rawbuf b) (w*h') (tospnt win) -- read enough chars from the buffer.
    let prompt = if isMini win then name b else ""

    let (rendered,bos,cur) = drawText h' w 
                                (tospnt win - length prompt) 
                                point markPoint 
                                selsty wsty 
                                (zip prompt (repeat wsty) ++ map (second styleToAttr) bufData ++ [(' ',attr)])
                             -- we always add one character which can be used to position the cursor at the end of file
                                                                                                 
    (modeLine0, []) <- runBuffer b getModeLine
    let modeLine = if m then Just modeLine0 else Nothing
    let modeLines = map (withStyle (modeStyle sty) . take w . (++ repeat ' ')) $ maybeToList $ modeLine
        modeStyle = if focused then modeline_focused else modeline        
        filler = take w (windowfill e : repeat ' ')
    
    return (Rendered { picture = vertcat (take h' (rendered ++ repeat (withStyle eofsty filler)) ++ modeLines),
                       cursor = cur}, 
            bos)
            
  
-- | Renders text in a rectangle.
-- This also returns 
-- * the index of the last character fitting in the rectangle
-- * the position of the Point in (x,y) coordinates, if in the window.
drawText :: Int -> Int -> Point -> Point -> Point -> Attr -> Attr -> [(Char,Attr)] -> ([Image], Point, Maybe (Int,Int))
drawText h w topPoint point markPoint selsty wsty bufData 
    | h == 0 || w == 0 = ([], topPoint, Nothing)
    | otherwise        = (rendered_lines, bottomPoint, pntpos)
  where [startSelect, stopSelect] = sort [markPoint,point]

        -- | Remember the point of each char
        annotateWithPoint text = zipWith (\(c,a) p -> (c,(a,p))) text [topPoint..]  

        lns0 = take h $ concatMap (wrapLine w) $ map (concatMap expandGraphic) $ lines' $ annotateWithPoint $ bufData

        bottomPoint = case lns0 of 
                        [] -> topPoint 
                        _ -> snd $ snd $ last $ last $ lns0

        pntpos = listToMaybe [(y,x) | (y,l) <- zip [0..] lns0, (x,(_char,(_attr,p))) <- zip [0..] l, p == point]

        rendered_lines = map fillColorLine $ lns0 -- fill lines with blanks, so the selection looks ok.
        colorChar (c, (a, x)) = renderChar (pointStyle x a) c
        pointStyle x a = if startSelect < x && x <= stopSelect && selsty /= wsty then selsty else a

        fillColorLine [] = renderHFill attr ' ' w
        fillColorLine l = horzcat (map colorChar l) <|> renderHFill (pointStyle x a) ' ' (w - length l)
            where (_,(a,x)) = last l

        -- | Cut a string in lines separated by a '\n' char. Note
        -- that we add a blank character where the \n was, so the
        -- cursor can be positioned there.

        lines' :: [(Char,a)] -> [[(Char,a)]]
        lines' [] =  []
        lines' s  =  let (l, s') = break ((== '\n') . fst) s in case s' of
                                                                  [] -> [l]
                                                                  ((_,x):s'') -> (l++[(' ',x)]) : lines' s''

        wrapLine :: Int -> [x] -> [[x]]
        wrapLine _ [] = []
        wrapLine n l = let (x,rest) = splitAt n l in x : wrapLine n rest
                                      
        expandGraphic (c,p) | ord c < 32 = [('^',p),(chr (ord c + 64),p)]
                            | ord c < 128 = [(c,p)]
                            | otherwise = zip ('\\':show (ord c)) (repeat p)
                                            


-- TODO: The above will actually require a bit of work, in order to handle tabs.

withStyle :: Style -> String -> Image
withStyle sty str = renderBS (styleToAttr sty) (B.pack str)


------------------------------------------------------------------------


-- | Schedule a refresh of the UI.
scheduleRefresh :: UI -> EditorM ()
scheduleRefresh ui = do
  modifyEditor_ $ \e -> return e {editorUpdates = []}
  liftIO $ scheduleRefresh' ui

prepareAction :: UI -> EditorM ()
prepareAction _ = return ()


scheduleRefresh' :: UI -> IO ()
scheduleRefresh' tui = tryPutMVar (uiRefresh tui) () >> return ()

-- | Calculate window heights, given all the windows and current height.
-- (No specific code for modelines)
computeHeights :: Int -> WindowSet Window  -> WindowSet Window
computeHeights height ws = result
  where (mwls, wls) = partition isMini (toList ws)
        (y,r) = getY (height - length mwls) (length wls) 
        (result, _) = runState (Data.Traversable.mapM distribute ws) ((y+r-1) : repeat y)

distribute :: Window -> State [Int] Window
distribute win = case isMini win of
                 True -> return win {height = 1}
                 False -> do h <- gets head
                             modify tail
                             return win {height = h}

getY :: Int -> Int -> (Int,Int)
getY screenHeight 0               = (screenHeight, 0)
getY screenHeight numberOfWindows = screenHeight `quotRem` numberOfWindows

setCmdLine :: UI -> String -> IO ()
setCmdLine i s = do 
  writeIORef (cmdline i) s
