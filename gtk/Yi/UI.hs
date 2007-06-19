--
-- Copyright (c) 2007 Jean-Philippe Bernardy
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


-- | This module defines a user interface implemented using gtk2hs.

module Yi.UI (start) where

import Prelude hiding (error, sequence_, elem, mapM_)

import Yi.Buffer
import Yi.Editor
import Yi.Event
import Yi.Debug
import Yi.Undo
import Yi.Monad
import qualified Yi.CommonUI as Common
import Yi.CommonUI (Window)
import qualified Yi.WindowSet as WS

import Control.Concurrent ( yield )
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.Reader (ask, lift, liftIO, liftM, when, MonadIO)
import Control.Monad (ap)

import Data.IORef
import Data.List ( nub )
import Data.Maybe
import Data.Unique
import Data.Foldable
import qualified Data.Map as M

import Graphics.UI.Gtk hiding ( Window, Event )          
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.SourceView

------------------------------------------------------------------------

data UI = UI {
              uiWindow :: Gtk.Window
             ,uiBox :: VBox
             ,uiCmdLine :: Label
             ,uiBuffers :: IORef (M.Map Unique SourceBuffer)
             ,windows   :: MVar (WS.WindowSet Window)     -- ^ all the windows
             ,windowCache :: IORef [WinInfo]
             }

data WinInfo = WinInfo 
    {
     bufkey      :: !Unique         -- ^ the buffer this window opens to
    ,textview    :: SourceView
    ,modeline    :: Label
    ,widget      :: Box            -- ^ Top-level widget for this window.
    ,isMini      :: Bool
    }

instance Show WinInfo where
    show w = "W" ++ show (hashUnique $ bufkey w)

winkey w = (isMini w, bufkey w)


mkUI ui = Common.UI 
  {
   Common.main                  = main                  ui,
   Common.end                   = end,
   Common.suspend               = suspend               ui,
   Common.refreshAll            = return (),
   Common.scheduleRefresh       = scheduleRefresh       ui,
   Common.prepareAction         = prepareAction         ui,
   Common.setCmdLine            = setCmdLine            ui
  }

-- | Initialise the ui
start :: MVar (WS.WindowSet Common.Window) -> EditorM (Chan Yi.Event.Event, Common.UI)
start ws0 = lift $ do
  initGUI

  win <- windowNew

  ch <- newChan
  onKeyPress win (processEvent ch)

  vb <- vBoxNew False 1  -- Top-level vbox
  vb' <- vBoxNew False 1

  set win [ containerChild := vb ]
  onDestroy win mainQuit
                
  cmd <- labelNew Nothing
  set cmd [ miscXalign := 0.01 ]
  f <- fontDescriptionNew
  fontDescriptionSetFamily f "Monospace"
  widgetModifyFont cmd (Just f)

  set vb [ containerChild := vb', 
           containerChild := cmd, 
           boxChildPacking cmd := PackNatural] 

  -- use our magic threads thingy (http://haskell.org/gtk2hs/archives/2005/07/24/writing-multi-threaded-guis/)
  timeoutAddFull (yield >> return True) priorityDefaultIdle 50

  widgetShowAll win

  bufs <- newIORef M.empty
  wc <- newIORef []
  let ui = UI win vb' cmd bufs ws0 wc

  return (ch, mkUI ui)


main :: UI -> IORef Editor -> IO ()
main _editor _ui = 
    do logPutStrLn "GTK main loop running"
       mainGUI


instance Show Gtk.Event where
    show (Key _eventRelease _eventSent _eventTime eventModifier' _eventWithCapsLock _eventWithNumLock 
                  _eventWithScrollLock _eventKeyVal eventKeyName' eventKeyChar') 
        = show eventModifier' ++ " " ++ show eventKeyName' ++ " " ++ show eventKeyChar'
    show _ = "Not a key event"

instance Show Gtk.Modifier where
    show Control = "Ctrl"
    show Alt = "Alt"
    show Shift = "Shift"
    show Apple = "Apple"
    show Compose = "Compose"
    
processEvent :: Chan Event -> Gtk.Event -> IO Bool
processEvent ch ev = do
  -- logPutStrLn $ "Gtk.Event: " ++ show ev
  -- logPutStrLn $ "Event: " ++ show (gtkToYiEvent ev)
  case gtkToYiEvent ev of
    Nothing -> logPutStrLn $ "Event not translatable: " ++ show ev
    Just e -> writeChan ch e
  -- This is very ridiculous, but improves responsivity dramatically.
  -- The idea is to give other threads the chance to process their queues immediately.
  -- One yield is not enough for this (I guess GHC 6.6.1 scheduler is to blame), I 
  -- empirically found that 4 is a good value for me.
  yield 
  yield
  yield
  yield
  return True
            
gtkToYiEvent :: Gtk.Event -> Maybe Event
gtkToYiEvent (Key {eventKeyName = keyName, eventModifier = modifier, eventKeyChar = char})
    = fmap (\k -> Event k $ (nub $ (if isShift then filter (not . (== MShift)) else id) $ map modif modifier)) key'
      where (key',isShift) = 
                case char of
                  Just c -> (Just $ KASCII c, True)
                  Nothing -> (M.lookup keyName keyTable, False)
            modif Control = MCtrl
            modif Alt = MMeta
            modif Shift = MShift
            modif Apple = MMeta
            modif Compose = MMeta
gtkToYiEvent _ = Nothing

-- | Map GTK long names to Keys
keyTable :: M.Map String Key
keyTable = M.fromList 
    [("Down",       KDown)
    ,("Up",         KUp)
    ,("Left",       KLeft)
    ,("Right",      KRight)
    ,("Home",       KHome)
    ,("End",        KEnd)
    ,("BackSpace",  KBS)
    ,("Delete",     KDel)
    ,("Page_Up",    KPageUp)
    ,("Page_Down",  KPageDown)
    ,("Insert",     KIns)
    ,("Escape",     KEsc)
    ,("Return",     KEnter)
    ,("Tab",        KASCII '\t')
    ]


-- | Clean up and go home
end :: IO ()
end = mainQuit

-- | Suspend the program
suspend :: UI -> EditorM ()
suspend ui = do 
  lift $ windowIconify (uiWindow ui) 

refreshAll :: EditorM ()
refreshAll = return ()

syncWindows :: Editor -> UI -> [(Window, Bool)] -> [WinInfo] -> IO [WinInfo]
syncWindows e ui (wfocused@(w,focused):ws) (c:cs) 
    | Common.winkey w == winkey c = do when focused (setFocus c)
                                       return (c:) `ap` syncWindows e ui ws cs
    | Common.winkey w `elem` map winkey cs = removeWindow ui c >> syncWindows e ui (wfocused:ws) cs
    | otherwise = do c' <- insertWindowBefore e ui w c
                     when focused (setFocus c')
                     return (c':) `ap` syncWindows e ui ws (c:cs)
syncWindows e ui ws [] = mapM (insertWindowAtEnd e ui) (map fst ws)
syncWindows e ui [] cs = mapM_ (removeWindow ui) cs >> return []
    
setFocus w = widgetGrabFocus (textview w)

removeWindow i win = containerRemove (uiBox i) (widget win)

-- | Make A new window
newWindow :: UI -> Bool -> FBuffer -> IO WinInfo
newWindow ui mini b = do
    wu <- newUnique

    f <- fontDescriptionNew
    fontDescriptionSetFamily f "Monospace"

    ml <- labelNew Nothing
    widgetModifyFont ml (Just f)
    set ml [ miscXalign := 0.01 ] -- so the text is left-justified.

    v <- sourceViewNew
    widgetModifyFont v (Just f)


    box <- if mini 
     then do
      widgetSetSizeRequest v (-1) 1

      prompt <- labelNew (Just $ name b)
      widgetModifyFont prompt (Just f)

      hb <- hBoxNew False 1
      set hb [ containerChild := prompt, 
               containerChild := v, 
               boxChildPacking prompt := PackNatural,
               boxChildPacking v := PackNatural] 

      return (castToBox hb)
     else do
      scroll <- scrolledWindowNew Nothing Nothing
      set scroll [scrolledWindowPlacement := CornerTopRight,
                  scrolledWindowVscrollbarPolicy := PolicyAlways,
                  scrolledWindowHscrollbarPolicy := PolicyAutomatic,
                  containerChild := v]

      vb <- vBoxNew False 1
      set vb [ containerChild := scroll, 
               containerChild := ml, 
               boxChildPacking ml := PackNatural] 
      return (castToBox vb)

    gtkBuf <- getGtkBuffer ui b

    textViewSetBuffer v gtkBuf

    let win = WinInfo {
                    bufkey    = (keyB b)
                   ,textview  = v
                   ,modeline  = ml
                   ,widget    = box
                   ,isMini    = mini
              }
    return win

insertWindowBefore e i w c = insertWindow e i w

insertWindowAtEnd e i w = insertWindow e i w

insertWindow e i win = do
  let buf = findBufferWith e (Common.bufkey win)
  liftIO $ do w <- newWindow i (Common.isMini win) buf
              set (uiBox i) [containerChild := widget w,
                             boxChildPacking (widget w) := if isMini w then PackNatural else PackGrow]
              -- FIXME: textview w `onFocusIn` (\_event -> (modifyRef (windows i) (WS.setFocus w)) >> return False)
              -- We have to return false so that GTK correctly focuses the window when we use widgetGrabFocus
              widgetShowAll (widget w)
              return w


scheduleRefresh :: UI -> EditorM ()
scheduleRefresh ui = modifyEditor_ $ \e -> withMVar (windows ui) $ \ws -> do
    cache <- readRef $ windowCache ui
    sequence_ [do buf <- getGtkBuffer ui (findBufferWith e b); applyUpdate buf u; logPutStrLn (show $ u) | (b,u) <- editorUpdates e]

    cache' <- syncWindows e ui (toList $ WS.withFocus $ ws) cache  
    WS.debug "syncing" ws
    logPutStrLn $ "with: " ++ show cache
    logPutStrLn $ "Yields: " ++ show cache'
    writeRef (windowCache ui) cache'
    forM_ cache' $ \w -> 
        do let buf = findBufferWith e (bufkey w)
           gtkBuf <- getGtkBuffer ui buf
           (p0, []) <- runBuffer buf pointB
           insertMark <- textBufferGetInsert gtkBuf
           i <- textBufferGetIterAtOffset gtkBuf p0
           textBufferPlaceCursor gtkBuf i
           textViewScrollMarkOnscreen (textview w) insertMark
           (txt, []) <- runBuffer buf getModeLine 
           set (modeline w) [labelText := txt]
    return e {editorUpdates = []}


applyUpdate :: SourceBuffer -> URAction -> IO ()
applyUpdate buf (Insert p s) = do
  i <- textBufferGetIterAtOffset buf p
  textBufferInsert buf i s
applyUpdate buf (Delete p s) = do
  i0 <- textBufferGetIterAtOffset buf p
  i1 <- textBufferGetIterAtOffset buf (p + s)
  textBufferDelete buf i0 i1

prepareAction :: UI -> EditorM ()
prepareAction ui = do
  let bufsRef = uiBuffers ui
  withBuffer0 $ do
    p0 <- pointB
    b <- ask
    
    p1 <- lift $ do
            tb <- liftM (M.! bkey b) $ readIORef bufsRef
            insertMark <- textBufferGetInsert tb
            i <- textBufferGetIterAtMark tb insertMark
            get i textIterOffset
    when (p1 /= p0) $ do
       moveTo p1 -- as a side effect we forget the prefered column

setCmdLine :: UI -> String -> IO ()
setCmdLine i s = do
  set (uiCmdLine i) [labelText := if length s > 132 then take 129 s ++ "..." else s]

getGtkBuffer :: UI -> FBuffer -> IO SourceBuffer
getGtkBuffer ui b = do
    let bufsRef = uiBuffers ui
    bufs <- readRef bufsRef
    gtkBuf <- case M.lookup (bkey b) bufs of
      Just gtkBuf -> return gtkBuf
      Nothing -> newGtkBuffer b
    modifyRef bufsRef (M.insert (bkey b) gtkBuf)
    return gtkBuf

-- FIXME: when a buffer is deleted its GTK counterpart should be too.
newGtkBuffer :: FBuffer -> IO SourceBuffer
newGtkBuffer b = do
  buf <- sourceBufferNew Nothing
  lm <- sourceLanguagesManagerNew
  Just haskellLang <- sourceLanguagesManagerGetLanguageFromMimeType lm "text/x-haskell"
  sourceBufferSetLanguage buf haskellLang
  sourceBufferSetHighlight buf True
  (txt, []) <- runBuffer b elemsB
  textBufferSetText buf txt
  return buf
