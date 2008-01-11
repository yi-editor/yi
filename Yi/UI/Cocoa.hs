{-# OPTIONS -XTemplateHaskell #-}
--
-- Copyright (c) 2007 Jean-Philippe Bernardy
-- Copyright (c) 2008 Gustav Munkby
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


-- | This module defines a user interface implemented using Cocoa.

module Yi.UI.Cocoa (start) where

import Prelude hiding (init, error, sequence_, elem, mapM_, mapM, concatMap)

import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Editor
import Yi.Event
import Yi.Debug
import Yi.FastBuffer
import Yi.Monad
import qualified Yi.UI.Common as Common
import Yi.UI.Common (Window)
import qualified Yi.WindowSet as WS

import Control.Concurrent ( yield )
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad.Reader (liftIO, when, MonadIO)
import Control.Monad (ap)

import Data.Bits ((.|.), (.&.), bit)
import Data.IORef
import Data.Maybe
import Data.Unique
import Data.Foldable
import Data.Traversable
import qualified Data.Map as M

import Foundation hiding (length, name, new, parent, error)
import Foundation.NSObject (init)
import Foundation.NSAttributedString (string)

import AppKit hiding (name, windows, start, rect, width, content, prompt)
import AppKit.NSWindow (contentView)
import AppKit.NSEvent (characters)
import AppKit.NSText (selectedRange)
import qualified AppKit.NSView as NSView

import HOC
import HOC.ID

import Foreign.C
import Foreign hiding (new)

foreign import ccall "Processes.h TransformProcessType" transformProcessType :: Ptr (CInt) -> CInt -> IO (CInt)
foreign import ccall "Processes.h SetFrontProcess" setFrontProcess :: Ptr (CInt) -> IO (CInt)
foreign import ccall "Processes.h GetCurrentProcess" getCurrentProcess :: Ptr (CInt) -> IO (CInt)

------------------------------------------------------------------------

-- The selector is used since NSEvent#type treats the c enum
-- in a type-safe way, but Cocoa receives values which are not
-- defined in the c enum, which results in a pattern mismatch...
$(declareRenamedSelector "type" "rawType" [t| IO CInt |])
instance Has_rawType (NSEvent a)

-- This declares an application delegate which ensures the application
-- terminates when the last (and only) cocoa window is closed
$(declareClass "YiController" "NSObject")
$(exportClass "YiController" "yc_" [
    InstanceMethod 'applicationShouldTerminateAfterLastWindowClosed -- '
  ])

yc_applicationShouldTerminateAfterLastWindowClosed app self = return True

------------------------------------------------------------------------

-- This declares an application subclass which enables us to insert
-- ourselves into the application event loop and trap key-events application wide
$(declareClass "YiApplication" "NSApplication")
$(exportClass "YiApplication" "ya_" [
    InstanceVariable "eventChannel" [t| Maybe (Chan Yi.Event.Event) |] [| Nothing |]
  , InstanceVariable "running" [t| Bool |] [| True |]
  , InstanceMethod 'terminate_ -- '
  , InstanceMethod 'isRunning -- '
  , InstanceMethod 'sendEvent -- '
  , InstanceMethod 'nextEventMatchingMaskUntilDateInModeDequeue -- '
  ])

ya_nextEventMatchingMaskUntilDateInModeDequeue mask date mode deq self = do
  yield >> yield >> yield >> yield
  super self # nextEventMatchingMaskUntilDateInModeDequeue mask date mode deq 

ya_isRunning self = self # getIVar _eventChannel >>= return . isJust

ya_terminate_ _ self = self # setIVar _eventChannel Nothing

ya_sendEvent event self = do
  t <- event # rawType
  if t == fromCEnum nsKeyDown
    then self #. _eventChannel >>= handleKeyEvent event
    else do
      --d <- event # description >>= haskellString
      --logPutStrLn $ "Event " ++ d
      super self # sendEvent event

handleKeyEvent event ch = do
  mask <- event # modifierFlags
  code <- event # keyCode
  str <- event # charactersIgnoringModifiers >>= haskellString
  let (k,shift) = case str of
                "\r"     -> (Just KEnter, True)
                "\DEL"   -> (Just KBS, True)
                "\ESC"   -> (Just KEsc, True)
                "\63232" -> (Just KUp, True)
                "\63233" -> (Just KDown, True)
                "\63234" -> (Just KLeft, True)
                "\63235" -> (Just KRight, True)
                "\63273" -> (Just KHome, True)
                "\63275" -> (Just KEnd, True)
                "\63276" -> (Just KPageUp, True)
                "\63277" -> (Just KPageDown, True)
                [c]      -> (Just $ KASCII c, False)
                _        -> (Nothing, True)
  case (k,ch) of
    (Just k, Just ch) -> writeChan ch (Event k (modifiers shift mask))
    _                 -> return ()

modifierTable :: Bool -> [(CUInt, Modifier)]
modifierTable False = [(bit 18,MCtrl), (bit 19,MMeta)]
modifierTable True  = (bit 17,MShift) : modifierTable False
modifiers shift mask = [yi | (cocoa, yi) <- modifierTable shift, (cocoa .&. mask) /= 0]

------------------------------------------------------------------------

-- This declares our special text-view class. The textview interpretes
-- mouse events so that mouse-selection in Yi should function as in any
-- Cocoa application
$(declareClass "YiTextView" "NSTextView")
$(exportClass "YiTextView" "ytv_" [
    InstanceVariable "runBuffer" [t| BufferM () -> IO () |] [| \_ -> return () |]
  , InstanceMethod 'mouseDown -- '
  ])

ytv_mouseDown event self = do
  -- Determine the starting location before tracking mouse
  layout <- self # layoutManager
  container <- self # textContainer
  mousewin <- event # locationInWindow
  NSPoint ex ey <- self # convertPointFromView mousewin nil
  NSPoint ox oy <- self # textContainerOrigin
  let mouse@(NSPoint mx my) = NSPoint (ex - ox) (ey - oy)
  index <- layout # glyphIndexForPointInTextContainer mouse container >>= return . fromEnum
  NSRect (NSPoint cx _) (NSSize cw _) <-
    layout # boundingRectForGlyphRangeInTextContainer (NSRange (toEnum index) 1) container
  let start = if mx - cx < cx + cw - mx then index else index + 1

  -- The super-class deals Cocoa-ishly with mouse events
  super self # mouseDown event

  -- Update our selection marker and position to reflect what Cocoa wants
  NSRange p l <- selectedRange self
  let p1 = fromEnum p
      p2 = p1 + fromEnum l
  run <- self #. _runBuffer
  run $ do
    if p1 == p2
      then do
        unsetMarkB -- Should really be called unsetSelectionMarkB?
        moveTo p1
      else if abs (start - p2) < min (abs (start - p1)) 2
        then setSelectionMarkPointB p2 >> moveTo p1
        else setSelectionMarkPointB p1 >> moveTo p2

------------------------------------------------------------------------

data UI = forall action. UI {
              uiWindow :: NSWindow ()
             ,uiBox :: NSView ()
             ,uiCmdLine :: NSTextField ()
             ,uiBuffers :: IORef (M.Map BufferRef (NSTextStorage ()))
             ,windows   :: MVar (WS.WindowSet Window)     -- ^ all the windows
             ,windowCache :: IORef [WinInfo]
             ,uiRunEditor :: EditorM () -> IO ()
             }

data WinInfo = WinInfo 
    {
     bufkey      :: !BufferRef         -- ^ the buffer this window opens to
    ,wkey        :: !Unique
    ,textview    :: YiTextView ()
    ,modeline    :: NSTextField ()
    ,widget      :: NSView ()          -- ^ Top-level widget for this window.
    ,isMini      :: Bool
    }

instance Show WinInfo where
    show w = "W" ++ show (hashUnique $ wkey w) ++ " on " ++ show (bufkey w) 


-- | Get the identification of a window.
winkey :: WinInfo -> (Bool, BufferRef)
winkey w = (isMini w, bufkey w)


mkUI :: UI -> Common.UI
mkUI ui = Common.UI 
  {
   Common.main                  = main                  ui,
   Common.end                   = end,
   Common.suspend               = uiWindow ui # performMiniaturize nil,
   Common.scheduleRefresh       = scheduleRefresh       ui,
   Common.prepareAction         = prepareAction         ui
  }

rect :: Float -> Float -> Float -> Float -> NSRect
rect x y w h = NSRect (NSPoint x y) (NSSize w h)

new x = do 
  d <- description x >>= haskellString
  o <- alloc x
  logPutStrLn $ "New " ++ d
  init o
autonew x = new x >>= autoreleased
autoreleased o = do
  retain o
  autorelease o
  return o
  
allSizable, normalWindowMask :: CUInt
allSizable = nsViewWidthSizable .|. nsViewHeightSizable
normalWindowMask =
  nsTitledWindowMask .|. nsResizableWindowMask .|. nsClosableWindowMask .|. nsMiniaturizableWindowMask

initWithContentRect r =
  initWithContentRectStyleMaskBackingDefer r normalWindowMask nsBackingStoreBuffered True
width (NSRect _ (NSSize w _)) = w
height (NSRect _ (NSSize _ h)) = h

toNSView :: forall t. ID () -> NSView t
toNSView = castObject

toYiApplication :: forall t1 t2. NSApplication t1 -> YiApplication t2
toYiApplication = castObject
toYiController :: forall t1 t2. NSObject t1 -> YiController t2
toYiController = castObject

setMonospaceFont :: Has_setFont v => v -> IO ()
setMonospaceFont view = do
  f <- _NSFont # userFixedPitchFontOfSize 0.0
  setFont f view

newTextLine :: IO (NSTextField ())
newTextLine = do
  tl <- new _NSTextField
  tl # setAlignment nsLeftTextAlignment
  tl # setAutoresizingMask nsViewWidthSizable
  tl # setMonospaceFont
  tl # setSelectable True
  tl # sizeToFit
  return tl

addSubviewWithTextLine view parent = do
  view # setAutoresizingMask allSizable
  parent # addSubview view

  text <- newTextLine
  parent # addSubview text

  -- Adjust frame sizes, as superb cocoa cannot do this itself...
  txtbox <- text # frame
  winbox <- parent # bounds
  view # setFrame (rect 0 (height txtbox) (width winbox) (height winbox - height txtbox))
  text # setFrame (rect 0 0 (width winbox) (height txtbox))

  return text

-- | Initialise the ui
start :: Chan Yi.Event.Event -> Chan action ->
         Editor -> (EditorM () -> action) -> 
         MVar (WS.WindowSet Common.Window) -> IO Common.UI
start ch outCh _ed runEd ws0 = do

  -- Publish Objective-C classes...
  initializeClass_YiApplication
  initializeClass_YiController
  initializeClass_YiTextView

  app <- _YiApplication # sharedApplication >>= return . toYiApplication
  app # setIVar _eventChannel (Just ch)

  -- Initialize the app delegate, which allows quit-on-window-close
  controller <- autonew _YiController >>= return . toYiController
  app # setDelegate controller
  
  -- Create main cocoa window...
  win <- _NSWindow # alloc >>= initWithContentRect (rect 0 0 480 340)
  win # setTitle (toNSString "Yi")
  content <- win # contentView >>= return . toNSView
  content # setAutoresizingMask allSizable

  -- Create yi window container
  main <- new _NSView
  cmd <- content # addSubviewWithTextLine main

  -- Ensure that our command line application is also treated as a gui application
  fptr <- mallocForeignPtrBytes 32 -- way to many bytes, but hey...
  withForeignPtr fptr $ getCurrentProcess
  withForeignPtr fptr $ (flip transformProcessType) 1
  withForeignPtr fptr $ setFrontProcess

  -- Activate application window
  win # makeKeyAndOrderFront nil
  app # activateIgnoringOtherApps False
    
  bufs <- newIORef M.empty
  wc <- newIORef []

  return (mkUI $ UI win main cmd bufs ws0 wc (writeChan outCh . runEd))


-- | Run the main loop
main :: UI -> IO ()
main _ = do
  logPutStrLn "Cocoa main loop running"
  app <- _YiApplication # sharedApplication >>= return . toYiApplication

  -- This should really be app # run, but that doesn't seem to work
  -- We therefore need to replicate the run-loop in Haskell, which is
  -- only done for the rudimentary parts, since we shouldn't have to
  -- do this.
  let mode = toNSString "kCFRunLoopDefaultMode"
      loop = do
        -- GAH, we need these in the run loop, or else the other haskell threads don't run
        yield >> yield >> yield >> yield
        future <- _NSDate # dateWithTimeIntervalSinceNow 1.0
        event <- app # nextEventMatchingMaskUntilDateInModeDequeue 0xffffffff (castObject future) mode True
        if event == nil then return () else app # sendEvent event
        running <- app # isRunning
        if running then loop else return ()

  loop

-- | Clean up and go home
end :: IO ()
end = _YiApplication # sharedApplication >>= terminate_ nil

syncWindows :: Editor -> UI -> [(Window, Bool)] -> [WinInfo] -> IO [WinInfo]
syncWindows e ui (wfocused@(w,focused):ws) (c:cs) 
    | Common.winkey w == winkey c = do when focused (setFocus c)
                                       return (c:) `ap` syncWindows e ui ws cs
    | Common.winkey w `elem` map winkey cs = removeWindow ui c >> syncWindows e ui (wfocused:ws) cs
    | otherwise = do c' <- insertWindowBefore e ui w c
                     when focused (setFocus c')
                     return (c':) `ap` syncWindows e ui ws (c:cs)
syncWindows e ui ws [] = mapM (insertWindowAtEnd e ui) (map fst ws)
syncWindows _e ui [] cs = mapM_ (removeWindow ui) cs >> return []
    
setFocus :: WinInfo -> IO ()
setFocus w = do
  logPutStrLn $ "Cocoa focusing " ++ show w
  (textview w) # NSView.window >>= makeFirstResponder (textview w) >> return ()

removeWindow :: UI -> WinInfo -> IO ()
removeWindow _i win = (widget win) # removeFromSuperview

-- | Make A new window
newWindow :: UI -> Bool -> FBuffer -> IO WinInfo
newWindow ui mini b = flip catch (\e -> logPutStrLn (show e) >> error "Bläp") $ do

  v <- alloc _YiTextView >>= initWithFrame (rect 0 0 100 100)
  v # setRichText False
  v # setSelectable True
  v # setAlignment nsLeftTextAlignment
  v # setMonospaceFont
  v # sizeToFit
  v # setHorizontallyResizable True
  v # setVerticallyResizable True
  v # setIVar _runBuffer ((uiRunEditor ui) . withGivenBuffer0 (keyB b))

  view <- if mini 
   then do
    prompt <- newTextLine
    prompt # setStringValue (toNSString (name b))

    hb <- new _NSView
    hb # addSubview prompt
    hb # addSubview v

    return hb
   else do
    clip <- new _NSClipView
    clip # setDocumentView v
    clip # setAutoresizingMask allSizable
    
    scroll <- new _NSScrollView
    scroll # setContentView clip
    scroll # setDocumentView v
    clip # setAutoresizingMask allSizable

    scroll # setHasVerticalScroller True
    scroll # setHasHorizontalScroller False
    scroll # setAutohidesScrollers False

    return (toNSView $ toID scroll)

  ml <- (uiBox ui) # addSubviewWithTextLine view

  storage <- getTextStorage ui b
  layoutManager v >>= replaceTextStorage storage

  k <- newUnique
  let win = WinInfo {
                  bufkey    = (keyB b)
                 ,wkey      = k
                 ,textview  = v
                 ,modeline  = ml
                 ,widget    = view
                 ,isMini    = mini
            }
  return win

insertWindowBefore :: Editor -> UI -> Window -> WinInfo -> IO WinInfo
insertWindowBefore e i w _c = insertWindow e i w

insertWindowAtEnd :: Editor -> UI -> Window -> IO WinInfo
insertWindowAtEnd e i w = insertWindow e i w

insertWindow :: Editor -> UI -> Window -> IO WinInfo
insertWindow e i win = do
  let buf = findBufferWith (Common.bufkey win) e
  liftIO $ do w <- newWindow i (Common.isMini win) buf
              (widget w) # setAutoresizingMask (if isMini w
	                                        then nsViewWidthSizable
	                                        else allSizable)
              (uiBox i) # addSubview (widget w)
              return w

scheduleRefresh :: UI -> Editor -> IO ()
scheduleRefresh ui e = withMVar (windows ui) $ \ws -> do
    logPutStrLn $ "scheduleRefresh"
    let takeEllipsis s = if length s > 132 then take 129 s ++ "..." else s
    (uiCmdLine ui) # setStringValue (toNSString (takeEllipsis (statusLine e)))

    cache <- readRef $ windowCache ui
    forM_ (editorUpdates e) $ \(b,u) -> do
      let buf = findBufferWith b e
      storage <- getTextStorage ui buf
      applyUpdate storage u
      contents <- storage # string >>= haskellString
      logPutStrLn $ "Contents is " ++ show contents
      --TODO:
      --let (size, _, []) = runBuffer buf sizeB
      --let p = updatePoint u
      --replaceTagsIn ui (inBounds (p-100) size) (inBounds (p+100) size) buf storage

    WS.debug "syncing" ws
    logPutStrLn $ "with: " ++ show cache
    cache' <- syncWindows e ui (toList $ WS.withFocus $ ws) cache  
    logPutStrLn $ "Yields: " ++ show cache'
    writeRef (windowCache ui) cache'
    forM_ cache' $ \w -> 
        do let buf = findBufferWith (bufkey w) e
           let (p0, _, []) = runBuffer buf pointB
           let (p1, _, []) = runBuffer buf (getSelectionMarkB >>= getMarkPointB)
           range@(NSRange _ _) <- (textview w) # selectedRange
           (textview w) # setSelectedRange (NSRange (toEnum $ min p0 p1) (toEnum $ abs $ p1-p0))
           (textview w) # scrollRangeToVisible range
           let (txt, _, []) = runBuffer buf getModeLine 
           (modeline w) # setStringValue (toNSString txt)

applyUpdate :: NSTextStorage () -> Update -> IO ()
applyUpdate buf (Insert p s) =
  buf # mutableString >>= insertStringAtIndex (toNSString s) (toEnum p)

applyUpdate buf (Delete p s) = 
  buf # mutableString >>= deleteCharactersInRange (NSRange (toEnum p) (toEnum s))

prepareAction :: UI -> IO (EditorM ())
prepareAction _ui = return (return ())
--     NSRange visibleGlyphRange, visibleCharRange;
--     NSLayoutManager *layoutManager = [textView layoutManager];
--     NSTextContainer *textContainer = [textView textContainer];
--     NSPoint containerOrigin = [textView textContainerOrigin];
--     NSRect visibleRect = [textView visibleRect];
--
--     visibleRect.origin.x -= containerOrigin.x;	// convert from view 
--coordinates to container coordinates
--     visibleRect.origin.y -= containerOrigin.y;
--     visibleGlyphRange = [layoutManager 
--glyphRangeForBoundingRect:visibleRect inTextContainer:textContainer];
--     visibleCharRange = [layoutManager 
--characterRangeForGlyphRange:visibleGlyphRange actualGlyphRange:NULL];
--do
--    gtkWins <- readRef (windowCache ui)
--    heights <- forM gtkWins $ \w -> do
--                     let gtkWin = textview w
--                     d <- widgetGetDrawWindow gtkWin
--                     (_w,h) <- drawableGetSize d
--                     (_,y0) <- textViewWindowToBufferCoords gtkWin TextWindowText (0,0)
--                     (i0,_) <- textViewGetLineAtY gtkWin y0
--                     l0 <- get i0 textIterLine
--                     (_,y1) <- textViewWindowToBufferCoords gtkWin TextWindowText (0,h)
--                     (i1,_) <- textViewGetLineAtY gtkWin y1
--                     l1 <- get i1 textIterLine
--                     return (l1 - l0)
--    modifyMVar (windows ui) $ \ws -> do 
--        let (ws', _) = runState (mapM distribute ws) heights
--        return (ws', setBuffer (Common.bufkey $ WS.current ws') >> return ())
--
--distribute :: Window -> State [Int] Window
--distribute win = do
--  h <- gets head
--  modify tail
--  return win {Common.height = h}

getTextStorage :: UI -> FBuffer -> IO (NSTextStorage ())
getTextStorage ui b = do
    let bufsRef = uiBuffers ui
    bufs <- readRef bufsRef
    storage <- case M.lookup (bkey b) bufs of
      Just storage -> return storage
      Nothing -> newTextStorage ui b
    modifyRef bufsRef (M.insert (bkey b) storage)
    return storage

newTextStorage :: UI -> FBuffer -> IO (NSTextStorage ())
newTextStorage _ui b = do
  buf <- new _NSTextStorage
  let (txt, _, []) = runBuffer b elemsB
  buf # mutableString >>= setString (toNSString txt)
  --replaceTagsIn ui 0 (length txt) b buf
  return buf

-- Debugging helpers

data Hierarchy = View String NSRect [Hierarchy]
  deriving Show

haskellList :: forall t1. NSArray t1 -> IO [ID ()]
haskellList a = a # objectEnumerator >>= helper
  where 
    helper enum = do
      e <- enum # nextObject
      if e == nil
        then return []
        else helper enum >>= return . (e :)

mkHierarchy :: forall t. NSView t -> IO Hierarchy
mkHierarchy v = do
  d <- v # description >>= haskellString
  f <- v # frame
  ss <- v # subviews >>= haskellList >>= mapM (mkHierarchy . toNSView) 
  return $ View d f ss

