{-# LANGUAGE TemplateHaskell #-}
--
-- Copyright (c) 2007 Jean-Philippe Bernardy
-- Copyright (c) 2008 Gustav Munkby
--
--


-- | This module defines a user interface implemented using Cocoa.

module Yi.UI.Cocoa (start) where

import Prelude hiding (init, error, sequence_, elem, mapM_, mapM, concatMap)

import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Editor (Editor, EditorM, withGivenBuffer0, findBufferWith, statusLine, buffers)
import qualified Yi.Editor as Editor
import Yi.Event
import Yi.Debug
import Yi.Buffer.Implementation
import Yi.Monad
import Yi.Style hiding (modeline)
import qualified Yi.UI.Common as Common
import qualified Yi.WindowSet as WS
import qualified Yi.Window as Window
import Yi.Window (Window)
import Paths_yi (getDataFileName)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.Reader (liftIO, when, MonadIO)
import Control.Monad (ap)

import Data.List (groupBy)
import Data.IORef
import Data.Maybe
import Data.Unique
import Data.Foldable
import Data.Traversable
import qualified Data.Map as M

import Foundation hiding (length, name, new, parent, error, self, null)
import Foundation.NSObject (init)

import AppKit hiding (windows, start, rect, width, content, prompt, dictionary, icon)
import AppKit.NSWindow (contentView)
import AppKit.NSText (selectedRange)
import qualified AppKit.NSView as NSView

import HOC

import Foreign.C
import Foreign hiding (new)

foreign import ccall "Processes.h TransformProcessType" transformProcessType :: Ptr (CInt) -> CInt -> IO (CInt)
foreign import ccall "Processes.h SetFrontProcess" setFrontProcess :: Ptr (CInt) -> IO (CInt)
foreign import ccall "Processes.h GetCurrentProcess" getCurrentProcess :: Ptr (CInt) -> IO (CInt)


logNSException string action =
  catchNS action (\e -> description e >>= haskellString >>=
                        logPutStrLn . (("NSException " ++ string ++ ":") ++))

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

yc_applicationShouldTerminateAfterLastWindowClosed :: forall t. NSApplication t -> YiController () -> IO Bool
yc_applicationShouldTerminateAfterLastWindowClosed _app _self = return True

------------------------------------------------------------------------

-- This declares an application subclass which enables us to insert
-- ourselves into the application event loop and trap key-events application wide
$(declareClass "YiApplication" "NSApplication")
$(declareSelector "doTick" [t| IO () |])
$(declareSelector "setAppleMenu:" [t| forall t. NSMenu t -> IO () |] )
instance Has_setAppleMenu (NSApplication a)
$(exportClass "YiApplication" "ya_" [
    InstanceVariable "eventChannel" [t| Maybe (Chan Yi.Event.Event) |] [| Nothing |]
  , InstanceVariable "running" [t| Bool |] [| True |]
  , InstanceMethod 'run -- '
  , InstanceMethod 'doTick -- '
  , InstanceMethod 'sendEvent -- '
  ])

ya_doTick :: YiApplication () -> IO ()
ya_doTick _self = return () -- Does nothing, just enables some Haskell to run...

ya_run :: YiApplication () -> IO ()
ya_run self = do
  -- Schedule a timer that repeatedly invokes ya_doTick in order to have
  -- some Haskell code running all the time. This will prevent other
  -- Haskell threads to stall while waiting for the Cocoa run loop to finish.
  _NSTimer # scheduledTimerWithTimeIntervalTargetSelectorUserInfoRepeats 
                0.05 self (getSelectorForName "doTick") nil True
  super self # run

ya_sendEvent :: forall t. NSEvent t -> YiApplication () -> IO ()
ya_sendEvent event self = logNSException "sendEvent" $ do
  t <- event # rawType
  if t == fromCEnum nsKeyDown
    then self #. _eventChannel >>= handleKeyEvent event
    else do
      --d <- event # description >>= haskellString
      --logPutStrLn $ "Event " ++ d
      super self # sendEvent event

handleKeyEvent :: forall t. NSEvent t -> Maybe (Chan Yi.Event.Event) -> IO ()
handleKeyEvent event ch = do
  mask <- event # modifierFlags
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

modifiers :: Bool -> CUInt -> [Modifier]
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

ytv_mouseDown :: forall t. NSEvent t -> YiTextView () -> IO ()
ytv_mouseDown event self = do
  -- Determine the starting location before tracking mouse
  layout <- self # layoutManager
  container <- self # textContainer
  mousewin <- event # locationInWindow
  NSPoint ex ey <- self # convertPointFromView mousewin nil
  NSPoint ox oy <- self # textContainerOrigin
  let mouse@(NSPoint mx _) = NSPoint (ex - ox) (ey - oy)
  index <- layout # glyphIndexForPointInTextContainer mouse container >>= return . fromEnum
  NSRect (NSPoint cx _) (NSSize cw _) <-
    layout # boundingRectForGlyphRangeInTextContainer (NSRange (toEnum index) 1) container
  let startIndex = if mx - cx < cx + cw - mx then index else index + 1

  -- The super-class deals Cocoa-ishly with mouse events
  super self # mouseDown event

  -- Update our selection marker and position to reflect what Cocoa wants
  NSRange p l <- selectedRange self
  let p1 = fromEnum p
      p2 = p1 + fromEnum l
  runbuf <- self #. _runBuffer
  runbuf $ do
    if p1 == p2
      then do
        unsetMarkB -- Should really be called unsetSelectionMarkB?
        moveTo p1
      else if abs (startIndex - p2) < min (abs (startIndex - p1)) 2
        then setSelectionMarkPointB p2 >> moveTo p1
        else setSelectionMarkPointB p1 >> moveTo p2

------------------------------------------------------------------------

data UI = forall action. UI {
              uiWindow :: NSWindow ()
             ,uiBox :: NSSplitView ()
             ,uiCmdLine :: NSTextField ()
             ,uiBuffers :: IORef (M.Map BufferRef (NSTextStorage ()))
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
   Common.end                   = end ,
   Common.suspend               = uiWindow ui # performMiniaturize nil,
   Common.refresh               = refresh       ui,
   Common.prepareAction         = prepareAction         ui
  }

rect :: Float -> Float -> Float -> Float -> NSRect
rect x y w h = NSRect (NSPoint x y) (NSSize w h)

new, autonew :: forall t. Class (NSObject_ t) -> IO (NSObject t)
new x = do 
  d <- description x >>= haskellString
  o <- alloc x
  logPutStrLn $ "New " ++ d
  init o
autonew x = new x >>= autoreleased

autoreleased :: forall t. NSObject t -> IO (NSObject t)
autoreleased o = do
  retain o
  autorelease o
  return o
  
allSizable, normalWindowMask :: CUInt
allSizable = nsViewWidthSizable .|. nsViewHeightSizable
normalWindowMask =
  nsTitledWindowMask .|. nsResizableWindowMask .|. nsClosableWindowMask .|. nsMiniaturizableWindowMask

initWithContentRect :: NSRect -> NewlyAllocated (NSWindow ()) -> IO (NSWindow ())
initWithContentRect r =
  initWithContentRectStyleMaskBackingDefer r normalWindowMask nsBackingStoreBuffered True
width, height :: NSRect -> Float
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
  tl # setAutoresizingMask (nsViewWidthSizable .|. nsViewMaxYMargin) 
  tl # setMonospaceFont
  tl # setSelectable True
  tl # setEditable False   
  tl # sizeToFit
  return tl

addSubviewWithTextLine :: forall t1 t2. NSView t1 -> NSView t2 -> IO (NSTextField (), NSView ())
addSubviewWithTextLine view parent = do
  container <- new _NSView 
  parent # bounds >>= flip setFrame container
  container # setAutoresizingMask allSizable
  view # setAutoresizingMask allSizable
  container # addSubview view

  text <- newTextLine
  container # addSubview text
  parent # addSubview container
  -- Adjust frame sizes, as superb cocoa cannot do this itself...
  txtbox <- text # frame
  winbox <- container # bounds
  view # setFrame (rect 0 (height txtbox) (width winbox) (height winbox - height txtbox))
  text # setFrame (rect 0 0 (width winbox) (height txtbox))

  return (text, container)

-- | Initialise the ui
start :: Chan Yi.Event.Event -> Chan action ->
         Editor -> (EditorM () -> action) -> 
         IO Common.UI
start ch outCh _ed runEd = do

  -- Ensure that our command line application is also treated as a gui application
  fptr <- mallocForeignPtrBytes 32 -- way to many bytes, but hey...
  withForeignPtr fptr $ getCurrentProcess
  withForeignPtr fptr $ (flip transformProcessType) 1
  withForeignPtr fptr $ setFrontProcess

  -- Publish Objective-C classes...
  initializeClass_YiApplication
  initializeClass_YiController
  initializeClass_YiTextView

  app <- _YiApplication # sharedApplication >>= return . toYiApplication
  app # setIVar _eventChannel (Just ch)
  
  icon <- getDataFileName "art/yi+lambda-fat.pdf"
  _NSImage # alloc >>=
    initWithContentsOfFile (toNSString icon) >>=
    flip setApplicationIconImage app

  -- Initialize the app delegate, which allows quit-on-window-close
  controller <- autonew _YiController >>= return . toYiController
  app # setDelegate controller

  -- init menus
  mm <- _NSMenu # alloc >>= init
  mm' <- _NSMenu # alloc >>= init
  mm'' <- _NSMenu # alloc >>= init
  app # setMainMenu mm 
  app # setAppleMenu mm' 
  app # setWindowsMenu mm'' 

  
  -- Create main cocoa window...
  win <- _NSWindow # alloc >>= initWithContentRect (rect 0 0 480 340)
  win # setTitle (toNSString "Yi")
  content <- win # contentView >>= return . toNSView
  content # setAutoresizingMask allSizable

  -- Create yi window container
  winContainer <- new _NSSplitView
  (cmd,_) <- content # addSubviewWithTextLine winContainer


  -- Activate application window
  win # center
  win # setFrameAutosaveName (toNSString "main")
  win # makeKeyAndOrderFront nil
  app # activateIgnoringOtherApps False
    
  bufs <- newIORef M.empty
  wc <- newIORef []

  return (mkUI $ UI win winContainer cmd bufs wc (writeChan outCh . runEd))



-- | Run the main loop
main :: UI -> IO ()
main _ui = do
  logPutStrLn "Cocoa main loop running"
  _YiApplication # sharedApplication >>= run

-- | Clean up and go home
end :: IO ()
end = _YiApplication # sharedApplication >>= terminate_ nil

syncWindows :: Editor -> UI -> [(Window, Bool)] -> [WinInfo] -> IO [WinInfo]
syncWindows e ui (wfocused@(w,focused):ws) (c:cs) 
    | Window.winkey w == winkey c = do when focused (setFocus c)
                                       return (c:) `ap` syncWindows e ui ws cs
    | Window.winkey w `elem` map winkey cs = removeWindow ui c >> syncWindows e ui (wfocused:ws) cs
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
removeWindow _i win = (widget win)  # removeFromSuperview

-- | Make A new window
newWindow :: UI -> Bool -> FBuffer -> IO WinInfo
newWindow ui mini b = do
  -- This mysterious thread delay seems to solve the Cocoa issues...
  -- It seems as if initWithFrame mustn't be active while the application
  -- run method is called... 
  threadDelay 100

  v <- alloc _YiTextView >>= initWithFrame (rect 0 0 100 100)
  v # setRichText False
  v # setSelectable True
  v # setAlignment nsLeftTextAlignment
  v # sizeToFit
  v # setIVar _runBuffer ((uiRunEditor ui) . withGivenBuffer0 (keyB b))

  (ml, view) <- if mini 
   then do
    v # setHorizontallyResizable False
    v # setVerticallyResizable False
    prompt <- newTextLine
    prompt # setStringValue (toNSString (name b))
    prompt # sizeToFit
    prompt # setAutoresizingMask nsViewNotSizable

    prect <- prompt # frame 
    vrect <- v # frame
    
    hb <- _NSView # alloc >>= initWithFrame (rect 0 0 (width prect + width vrect) (height prect))
    v # setFrame (rect (width prect) 0 (width vrect) (height prect))
    v # setAutoresizingMask nsViewWidthSizable
    hb # addSubview prompt
    hb # addSubview v
    hb # setAutoresizingMask nsViewWidthSizable
   
    brect <- (uiBox ui) # bounds
    hb # setFrame (rect 0 0 (width brect) (height prect))

    (uiBox ui) # addSubview hb
    dummy <- _NSTextField # alloc >>= init

    return (dummy, hb)
   else do
    v # setHorizontallyResizable True
    v # setVerticallyResizable True
    
    scroll <- new _NSScrollView
    scroll # setDocumentView v
    scroll # setAutoresizingMask allSizable

    scroll # setHasVerticalScroller True
    scroll # setHasHorizontalScroller False
    scroll # setAutohidesScrollers False

    addSubviewWithTextLine scroll (uiBox ui)

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
  let buf = findBufferWith (Window.bufkey win) e
  liftIO $ newWindow i (Window.isMini win) buf

groupOn :: Eq a => (b -> a) -> [b] -> [[b]]
groupOn f = groupBy (\x y -> f x == f y)

refresh :: UI -> Editor -> IO ()
refresh ui e = logNSException "refresh" $ do
    let ws = Editor.windows e
    let takeEllipsis s = if length s > 132 then take 129 s ++ "..." else s
    (uiCmdLine ui) # setStringValue (toNSString (takeEllipsis (statusLine e)))

    cache <- readRef $ windowCache ui
    forM_ (buffers e) $ \buf -> when (not $ null $ pendingUpdates $ buf) $ do
      storage <- getTextStorage ui buf
      storage # beginEditing
      forM_ (pendingUpdates buf) $ applyUpdate storage
      storage # endEditing
      contents <- storage # string >>= haskellString
      logPutStrLn $ "Contents is " ++ show contents
      let (size,_) = runBuffer buf sizeB
      storage # setMonospaceFont -- FIXME: Why is this needed for mini buffers?
      replaceTagsIn 0 size buf storage

    (uiWindow ui) # setAutodisplay False -- avoid redrawing while window syncing
    WS.debug "syncing" ws
    logPutStrLn $ "with: " ++ show cache
    cache' <- syncWindows e ui (toList $ WS.withFocus $ ws) cache  
    logPutStrLn $ "Yields: " ++ show cache'
    writeRef (windowCache ui) cache'
    (uiBox ui) # adjustSubviews -- FIX: maybe it is not needed
    (uiWindow ui) # setAutodisplay True -- reenable automatic redrawing

    forM_ cache' $ \w -> 
        do let buf = findBufferWith (bufkey w) e
           let (p0, _) = runBufferDummyWindow buf pointB
           let (p1, _) = runBufferDummyWindow buf (getSelectionMarkB >>= getMarkPointB)
           (textview w) # setSelectedRange (NSRange (toEnum $ min p0 p1) (toEnum $ abs $ p1-p0))
           (textview w) # scrollRangeToVisible (NSRange (toEnum p0) 0)
           let (txt, _) = runBufferDummyWindow buf getModeLine 
           (modeline w) # setStringValue (toNSString txt)


replaceTagsIn :: forall t. Point -> Point -> FBuffer -> NSTextStorage t -> IO ()
replaceTagsIn from to buf storage = do
  let (styleSpans, _) = runBufferDummyWindow buf (styleRangesB (to - from) from)
  forM_ (zip styleSpans (drop 1 styleSpans)) $ \((l,Style fg bg),(r,_)) -> do
    logPutStrLn $ "Setting style " ++ show fg ++ show bg ++ " on " ++ show l ++ " - " ++ show r
    fg' <- color True fg
    bg' <- color False bg
    let range = NSRange (toEnum l) (toEnum $ r-l)
    storage # addAttributeValueRange nsForegroundColorAttributeName fg' range
    storage # addAttributeValueRange nsBackgroundColorAttributeName bg' range
  where 
    color fg Default = if fg then _NSColor # blackColor else _NSColor # whiteColor
    color fg Reverse = if fg then _NSColor # whiteColor else _NSColor # blackColor
    color _g (RGB r g b) = _NSColor # colorWithDeviceRedGreenBlueAlpha ((fromIntegral r)/255) ((fromIntegral g)/255) ((fromIntegral b)/255) 1.0

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
--        return (ws', setBuffer (Window.bufkey $ WS.current ws') >> return ())
--
--distribute :: Window -> State [Int] Window
--distribute win = do
--  h <- gets head
--  modify tail
--  return win {Window.height = h}

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
  let (txt, _) = runBufferDummyWindow b (revertPendingUpdatesB >> elemsB)
  buf # mutableString >>= setString (toNSString txt)
  buf # setMonospaceFont
  replaceTagsIn 0 (length txt) b buf
  return buf

-- Debugging helpers

{-

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

-}