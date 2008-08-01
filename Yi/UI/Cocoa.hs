{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface, ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
--
-- Copyright (c) 2007, 2008 Jean-Philippe Bernardy
-- Copyright (c) 2008 Gustav Munkby
--

-- | This module defines a user interface implemented using Cocoa.

module Yi.UI.Cocoa (start) where

import Prelude hiding (init, length, error, sequence_, elem, mapM_, mapM, concat, concatMap)

import Yi.UI.Cocoa.Application
import Yi.UI.Cocoa.TextStorage
import Yi.UI.Cocoa.TextView
import Yi.UI.Cocoa.Utils

import Yi.Prelude hiding (init)
import Yi.Accessor
import Yi.Buffer
import Yi.Editor (Editor, withGivenBuffer0, findBufferWith, statusLine, buffers)
import qualified Yi.Editor as Editor
import Yi.Debug
import Yi.Keymap
import Yi.Buffer.Implementation
import Yi.Monad
import Yi.Config
import qualified Yi.UI.Common as Common
import qualified Yi.WindowSet as WS
import qualified Yi.Window as Window
import Yi.Window (Window)
import Paths_yi (getDataFileName)

import Control.Monad.Reader (liftIO, when, MonadIO)
import Control.Monad (ap)

import qualified Data.List as L
import Data.IORef
import Data.Maybe
import Data.Unique
import Data.Foldable
import Data.Traversable
import qualified Data.Map as M

import Foundation hiding (name, new, parent, error, self, null)
import Foundation.NSObject (init)

import AppKit hiding (windows, start, rect, width, content, prompt, dictionary, icon, concat)
import qualified AppKit.NSWindow (contentView)
import qualified AppKit.NSView as NSView

import HOC

import Foreign.C
import Foreign hiding (new)

foreign import ccall "Processes.h TransformProcessType" transformProcessType :: Ptr (CInt) -> CInt -> IO (CInt)
foreign import ccall "Processes.h SetFrontProcess" setFrontProcess :: Ptr (CInt) -> IO (CInt)
foreign import ccall "Processes.h GetCurrentProcess" getCurrentProcess :: Ptr (CInt) -> IO (CInt)

------------------------------------------------------------------------

data UI = UI {uiWindow :: NSWindow ()
             ,uiBox :: NSSplitView ()
             ,uiCmdLine :: NSTextField ()
             ,uiBuffers :: IORef (M.Map BufferRef TextStorage)
             ,windowCache :: IORef [WinInfo]
             ,uiActionCh :: Action -> IO ()
             ,uiConfig :: UIConfig
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
   Common.main                  = main,
   Common.end                   = end,
   Common.suspend               = uiWindow ui # performMiniaturize nil,
   Common.refresh               = refresh ui,
   Common.prepareAction         = return (return ()),
   Common.reloadProject         = \_ -> return ()
  }

rect :: Float -> Float -> Float -> Float -> NSRect
rect x y w h = NSRect (NSPoint x y) (NSSize w h)

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

{-
-- UNUSED:
translate :: Float -> Float -> NSRect -> NSRect
translate dx dy (NSRect (NSPoint x y) s) = NSRect (NSPoint (x + dx) (y + dy)) s
-}

toNSView :: forall t. ID () -> NSView t
toNSView = castObject

toYiApplication :: forall t1 t2. NSApplication t1 -> YiApplication t2
toYiApplication = castObject
toYiController :: forall t1 t2. NSObject t1 -> YiController t2
toYiController = castObject

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
start :: UIBoot
start cfg ch outCh _ed = do

  -- Ensure that our command line application is also treated as a gui application
  fptr <- mallocForeignPtrBytes 32 -- way to many bytes, but hey...
  withForeignPtr fptr $ getCurrentProcess
  withForeignPtr fptr $ (flip transformProcessType) 1
  withForeignPtr fptr $ setFrontProcess

  -- Publish Objective-C classes...
  initializeClass_Application
  initializeClass_YiTextView
  initializeClass_TextStorage
  initializeClass_YiScrollView

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
  content <- win # AppKit.NSWindow.contentView >>= return . toNSView
  content # setAutoresizingMask allSizable

  -- Create yi window container
  winContainer <- new _NSSplitView
  (cmd,_) <- content # addSubviewWithTextLine winContainer


  -- Activate application window
  win # center
  win # setFrameAutosaveName (toNSString "main")
  win # makeKeyAndOrderFront nil
  app # activateIgnoringOtherApps False

  -- Update the font configuration if desired...
  case configFontSize $ configUI cfg of
    Just fontSize ->
      userFixedPitchFontOfSize (fromIntegral fontSize) _NSFont >>=
          flip setUserFixedPitchFont _NSFont
    Nothing ->
      return ()

  bufs <- newIORef M.empty
  wc <- newIORef []

  return $ mkUI $ UI win winContainer cmd bufs wc (outCh . singleton) (configUI cfg)

-- | Run the main loop
main :: IO ()
main = _YiApplication # sharedApplication >>= run

-- | Clean up and go home
end :: IO ()
end = _YiApplication # sharedApplication >>= terminate_ nil

syncWindows :: Editor -> UI -> [(Window, Bool)] -> [WinInfo] -> IO [WinInfo]
syncWindows e ui (wfocused@(w,focused):ws) (c:cs)
    | Window.winkey w == winkey c = do when focused (setFocus c)
                                       return (c:) `ap` syncWindows e ui ws cs
    | Window.winkey w `elem` map winkey cs = do (widget c) # removeFromSuperview
                                                syncWindows e ui (wfocused:ws) cs
    | otherwise = do c' <- insertWindow e ui w
                     when focused (setFocus c')
                     return (c':) `ap` syncWindows e ui ws (c:cs)
syncWindows e ui ws [] = mapM (insertWindow e ui) (map fst ws)
syncWindows _e _ui [] cs = mapM_ (removeFromSuperview . widget) cs >> return []

setFocus :: WinInfo -> IO ()
setFocus w = do
  logPutStrLn $ "Cocoa focusing " ++ show w
  (textview w) # NSView.window >>= makeFirstResponder (textview w) >> return ()

-- | Make A new window
newWindow :: UI -> Bool -> FBuffer -> IO WinInfo
newWindow ui mini b = do
  v <- alloc _YiTextView >>= initWithFrame (rect 0 0 100 100)
  v # setRichText False
  v # setSelectable True
  v # setAlignment nsLeftTextAlignment
  v # sizeToFit
  v # setIVar _runBuffer (uiActionCh ui . makeAction . withGivenBuffer0 (keyB b))

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
    
    when (not $ configLineWrap $ uiConfig ui) $ do
      tc <- v # textContainer
      NSSize _ h <- tc # containerSize
      tc # setContainerSize (NSSize 1.0e7 h)
      tc # setWidthTracksTextView False

    scroll <- new _YiScrollView
    scroll # setDocumentView v
    scroll # setAutoresizingMask allSizable

    scroll # setHasVerticalScroller True
    scroll # setHasHorizontalScroller False
    scroll # setAutohidesScrollers (configAutoHideScrollBar $ uiConfig ui)
    scroll # setIVar _leftScroller (configLeftSideScrollBar $ uiConfig ui)
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

insertWindow :: Editor -> UI -> Window -> IO WinInfo
insertWindow e i win = do
  let buf = findBufferWith (Window.bufkey win) e
  liftIO $ newWindow i (Window.isMini win) buf

refresh :: UI -> Editor -> IO ()
refresh ui e = logNSException "refresh" $ do
    let ws = Editor.windows e
    let takeEllipsis s = if L.length s > 132 then take 129 s ++ "..." else s
    (uiCmdLine ui) # setStringValue (toNSString (takeEllipsis (statusLine e)))

    cache <- readRef $ windowCache ui
    forM_ (buffers e) $ \buf -> do
      storage <- getTextStorage ui buf
      storage # setMonospaceFont -- FIXME: Why is this needed for mini buffers?
      storage # setTextStorageBuffer buf

    (uiWindow ui) # setAutodisplay False -- avoid redrawing while window syncing
    cache' <- syncWindows e ui (toList $ WS.withFocus $ ws) cache
    writeRef (windowCache ui) cache'
    (uiBox ui) # adjustSubviews -- FIX: maybe it is not needed
    (uiWindow ui) # setAutodisplay True -- reenable automatic redrawing

    forM_ cache' $ \w ->
        do let buf = findBufferWith (bufkey w) e
           let p0 = runBufferDummyWindow buf pointB
           let p1 = runBufferDummyWindow buf (getMarkPointB staticSelMark)
           let showSel = runBufferDummyWindow buf (getA highlightSelectionA)
           let (p,l) = if showSel then (min p0 p1, abs $ p1~-p0) else (p0,0)
           (textview w) # setSelectedRange (NSRange (fromIntegral p) (fromIntegral l))
           (textview w) # scrollRangeToVisible (NSRange (fromIntegral p0) 0)
           let txt = runBufferDummyWindow buf getModeLine
           (modeline w) # setStringValue (toNSString txt)

getTextStorage :: UI -> FBuffer -> IO TextStorage
getTextStorage ui b = do
    let bufsRef = uiBuffers ui
    bufs <- readRef bufsRef
    storage <- case M.lookup (bkey b) bufs of
      Just storage -> return storage
      Nothing -> newTextStorage (configStyle $ uiConfig ui) b
    modifyRef bufsRef (M.insert (bkey b) storage)
    return storage
