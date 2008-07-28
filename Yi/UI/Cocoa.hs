{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface, ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
--
-- Copyright (c) 2007, 2008 Jean-Philippe Bernardy
-- Copyright (c) 2008 Gustav Munkby
--

-- | This module defines a user interface implemented using Cocoa.

-- For a Cocoa application to work we need to have the Cocoa
-- event-loop running. Since we don't want to re-implement the
-- event-loop in Haskell, we simply dispatch to the Objective-C
-- version and hook into events.
--
-- This however, is not completely trivial, since calling the
-- long-running Objective-C loop causes Haskell code not to be
-- executed at all. Upon receiving an event we must also make
-- sure to dispatch to other Haskell threads in order to make
-- progress.

module Yi.UI.Cocoa (start) where

import Prelude hiding (init, length, error, sequence_, elem, mapM_, mapM, concat, concatMap)

import Yi.Prelude hiding (init)
import Yi.Accessor
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Editor (Editor, EditorM, withGivenBuffer0, findBufferWith, statusLine, buffers)
import qualified Yi.Editor as Editor
import Yi.Event
import Yi.Debug
import Yi.Keymap
import Yi.Buffer.Implementation
import Yi.Monad
import Yi.Syntax (Stroke)
import Yi.Style hiding (modeline)
import Yi.Config
import qualified Yi.UI.Common as Common
import qualified Yi.WindowSet as WS
import qualified Yi.Window as Window
import Yi.Window (Window, dummyWindow)
import Paths_yi (getDataFileName)

import Control.Concurrent (yield)
import Control.Concurrent.MVar
import Control.Monad.Reader (liftIO, when, MonadIO)
import Control.Monad (ap, replicateM_)
import Control.Applicative ((<*>), (<$>), pure)

import qualified Data.List as L
import Data.IORef
import Data.Maybe
import Data.Unique
import Data.Foldable
import Data.Traversable
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8

import Foundation hiding (name, new, parent, error, self, null)
import Foundation.NSObject (init)

import AppKit hiding (windows, start, rect, width, content, prompt, dictionary, icon, concat)
import qualified AppKit.NSWindow (contentView)
import AppKit.NSText (selectedRange)
import qualified AppKit.NSScrollView (contentView)
import qualified AppKit.NSView as NSView

import HOC

import Foreign.C
import Foreign hiding (new)

foreign import ccall "Processes.h TransformProcessType" transformProcessType :: Ptr (CInt) -> CInt -> IO (CInt)
foreign import ccall "Processes.h SetFrontProcess" setFrontProcess :: Ptr (CInt) -> IO (CInt)
foreign import ccall "Processes.h GetCurrentProcess" getCurrentProcess :: Ptr (CInt) -> IO (CInt)


logNSException :: String -> IO () -> IO ()
logNSException str act =
  catchNS act (\e -> description e >>= haskellString >>=
                     logPutStrLn . (("NSException " ++ str ++ ":") ++))

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
    InstanceVariable "eventChannel" [t| Maybe (Yi.Event.Event -> IO ()) |] [| Nothing |]
  , InstanceMethod 'run -- '
  , InstanceMethod 'doTick -- '
  , InstanceMethod 'sendEvent -- '
  ])

ya_doTick :: YiApplication () -> IO ()
ya_doTick _ = replicateM_ 4 yield

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
    else super self # sendEvent event

handleKeyEvent :: forall t. NSEvent t -> Maybe (Yi.Event.Event -> IO ()) -> IO ()
handleKeyEvent event ch = do
  mask <- event # modifierFlags
  str <- event # charactersIgnoringModifiers >>= haskellString
  logPutStrLn $ "Key " ++ str
  let (k,shift') = case str of
                "\r"     -> (Just KEnter, True)
                "\t"     -> (Just KTab, True)
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
    (Just k, Just ch) -> ch (Event k (modifiers shift' mask))
    _                 -> return ()

modifierTable :: Bool -> [(CUInt, Modifier)]
modifierTable False = [(bit 18,MCtrl), (bit 19,MMeta)]
modifierTable True  = (bit 17,MShift) : modifierTable False

modifiers :: Bool -> CUInt -> [Modifier]
modifiers shift' mask = [yi | (cocoa, yi) <- modifierTable shift', (cocoa .&. mask) /= 0]

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
  -- TODO: Is this ok? Is startIndex a utf8 index or a point?
  let startIndex = if mx - cx < cx + cw - mx then (Point index) else (Point index + 1)

  -- The super-class deals Cocoa-ishly with mouse events
  super self # mouseDown event

  -- Update our selection marker and position to reflect what Cocoa wants
  NSRange p l <- selectedRange self
  let p1 = fromIntegral p
      p2 = p1 + fromIntegral l
  runbuf <- self #. _runBuffer
  runbuf $ do
    setVisibleSelection (p1 /= p2)
    if p1 == p2
      then do
        moveTo p1 
      else if abs (startIndex - p2) < min (abs (startIndex - p1)) 2
        then setSelectionMarkPointB p2 >> moveTo p1
        else setSelectionMarkPointB p1 >> moveTo p2

-- This declares our special text-view class. The textview interpretes
-- mouse events so that mouse-selection in Yi should function as in any
-- Cocoa application
$(declareClass "YiScrollView" "NSScrollView")
$(exportClass "YiScrollView" "ysv_" [
    InstanceVariable "leftScroller" [t| Bool |] [| False |]
  , InstanceMethod 'tile -- '
  ])
  
ysv_tile self = do
  super self # tile
  moveScroller <- self #. _leftScroller
  if not moveScroller
    then return ()
    else do
      -- Copied from NostalgicScrollView (found on /.)
      c <- self # AppKit.NSScrollView.contentView
      s <- self # verticalScroller
      sf <- s # frame
      s # setFrameOrigin (NSPoint 0.0 (nsMinY sf))
      c # frame >>= (flip setFrameOrigin c) . (NSPoint (nsWidth sf)) . nsMinY


-- Unfortunately, my version of hoc does not handle typedefs correctly,
-- and thus misses every selector that uses the "unichar" type, even
-- though it has introduced a type alias for it...
$(declareRenamedSelector "characterAtIndex:" "characterAtIndex" [t| CUInt -> IO Unichar |])
instance Has_characterAtIndex (NSString a)
$(declareRenamedSelector "getCharacters:range:" "getCharactersRange" [t| Ptr Unichar -> NSRange -> IO () |])
instance Has_getCharactersRange (NSString a)

-- Introduce a NSString subclass that has a lazy bytestring internally
-- A NSString subclass needs to implement length and characterAtIndex,
-- and for performance reasons getCharactersRange
-- The implementation here is a quick hack and I have no idea how it
-- works with anything except ASCII characters. Cocoa uses UTF16 to
-- store characters, and Yi uses UTF8, so supposedly some recoding
-- has to take place. For UTF8 is converted to Char's that are then
-- just dealt with as if they were in UTF16...

$(declareClass "YiLBString" "NSString")
$(exportClass "YiLBString" "yls_" [
    InstanceVariable "string" [t| LB.ByteString |] [| LB.empty |]
  , InstanceMethod 'length -- '
  , InstanceMethod 'characterAtIndex -- '
  , InstanceMethod 'getCharactersRange -- '
  ])

yls_length :: YiLBString () -> IO CUInt
yls_length self = do
  -- logPutStrLn $ "Calling yls_length (gah...)"
  self #. _string >>= return . fromIntegral . LB.length

-- TODO: The result type should be UTF16...
yls_characterAtIndex :: CUInt -> YiLBString () -> IO Unichar
yls_characterAtIndex i self = do
  -- logPutStrLn $ "Calling yls_characterAtIndex " ++ show i
  self #. _string >>= return . fromIntegral . flip LB.index (fromIntegral i)

-- TODO: Should get an array of characters in UTF16...
yls_getCharactersRange :: Ptr Unichar -> NSRange -> YiLBString () -> IO ()
yls_getCharactersRange p r@(NSRange i l) self = do
  -- logPutStrLn $ "Calling yls_getCharactersRange " ++ show r
  self #. _string >>=
    pokeArray p .
    take (fromIntegral l) . -- TODO: Is l given in bytes or characters?
    map fromIntegral . -- TODO: UTF16 recode
    LB.unpack .
    LB.drop (fromIntegral i)


-- An implementation of NSTextStorage that uses Yi's FBuffer as
-- the backing store. An implementation must at least implement
-- a O(1) string method and attributesAtIndexEffectiveRange.
-- For performance reasons, attributeAtIndexEffectiveRange is
-- implemented to deal with specific properties such as font.

-- Judging by usage logs, the environment using the text storage
-- seem to rely on strings O(1) behavior and thus caching the
-- result seems like a good idea. In addition attributes are
-- queried for the same location multiple times, and thus caching
-- them as well also seems fruitful.

-- Unfortunately HOC does not export Instance Variables, and thus
-- we cannot provide a type signature for withCache
-- withCache :: (InstanceVariables st iv) => st -> IVar iv (Maybe vt) -> (vt -> Bool) -> IO vt -> IO vt

-- | Obtain the result of the action and cache that as the
--   instance variable ivar in self. Use existing cache if
--   a result is stored, and cond says it is still valid.
withCache self ivar cond action = do
  cache <- self #. ivar
  case cache of
    Just val | cond val -> return val
    otherwise -> do
      val <- action
      self # setIVar ivar (Just val)
      return val

-- | Use this as the base length of computed stroke ranges
strokeRangeExtent :: Point
strokeRangeExtent = 100

-- TODO: Investigate whether it is a good idea to cache
--       NSDictionary objects also in some fashion

$(declareClass "YiTextStorage" "NSTextStorage")
$(exportClass "YiTextStorage" "yts_" [
    InstanceVariable "buffer" [t| Maybe FBuffer |] [| Nothing |]
  , InstanceVariable "attributesCache" [t| Maybe (NSRange, NSDictionary ()) |] [| Nothing |]
  , InstanceVariable "stringCache" [t| Maybe (NSString ()) |] [| Nothing |]
  , InstanceMethod 'string -- '
  , InstanceMethod 'attributeAtIndexEffectiveRange -- '
  , InstanceMethod 'attributesAtIndexEffectiveRange -- '
  , InstanceMethod 'replaceCharactersInRangeWithString -- '
  , InstanceMethod 'setAttributesRange -- '
  , InstanceMethod 'length -- '
  ])

yts_length :: YiTextStorage () -> IO CUInt
yts_length self = do
  -- logPutStrLn "Calling yts_length "
  (fromIntegral . flip runBufferDummyWindow sizeB . fromJust) <$> self #. _buffer

yts_string :: YiTextStorage () -> IO (NSString ())
yts_string self = do
  withCache self _stringCache (const True) $ do
    s <- autonew _YiLBString
    Just b <- self #. _buffer
    s # setIVar _string (runBufferDummyWindow b (streamB Forward 0))
    castObject <$> return s

yts_attributesAtIndexEffectiveRange :: CUInt -> NSRangePointer -> YiTextStorage () -> IO (NSDictionary ())
yts_attributesAtIndexEffectiveRange i er self = do
  (r,dict) <- withCache self _attributesCache (nsLocationInRange i . fst) $ do
    strokes <- self # runStrokesAround i
    let (e,s) = minimalStyle (fromIntegral i + strokeRangeExtent) strokes
    let r = NSRange i (fromIntegral e - i)
    -- logPutStrLn $ "Calling yts_attributesAtIndexEffectiveRange " ++ show r
    (,) <$> pure r <*> convertStyle s
  safePoke er r
  return dict

yts_attributeAtIndexEffectiveRange :: forall t. NSString t -> CUInt -> NSRangePointer -> YiTextStorage () -> IO (ID ())
yts_attributeAtIndexEffectiveRange attr i er self = do
  attr' <- haskellString attr
  case attr' of
    "NSFont" -> do
      safePokeFullRange >> castObject <$> userFixedPitchFontOfSize 0 _NSFont
    "NSGlyphInfo" -> do
      safePokeFullRange >> return nil
    "NSAttachment" -> do
      safePokeFullRange >> return nil
    "NSCursor" -> do
      safePokeFullRange >> castObject <$> ibeamCursor _NSCursor
    "NSToolTip" -> do
      safePokeFullRange >> return nil
    "NSLanguage" -> do
      safePokeFullRange >> return nil
    "NSParagraphStyle" -> do
      -- TODO: Adjust line break property...
      safePokeFullRange >> castObject <$> defaultParagraphStyle _NSParagraphStyle
    "NSBackgroundColor" -> do
      bg <- minimalAttr background <$> self # runStrokesAround i
      let (s, Background c) = fromMaybe (fromIntegral i + strokeRangeExtent, Background Default) bg
      safePoke er (NSRange i (fromIntegral s - i))
      castObject <$> getColor False c
    _ -> do
      -- TODO: Optimize the other queries as well (if needed)
      logPutStrLn $ "Unoptimized yts_attributeAtIndexEffectiveRange " ++ attr' ++ " at " ++ show i
      super self # attributeAtIndexEffectiveRange attr i er
  where
    safePokeFullRange = do
      Just b <- self #. _buffer
      safePoke er (NSRange 0 (fromIntegral $ runBufferDummyWindow b sizeB))

-- These methods are used to modify the contents of the NSTextStorage.
-- We do not allow direct updates of the contents this way, though.
yts_replaceCharactersInRangeWithString :: forall t. NSRange -> NSString t -> YiTextStorage () -> IO ()
yts_replaceCharactersInRangeWithString _ _ _ = return ()
yts_setAttributesRange :: forall t. NSDictionary t -> NSRange -> YiTextStorage () -> IO ()
yts_setAttributesRange _ _ _ = return ()



-- | Obtain the maximal range with a consistent style.
--   This negatively assumes that any two adjacent strokes
--   have different styles, and positively assumes that
--   the start of all strokes are the same.
minimalStyle :: Point -> [[Stroke]] -> (Point,Style)
minimalStyle q xs =
  (\ (es, ss) -> (L.minimum (q:es), concat ss)) $ unzip [ (e,s) | (_, s, e):_ <- xs ]

-- | Obtain the maximal range for a particular attribute.
minimalAttr :: (Style -> Maybe Attr) -> [[Stroke]] -> Maybe (Point, Attr)
minimalAttr f xs =
  listToMaybe $ catMaybes [ (,) <$> pure b <*> f s | (_, s, b):_ <- xs ]

-- | Use this with minimalAttr to get background information
background :: Style -> Maybe Attr
background s = listToMaybe [x | x@(Background _) <- s]


-- TODO: Integrate defaults into below, and cache(?)
-- | Convert style information into Cocoa compatible format
convertStyle :: Style -> IO (NSDictionary ())
convertStyle s = do
  d <- castObject <$> dictionary _NSMutableDictionary
  ft <- userFixedPitchFontOfSize 0 _NSFont
  setValueForKey ft nsFontAttributeName d
  fillStyleDict d s
  castObject <$> return d

-- | Fill and return the filled dictionary with the style information
fillStyleDict :: NSMutableDictionary t -> Style -> IO ()
fillStyleDict _ [] = return ()
fillStyleDict d (x:xs) = do
  fillStyleDict d xs
  getDictStyle x >>= flip (uncurry setValueForKey) d

-- | Return a (value, key) pair for insertion into the style dictionary
getDictStyle :: Attr -> IO (NSColor (), NSString ())
getDictStyle (Foreground c) = (,) <$> getColor True c  <*> pure nsForegroundColorAttributeName
getDictStyle (Background c) = (,) <$> getColor False c <*> pure nsBackgroundColorAttributeName

-- | Convert a Yi color into a Cocoa color
getColor :: Bool -> Color -> IO (NSColor ())
getColor fg Default = if fg then _NSColor # blackColor else _NSColor # whiteColor
getColor fg Reverse = if fg then _NSColor # whiteColor else _NSColor # blackColor
getColor _g (RGB r g b) =
  let conv = (/255) . fromIntegral in
  _NSColor # colorWithDeviceRedGreenBlueAlpha (conv r) (conv g) (conv b) 1.0

-- | A version of poke that does nothing if p is null.
safePoke :: (Storable a) => Ptr a -> a -> IO ()
safePoke p x = if p == nullPtr then return () else poke p x

-- | Execute strokeRangesB on the buffer, and update the buffer
--   so that we keep around cached syntax information...
runStrokesAround :: CUInt -> YiTextStorage () -> IO [[Stroke]]
runStrokesAround i self = do
  Just b <- self #. _buffer
  let p = fromIntegral i
  let (strokes,b') = runBuffer (dummyWindow $ bkey b) b (strokesRangesB Nothing p (p + strokeRangeExtent))
  self # setIVar _buffer (Just b')
  return strokes


------------------------------------------------------------------------

data UI = UI {uiWindow :: NSWindow ()
             ,uiBox :: NSSplitView ()
             ,uiCmdLine :: NSTextField ()
             ,uiBuffers :: IORef (M.Map BufferRef (YiTextStorage ()))
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
   Common.main                  = main                  ui,
   Common.end                   = end ,
   Common.suspend               = uiWindow ui # performMiniaturize nil,
   Common.refresh               = refresh       ui,
   Common.prepareAction         = prepareAction         ui,
   Common.reloadProject         = \_ -> return ()
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

setMonospaceFont :: Has_setFont v => v -> IO ()
setMonospaceFont view = do
  userFixedPitchFontOfSize 0 _NSFont >>= flip setFont view

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
  initializeClass_YiApplication
  initializeClass_YiController
  initializeClass_YiTextView
  initializeClass_YiLBString
  initializeClass_YiTextStorage
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
main :: UI -> IO ()
main _ = do
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

insertWindowBefore :: Editor -> UI -> Window -> WinInfo -> IO WinInfo
insertWindowBefore e i w _c = insertWindow e i w

insertWindowAtEnd :: Editor -> UI -> Window -> IO WinInfo
insertWindowAtEnd e i w = insertWindow e i w

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
    forM_ (buffers e) $ \buf -> when (not $ null $ pendingUpdates $ buf) $ do
      storage <- getTextStorage ui buf
      storage # setMonospaceFont -- FIXME: Why is this needed for mini buffers?
      if null (pendingUpdates buf)
        then return ()
        else do
          storage # beginEditing
          forM_ ([u | TextUpdate u <- pendingUpdates buf]) $ applyUpdate storage
          storage # setIVar _buffer (Just buf)
          storage # setIVar _stringCache Nothing
          storage # setIVar _attributesCache Nothing
          storage # endEditing

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
           let p0 = runBufferDummyWindow buf pointB
           let p1 = runBufferDummyWindow buf (getMarkPointB staticSelMark)
           let showSel = runBufferDummyWindow buf (getA highlightSelectionA)
           let (p,l) = if showSel then (min p0 p1, abs $ p1~-p0) else (p0,0)
           (textview w) # setSelectedRange (NSRange (fromIntegral p) (fromIntegral l))
           (textview w) # scrollRangeToVisible (NSRange (fromIntegral p0) 0)
           let txt = runBufferDummyWindow buf getModeLine
           (modeline w) # setStringValue (toNSString txt)

applyUpdate :: YiTextStorage () -> Update -> IO ()
applyUpdate buf (Insert p _ s) =
  buf # editedRangeChangeInLength nsTextStorageEditedCharacters
          (NSRange (fromIntegral p) 0) (fromIntegral $ LB.length s)

applyUpdate buf (Delete p _ s) =
  let len = LB.length s in
  buf # editedRangeChangeInLength nsTextStorageEditedCharacters
          (NSRange (fromIntegral p) (fromIntegral len)) (fromIntegral (negate len))

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

getTextStorage :: UI -> FBuffer -> IO (YiTextStorage ())
getTextStorage ui b = do
    let bufsRef = uiBuffers ui
    bufs <- readRef bufsRef
    storage <- case M.lookup (bkey b) bufs of
      Just storage -> return storage
      Nothing -> newTextStorage b
    modifyRef bufsRef (M.insert (bkey b) storage)
    return storage

newTextStorage :: FBuffer -> IO (YiTextStorage ())
newTextStorage b = do
  buf <- new _YiTextStorage
  buf # setIVar _buffer (Just b)
  buf # setMonospaceFont
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
