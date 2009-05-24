{-# LANGUAGE CPP, BangPatterns, ExistentialQuantification, RecursiveDo,
    ParallelListComp #-}

-- Copyright (c) 2007, 2008 Jean-Philippe Bernardy

-- | This module defines a user interface implemented using gtk2hs and
-- pango for direct text rendering.

module Yi.UI.Pango (start) where

import Prelude (filter, map, round, FilePath, (/), zipWith)

import Control.Concurrent (yield)
import Control.Monad (ap)
import Control.Monad.Reader (liftIO, when, MonadIO)
import Control.Monad.State (gets, modify, runState, State)
import Data.Prototype
import Data.IORef
import Data.List (intercalate, nub, findIndex, zip, drop, repeat)
import qualified Data.List.PointedList.Circular as PL
import Data.Maybe
import qualified Data.Map as M

import Graphics.UI.Gtk hiding (on, Region, Window, Action, Point, Style)
import qualified Graphics.UI.Gtk.Gdk.Events as Gdk.Events
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.GError

import Yi.Prelude 
import Yi.Buffer
import qualified Yi.Editor as Editor
import Yi.Editor hiding (windows)
import Yi.Window
import Yi.Event
import Yi.Keymap
import Yi.Monad
import qualified Yi.UI.Common as Common
import Yi.UI.Pango.Utils
import Yi.UI.Utils
import Yi.Config
import Yi.Style

#ifdef GNOME_ENABLED
import Yi.UI.Pango.Gnome(watchSystemFont)
#endif

------------------------------------------------------------------------

data UI = UI
    { uiWindow    :: Gtk.Window
    , uiBox       :: VBox
    , uiCmdLine   :: Label
    , windowCache :: IORef [WinInfo]
    , uiActionCh  :: Action -> IO ()
    , uiConfig    :: UIConfig
    , uiFont      :: IORef FontDescription
    }

data WinInfo = WinInfo
    { coreWin         :: Window
    , shownRegion     :: IORef Region
    , renderer        :: IORef (ConnectId DrawingArea)
    , winMotionSignal :: IORef (Maybe (ConnectId DrawingArea))
    , winLayout       :: PangoLayout
    , winMetrics      :: FontMetrics
    , textview        :: DrawingArea
    , modeline        :: Label
    , widget          :: Box -- ^ Top-level widget for this window.
    }

instance Show WinInfo where
    show w = show (coreWin w)

mkUI :: UI -> Common.UI
mkUI ui = Common.dummyUI
    { Common.main          = main ui
    , Common.end           = const end
    , Common.suspend       = windowIconify (uiWindow ui)
    , Common.refresh       = \e -> refresh ui e >> return e
    , Common.prepareAction = prepareAction ui
    , Common.reloadProject = reloadProject ui
    }

updateFont :: UIConfig -> IORef FontDescription -> IORef [WinInfo] -> Label
                  -> FontDescription -> IO ()
updateFont cfg fontRef wc cmd font = do
  maybe (return ()) (fontDescriptionSetFamily font) (configFontName cfg)
  maybe (return ()) (fontDescriptionSetSize font . fromIntegral) (configFontSize cfg)
  writeIORef fontRef font
  widgetModifyFont cmd (Just font)
  wcs <- readIORef wc
  forM_ wcs $ \wininfo -> do
      layoutSetFontDescription (winLayout wininfo) (Just font)
      -- This will cause the textview to redraw
      widgetModifyFont (textview wininfo) (Just font)
      widgetModifyFont (modeline wininfo) (Just font)

askBuffer :: Window -> FBuffer -> BufferM a -> a
askBuffer w b f = fst $ runBuffer w b f

-- | Initialise the ui
start :: UIBoot
start cfg ch outCh ed = catchGError (startNoMsg cfg ch outCh ed) (\(GError _dom _code msg) -> fail msg)

startNoMsg :: UIBoot
startNoMsg cfg ch outCh _ed = do
  unsafeInitGUIForThreadedRTS

  win <- windowNew
  windowSetDefaultSize win 900 700
  windowSetTitle win "Yi"
  ico <- loadIcon "yi+lambda-fat.32.png"
  windowSetIcon win ico

  onKeyPress win (processEvent ch)

  paned <- hPanedNew

  vb <- vBoxNew False 1  -- Top-level vbox

  -- Disable the left pane (file/module browser) until Shim/Scion discussion has
  -- concluded. Shim causes crashes, but it's not worth fixing if we'll soon
  -- replace it.

  {-
  tabs <- notebookNew
  widgetSetSizeRequest tabs 200 (-1)
  notebookSetTabPos tabs PosBottom
  panedAdd1 paned tabs

  -- Create the tree views for files and modules
  (filesProject, modulesProject) <- loadProject =<< getCurrentDirectory

  filesStore   <- treeStoreNew [filesProject]
  modulesStore <- treeStoreNew [modulesProject]

  filesTree   <- projectTreeNew (outCh . singleton) filesStore
  modulesTree <- projectTreeNew (outCh . singleton) modulesStore

  scrlProject <- scrolledWindowNew Nothing Nothing
  scrolledWindowAddWithViewport scrlProject filesTree
  scrolledWindowSetPolicy scrlProject PolicyAutomatic PolicyAutomatic
  notebookAppendPage tabs scrlProject "Project"

  scrlModules <- scrolledWindowNew Nothing Nothing
  scrolledWindowAddWithViewport scrlModules modulesTree
  scrolledWindowSetPolicy scrlModules PolicyAutomatic PolicyAutomatic
  notebookAppendPage tabs scrlModules "Modules"
  -}

  vb' <- vBoxNew False 1
  panedAdd2 paned vb'

  set win [ containerChild := vb ]
  onDestroy win mainQuit

  cmd <- labelNew Nothing
  set cmd [ miscXalign := 0.01 ]

  set vb [ containerChild := paned,
           containerChild := cmd,
           boxChildPacking cmd  := PackNatural ]

  fontRef <- newIORef undefined
  wc <- newIORef []

#ifdef GNOME_ENABLED
  let watchFont = watchSystemFont
#else
  let watchFont = (fontDescriptionFromString "Monospace 10" >>=)
#endif
  watchFont $ updateFont (configUI cfg) fontRef wc cmd

  -- use our magic threads thingy (http://haskell.org/gtk2hs/archives/2005/07/24/writing-multi-threaded-guis/)
  timeoutAddFull (yield >> return True) priorityDefaultIdle 50

  widgetShowAll win

  let ui = UI win vb' cmd wc (outCh . singleton) (configUI cfg) fontRef

  return (mkUI ui)

main :: UI -> IO ()
main _ui =
    do logPutStrLn "GTK main loop running"
       mainGUI

processEvent :: (Event -> IO ()) -> Gdk.Events.Event -> IO Bool
processEvent ch ev = do
  -- logPutStrLn $ "Gtk.Event: " ++ show ev
  -- logPutStrLn $ "Event: " ++ show (gtkToYiEvent ev)
  case gtkToYiEvent ev of
    Nothing -> logPutStrLn $ "Event not translatable: " ++ show ev
    Just e -> ch e
  return True

gtkToYiEvent :: Gdk.Events.Event -> Maybe Event
gtkToYiEvent (Gdk.Events.Key {Gdk.Events.eventKeyName = key, Gdk.Events.eventModifier = evModifier, Gdk.Events.eventKeyChar = char})
    = fmap (\k -> Event k $ (nub $ (if isShift then filter (/= MShift) else id) $ concatMap modif evModifier)) key'
      where (key',isShift) =
                case char of
                  Just c -> (Just $ KASCII c, True)
                  Nothing -> (M.lookup key keyTable, False)
            modif Gdk.Events.Control = [MCtrl]
            modif Gdk.Events.Alt = [MMeta]
            modif Gdk.Events.Shift = [MShift]
            modif _ = []
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
    ,("Tab",        KTab)
    ,("ISO_Left_Tab", KTab)
    ]

-- | Clean up and go home
end :: IO ()
end = mainQuit

-- | Synchronize the windows displayed by GTK with the status of windows in the Core.
syncWindows :: Editor -> UI -> [(Window, Bool)] -- ^ windows paired with their "isFocused" state.
            -> [WinInfo] -> IO [WinInfo]
syncWindows e ui (wfocused@(w,focused):ws) (c:cs)
    | winkey w == winkey (coreWin c) =
        do when focused $ do let bufferName = shortIdentString (commonNamePrefix e) $ findBufferWith (bufkey w) e
                             windowSetTitle (uiWindow ui) $ bufferName ++ " - Yi"
                             setFocus c
           (:) <$> syncWin e w c <*> syncWindows e ui ws cs
    | winkey w `elem` map (winkey . coreWin) cs = removeWindow ui c >> syncWindows e ui (wfocused:ws) cs
    | otherwise = do c' <- insertWindowBefore e ui w c
                     when focused (setFocus c')
                     return (c':) `ap` syncWindows e ui ws (c:cs)
syncWindows e ui ws [] = mapM (insertWindowAtEnd e ui) (map fst ws)
syncWindows _e ui [] cs = mapM_ (removeWindow ui) cs >> return []

syncWin :: Editor -> Window -> WinInfo -> IO WinInfo
syncWin e w wi = do
  let b = findBufferWith (bufkey w) e
      reg = askBuffer w b winRegionB
  logPutStrLn $ "Updated one: " ++ show w ++ " to " ++ show reg
  writeRef (shownRegion wi) reg
  return (wi {coreWin = w})

setFocus :: WinInfo -> IO ()
setFocus w = do
  logPutStrLn $ "gtk focusing " ++ show w
  hasFocus <- get (textview w) widgetIsFocus
  when (not hasFocus) $ widgetGrabFocus (textview w)

removeWindow :: UI -> WinInfo -> IO ()
removeWindow i win = containerRemove (uiBox i) (widget win)

handleClick :: UI -> WinInfo -> Gdk.Events.Event -> IO Bool
handleClick ui w event = do
  -- logPutStrLn $ "Click: " ++ show (eventX e, eventY e, eventClick e)

  -- retrieve the clicked offset.
  (_,layoutIndex,_) <- layoutXYToIndex (winLayout w) (Gdk.Events.eventX event) (Gdk.Events.eventY event)
  r <- readRef (shownRegion w)
  let p1 = regionStart r + fromIntegral layoutIndex

  -- maybe focus the window
  logPutStrLn $ "Clicked inside window: " ++ show w
  wCache <- readIORef (windowCache ui)
  let Just idx = findIndex (((==) `on` (winkey . coreWin)) w) wCache
      focusWindow = modA windowsA (fromJust . PL.move idx)

  case (Gdk.Events.eventClick event, Gdk.Events.eventButton event) of
     (Gdk.Events.SingleClick, Gdk.Events.LeftButton) -> do
       writeRef (winMotionSignal w) =<< Just <$> onMotionNotify (textview w) False (handleMove ui w p1)

     _ -> do maybe (return ()) signalDisconnect =<< readRef (winMotionSignal w)
             writeRef (winMotionSignal w) Nothing
             

  let runAction = uiActionCh ui . makeAction
  case (Gdk.Events.eventClick event, Gdk.Events.eventButton event) of
    (Gdk.Events.SingleClick, Gdk.Events.LeftButton) -> runAction $ do
        b <- gets $ (bkey . findBufferWith (bufkey $ coreWin w))
        focusWindow
        withGivenBufferAndWindow0 (coreWin w) b $ moveTo p1
    (Gdk.Events.SingleClick, _) -> runAction focusWindow
    (Gdk.Events.ReleaseClick, Gdk.Events.MiddleButton) -> do
        disp <- widgetGetDisplay (textview w)
        cb <- clipboardGetForDisplay disp selectionPrimary
        let cbHandler Nothing = return ()
            cbHandler (Just txt) = runAction $ do
                b <- gets $ (bkey . findBufferWith (bufkey $ coreWin w))
                withGivenBufferAndWindow0 (coreWin w) b $ do
                pointB >>= setSelectionMarkPointB
                moveTo p1
                insertN txt
        clipboardRequestText cb cbHandler
    _ -> return ()

  return True

handleScroll :: UI -> WinInfo -> Gdk.Events.Event -> IO Bool
handleScroll ui _ event = do
  let editorAction = do 
        withBuffer0 $ vimScrollB $ case Gdk.Events.eventDirection event of
                        Gdk.Events.ScrollUp   -> (-1)
                        Gdk.Events.ScrollDown -> 1
                        _ -> 0 -- Left/right scrolling not supported

  uiActionCh ui (makeAction editorAction)
  return True

handleMove :: UI -> WinInfo -> Point -> Gdk.Events.Event -> IO Bool
handleMove ui w p0 event = do
  logPutStrLn $ "Motion: " ++ show (Gdk.Events.eventX event, Gdk.Events.eventY event)

  -- retrieve the clicked offset.
  (_,layoutIndex,_) <- layoutXYToIndex (winLayout w) (Gdk.Events.eventX event) (Gdk.Events.eventY event)
  r <- readRef (shownRegion w)
  let p1 = regionStart r + fromIntegral layoutIndex


  let editorAction = do
        txt <- withBuffer0 $ do
           if p0 /= p1 
            then Just <$> do
              m <- selMark <$> askMarks
              setMarkPointB m p0
              moveTo p1
              setVisibleSelection True
              readRegionB =<< getSelectRegionB
            else return Nothing
        maybe (return ()) setRegE txt

  uiActionCh ui (makeAction editorAction)
  -- drawWindowGetPointer (textview w) -- be ready for next message.

  -- Relies on uiActionCh being synchronous
  selection <- newIORef ""
  let yiAction = do
      txt <- withEditor (withBuffer0 (readRegionB =<< getSelectRegionB))
             :: YiM String
      liftIO $ writeIORef selection txt
  uiActionCh ui (makeAction yiAction)
  txt <- readIORef selection

  disp <- widgetGetDisplay (textview w)
  cb <- clipboardGetForDisplay disp selectionPrimary
  clipboardSetWithData cb [(targetString,0)]
      (\0 -> selectionDataSetText txt >> return ()) (return ())

  return True

-- | Make A new window
newWindow :: Editor -> UI -> Window -> FBuffer -> IO WinInfo
newWindow e ui w b = mdo
    f <- readIORef (uiFont ui)

    ml <- labelNew Nothing
    widgetModifyFont ml (Just f)
    set ml [ miscXalign := 0.01 ] -- so the text is left-justified.

    v <- drawingAreaNew
    widgetModifyFont v (Just f)
    widgetAddEvents v [Button1MotionMask]
    widgetModifyBg v StateNormal $ mkCol False $ Yi.Style.background $ baseAttributes $ configStyle $ uiConfig ui

    box <- if isMini w
     then do
      widgetSetSizeRequest v (-1) 1

      prompt <- labelNew (Just $ identString b)
      widgetModifyFont prompt (Just f)

      hb <- hBoxNew False 1
      set hb [ containerChild := prompt,
               containerChild := v,
               boxChildPacking prompt := PackNatural,
               boxChildPacking v := PackGrow]

      return (castToBox hb)
     else do
      vb <- vBoxNew False 1
      set vb [ containerChild := v,
               containerChild := ml,
               boxChildPacking ml := PackNatural]
      return (castToBox vb)
     
    sig       <- newIORef =<< (v `onExpose` render e ui b win)
    rRef      <- newIORef (askBuffer w b winRegionB)
    context   <- widgetCreatePangoContext v
    layout    <- layoutEmpty context
    language  <- contextGetLanguage context
    metrics   <- contextGetMetrics context f language
    motionSig <- newIORef Nothing

    layoutSetWrap layout WrapAnywhere
    layoutSetFontDescription layout (Just f)

    let win = WinInfo { coreWin   = w
                      , winLayout = layout
                      , winMetrics = metrics
                      , winMotionSignal = motionSig
                      , textview  = v
                      , modeline  = ml
                      , widget    = box
                      , renderer  = sig
                      , shownRegion = rRef
                      }

    return win

insertWindowBefore :: Editor -> UI -> Window -> WinInfo -> IO WinInfo
insertWindowBefore e i w _c = insertWindow e i w

insertWindowAtEnd :: Editor -> UI -> Window -> IO WinInfo
insertWindowAtEnd e i w = insertWindow e i w

insertWindow :: Editor -> UI -> Window -> IO WinInfo
insertWindow e i win = do
  let buf = findBufferWith (bufkey win) e
  liftIO $ do w <- newWindow e i win buf
              set (uiBox i) [containerChild := widget w,
                             boxChildPacking (widget w) := if isMini (coreWin w) then PackNatural else PackGrow]
              textview w `onButtonRelease` handleClick i w
              textview w `onButtonPress` handleClick i w
              textview w `onScroll` handleScroll i w
              widgetShowAll (widget w)
              return w

refresh :: UI -> Editor -> IO ()
refresh ui e = do
    let ws = Editor.windows e
    set (uiCmdLine ui) [labelText := intercalate "  " $ statusLine e,
                        labelEllipsize := EllipsizeEnd]

    cache <- readRef $ windowCache ui
    logPutStrLn $ "syncing: " ++ show ws
    logPutStrLn $ "with: " ++ show cache
    cache' <- syncWindows e ui (toList $ PL.withFocus $ ws) cache
    logPutStrLn $ "Gives: " ++ show cache'
    writeRef (windowCache ui) cache'
    forM_ cache' $ \w -> do
        let b = findBufferWith (bufkey (coreWin w)) e
        -- when (not $ null $ pendingUpdates b) $ 
        do 
           sig <- readIORef (renderer w)
           signalDisconnect sig
           writeRef (renderer w) =<< (textview w `onExpose` render e ui b w)
           widgetQueueDraw (textview w)

winEls :: Point -> Int -> BufferM Yi.Buffer.Size
winEls tospnt h = savingPointB $ do
             moveTo tospnt
             gotoLnFrom (h-1)
             moveToEol
             p <- pointB
             return (p ~- tospnt)

render :: Editor -> UI -> FBuffer -> WinInfo -> t -> IO Bool
render e ui b w _ev = do
  reg               <- readRef (shownRegion w)
  drawWindow        <- widgetGetDrawWindow $ textview w
  (width', height') <- widgetGetSize $ textview w

  let win                 = coreWin w
      [width'', height''] = map fromIntegral [width', height']
      metrics             = winMetrics w
      layout              = winLayout w
      winh                = round (height'' / (ascent metrics + descent metrics))

      (point, text)       = askBuffer win b $ do
                              numChars <- winEls (regionStart reg) winh
                              p        <- pointB
                              content  <- nelemsB (fromIntegral numChars) (regionStart reg)
                              return (p, content)

  layoutSetWidth layout (Just width'')
  layoutSetText layout text

  (_,bos,_) <- layoutXYToIndex layout width'' height''
  let r' = mkRegion (regionStart reg) (fromIntegral bos + regionStart reg)

  -- Scroll the window when the cursor goes out of it:
  logPutStrLn $ "prewin: " ++ show r'
  logPutStrLn $ "point: " ++ show point
  r'' <- if inRegion point r'
    then return r'
    else do
      logPutStrLn $ "point moved out of visible region"

      let (topOfScreen, numChars, text') = askBuffer win b $ do
            top       <- indexOfSolAbove (winh `div` 2)
            charCount <- winEls top winh
            content   <- nelemsB (fromIntegral numChars) top
            return (top, charCount, content)

      layoutSetText layout text'

      return $ mkSizeRegion topOfScreen numChars

  writeRef (shownRegion w) r''
  logPutStrLn $ "updated: " ++ show r''

  -- add color attributes.
  let picture = askBuffer win b $ attributesPictureAndSelB sty (currentRegex e) r''
      sty = extractValue $ configTheme (uiConfig ui)
      strokes = [(start',s,end') | ((start', s), end') <- zip picture (drop 1 (map fst picture) ++ [regionEnd r'']),
                  s /= emptyAttributes]
      rel p = fromIntegral (p - regionStart r'')
      allAttrs = concat $ do
        (p1, Attributes fg bg _rv bd itlc udrl, p2) <- strokes
        return $ [ AttrForeground (rel p1) (rel p2) (mkCol True fg)
                 , AttrBackground (rel p1) (rel p2) (mkCol False bg)
                 , AttrStyle      (rel p1) (rel p2) (if itlc then StyleItalic     else StyleNormal)
                 , AttrUnderline  (rel p1) (rel p2) (if udrl then UnderlineSingle else UnderlineNone)
                 , AttrWeight     (rel p1) (rel p2) (if bd   then WeightBold      else WeightNormal)
                 ]

  layoutSetAttributes layout allAttrs

  (PangoRectangle curx cury curw curh, _) <- layoutGetCursorPos layout (fromPoint (point - regionStart r''))

  gc <- gcNew drawWindow
  drawLayout drawWindow gc 0 0 layout

  -- paint the cursor   
  gcSetValues gc (newGCValues { Gtk.foreground = mkCol True $ Yi.Style.foreground $ baseAttributes $ configStyle $ uiConfig ui })
  drawLine drawWindow gc (round curx, round cury) (round $ curx + curw, round $ cury + curh) 
  return True

prepareAction :: UI -> IO (EditorM ())
prepareAction ui = do
    wins <- readRef (windowCache ui)
    let ws = fmap coreWin wins
    rs <- mapM (readRef . shownRegion) wins
    logPutStrLn $ "new wins: " ++ show wins
    logPutStrLn $ "new regs: " ++ show rs
    heights <- forM wins $ \w -> do
      (_, h) <- widgetGetSize $ textview w
      let metrics = winMetrics w
      return $ round ((fromIntegral h) / (ascent metrics + descent metrics))
    return $ do
      let updWin w r = do
             withGivenBufferAndWindow0 w (bufkey w) $ do
                 Just (MarkSet f _ _ t) <- getMarks w
                 setMarkPointB f (regionStart r)
                 setMarkPointB t (regionEnd   r)
      sequence_ $ zipWith updWin ws rs
      modA windowsA (computeHeights $ heights ++ repeat 0)
      -- TODO: bos needs to be set also.
      -- FIXME: Get rid of 'repeat 0'; it seems to be necessary because no
      -- windows are available when this is first executed.

-- | Calculate window heights, given all the windows and current height.
computeHeights :: [Int] -> PL.PointedList Window -> PL.PointedList Window
computeHeights heights ws = fst $ runState (mapM distribute ws) heights

distribute :: Window -> State [Int] Window
distribute win = case isMini win of
                 True -> return win {height = 1}
                 False -> do h <- gets head
                             modify tail
                             return win {height = h}

reloadProject :: UI -> FilePath -> IO ()
reloadProject _ _ = return ()

mkCol :: Bool -- ^ is foreground?
      -> Yi.Style.Color -> Gtk.Color
mkCol True  Default = Color 0 0 0
mkCol False Default = Color maxBound maxBound maxBound
mkCol _ (RGB x y z) = Color (fromIntegral x * 256)
                            (fromIntegral y * 256)
                            (fromIntegral z * 256)

