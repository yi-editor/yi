{-# LANGUAGE CPP, BangPatterns, ExistentialQuantification, DoRec,
    ParallelListComp #-}

-- Copyright (c) 2007, 2008 Jean-Philippe Bernardy

-- | This module defines a user interface implemented using gtk2hs and
-- pango for direct text rendering.

module Yi.UI.Pango (start, processEvent) where

import Prelude (filter, map, round, FilePath, (/))

import Control.Concurrent (yield)
import Control.Monad (ap)
import Control.Monad.Reader (liftIO, when, MonadIO)
import Data.Prototype
import Data.IORef
import Data.List (drop, intercalate, nub, zip)
import qualified Data.List.PointedList.Circular as PL
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Rope as Rope

import Graphics.UI.Gtk hiding (on, Region, Window, Action, Point, Style)
import Graphics.UI.Gtk.Gdk.GC hiding (foreground)
import qualified Graphics.UI.Gtk.Gdk.Events as Gdk.Events
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Gdk.GC as Gtk
import System.Glib.GError

import Yi.Prelude

import Yi.Buffer
import Yi.Config
import qualified Yi.Editor as Editor
import Yi.Editor hiding (windows)
import Yi.Event
import Yi.Keymap
import Yi.Monad
import Yi.Style
import Yi.Window

import qualified Yi.UI.Common as Common
import Yi.UI.Pango.Utils
import Yi.UI.TabBar
import Yi.UI.Utils

#ifdef GNOME_ENABLED
import Yi.UI.Pango.Gnome(watchSystemFont)
#endif

data UI = UI
    { uiWindow    :: Gtk.Window
    , uiNotebook  :: Notebook
    , uiStatusbar :: Statusbar
    , tabCache    :: IORef [TabInfo]
    , uiActionCh  :: Action -> IO ()
    , uiConfig    :: UIConfig
    , uiFont      :: IORef FontDescription
    }

data TabInfo = TabInfo
    { coreTab     :: PL.PointedList Window
    , page        :: VBox
    , windowCache :: [WinInfo]
    }

instance Show TabInfo where
    show t = show (coreTab t)

data WinInfo = WinInfo
    { coreWin         :: Window
    , shownTos        :: IORef Point
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
    , Common.refresh       = refresh ui
    , Common.layout        = doLayout ui
    , Common.reloadProject = reloadProject ui
    }

updateFont :: UIConfig -> IORef FontDescription -> IORef [TabInfo] -> Statusbar
                  -> FontDescription -> IO ()
updateFont cfg fontRef tc status font = do
    maybe (return ()) (fontDescriptionSetFamily font) (configFontName cfg)
    maybe (return ()) (fontDescriptionSetSize font . fromIntegral) (configFontSize cfg)
    writeIORef fontRef font
    widgetModifyFont status (Just font)
    tcs <- readIORef tc
    forM_ tcs $ \tabinfo -> do
    let wcs = windowCache tabinfo
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
  logPutStrLn "startNoMsg"
  unsafeInitGUIForThreadedRTS

  win <- windowNew
  windowSetDefaultSize win 900 700
  windowSetTitle win "Yi"
  ico <- loadIcon "yi+lambda-fat.32.png"
  windowSetIcon win (Just ico)

  onKeyPress win (processEvent ch)

  paned <- hPanedNew

  vb <- vBoxNew False 1  -- Top-level vbox

  -- Disable the left pane (file/module browser) until Shim/Scion discussion has
  -- concluded. Shim causes crashes, but it's not worth fixing if we'll soon
  -- replace it.

  {-
  tabs' <- notebookNew
  widgetSetSizeRequest tabs' 200 (-1)
  notebookSetTabPos tabs' PosBottom
  panedAdd1 paned tabs'

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

  tabs <- notebookNew
  panedAdd2 paned tabs

  set win [ containerChild := vb ]
  onDestroy win mainQuit

  status  <- statusbarNew
  statusbarGetContextId status "global"

  set vb [ containerChild := paned,
           containerChild := status,
           boxChildPacking status := PackNatural ]

  fontRef <- newIORef undefined
  tc <- newIORef []

#ifdef GNOME_ENABLED
  let watchFont = watchSystemFont
#else
  let watchFont = (fontDescriptionFromString "Monospace 10" >>=)
#endif
  watchFont $ updateFont (configUI cfg) fontRef tc status

  -- use our magic threads thingy (http://haskell.org/gtk2hs/archives/2005/07/24/writing-multi-threaded-guis/)
  timeoutAddFull (yield >> return True) priorityDefaultIdle 50

  widgetShowAll win

  let ui = UI win tabs status tc (outCh . singleton) (configUI cfg) fontRef

  -- Keep the current tab focus up to date
  let move n pl = maybe pl id (PL.move n pl)
      runAction = uiActionCh ui . makeAction
  -- why does this cause a hang without postGUIAsync?
  onSwitchPage (uiNotebook ui) $ \n -> postGUIAsync $
    runAction (modA tabsA (move n) :: EditorM ())

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

syncTabs :: Editor -> UI -> [(PL.PointedList Window, Bool)] -> [TabInfo] -> IO [TabInfo]
syncTabs e ui (tfocused@(t,focused):ts) (c:cs)
    | t == coreTab c =
        do when focused $ setTabFocus ui c
           let wCache = windowCache c
           (:) <$> syncTab e ui c t wCache <*> syncTabs e ui ts cs
    | t `elem` map coreTab cs =
        do removeTab ui c
           syncTabs e ui (tfocused:ts) cs
    | otherwise =
        do c' <- insertTabBefore e ui t c
           when focused $ setTabFocus ui c'
           return (c':) `ap` syncTabs e ui ts (c:cs)
syncTabs e ui ts [] = mapM (\(t,focused) -> do c' <- insertTab e ui t
                                               when focused $ setTabFocus ui c'
                                               return c')
                           ts
syncTabs _ ui [] cs = mapM_ (removeTab ui) cs >> return []

syncTab :: Editor -> UI -> TabInfo -> PL.PointedList Window -> [WinInfo] -> IO TabInfo
syncTab e ui tab ws cache = do
    wCache <- syncWindows e ui tab (toList $ PL.withFocus ws) cache
    return tab { windowCache = wCache }

-- | Synchronize the windows displayed by GTK with the status of windows in the Core.
syncWindows :: Editor -> UI -> TabInfo -> [(Window, Bool)] -- ^ windows paired with their "isFocused" state.
            -> [WinInfo] -> IO [WinInfo]
syncWindows e ui tab (wfocused@(w,focused):ws) (c:cs)
    | w == coreWin c =
        do when focused $ setWindowFocus e ui tab c
           (c { coreWin = w}:) <$> syncWindows e ui tab ws cs
    | w `elem` map coreWin cs =
        removeWindow ui tab c >> syncWindows e ui tab (wfocused:ws) cs
    | otherwise = do
        c' <- insertWindowBefore e ui tab w c
        when focused (setWindowFocus e ui tab c')
        return (c':) `ap` syncWindows e ui tab ws (c:cs)
syncWindows e ui tab ws [] = mapM (\(w,focused) -> do c' <- insertWindowAtEnd e ui tab w
                                                      when focused (setWindowFocus e ui tab c')
                                                      return c')
                                  ws
syncWindows _ ui tab [] cs = mapM_ (removeWindow ui tab) cs >> return []

setTabFocus :: UI -> TabInfo -> IO ()
setTabFocus ui t = do
  p <- notebookPageNum (uiNotebook ui) (page t)
  case p of
    Just n  -> update (uiNotebook ui) notebookPage n
    Nothing -> return ()

-- Only set an attribute if has actually changed.
-- This makes setting window titles much faster.
update :: forall o a. (Eq a) => o -> ReadWriteAttr o a a -> a -> IO ()
update w attr val = do oldVal <- get w attr
                       when (val /= oldVal) $ set w [attr := val]

setWindowFocus :: Editor -> UI -> TabInfo -> WinInfo -> IO ()
setWindowFocus e ui t w = do
  let bufferName = shortIdentString (commonNamePrefix e) $ findBufferWith (bufkey $ coreWin w) e
      ml = askBuffer (coreWin w) (findBufferWith (bufkey $ coreWin w) e) $ getModeLine (commonNamePrefix e)

  update (textview w) widgetIsFocus True
  update (modeline w) labelText ml
  update (uiWindow ui) windowTitle $ bufferName ++ " - Yi"
  update (uiNotebook ui) (notebookChildTabLabel (page t)) (tabAbbrevTitle bufferName)

removeTab :: UI -> TabInfo -> IO ()
removeTab ui  t = do
    p <- notebookPageNum (uiNotebook ui) (page t)
    case p of
        Just n  -> notebookRemovePage (uiNotebook ui) n
        Nothing -> return ()

removeWindow :: UI -> TabInfo -> WinInfo -> IO ()
removeWindow _ tab win = do
    containerRemove (page tab) (widget win)

getWinInfo :: WindowRef -> [TabInfo] -> (Int, Int, WinInfo)
getWinInfo ref tabInfos =
  head [ (tabIx, winIx, winInfo)
       | (tabIx, tabInfo) <- zip [0..] tabInfos
       , (winIx, winInfo) <- zip [0..] (windowCache tabInfo)
       , ref == (wkey . coreWin) winInfo
       ]

handleClick :: UI -> WindowRef -> Gdk.Events.Event -> IO Bool
handleClick ui ref event = do
  (_tabIdx,winIdx,w) <- getWinInfo ref <$> readIORef (tabCache ui)

  logPutStrLn $ "Click: " ++ show (Gdk.Events.eventX event,
                                   Gdk.Events.eventY event,
                                   Gdk.Events.eventClick event)

  -- retrieve the clicked offset.
  (_,layoutIndex,_) <- io $ layoutXYToIndex (winLayout w) (Gdk.Events.eventX event) (Gdk.Events.eventY event)
  tos <- readRef (shownTos w)
  let p1 = tos + fromIntegral layoutIndex

  -- maybe focus the window
  logPutStrLn $ "Clicked inside window: " ++ show w

  let focusWindow = do
      -- TODO: check that tabIdx is the focus?
      modA windowsA (fromJust . PL.move winIdx)

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
        withGivenBufferAndWindow0 (coreWin w) b $ do
            moveTo p1
            setVisibleSelection False
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

handleScroll :: UI -> WindowRef -> Gdk.Events.Event -> IO Bool
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
  tos <- readRef (shownTos w)
  let p1 = tos + fromIntegral layoutIndex


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

handleConfigure :: UI -> WindowRef -> Gdk.Events.Event -> IO Bool
handleConfigure ui _ref _ev = do
  -- trigger a layout
  -- why does this cause a hang without postGUIAsync?
  postGUIAsync $ uiActionCh ui (makeAction (return () :: EditorM ()))
  return False -- allow event to be propagated

-- | Make a new tab.
newTab :: Editor -> UI -> VBox -> PL.PointedList Window -> IO TabInfo
newTab e ui vb ws = do
    let t' = TabInfo { coreTab = ws
                     , page    = vb
                     , windowCache = []
                     }
    cache <- syncWindows e ui t' (toList $ PL.withFocus ws) []
    return t' { windowCache = cache }

-- | Make a new window.
newWindow :: Editor -> UI -> Window -> FBuffer -> IO WinInfo
newWindow e ui w b = do
    f <- readIORef (uiFont ui)

    ml <- labelNew Nothing
    widgetModifyFont ml (Just f)
    set ml [ miscXalign := 0.01 ] -- so the text is left-justified.

    v <- drawingAreaNew
    widgetModifyFont v (Just f)
    widgetAddEvents v [Button1MotionMask]
    widgetModifyBg v StateNormal $ mkCol False $ Yi.Style.background $ baseAttributes $ configStyle $ uiConfig ui

    sw <- scrolledWindowNew Nothing Nothing
    scrolledWindowAddWithViewport sw v
    scrolledWindowSetPolicy sw PolicyAutomatic PolicyNever

    box <- if isMini w
     then do
      prompt <- labelNew (Just $ miniIdentString b)
      widgetModifyFont prompt (Just f)

      hb <- hBoxNew False 1
      set hb [ containerChild := prompt,
               containerChild := sw,
               boxChildPacking prompt := PackNatural,
               boxChildPacking sw := PackGrow]

      return (castToBox hb)
     else do
      vb <- vBoxNew False 1
      set vb [ containerChild := sw,
               containerChild := ml,
               boxChildPacking ml := PackNatural]
      return (castToBox vb)

    sig       <- newIORef =<< (v `onExpose` render e ui b (wkey w))
    tosRef    <- newIORef (askBuffer w b (getMarkPointB =<< fromMark <$> askMarks))
    context   <- widgetCreatePangoContext v
    layout    <- layoutEmpty context
    language  <- contextGetLanguage context
    metrics   <- contextGetMetrics context f language
    motionSig <- newIORef Nothing

    layoutSetFontDescription layout (Just f)
    layoutSetText layout "" -- stops layoutGetText crashing (as of gtk2hs 0.10.1)

    let win = WinInfo { coreWin   = w
                      , winLayout = layout
                      , winMetrics = metrics
                      , winMotionSignal = motionSig
                      , textview  = v
                      , modeline  = ml
                      , widget    = box
                      , renderer  = sig
                      , shownTos  = tosRef
                      }

    return win

insertTabBefore :: Editor -> UI -> PL.PointedList Window -> TabInfo -> IO TabInfo
insertTabBefore e ui ws c = do
    Just p <- notebookPageNum (uiNotebook ui) (page c)
    vb <- vBoxNew False 1
    notebookInsertPage (uiNotebook ui) vb "" p
    widgetShowAll $ vb
    t <- newTab e ui vb ws
    return t

insertTab :: Editor -> UI -> PL.PointedList Window -> IO TabInfo
insertTab e ui ws = do
    vb <- vBoxNew False 1
    notebookAppendPage (uiNotebook ui) vb ""
    widgetShowAll $ vb
    t <- newTab e ui vb ws
    return t

insertWindowBefore :: Editor -> UI -> TabInfo -> Window -> WinInfo -> IO WinInfo
insertWindowBefore e ui tab w _c = insertWindow e ui tab w

insertWindowAtEnd :: Editor -> UI -> TabInfo -> Window -> IO WinInfo
insertWindowAtEnd e ui tab w = insertWindow e ui tab w

insertWindow :: Editor -> UI -> TabInfo -> Window -> IO WinInfo
insertWindow e ui tab win = do
  let buf = findBufferWith (bufkey win) e
  liftIO $ do w <- newWindow e ui win buf

              set (page tab) $ 
                [ containerChild := widget w
                , boxChildPacking (widget w) :=
                    if isMini (coreWin w)
                        then PackNatural
                        else PackGrow
                ]

              let ref = (wkey . coreWin) w
              textview w `onButtonRelease` handleClick ui ref
              textview w `onButtonPress` handleClick ui ref
              textview w `onScroll` handleScroll ui ref
              textview w `onConfigure` handleConfigure ui ref
              widgetShowAll (widget w)

              return w

updateCache :: UI -> Editor -> IO ()
updateCache ui e = do
    let tabs = e ^. tabsA
    cache <- readRef $ tabCache ui
    cache' <- syncTabs e ui (toList $ PL.withFocus tabs) cache
    writeRef (tabCache ui) cache'

refresh :: UI -> Editor -> IO ()
refresh ui e = do
    contextId <- statusbarGetContextId (uiStatusbar ui) "global"
    statusbarPop  (uiStatusbar ui) contextId
    statusbarPush (uiStatusbar ui) contextId $ intercalate "  " $ statusLine e

    updateCache ui e -- The cursor may have changed since doLayout
    cache <- readRef $ tabCache ui
    forM_ cache $ \t -> do
        forM_ (windowCache t) $ \w -> do
            let b = findBufferWith (bufkey (coreWin w)) e
            -- when (not $ null $ b ^. pendingUpdatesA) $
            do
                sig <- readIORef (renderer w)
                signalDisconnect sig
                writeRef (renderer w) =<< (textview w `onExpose` render e ui b (wkey (coreWin w)))
                widgetQueueDraw (textview w)

render :: Editor -> UI -> FBuffer -> WindowRef -> t -> IO Bool
render e ui b ref _ev = do
  (_,_,w) <- getWinInfo ref <$> readIORef (tabCache ui)
  let win = coreWin w
  let tos = max 0 (regionStart (winRegion win))
  let bos = regionEnd (winRegion win)
  let (cur, _) = runBuffer win b pointB

  writeRef (shownTos w) tos
  drawWindow    <- widgetGetDrawWindow $ textview w

  -- add color attributes.
  let picture = askBuffer (coreWin w) b $ attributesPictureAndSelB sty (currentRegex e) (mkRegion tos bos)
      sty = extractValue $ configTheme (uiConfig ui)
      strokes = [(start',s,end') | ((start', s), end') <- zip picture (drop 1 (map fst picture) ++ [bos]),
                  s /= emptyAttributes]
      rel p = fromIntegral (p - tos)
      allAttrs = concat $ do
        (p1, Attributes fg bg _rv bd itlc udrl, p2) <- strokes
        return $ [ AttrForeground (rel p1) (rel p2) (mkCol True fg)
                 , AttrBackground (rel p1) (rel p2) (mkCol False bg)
                 , AttrStyle      (rel p1) (rel p2) (if itlc then StyleItalic     else StyleNormal)
                 , AttrUnderline  (rel p1) (rel p2) (if udrl then UnderlineSingle else UnderlineNone)
                 , AttrWeight     (rel p1) (rel p2) (if bd   then WeightBold      else WeightNormal)
                 ]
      layout = winLayout w

  layoutSetAttributes layout allAttrs

  (PangoRectangle curx cury curw curh, _) <- layoutGetCursorPos layout (rel cur)
  PangoRectangle chx chy chw chh          <- layoutIndexToPos layout (rel cur)

  gc <- gcNew drawWindow
  drawLayout drawWindow gc 0 0 layout

  -- paint the cursor   
  gcSetValues gc (newGCValues { Gtk.foreground = mkCol True $ Yi.Style.foreground $ baseAttributes $ configStyle $ uiConfig ui })
  if askBuffer (coreWin w) b $ getA insertingA
     then do drawLine drawWindow gc (round curx, round cury) (round $ curx + curw, round $ cury + curh) 
     else do drawRectangle drawWindow gc False (round chx) (round chy) (if chw > 0 then round chw else 8) (round chh)

  return True

doLayout :: UI -> Editor -> IO Editor
doLayout ui e = do
    updateCache ui e
    tabs <- readRef $ tabCache ui
    f <- readRef (uiFont ui)
    heights <- concat <$> mapM (getHeightsInTab ui f e) tabs
    let e' = (tabsA ^: fmap (fmap updateWin)) e
        updateWin w = case find (\(ref,_,_) -> (wkey w == ref)) heights of
                          Nothing -> w
                          Just (_,h,rgn) -> w { height = h, winRegion = rgn }

    -- Don't leak references to old Windows
    let forceWin x w = height w `seq` winRegion w `seq` x
    return $ (foldl . foldl) forceWin e' (e' ^. tabsA)

getHeightsInTab :: UI -> FontDescription -> Editor -> TabInfo -> IO [(WindowRef,Int,Region)]
getHeightsInTab ui f e tab = do
  forM (windowCache tab) $ \wi -> do
    (_, h) <- widgetGetSize $ textview wi
    let metrics = winMetrics wi
        lineHeight = ascent metrics + descent metrics
    let b0 = findBufferWith (bufkey (coreWin wi)) e
    rgn <- shownRegion ui f wi b0
    let ret= (wkey (coreWin wi), round $ fromIntegral h / lineHeight, rgn)
    return ret

shownRegion :: UI -> FontDescription -> WinInfo -> FBuffer -> IO Region
shownRegion ui f w b = do
   (tos, _, bos) <- updatePango ui f w b layout
   return $ mkRegion tos bos
  where layout = winLayout w

updatePango :: UI -> FontDescription -> WinInfo -> FBuffer -> PangoLayout -> IO (Point, Point, Point)
updatePango ui font w b layout = do
  (width', height') <- widgetGetSize $ textview w

  oldFont <- layoutGetFontDescription layout
  oldFontStr <- maybe (return Nothing) (fmap Just . fontDescriptionToString) oldFont
  newFontStr <- Just <$> fontDescriptionToString font
  when (oldFontStr /= newFontStr) (layoutSetFontDescription layout (Just font))

  let win                 = coreWin w
      [width'', height''] = map fromIntegral [width', height']
      metrics             = winMetrics w
      lineHeight          = ascent metrics + descent metrics
      winh                = max 1 $ floor (height'' / lineHeight)

      (tos, point, text)  = askBuffer win b $ do
                              from     <- getMarkPointB =<< fromMark <$> askMarks
                              rope     <- streamB Forward from
                              p        <- pointB
                              let content = fst $ Rope.splitAtLine winh rope
                              -- allow BOS offset to be just after the last line
                              let addNL = if Rope.countNewLines content == winh
                                              then id
                                              else (++"\n")
                              return (from, p, addNL $ Rope.toString content)

  if configLineWrap $ uiConfig ui
    then do oldWidth <- layoutGetWidth layout
            when (oldWidth /= Just width'') (layoutSetWidth layout $ Just width'')
    else do (Rectangle px _py pwidth _pheight, _) <- layoutGetPixelExtents layout
            widgetSetSizeRequest (textview w) (px+pwidth) (-1)

  -- optimize for cursor movement
  oldText <- layoutGetText layout
  when (oldText /= text) (layoutSetText layout text)

  (_, bosOffset, _) <- layoutXYToIndex layout width'' (fromIntegral winh * lineHeight - 1)
  return (tos, point, tos + fromIntegral bosOffset + 1)

reloadProject :: UI -> FilePath -> IO ()
reloadProject _ _ = return ()

mkCol :: Bool -- ^ is foreground?
      -> Yi.Style.Color -> Gtk.Color
mkCol True  Default = Color 0 0 0
mkCol False Default = Color maxBound maxBound maxBound
mkCol _ (RGB x y z) = Color (fromIntegral x * 256)
                            (fromIntegral y * 256)
                            (fromIntegral z * 256)
