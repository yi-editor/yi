{-# LANGUAGE RecordWildCards, ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable,
    StandaloneDeriving, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
--
-- Module      :  Yi.UI.Pango.Control
-- Copyright   :  2007-2009 Jean-Philippe Bernardy, Hamish Mackenzie
-- License     :  GPL
--
-- |
--
-----------------------------------------------------------------------------

module Yi.UI.Pango.Control (
    Control(..)
,   ControlM(..)
,   Buffer(..)
,   View(..)
,   Iter(..)
,   startControl
,   runControl
,   controlIO
,   liftYi
,   getControl
,   newBuffer
,   newView
,   getBuffer
,   setBufferMode
,   withBuffer
,   setText
,   getText
) where

import Prelude (map)

import Data.Maybe (maybe, fromJust)
import Data.IORef
import Data.List (drop, zip, take, length)
import Data.Prototype
import qualified Data.Rope as Rope
import qualified Data.Map as Map
import Yi.Prelude
import Yi.Core (startEditor, focusAllSyntax)
import Yi.Buffer
import Yi.Config
import Yi.Window as Yi
import Yi.Editor
import Yi.Event
import Yi.Keymap hiding(withBuffer)
import Yi.Monad
import Yi.Style
import Yi.UI.Utils
import Graphics.UI.Gtk as Gtk hiding(Action, Point, Region, get)
import qualified Graphics.UI.Gtk.Gdk.Events as Gdk.Events
import System.Glib.GError
import Control.Monad.Reader (liftIO, ask, asks, MonadReader(..))
import Control.Monad.State (liftM, ap, get, put, modify)
import Control.Monad.Writer (MonadIO(..))
import Control.Concurrent (newMVar, modifyMVar, MVar(..), newEmptyMVar, putMVar, readMVar, isEmptyMVar)
import Data.Typeable
import qualified Data.List.PointedList as  PL (insertRight, withFocus, PointedList(..), singleton)
import Yi.Regex
import System.FilePath
import qualified Yi.UI.Common as Common
import Yi.UI.Pango (processEvent)

data Control = Control
    { controlYi :: Yi
    , tabCache  :: IORef [TabInfo]
    , views     :: IORef (Map.Map WindowRef View)
    }
--    { config  :: Config
--    , editor  :: Editor
--    , input   :: Event -> IO ()
--    , output  :: Action -> IO ()
--    }

data TabInfo = TabInfo
    { coreTab     :: PL.PointedList Yi.Window
--    , page        :: VBox
    }

instance Show TabInfo where
    show t = show (coreTab t)

--type ControlM = YiM
newtype ControlM a = ControlM { runControl'' :: ReaderT Control IO a }
    deriving (Monad, MonadIO, MonadReader Control, Typeable, Functor, Applicative)

-- Helper functions to avoid issues with mismatching monad libraries
controlIO :: IO a -> ControlM a
controlIO = liftIO

getControl :: ControlM Control
getControl = ask

liftYi :: YiM a -> ControlM a
liftYi m = do
    yi <- asks controlYi
    liftIO $ runReaderT (runYiM m) yi

--instance MonadState Editor ControlM where
--    get = readRef =<< editor <$> ask
--    put v = flip modifyRef (const v) =<< editor <$> ask

--instance MonadEditor ControlM where
--    askCfg = config <$> ask
--    withEditor f = do
--      r <- asks editor
--      cfg <- asks config
--      liftIO $ controlUnsafeWithEditor cfg r f

startControl :: Config -> ControlM () -> IO ()
startControl config main = do
    startEditor (config { startFrontEnd = start main } ) Nothing

runControl' :: ControlM a -> MVar Control -> IO (Maybe a)
runControl' m yiMVar = do
    empty <- isEmptyMVar yiMVar
    if empty
        then return Nothing
        else do
            yi <- readMVar yiMVar
            result <- runControl m yi
            return $ Just result

-- runControl :: ControlM a -> Yi -> IO a
-- runControl m yi = runReaderT (runYiM m) yi

runControl :: ControlM a -> Control -> IO a
runControl f s = runReaderT (runControl'' f) s

-- runControlEditor f yiMVar = yiMVar

runAction :: Action -> ControlM ()
runAction action = do
    out <- liftYi $ asks output
    liftIO $ out [action]

-- | Test 2
mkUI :: IO () -> MVar Control -> Common.UI
mkUI main yiMVar = Common.dummyUI
    { Common.main          = main
    , Common.end           = \_ -> runControl' end yiMVar >> return ()
    , Common.suspend       = runControl' suspend yiMVar >> return ()
    , Common.refresh       = \e -> runControl' (refresh e) yiMVar >> return ()
    , Common.layout        = \e -> liftM (maybe e id) $ runControl' (doLayout e) yiMVar
    , Common.reloadProject = \f -> runControl' (reloadProject f) yiMVar >> return ()
    }

start :: ControlM () -> UIBoot
start main cfg ch outCh ed = catchGError (startNoMsg main cfg ch outCh ed) (\(GError _dom _code msg) -> fail msg)

makeControl :: MVar Control -> YiM ()
makeControl controlMVar = do
    controlYi <- ask
    tabCache  <- liftIO $ newIORef []
    views  <- liftIO $ newIORef Map.empty
    liftIO $ putMVar controlMVar Control{..}

startNoMsg :: ControlM () -> UIBoot
startNoMsg main config input output ed = do
    control <- newEmptyMVar
    let wrappedMain = do
        output [makeAction $ makeControl control]
        runControl' main control >> return ()
    return (mkUI wrappedMain control)

end :: ControlM ()
end = do
    liftIO $ putStrLn "Yi Control End"
    liftIO $ mainQuit

suspend :: ControlM ()
suspend = do
    liftIO $ putStrLn "Yi Control Suspend"
    return ()

refresh :: Editor -> ControlM ()
refresh e = do
    --contextId <- statusbarGetContextId (uiStatusbar ui) "global"
    --statusbarPop  (uiStatusbar ui) contextId
    --statusbarPush (uiStatusbar ui) contextId $ intercalate "  " $ statusLine e

    updateCache e -- The cursor may have changed since doLayout
    viewsRef <- asks views
    vs <- liftIO $ readRef viewsRef
    forM_ (Map.elems vs) $ \v -> do
        let b = findBufferWith (viewFBufRef v) e
        -- when (not $ null $ b ^. pendingUpdatesA) $
        do
            -- sig <- readIORef (renderer w)
            -- signalDisconnect sig
            -- writeRef (renderer w) =<< (textview w `onExpose` render e ui b (wkey (coreWin w)))
            liftIO $ widgetQueueDraw (drawArea v)

doLayout :: Editor -> ControlM Editor
doLayout e = do
    liftIO $ putStrLn "Yi Control Do Layout"
    updateCache e
    cacheRef <- asks tabCache
    tabs <- liftIO $ readRef cacheRef
    heights <- concat <$> mapM (getHeightsInTab e) tabs
    let e' = (tabsA ^: fmap (fmap updateWin)) e
        updateWin w = case find (\(ref,_,_) -> (wkey w == ref)) heights of
                          Nothing -> w
                          Just (_,h,rgn) -> w { height = h, winRegion = rgn }

    -- Don't leak references to old Windows
    let forceWin x w = height w `seq` winRegion w `seq` x
    return $ (foldl . foldl) forceWin e' (e' ^. tabsA)

getHeightsInTab :: Editor -> TabInfo -> ControlM [(WindowRef,Int,Region)]
getHeightsInTab e tab = do
  viewsRef <- asks views
  vs <- liftIO $ readRef viewsRef
  foldlM (\a w -> do
        case Map.lookup (wkey w) vs of
            Just v -> do
                (_, h) <- liftIO $ widgetGetSize $ drawArea v
                let lineHeight = ascent (metrics v) + descent (metrics v)
                let b0 = findBufferWith (viewFBufRef v) e
                rgn <- shownRegion e v b0
                let ret= (windowRef v, round $ fromIntegral h / lineHeight, rgn)
                return $ a ++ [ret]
            Nothing -> return a)
      [] (coreTab tab)

shownRegion :: Editor -> View -> FBuffer -> ControlM Region
shownRegion e v b = do
   (tos, _, bos) <- updatePango e v b (layout v)
   return $ mkRegion tos bos

updatePango :: Editor -> View -> FBuffer -> PangoLayout -> ControlM (Point, Point, Point)
updatePango e v b layout = do
  (width', height') <- liftIO $ widgetGetSize $ drawArea v

  font <- liftIO $ layoutGetFontDescription layout

  --oldFont <- layoutGetFontDescription layout
  --oldFontStr <- maybe (return Nothing) (fmap Just . fontDescriptionToString) oldFont
  --newFontStr <- Just <$> fontDescriptionToString font
  --when (oldFontStr /= newFontStr) (layoutSetFontDescription layout (Just font))

  let win                 = findWindowWith (windowRef v) e
      [width'', height''] = map fromIntegral [width', height']
      lineHeight          = ascent (metrics v) + descent (metrics v)
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

  config   <- liftYi $ askCfg
  if configLineWrap $ configUI config
    then do oldWidth <- liftIO $ layoutGetWidth layout
            when (oldWidth /= Just width'') $ liftIO $ layoutSetWidth layout $ Just width''
    else do (Rectangle px _py pwidth _pheight, _) <- liftIO $ layoutGetPixelExtents layout
            liftIO $ widgetSetSizeRequest (drawArea v) (px+pwidth) (-1)

  -- optimize for cursor movement
  oldText <- liftIO $ layoutGetText layout
  when (oldText /= text) $ liftIO $ layoutSetText layout text

  (_, bosOffset, _) <- liftIO $ layoutXYToIndex layout width'' (fromIntegral winh * lineHeight - 1)
  return (tos, point, tos + fromIntegral bosOffset + 1)

updateCache :: Editor -> ControlM ()
updateCache e = do
    let tabs = e ^. tabsA
    cacheRef <- asks tabCache
    cache <- liftIO $ readRef cacheRef
    cache' <- syncTabs e (toList $ PL.withFocus tabs) cache
    liftIO $ writeRef cacheRef cache'

syncTabs :: Editor -> [(PL.PointedList Yi.Window, Bool)] -> [TabInfo] -> ControlM [TabInfo]
syncTabs e (tfocused@(t,focused):ts) (c:cs)
    | t == coreTab c =
        do when focused $ setTabFocus c
--           let vCache = views c
           (:) <$> syncTab e c t <*> syncTabs e ts cs
    | t `elem` map coreTab cs =
        do removeTab c
           syncTabs e (tfocused:ts) cs
    | otherwise =
        do c' <- insertTabBefore e t c
           when focused $ setTabFocus c'
           return (c':) `ap` syncTabs e ts (c:cs)
syncTabs e ts [] = mapM (\(t,focused) -> do
        c' <- insertTab e t
        when focused $ setTabFocus c'
        return c') ts
syncTabs _ [] cs = mapM_ removeTab cs >> return []

syncTab :: Editor -> TabInfo -> PL.PointedList Yi.Window -> ControlM TabInfo
syncTab e tab ws = do
    -- TODO Maybe do something here
    return tab

setTabFocus :: TabInfo -> ControlM ()
setTabFocus t = do
  -- TODO this needs to set the tab focus with callback
  -- but only if the tab focus has changed
  return ()

askBuffer :: Yi.Window -> FBuffer -> BufferM a -> a
askBuffer w b f = fst $ runBuffer w b f

setWindowFocus :: Editor -> TabInfo -> View -> ControlM ()
setWindowFocus e t v = do
  let bufferName = shortIdentString (commonNamePrefix e) $ findBufferWith (viewFBufRef v) e
      window = findWindowWith (windowRef v) e
      ml = askBuffer window (findBufferWith (viewFBufRef v) e) $ getModeLine (commonNamePrefix e)

-- TODO
--  update (textview w) widgetIsFocus True
--  update (modeline w) labelText ml
--  update (uiWindow ui) windowTitle $ bufferName ++ " - Yi"
--  update (uiNotebook ui) (notebookChildTabLabel (page t)) (tabAbbrevTitle bufferName)
  return ()

removeTab :: TabInfo -> ControlM ()
removeTab t = do
  -- TODO this needs to close the views in the tab with callback
  return ()

removeView :: TabInfo -> View -> ControlM ()
removeView tab view = do
  -- TODO this needs to close the view with callback
  return ()

-- | Make a new tab.
newTab :: Editor -> PL.PointedList Yi.Window -> ControlM TabInfo
newTab e ws = do
    let t' = TabInfo { coreTab = ws }
--    cache <- syncWindows e t' (toList $ PL.withFocus ws) []
    return t' -- { views = cache }

insertTabBefore :: Editor -> PL.PointedList Yi.Window -> TabInfo -> ControlM TabInfo
insertTabBefore e ws c = do
    -- Just p <- notebookPageNum (uiNotebook ui) (page c)
    -- vb <- vBoxNew False 1
    -- notebookInsertPage (uiNotebook ui) vb "" p
    -- widgetShowAll $ vb
    t <- newTab e ws
    return t

insertTab :: Editor -> PL.PointedList Yi.Window -> ControlM TabInfo
insertTab e ws = do
    -- vb <- vBoxNew False 1
    -- notebookAppendPage (uiNotebook ui) vb ""
    -- widgetShowAll $ vb
    t <- newTab e ws
    return t

{-
insertWindowBefore :: Editor -> TabInfo -> Yi.Window -> WinInfo -> IO WinInfo
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
-}

reloadProject :: FilePath -> ControlM ()
reloadProject _ = return ()

controlUnsafeWithEditor :: Config -> MVar Editor -> EditorM a -> IO a
controlUnsafeWithEditor cfg r f = modifyMVar r $ \e -> do
  let (e',a) = runEditor cfg f e
  -- Make sure that the result of runEditor is evaluated before
  -- replacing the editor state. Otherwise, we might replace e
  -- with an exception-producing thunk, which makes it impossible
  -- to look at or update the editor state.
  -- Maybe this could also be fixed by -fno-state-hack flag?
  -- TODO: can we simplify this?
  e' `seq` a `seq` return (e', a)

data Buffer = Buffer
    { fBufRef     :: BufferRef
    }

data View = View
    { viewFBufRef :: BufferRef
    , windowRef   :: WindowRef
    , drawArea    :: DrawingArea
    , layout      :: PangoLayout
    , language    :: Language
    , metrics     :: FontMetrics
    , scrollWin   :: ScrolledWindow
    , shownTos    :: IORef Point
    , winMotionSignal :: IORef (Maybe (ConnectId DrawingArea))
    }

data Iter = Iter
    { iterFBufRef :: BufferRef
    , point       :: Point
    }

newBuffer :: BufferId -> String -> ControlM Buffer
newBuffer id text = do
    fBufRef <- liftYi $ withEditor $ newBufferE id $ Rope.fromString text
    return Buffer{..}

newView :: Buffer -> FontDescription -> ControlM View
newView buffer font = do
    control  <- ask
    config   <- liftYi $ askCfg
    let viewFBufRef = fBufRef buffer
    newWindow   <- fmap (\w -> w{height=50, winRegion = mkRegion (Point 0) (Point 2000)}) $ liftYi $ withEditor $ newWindowE False viewFBufRef
    let windowRef = wkey newWindow
    liftYi $ withEditor $ do
        modA windowsA (PL.insertRight newWindow)
        e <- get
        put $ focusAllSyntax e
    drawArea <- liftIO $ drawingAreaNew
    liftIO $ widgetModifyBg drawArea StateNormal $ mkCol False $ Yi.Style.background $ baseAttributes $ configStyle $ configUI config
    context  <- liftIO $ widgetCreatePangoContext drawArea
    layout   <- liftIO $ layoutEmpty context
    liftIO $ layoutSetFontDescription layout (Just font)
    language <- liftIO $ contextGetLanguage context
    metrics  <- liftIO $ contextGetMetrics context font language
    liftIO $ layoutSetText layout ""

    scrollWin <- liftIO $ scrolledWindowNew Nothing Nothing
    liftIO $ do
        scrolledWindowAddWithViewport scrollWin drawArea
        scrolledWindowSetPolicy scrollWin PolicyAutomatic PolicyNever

    initialTos <- liftYi $ withEditor $ withGivenBufferAndWindow0 newWindow viewFBufRef $
        getMarkPointB =<< fromMark <$> askMarks
    shownTos <- liftIO $ newIORef initialTos
    winMotionSignal <- liftIO $ newIORef Nothing

    let view = View {..}

    liftIO $ Gtk.widgetAddEvents drawArea [KeyPressMask]
    liftIO $ Gtk.set drawArea [Gtk.widgetCanFocus := True]

    liftIO $ drawArea `Gtk.onKeyPress` \event -> do
        putStrLn $ "Yi Control Key Press = " ++ show event
        runControl (do
            runAction $ makeAction $ do
                focusWindowE windowRef
                switchToBufferE viewFBufRef) control
        result <- processEvent (input $ controlYi control) event
        widgetQueueDraw drawArea
        return result

    liftIO $ drawArea `Gtk.onButtonPress` \event -> do
        widgetGrabFocus drawArea
        runControl (handleClick view event) control

    liftIO $ drawArea `Gtk.onButtonRelease` \event -> do
        runControl (handleClick view event) control

    liftIO $ drawArea `Gtk.onScroll` \event -> do
        runControl (handleScroll view event) control

    liftIO $ drawArea `Gtk.onExpose` \event -> do
        (text, allAttrs, debug, tos, rel, point, inserting) <- runControl (liftYi $ withEditor $ do
            window <- (findWindowWith windowRef) <$> get
            modA buffersA (fmap (clearSyntax . clearHighlight))
            let winh = height window
            let tos = max 0 (regionStart (winRegion window))
            let bos = regionEnd (winRegion window)
            let rel p = fromIntegral (p - tos)

            withGivenBufferAndWindow0 window viewFBufRef $ do
                -- tos       <- getMarkPointB =<< fromMark <$> askMarks
                rope      <- streamB Forward tos
                point     <- pointB
                inserting <- getA insertingA

                modeNm <- gets (withMode0 modeName)

    --            let (tos, point, text, picture) = do runBu
    --                        from     <- getMarkPointB =<< fromMark <$> askMarks
    --                        rope     <- streamB Forward from
    --                        p        <- pointB
                let content = fst $ Rope.splitAtLine winh rope
                -- allow BOS offset to be just after the last line
                let addNL = if Rope.countNewLines content == winh
                              then id
                              else (++"\n")
                    sty = extractValue $ configTheme (configUI config)
                            -- attributesPictureAndSelB sty (currentRegex e) (mkRegion tos bos)
    --                        return (from, p, addNL $ Rope.toString content, picture)
                let text = addNL $ Rope.toString content

                picture <- attributesPictureAndSelB sty Nothing (mkRegion tos bos)

                -- add color attributes.
                let strokes = [(start',s,end') | ((start', s), end') <- zip picture (drop 1 (map fst picture) ++ [bos]),
                              s /= emptyAttributes]
                    allAttrs = concat $ do
                        (p1, Attributes fg bg _rv bd itlc udrl, p2) <- strokes
                        return $ [ AttrForeground (rel p1) (rel p2) (mkCol True fg)
                                 , AttrBackground (rel p1) (rel p2) (mkCol False bg)
                                 , AttrStyle      (rel p1) (rel p2) (if itlc then StyleItalic     else StyleNormal)
                                 , AttrUnderline  (rel p1) (rel p2) (if udrl then UnderlineSingle else UnderlineNone)
                                 , AttrWeight     (rel p1) (rel p2) (if bd   then WeightBold      else WeightNormal)
                                 ]
                return (text, allAttrs, (picture, strokes, modeNm, window, tos, bos, winh), tos, rel, point, inserting)) control

        -- putStrLn $ "Setting Layout Attributes " ++ show debug
        layoutSetAttributes layout allAttrs
        -- putStrLn "Done Stting Layout Attributes"
        dw      <- widgetGetDrawWindow drawArea
        gc      <- gcNew dw
        oldText <- layoutGetText layout
        when (text /= oldText) $ layoutSetText layout text
        drawLayout dw gc 0 0 layout
        liftIO $ writeRef shownTos tos

        -- paint the cursor
        (PangoRectangle curx cury curw curh, _) <- layoutGetCursorPos layout (rel point)
        PangoRectangle chx chy chw chh          <- layoutIndexToPos layout (rel point)

        gcSetValues gc (newGCValues { Gtk.foreground = mkCol True $ Yi.Style.foreground $ baseAttributes $ configStyle $ configUI config })
        if inserting
            then do drawLine dw gc (round curx, round cury) (round $ curx + curw, round $ cury + curh)
            else do drawRectangle dw gc False (round chx) (round chy) (if chw > 0 then round chw else 8) (round chh)

        return True

    liftIO $ widgetGrabFocus drawArea

    tabsRef <- asks tabCache
    ts <- liftIO $ readRef tabsRef
    liftIO $ writeRef tabsRef (TabInfo (PL.singleton newWindow):ts)

    viewsRef <- asks views
    vs <- liftIO $ readRef viewsRef
    liftIO $ writeRef viewsRef $ Map.insert windowRef view vs

    return view
  where
    clearHighlight fb =
      -- if there were updates, then hide the selection.
      let h = getVal highlightSelectionA fb
          us = getVal pendingUpdatesA fb
      in highlightSelectionA ^= (h && null us) $ fb

setBufferMode :: FilePath -> Buffer -> ControlM ()
setBufferMode f buffer = do
    let bufRef = fBufRef buffer
    -- adjust the mode
    tbl <- liftYi $ asks (modeTable . yiConfig)
    contents <- liftYi $ withEditor $ withGivenBuffer0 bufRef $ elemsB
    let header = take 1024 contents
        hmode = case header =~ "\\-\\*\\- *([^ ]*) *\\-\\*\\-" of
            AllTextSubmatches [_,m] -> m
            _ -> ""
        Just mode = (find (\(AnyMode m)-> modeName m == hmode) tbl) <|>
                    (find (\(AnyMode m)-> modeApplies m f contents) tbl) <|>
                    Just (AnyMode emptyMode)
    case mode of
        AnyMode newMode -> do
            -- liftIO $ putStrLn $ show (f, modeName newMode)
            liftYi $ withEditor $ do
                withGivenBuffer0 bufRef $ do
                    setMode newMode
                    modify clearSyntax
                switchToBufferE bufRef
            -- withEditor focusAllSyntax

withBuffer :: Buffer -> BufferM a -> ControlM a
withBuffer Buffer{fBufRef = b} f = liftYi $ withEditor $ withGivenBuffer0 b f

getBuffer :: View -> Buffer
getBuffer view = Buffer {fBufRef = viewFBufRef view}

setText :: Buffer -> String -> ControlM ()
setText b text = withBuffer b $ do
    r <- regionOfB Document

    replaceRegionClever r text

getText :: Buffer -> Iter -> Iter -> ControlM String
getText b Iter{point = p1} Iter{point = p2} = withBuffer b $ readRegionB $ mkRegion p1 p2

mkCol :: Bool -- ^ is foreground?
      -> Yi.Style.Color -> Gtk.Color
mkCol True  Default = Color 0 0 0
mkCol False Default = Color maxBound maxBound maxBound
mkCol _ (RGB x y z) = Color (fromIntegral x * 256)
                            (fromIntegral y * 256)
                            (fromIntegral z * 256)

handleClick :: View -> Gdk.Events.Event -> ControlM Bool
handleClick view event = do
  control  <- ask
  -- (_tabIdx,winIdx,w) <- getWinInfo ref <$> readIORef (tabCache ui)

  logPutStrLn $ "Click: " ++ show (Gdk.Events.eventX event,
                                   Gdk.Events.eventY event,
                                   Gdk.Events.eventClick event)

  -- retrieve the clicked offset.
  (_,layoutIndex,_) <- io $ layoutXYToIndex (layout view) (Gdk.Events.eventX event) (Gdk.Events.eventY event)
  tos <- readRef (shownTos view)
  let p1 = tos + fromIntegral layoutIndex

  let winRef = windowRef view

  -- maybe focus the window
  -- logPutStrLn $ "Clicked inside window: " ++ show view

--  let focusWindow = do
      -- TODO: check that tabIdx is the focus?
--      modA windowsA (fromJust . PL.move winIdx)

  liftIO $ case (Gdk.Events.eventClick event, Gdk.Events.eventButton event) of
     (Gdk.Events.SingleClick, Gdk.Events.LeftButton) -> do
        cid <- onMotionNotify (drawArea view) False $ \event -> do
            runControl (handleMove view p1 event) control
        writeRef (winMotionSignal view) $ Just cid

     _ -> do maybe (return ()) signalDisconnect =<< readRef (winMotionSignal view)
             writeRef (winMotionSignal view) Nothing

  case (Gdk.Events.eventClick event, Gdk.Events.eventButton event) of
    (Gdk.Events.SingleClick, Gdk.Events.LeftButton) -> runAction . makeAction $ do
        -- b <- gets $ (bkey . findBufferWith (viewFBufRef view))
        -- focusWindow
        window <- (findWindowWith winRef) <$> get
        withGivenBufferAndWindow0 window (viewFBufRef view) $ do
            moveTo p1
            setVisibleSelection False
    -- (Gdk.Events.SingleClick, _) -> runAction focusWindow
    (Gdk.Events.ReleaseClick, Gdk.Events.MiddleButton) -> do
        disp <- liftIO $ widgetGetDisplay (drawArea view)
        cb <- liftIO $ clipboardGetForDisplay disp selectionPrimary
        let cbHandler Nothing = return ()
            cbHandler (Just txt) = runControl (runAction . makeAction $ do
                window <- (findWindowWith winRef) <$> get
                withGivenBufferAndWindow0 window (viewFBufRef view) $ do
                    pointB >>= setSelectionMarkPointB
                    moveTo p1
                    insertN txt) control
        liftIO $ clipboardRequestText cb cbHandler
    _ -> return ()

  liftIO $ widgetQueueDraw (drawArea view)
  return True

handleScroll :: View -> Gdk.Events.Event -> ControlM Bool
handleScroll view event = do
  let editorAction = do
        withBuffer0 $ vimScrollB $ case Gdk.Events.eventDirection event of
                        Gdk.Events.ScrollUp   -> (-1)
                        Gdk.Events.ScrollDown -> 1
                        _ -> 0 -- Left/right scrolling not supported

  runAction $ makeAction editorAction
  liftIO $ widgetQueueDraw (drawArea view)
  return True

handleMove :: View -> Point -> Gdk.Events.Event -> ControlM Bool
handleMove view p0 event = do
  logPutStrLn $ "Motion: " ++ show (Gdk.Events.eventX event, Gdk.Events.eventY event)

  -- retrieve the clicked offset.
  (_,layoutIndex,_) <- liftIO $ layoutXYToIndex (layout view) (Gdk.Events.eventX event) (Gdk.Events.eventY event)
  tos <- readRef (shownTos view)
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

  runAction $ makeAction editorAction
  -- drawWindowGetPointer (textview w) -- be ready for next message.

  -- Relies on uiActionCh being synchronous
  selection <- liftIO $ newIORef ""
  let yiAction = do
      txt <- withEditor (withBuffer0 (readRegionB =<< getSelectRegionB))
             :: YiM String
      liftIO $ writeIORef selection txt
  runAction $ makeAction yiAction
  txt <- liftIO $ readIORef selection

  disp <- liftIO $ widgetGetDisplay (drawArea view)
  cb <- liftIO $ clipboardGetForDisplay disp selectionPrimary
  liftIO $ clipboardSetWithData cb [(targetString,0)]
      (\0 -> selectionDataSetText txt >> return ()) (return ())

  liftIO $ widgetQueueDraw (drawArea view)
  return True
