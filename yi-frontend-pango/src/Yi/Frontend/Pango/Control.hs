{-# LANGUAGE RecordWildCards, ScopedTypeVariables, MultiParamTypeClasses
           , DeriveDataTypeable, OverloadedStrings
           , GeneralizedNewtypeDeriving, FlexibleContexts #-}

-- this module isn't finished, and there's heaps of warnings.
{-# OPTIONS_GHC -w #-}

-- |
-- Module      :  Yi.UI.Pango.Control
-- License     :  GPL

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
,   withCurrentBuffer
,   setText
,   getText
,   keyTable
) where

import Data.Text (unpack, pack, Text)
import qualified Data.Text as T
import Prelude hiding (concatMap, concat, foldl, elem, mapM_)
import Control.Exception (catch)
import Control.Monad        hiding (mapM_, forM_)
import Control.Monad.Reader hiding (mapM_, forM_)
import Control.Applicative
import Control.Lens hiding (views, Action)
import Data.Foldable
import Data.Maybe (maybe, fromJust, fromMaybe)
import Data.Monoid
import Data.IORef
import Data.List (nub, filter, drop, zip, take, length)
import Data.Prototype
import Yi.Rope (toText, splitAtLine, YiString)
import qualified Yi.Rope as R
import qualified Data.Map as Map
import Yi.Core (startEditor, focusAllSyntax)
import Yi.Buffer
import Yi.Config
import Yi.Tab
import Yi.Window as Yi
import Yi.Editor
import Yi.Event
import Yi.Keymap
import Yi.Monad
import Yi.Style
import Yi.UI.Utils
import Yi.Utils
import Yi.Debug
import Graphics.UI.Gtk as Gtk
       (Color(..), PangoRectangle(..), Rectangle(..), selectionDataSetText,
        targetString, clipboardSetWithData, clipboardRequestText,
        selectionPrimary, clipboardGetForDisplay, widgetGetDisplay,
        onMotionNotify, drawRectangle, drawLine,
        layoutIndexToPos, layoutGetCursorPos, drawLayout,
        widgetGetDrawWindow, layoutSetAttributes, widgetGrabFocus,
        scrolledWindowSetPolicy, scrolledWindowAddWithViewport,
        scrolledWindowNew, contextGetMetrics, contextGetLanguage,
        layoutSetFontDescription, layoutEmpty, widgetCreatePangoContext,
        widgetModifyBg, drawingAreaNew, FontDescription, ScrolledWindow,
        FontMetrics, Language, DrawingArea, layoutXYToIndex, layoutSetText,
        layoutGetText, widgetSetSizeRequest, layoutGetPixelExtents,
        layoutSetWidth, layoutGetWidth, layoutGetFontDescription,
        PangoLayout, descent, ascent, widgetGetSize, widgetQueueDraw,
        mainQuit, signalDisconnect, ConnectId(..), PolicyType(..),
        StateType(..), EventMask(..), AttrOp(..), Weight(..),
        PangoAttribute(..), Underline(..), FontStyle(..))
import Graphics.UI.Gtk.Gdk.GC as Gtk
  (newGCValues, gcSetValues, gcNew, foreground)
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as Gdk.Events
import System.Glib.GError
import Control.Monad.Reader (ask, asks, MonadReader(..))
import Control.Monad.State (ap, get, put, modify)
import Control.Monad.Base
import Control.Concurrent (newMVar, modifyMVar, MVar, newEmptyMVar, putMVar,
                           readMVar, isEmptyMVar)
import Data.Typeable
import qualified Data.List.PointedList as PL (insertRight, withFocus,
                                              PointedList(..), singleton)
import Yi.Regex
import Yi.String (showT)
import System.FilePath
import qualified Yi.UI.Common as Common

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
    { coreTab     :: Tab
--    , page        :: VBox
    }

instance Show TabInfo where
    show t = show (coreTab t)

--type ControlM = YiM
newtype ControlM a = ControlM { runControl'' :: ReaderT Control IO a }
    deriving (Monad, MonadBase IO, MonadReader Control, Typeable,
              Functor, Applicative)

-- Helper functions to avoid issues with mismatching monad libraries
controlIO :: IO a -> ControlM a
controlIO = liftBase

getControl :: ControlM Control
getControl = ask

liftYi :: YiM a -> ControlM a
liftYi m = do
    yi <- asks controlYi
    liftBase $ runReaderT (runYiM m) yi

--instance MonadState Editor ControlM where
--    get = readRef =<< editor <$> ask
--    put v = flip modifyRef (const v) =<< editor <$> ask

--instance MonadEditor ControlM where
--    askCfg = config <$> ask
--    withEditor f = do
--      r <- asks editor
--      cfg <- asks config
--      liftBase $ controlUnsafeWithEditor cfg r f

startControl :: Config -> ControlM () -> IO ()
startControl config main = startEditor (config { startFrontEnd = start main } ) Nothing

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
runControl f = runReaderT (runControl'' f)

-- runControlEditor f yiMVar = yiMVar

runAction :: Action -> ControlM ()
runAction action = do
    out <- liftYi $ asks yiOutput
    liftBase $ out MustRefresh [action]

-- | Test 2
mkUI :: IO () -> MVar Control -> Common.UI Editor
mkUI main yiMVar = Common.dummyUI
    { Common.main          = main
    , Common.end           = \_ -> void $ runControl' end yiMVar
    , Common.suspend       = void $ runControl' suspend yiMVar
    , Common.refresh       = \e -> void $ runControl' (refresh e) yiMVar
    , Common.layout        = \e -> fmap (fromMaybe e) $
                                   runControl' (doLayout e) yiMVar
    , Common.reloadProject = \f -> void $ runControl' (reloadProject f) yiMVar
    }

start :: ControlM () -> UIBoot
start main cfg ch outCh ed =
  catch (startNoMsg main cfg ch outCh ed) (\(GError _dom _code msg) ->
                                            fail $ unpack msg)

makeControl :: MVar Control -> YiM ()
makeControl controlMVar = do
    controlYi <- ask
    tabCache  <- liftBase $ newIORef []
    views  <- liftBase $ newIORef Map.empty
    liftBase $ putMVar controlMVar Control{..}

startNoMsg :: ControlM () -> UIBoot
startNoMsg main config input output ed = do
    control <- newEmptyMVar
    let wrappedMain = do
        output [makeAction $ makeControl control]
        void (runControl' main control)
    return (mkUI wrappedMain control)

end :: ControlM ()
end = do
    liftBase $ putStrLn "Yi Control End"
    liftBase mainQuit

suspend :: ControlM ()
suspend = do
    liftBase $ putStrLn "Yi Control Suspend"
    return ()

{-# ANN refresh ("HLint: ignore Redundant do" :: String) #-}
refresh :: Editor -> ControlM ()
refresh e = do
    --contextId <- statusbarGetContextId (uiStatusbar ui) "global"
    --statusbarPop  (uiStatusbar ui) contextId
    --statusbarPush (uiStatusbar ui) contextId $ intercalate "  " $ statusLine e

    updateCache e -- The cursor may have changed since doLayout
    viewsRef <- asks views
    vs <- liftBase $ readIORef viewsRef
    forM_ (Map.elems vs) $ \v -> do
        let b = findBufferWith (viewFBufRef v) e
        -- when (not $ null $ b ^. pendingUpdatesA) $
        do
            -- sig <- readIORef (renderer w)
            -- signalDisconnect sig
            -- writeRef (renderer w)
            -- =<< (textview w `onExpose` render e ui b (wkey (coreWin w)))
            liftBase $ widgetQueueDraw (drawArea v)

doLayout :: Editor -> ControlM Editor
doLayout e = do
    liftBase $ putStrLn "Yi Control Do Layout"
    updateCache e
    cacheRef <- asks tabCache
    tabs <- liftBase $ readIORef cacheRef
    dims <- concat <$> mapM (getDimensionsInTab e) tabs
    let e' = (tabsA %~ fmap (mapWindows updateWin)) e
        updateWin w = case find (\(ref,_,_,_) -> (wkey w == ref)) dims of
                          Nothing -> w
                          Just (_, wi, h,rgn) -> w { width = wi
                                                   , height = h
                                                   , winRegion = rgn }
    -- Don't leak references to old Windows
    let forceWin x w = height w `seq` winRegion w `seq` x
    return $ (foldl . tabFoldl) forceWin e' (e' ^. tabsA)

-- | Width, Height
getDimensionsInTab :: Editor -> TabInfo -> ControlM [(WindowRef,Int,Int,Region)]
getDimensionsInTab e tab = do
  viewsRef <- asks views
  vs <- liftBase $ readIORef viewsRef
  foldlM (\a w ->
        case Map.lookup (wkey w) vs of
            Just v -> do
                (wi, h) <- liftBase $ widgetGetSize $ drawArea v
                let lineHeight = ascent (metrics v) + descent (metrics v)
                    charWidth = Gtk.approximateCharWidth $ metrics v
                    b0 = findBufferWith (viewFBufRef v) e
                rgn <- shownRegion e v b0
                let ret= (windowRef v, round $ fromIntegral wi / charWidth, 
                          round $ fromIntegral h / lineHeight, rgn)
                return $ a <> [ret]
            Nothing -> return a)
      [] (coreTab tab ^. tabWindowsA)

shownRegion :: Editor -> View -> FBuffer -> ControlM Region
shownRegion e v b = do
   (tos, _, bos) <- updatePango e v b (layout v)
   return $ mkRegion tos bos

updatePango :: Editor -> View -> FBuffer -> PangoLayout
            -> ControlM (Point, Point, Point)
updatePango e v b layout = do
  (width', height') <- liftBase $ widgetGetSize $ drawArea v

  font <- liftBase $ layoutGetFontDescription layout

  --oldFont <- layoutGetFontDescription layout
  --oldFontStr <- maybe (return Nothing)
  --              (fmap Just . fontDescriptionToString) oldFont
  --newFontStr <- Just <$> fontDescriptionToString font
  --when (oldFontStr /= newFontStr)
  --  (layoutSetFontDescription layout (Just font))

  let win                 = findWindowWith (windowRef v) e
      [width'', height''] = map fromIntegral [width', height']
      lineHeight          = ascent (metrics v) + descent (metrics v)
      winh                = max 1 $ floor (height'' / lineHeight)

      (tos, point, text)  = askBuffer win b $ do
                              from <- (use . markPointA) =<< fromMark <$> askMarks
                              rope <- streamB Forward from
                              p    <- pointB
                              let content = fst $ splitAtLine winh rope
                              -- allow BOS offset to be just after the last line
                              let addNL = if R.countNewLines content == winh
                                          then id
                                          else (`R.snoc` '\n')
                              return (from, p, R.toText $ addNL content)

  config   <- liftYi askCfg
  if configLineWrap $ configUI config
    then do oldWidth <- liftBase $ layoutGetWidth layout
            when (oldWidth /= Just width'') $
              liftBase $ layoutSetWidth layout $ Just width''
    else do
    (Rectangle px _py pwidth _pheight, _) <- liftBase $
                                             layoutGetPixelExtents layout
    liftBase $ widgetSetSizeRequest (drawArea v) (px+pwidth) (-1)

  -- optimize for cursor movement
  oldText <- liftBase $ layoutGetText layout
  when (oldText /= text) $ liftBase $ layoutSetText layout text

  (_, bosOffset, _) <- liftBase $ layoutXYToIndex layout width''
                       (fromIntegral winh * lineHeight - 1)
  return (tos, point, tos + fromIntegral bosOffset + 1)

updateCache :: Editor -> ControlM ()
updateCache e = do
    let tabs = e ^. tabsA
    cacheRef <- asks tabCache
    cache <- liftBase $ readIORef cacheRef
    cache' <- syncTabs e (toList $ PL.withFocus tabs) cache
    liftBase $ writeIORef cacheRef cache'

syncTabs :: Editor -> [(Tab, Bool)] -> [TabInfo] -> ControlM [TabInfo]
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

syncTab :: Editor -> TabInfo -> Tab -> ControlM TabInfo
syncTab e tab ws =
  -- TODO Maybe do something here
  return tab

setTabFocus :: TabInfo -> ControlM ()
setTabFocus t =
  -- TODO this needs to set the tab focus with callback
  -- but only if the tab focus has changed
  return ()

askBuffer :: Yi.Window -> FBuffer -> BufferM a -> a
askBuffer w b f = fst $ runBuffer w b f

setWindowFocus :: Editor -> TabInfo -> View -> ControlM ()
setWindowFocus e t v = do
  let bufferName = shortIdentString (length $ commonNamePrefix e) $
                   findBufferWith (viewFBufRef v) e
      window = findWindowWith (windowRef v) e
      ml = askBuffer window (findBufferWith (viewFBufRef v) e) $
           getModeLine (T.pack <$> commonNamePrefix e)

-- TODO
--  update (textview w) widgetIsFocus True
--  update (modeline w) labelText ml
--  update (uiWindow ui) windowTitle $ bufferName <> " - Yi"
--  update (uiNotebook ui) (notebookChildTabLabel (page t))
--    (tabAbbrevTitle bufferName)
  return ()

removeTab :: TabInfo -> ControlM ()
removeTab t =
  -- TODO this needs to close the views in the tab with callback
  return ()

removeView :: TabInfo -> View -> ControlM ()
removeView tab view =
  -- TODO this needs to close the view with callback
  return ()

-- | Make a new tab.
newTab :: Editor -> Tab -> ControlM TabInfo
newTab e ws = do
    let t' = TabInfo { coreTab = ws }
--    cache <- syncWindows e t' (toList $ PL.withFocus ws) []
    return t' -- { views = cache }

{-# ANN insertTabBefore ("HLint: ignore Redundant do" :: String) #-}
insertTabBefore :: Editor -> Tab -> TabInfo -> ControlM TabInfo
insertTabBefore e ws c = do
    -- Just p <- notebookPageNum (uiNotebook ui) (page c)
    -- vb <- vBoxNew False 1
    -- notebookInsertPage (uiNotebook ui) vb "" p
    -- widgetShowAll $ vb
    newTab e ws

{-# ANN insertTab ("HLint: ignore Redundant do" :: String) #-}
insertTab :: Editor -> Tab -> ControlM TabInfo
insertTab e ws = do
    -- vb <- vBoxNew False 1
    -- notebookAppendPage (uiNotebook ui) vb ""
    -- widgetShowAll $ vb
    newTab e ws

{-
insertWindowBefore :: Editor -> TabInfo -> Yi.Window -> WinInfo -> IO WinInfo
insertWindowBefore e ui tab w _c = insertWindow e ui tab w

insertWindowAtEnd :: Editor -> UI -> TabInfo -> Window -> IO WinInfo
insertWindowAtEnd e ui tab w = insertWindow e ui tab w

insertWindow :: Editor -> UI -> TabInfo -> Window -> IO WinInfo
insertWindow e ui tab win = do
  let buf = findBufferWith (bufkey win) e
  liftBase $ do w <- newWindow e ui win buf

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

newBuffer :: BufferId -> R.YiString -> ControlM Buffer
newBuffer id text = do
    fBufRef <- liftYi . withEditor . newBufferE id $ text
    return Buffer{..}

newView :: Buffer -> FontDescription -> ControlM View
newView buffer font = do
    control  <- ask
    config   <- liftYi askCfg
    let viewFBufRef = fBufRef buffer
    newWindow <-
      fmap (\w -> w { height=50
                    , winRegion = mkRegion (Point 0) (Point 2000)
                    }) $ liftYi $ withEditor $ newWindowE False viewFBufRef
    let windowRef = wkey newWindow
    liftYi $ withEditor $ do
        windowsA %= PL.insertRight newWindow
        e <- get
        put $ focusAllSyntax e
    drawArea <- liftBase drawingAreaNew
    liftBase . widgetModifyBg drawArea StateNormal . mkCol False
      . Yi.Style.background . baseAttributes . configStyle $ configUI config
    context  <- liftBase $ widgetCreatePangoContext drawArea
    layout   <- liftBase $ layoutEmpty context
    liftBase $ layoutSetFontDescription layout (Just font)
    language <- liftBase $ contextGetLanguage context
    metrics  <- liftBase $ contextGetMetrics context font language
    liftBase $ layoutSetText layout ("" :: Text)

    scrollWin <- liftBase $ scrolledWindowNew Nothing Nothing
    liftBase $ do
        scrolledWindowAddWithViewport scrollWin drawArea
        scrolledWindowSetPolicy scrollWin PolicyAutomatic PolicyNever

    initialTos <-
      liftYi . withEditor . withGivenBufferAndWindow newWindow viewFBufRef $
        (use . markPointA) =<< fromMark <$> askMarks
    shownTos <- liftBase $ newIORef initialTos
    winMotionSignal <- liftBase $ newIORef Nothing

    let view = View {..}

    liftBase $ Gtk.widgetAddEvents drawArea [KeyPressMask]
    liftBase $ Gtk.set drawArea [Gtk.widgetCanFocus := True]

    liftBase $ drawArea `Gtk.onKeyPress` \event -> do
        putStrLn $ "Yi Control Key Press = " <> show event
        runControl (runAction $ makeAction $ do
            focusWindowE windowRef
            switchToBufferE viewFBufRef) control
        result <- processEvent (yiInput $ controlYi control) event
        widgetQueueDraw drawArea
        return result

    liftBase $ drawArea `Gtk.onButtonPress` \event -> do
        widgetGrabFocus drawArea
        runControl (handleClick view event) control

    liftBase $ drawArea `Gtk.onButtonRelease` \event ->
        runControl (handleClick view event) control

    liftBase $ drawArea `Gtk.onScroll` \event ->
        runControl (handleScroll view event) control

    liftBase $ drawArea `Gtk.onExpose` \event -> do
        (text, allAttrs, debug, tos, rel, point, inserting) <-
          runControl (liftYi $ withEditor $ do
            window <- findWindowWith windowRef <$> get
            (%=) buffersA (fmap (clearSyntax . clearHighlight))
            let winh = height window
            let tos = max 0 (regionStart (winRegion window))
            let bos = regionEnd (winRegion window)
            let rel p = fromIntegral (p - tos)

            withGivenBufferAndWindow window viewFBufRef $ do
                -- tos       <- getMarkPointB =<< fromMark <$> askMarks
                rope      <- streamB Forward tos
                point     <- pointB
                inserting <- use insertingA

                modeNm <- gets (withMode0 modeName)

    --            let (tos, point, text, picture) = do runBu
    --                        from     <- getMarkPointB =<< fromMark <$> askMarks
    --                        rope     <- streamB Forward from
    --                        p        <- pointB
                let content = fst $ splitAtLine winh rope
                -- allow BOS offset to be just after the last line
                let addNL = if R.countNewLines content == winh
                              then id
                              else (`R.snoc` '\n')
                    sty = configStyle $ configUI config
                          -- attributesPictureAndSelB sty (currentRegex e)
                          --   (mkRegion tos bos)
                          -- return (from, p, addNL $ Rope.toString content,
                          --         picture)
                let text = R.toText $ addNL content

                picture <- attributesPictureAndSelB sty Nothing
                           (mkRegion tos bos)

                -- add color attributes.
                let picZip = zip picture $ drop 1 (fst <$> picture) <> [bos]
                    strokes = [ (start',s,end') | ((start', s), end') <- picZip
                                                , s /= emptyAttributes ]

                    rel p = fromIntegral (p - tos)
                    allAttrs = concat $ do
                      (p1, Attributes fg bg _rv bd itlc udrl, p2) <- strokes
                      let atr x = x (rel p1) (rel p2)
                          if' p x y = if p then x else y
                      return [ atr AttrForeground $ mkCol True fg
                             , atr AttrBackground $ mkCol False bg
                             , atr AttrStyle $ if' itlc StyleItalic StyleNormal
                             , atr AttrUnderline $
                                 if' udrl UnderlineSingle UnderlineNone
                             , atr AttrWeight $ if' bd WeightBold WeightNormal
                             ]


                return (text, allAttrs, (picture, strokes, modeNm,
                                         window, tos, bos, winh),
                        tos, rel, point, inserting)) control

        -- putStrLn $ "Setting Layout Attributes " <> show debug
        layoutSetAttributes layout allAttrs
        -- putStrLn "Done Stting Layout Attributes"
        dw      <- widgetGetDrawWindow drawArea
        gc      <- gcNew dw
        oldText <- layoutGetText layout
        when (text /= oldText) $ layoutSetText layout text
        drawLayout dw gc 0 0 layout
        liftBase $ writeIORef shownTos tos

        -- paint the cursor
        (PangoRectangle curx cury curw curh, _) <-
          layoutGetCursorPos layout (rel point)
        PangoRectangle chx chy chw chh          <-
          layoutIndexToPos layout (rel point)

        gcSetValues gc
          (newGCValues { Gtk.foreground = mkCol True . Yi.Style.foreground
                                          . baseAttributes . configStyle $
                                          configUI config })
        if inserting
          then drawLine dw gc (round curx, round cury) (round $ curx + curw, round $ cury + curh)
          else drawRectangle dw gc False (round chx) (round chy) (if chw > 0 then round chw else 8) (round chh)

        return True

    liftBase $ widgetGrabFocus drawArea

    tabsRef <- asks tabCache
    ts <- liftBase $ readIORef tabsRef
    -- TODO: the Tab idkey should be assigned using
    -- Yi.Editor.newRef. But we can't modify that here, since our
    -- access to 'Yi' is readonly.
    liftBase $ writeIORef tabsRef (TabInfo (makeTab1 0 newWindow):ts)

    viewsRef <- asks views
    vs <- liftBase $ readIORef viewsRef
    liftBase $ writeIORef viewsRef $ Map.insert windowRef view vs

    return view
  where
    clearHighlight fb =
      -- if there were updates, then hide the selection.
      let h = view highlightSelectionA fb
          us = view pendingUpdatesA fb
      in highlightSelectionA .~ (h && null us) $ fb

{-# ANN setBufferMode ("HLint: ignore Redundant do" :: String) #-}
setBufferMode :: FilePath -> Buffer -> ControlM ()
setBufferMode f buffer = do
    let bufRef = fBufRef buffer
    -- adjust the mode
    tbl <- liftYi $ asks (modeTable . yiConfig)
    contents <- liftYi $ withGivenBuffer bufRef elemsB
    let header = R.toString $ R.take 1024 contents
        hmode = case header =~ ("\\-\\*\\- *([^ ]*) *\\-\\*\\-" :: String) of
            AllTextSubmatches [_,m] -> T.pack m
            _ -> ""
        Just mode = find (\(AnyMode m)-> modeName m == hmode) tbl <|>
                    find (\(AnyMode m)-> modeApplies m f contents) tbl <|>
                    Just (AnyMode emptyMode)
    case mode of
        AnyMode newMode -> do
            -- liftBase $ putStrLn $ show (f, modeName newMode)
            liftYi $ withEditor $ do
                withGivenBuffer bufRef $ do
                    setMode newMode
                    modify clearSyntax
                switchToBufferE bufRef
            -- withEditor focusAllSyntax

withBuffer :: Buffer -> BufferM a -> ControlM a
withBuffer Buffer{fBufRef = b} f = liftYi $ withGivenBuffer b f

getBuffer :: View -> Buffer
getBuffer view = Buffer {fBufRef = viewFBufRef view}

setText :: Buffer -> YiString -> ControlM ()
setText b text = withBuffer b $ do
    r <- regionOfB Document
    replaceRegionB r text

getText :: Buffer -> Iter -> Iter -> ControlM Text
getText b Iter{point = p1} Iter{point = p2} =
  fmap toText . withBuffer b . readRegionB $ mkRegion p1 p2

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

  logPutStrLn $ "Click: " <> showT (Gdk.Events.eventX event,
                                    Gdk.Events.eventY event,
                                    Gdk.Events.eventClick event)

  -- retrieve the clicked offset.
  (_,layoutIndex,_) <- io $ layoutXYToIndex (layout view)
                       (Gdk.Events.eventX event) (Gdk.Events.eventY event)
  tos <- liftBase $ readIORef (shownTos view)
  let p1 = tos + fromIntegral layoutIndex

  let winRef = windowRef view

  -- maybe focus the window
  -- logPutStrLn $ "Clicked inside window: " <> show view

--  let focusWindow = do
      -- TODO: check that tabIdx is the focus?
--      (%=) windowsA (fromJust . PL.move winIdx)

  liftBase $ case (Gdk.Events.eventClick event, Gdk.Events.eventButton event) of
     (Gdk.Events.SingleClick, Gdk.Events.LeftButton) -> do
        cid <- onMotionNotify (drawArea view) False $ \event ->
            runControl (handleMove view p1 event) control
        writeIORef (winMotionSignal view) $ Just cid

     _ -> do
       maybe (return ()) signalDisconnect =<< readIORef (winMotionSignal view)
       writeIORef (winMotionSignal view) Nothing

  case (Gdk.Events.eventClick event, Gdk.Events.eventButton event) of
    (Gdk.Events.SingleClick, Gdk.Events.LeftButton) ->
      runAction . EditorA $ do
        -- b <- gets $ (bkey . findBufferWith (viewFBufRef view))
        -- focusWindow
        window <- findWindowWith winRef <$> get
        withGivenBufferAndWindow window (viewFBufRef view) $ do
            moveTo p1
            setVisibleSelection False
    -- (Gdk.Events.SingleClick, _) -> runAction focusWindow
    (Gdk.Events.ReleaseClick, Gdk.Events.MiddleButton) -> do
        disp <- liftBase $ widgetGetDisplay (drawArea view)
        cb <- liftBase $ clipboardGetForDisplay disp selectionPrimary
        let cbHandler :: Maybe R.YiString -> IO ()
            cbHandler Nothing = return ()
            cbHandler (Just txt) = runControl (runAction . EditorA $ do
                window <- findWindowWith winRef <$> get
                withGivenBufferAndWindow window (viewFBufRef view) $ do
                    pointB >>= setSelectionMarkPointB
                    moveTo p1
                    insertN txt) control
        liftBase $ clipboardRequestText cb (cbHandler . fmap R.fromText)
    _ -> return ()

  liftBase $ widgetQueueDraw (drawArea view)
  return True

handleScroll :: View -> Gdk.Events.Event -> ControlM Bool
handleScroll view event = do
  let editorAction =
        withCurrentBuffer $ vimScrollB $ case Gdk.Events.eventDirection event of
                        Gdk.Events.ScrollUp   -> -1
                        Gdk.Events.ScrollDown -> 1
                        _ -> 0 -- Left/right scrolling not supported

  runAction $ EditorA editorAction
  liftBase $ widgetQueueDraw (drawArea view)
  return True

handleMove :: View -> Point -> Gdk.Events.Event -> ControlM Bool
handleMove view p0 event = do
  logPutStrLn $ "Motion: " <> showT (Gdk.Events.eventX event,
                                     Gdk.Events.eventY event)

  -- retrieve the clicked offset.
  (_,layoutIndex,_) <-
    liftBase $ layoutXYToIndex (layout view)
    (Gdk.Events.eventX event) (Gdk.Events.eventY event)
  tos <- liftBase $ readIORef (shownTos view)
  let p1 = tos + fromIntegral layoutIndex


  let editorAction = do
        txt <- withCurrentBuffer $
           if p0 /= p1
            then Just <$> do
              m <- selMark <$> askMarks
              markPointA m .= p0
              moveTo p1
              setVisibleSelection True
              readRegionB =<< getSelectRegionB
            else return Nothing
        maybe (return ()) setRegE txt

  runAction $ makeAction editorAction
  -- drawWindowGetPointer (textview w) -- be ready for next message.

  -- Relies on uiActionCh being synchronous
  selection <- liftBase $ newIORef ""
  let yiAction = do
      txt <- withCurrentBuffer (readRegionB =<< getSelectRegionB)
             :: YiM R.YiString
      liftBase $ writeIORef selection txt
  runAction $ makeAction yiAction
  txt <- liftBase $ readIORef selection

  disp <- liftBase $ widgetGetDisplay (drawArea view)
  cb <- liftBase $ clipboardGetForDisplay disp selectionPrimary
  liftBase $ clipboardSetWithData cb [(targetString,0)]
      (\0 -> void (selectionDataSetText $ R.toText txt)) (return ())

  liftBase $ widgetQueueDraw (drawArea view)
  return True

processEvent :: ([Event] -> IO ()) -> Gdk.Events.Event -> IO Bool
processEvent ch ev = do
  -- logPutStrLn $ "Gtk.Event: " <> show ev
  -- logPutStrLn $ "Event: " <> show (gtkToYiEvent ev)
  case gtkToYiEvent ev of
    Nothing -> logPutStrLn $ "Event not translatable: " <> showT ev
    Just e -> ch [e]
  return True

gtkToYiEvent :: Gdk.Events.Event -> Maybe Event
gtkToYiEvent (Gdk.Events.Key {Gdk.Events.eventKeyName = key
                             , Gdk.Events.eventModifier = evModifier
                             , Gdk.Events.eventKeyChar = char})
    = (\k -> Event k $ nub $ notMShift $ concatMap modif evModifier) <$> key'
      where (key',isShift) =
                case char of
                  Just c  -> (Just $ KASCII c, True)
                  Nothing -> (Map.lookup key keyTable, False)
            modif Gdk.Events.Control = [MCtrl]
            modif Gdk.Events.Alt     = [MMeta]
            modif Gdk.Events.Shift   = [MShift]
            modif _ = []
            notMShift | isShift   = filter (/= MShift)
                      | otherwise = id
gtkToYiEvent _ = Nothing

-- | Map GTK long names to Keys
keyTable :: Map.Map Text Key
keyTable = Map.fromList
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
