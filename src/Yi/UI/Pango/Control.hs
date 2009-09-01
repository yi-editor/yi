{-# LANGUAGE RecordWildCards, ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
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
,   newControl
,   runControl
,   newBuffer
,   newView
,   getBuffer
,   setBufferMode
) where

import Prelude (map)

import Data.List (drop, zip, take)
import qualified Data.Rope as Rope
import Yi
import Yi.Window
import Yi.Editor
import Yi.Monad
import Yi.Style
import Yi.UI.Utils
import Graphics.UI.Gtk as Gtk hiding(Point, Region)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer (MonadIO(..))
import Control.Concurrent (newMVar, modifyMVar, MVar(..))
import Data.Typeable
import qualified Data.List.PointedList as  PL (insertRight)
import Yi.Regex
import System.FilePath

data Control = Control
    { config :: Config
    , editor :: MVar Editor
    }

newtype ControlM a = ControlM { runControl' :: ReaderT Control IO a }
    deriving (Monad, MonadReader Control, MonadIO, Typeable, Functor)

instance MonadState Editor ControlM where
    get = readRef =<< editor <$> ask
    put v = flip modifyRef (const v) =<< editor <$> ask

instance MonadEditor ControlM where
    askCfg = config <$> ask
    withEditor f = do
      r <- asks editor
      cfg <- asks config
      liftIO $ controlUnsafeWithEditor cfg r f

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
    , scrollWin   :: ScrolledWindow
    }

data Iter = Iter
    { iterfBufRef :: BufferRef
    , point       :: Point
    }

newControl :: Config -> IO Control
newControl config = do
    editor <- newMVar emptyEditor
    return Control {..}

runControl :: ControlM a -> Control -> IO a
runControl f s = runReaderT (runControl' f) s

newBuffer :: BufferId -> Rope -> ControlM Buffer
newBuffer id r = do
    fBufRef <- liftEditor $ stringToNewBuffer id r
    return Buffer{..}

newView :: Buffer -> ControlM View
newView buffer = do
    control  <- ask
    config   <- askCfg
    let viewFBufRef = fBufRef buffer
    window   <- fmap (\w -> w{height=50, winRegion = mkRegion (Point 0) (Point 2000)}) $ liftEditor $ newWindowE False viewFBufRef
    let windowRef = wkey window
    liftEditor $ modA windowsA (PL.insertRight window)
    drawArea <- liftIO $ drawingAreaNew
    context  <- liftIO $ widgetCreatePangoContext drawArea
    layout   <- liftIO $ layoutEmpty context
    liftIO $ layoutSetText layout "Test"

    liftIO $ drawArea `Gtk.onExpose` \event -> do
        (text, allAttrs, debug) <- runControl (liftEditor $ do
            modA buffersA (fmap (clearSyntax . clearHighlight))
            let winh = height window
            let tos = max 0 (regionStart (winRegion window))
            let bos = regionEnd (winRegion window)

            withGivenBufferAndWindow0 window viewFBufRef $ do
                -- tos      <- getMarkPointB =<< fromMark <$> askMarks
                rope     <- streamB Forward tos
                point    <- pointB
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
                    rel p = fromIntegral (p - tos)
                    allAttrs = concat $ do
                        (p1, Attributes fg bg _rv bd itlc udrl, p2) <- strokes
                        return $ [ AttrForeground (rel p1) (rel p2) (mkCol True fg)
                                 , AttrBackground (rel p1) (rel p2) (mkCol False bg)
                                 , AttrStyle      (rel p1) (rel p2) (if itlc then StyleItalic     else StyleNormal)
                                 , AttrUnderline  (rel p1) (rel p2) (if udrl then UnderlineSingle else UnderlineNone)
                                 , AttrWeight     (rel p1) (rel p2) (if bd   then WeightBold      else WeightNormal)
                                 ]
                return (text, allAttrs, (picture, strokes))) control

        putStrLn $ "Setting Layout Attributes " ++ show debug
        layoutSetAttributes layout allAttrs
        putStrLn "Done Stting Layout Attributes"
        dw      <- widgetGetDrawWindow drawArea
        gc      <- gcNew dw
        oldText <- layoutGetText layout
        when (text /= oldText) $ layoutSetText layout text
        drawLayout dw gc 0 0 layout
        return True

    scrollWin <- liftIO $ scrolledWindowNew Nothing Nothing
    liftIO $ scrolledWindowAddWithViewport scrollWin drawArea
    return View {..}
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
    tbl <- asks (modeTable . config)
    contents <- liftEditor $ withGivenBuffer0 bufRef $ elemsB
    let header = take 1024 contents
        hmode = case header =~ "\\-\\*\\- *([^ ]*) *\\-\\*\\-" of
            AllTextSubmatches [_,m] -> m
            _ -> ""
        Just mode = (find (\(AnyMode m)-> modeName m == hmode) tbl) <|>
                    (find (\(AnyMode m)-> modeApplies m f contents) tbl) <|>
                    Just (AnyMode emptyMode)
    case mode of
        AnyMode newMode -> do
            liftIO $ putStrLn $ show (f, header, modeName newMode)
            liftEditor $ withGivenBuffer0 bufRef $ setMode newMode

getBuffer view = Buffer {fBufRef = viewFBufRef view}

mkCol :: Bool -- ^ is foreground?
      -> Yi.Style.Color -> Gtk.Color
mkCol True  Default = Color 0 0 0
mkCol False Default = Color maxBound maxBound maxBound
mkCol _ (RGB x y z) = Color (fromIntegral x * 256)
                            (fromIntegral y * 256)
                            (fromIntegral z * 256)

