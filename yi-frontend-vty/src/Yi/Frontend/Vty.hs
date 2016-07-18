{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Yi.Frontend.Vty
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines a user interface implemented using vty.
--
-- Originally derived from: riot/UI.hs Copyright (c) Tuomo Valkonen 2004.

module Yi.Frontend.Vty
    ( start
    ) where

import           Prelude                        hiding (concatMap, error,
                                                 reverse)

import           Control.Concurrent             (MVar, forkIO, myThreadId, newEmptyMVar,
                                                 takeMVar, tryPutMVar, tryTakeMVar)
import           Control.Concurrent.STM         (atomically, isEmptyTChan, readTChan)
import           Control.Exception              (IOException, handle)
import           Lens.Micro.Platform            (makeLenses, view, use)
import           Control.Monad                  (void, when)
import           Data.Char                      (chr, ord)
import           Data.Default                   (Default)
import qualified Data.DList                     as D (empty, snoc, toList)
import           Data.Foldable                  (concatMap, toList)
import           Data.IORef                     (IORef, newIORef, readIORef, writeIORef)
import qualified Data.List.PointedList.Circular as PL (PointedList (_focus), withFocus)
import qualified Data.Map.Strict                as M ((!))
import           Data.Maybe                     (maybeToList)
import           Data.Monoid                    (Endo (appEndo), (<>))
import qualified Data.Text                      as T (Text, cons, empty,
                                                      justifyLeft, length, pack,
                                                      singleton, snoc, take,
                                                      unpack)
import           Data.Typeable                  (Typeable)
import           GHC.Conc                       (labelThread)
import qualified Graphics.Vty                   as Vty (Attr, Cursor (Cursor, NoCursor),
                                                        Config,
                                                        Event (EvResize), Image,
                                                        Input (_eventChannel),
                                                        Output (displayBounds),
                                                        Picture (picCursor), Vty (inputIface, outputIface, refresh, shutdown, update),
                                                        bold, char, charFill,
                                                        defAttr, emptyImage,
                                                        horizCat, mkVty,
                                                        picForLayers,
                                                        standardIOConfig,
                                                        reverseVideo, text',
                                                        translate, underline,
                                                        vertCat, withBackColor,
                                                        withForeColor,
                                                        withStyle, (<|>))
import           Yi.Buffer
import           Yi.Config
import           Yi.Debug                       (logError, logPutStrLn)
import           Yi.Editor
import           Yi.Event                       (Event)
import           Yi.Style
import           Yi.Types                       (YiConfigVariable)
import qualified Yi.UI.Common                   as Common
import qualified Yi.UI.SimpleLayout             as SL
import           Yi.Layout                      (HasNeighborWest)
import           Yi.UI.TabBar                   (TabDescr (TabDescr), tabBarDescr)
import           Yi.UI.Utils                    (arrangeItems, attributesPictureAndSelB)
import           Yi.Frontend.Vty.Conversions          (colorToAttr, fromVtyEvent)
import           Yi.Window                      (Window (bufkey, isMini, wkey))


data Rendered = Rendered
    { picture :: !Vty.Image
    , cursor  :: !(Maybe (Int,Int))
    }

data FrontendState = FrontendState
    { fsVty :: Vty.Vty
    , fsConfig :: Config
    , fsEndMain :: MVar ()
    , fsEndInputLoop :: MVar ()
    , fsEndRenderLoop :: MVar ()
    , fsDirty :: MVar ()
    , fsEditorRef :: IORef Editor
    }

-- | Base vty configuration, named so to distinguish it from any vty
-- frontend configuration.
--
-- If this is set to its default (None) it will be replaced by the default
-- vty configuration from standardIOConfig. However, standardIOConfig
-- runs in the IO monad so we cannot set the real default here.
newtype BaseVtyConfig = BaseVtyConfig { _baseVtyConfig :: Maybe Vty.Config }
    deriving (Typeable, Default)

instance YiConfigVariable BaseVtyConfig

makeLenses ''BaseVtyConfig

start :: UIBoot
start config submitEvents submitActions editor = do
    let baseConfig = view (configVariable . baseVtyConfig) config
    vty <- Vty.mkVty =<< case baseConfig of
        Nothing -> Vty.standardIOConfig
        Just conf -> return conf
    let inputChan = Vty._eventChannel (Vty.inputIface vty)
    endInput <- newEmptyMVar
    endMain <- newEmptyMVar
    endRender <- newEmptyMVar
    dirty <- newEmptyMVar
    editorRef <- newIORef editor
    let -- | Action to read characters into a channel
        inputLoop :: IO ()
        inputLoop = tryTakeMVar endInput >>=
                    maybe (do
                            let go evs = do
                                    e <- getEvent
                                    done <- atomically (isEmptyTChan inputChan)
                                    if done
                                    then submitEvents (D.toList (evs `D.snoc` e))
                                    else go (evs `D.snoc` e)
                            go D.empty
                            inputLoop)
                          (const $ return ())

        -- | Read a key. UIs need to define a method for getting events.
        getEvent :: IO Yi.Event.Event
        getEvent = do
          event <- atomically (readTChan inputChan)
          case event of
            (Vty.EvResize _ _) -> do
                submitActions []
                getEvent
            _ -> return (fromVtyEvent event)

        renderLoop :: IO ()
        renderLoop = do
          takeMVar dirty
          tryTakeMVar endRender >>=
            maybe (handle (\(except :: IOException) -> do
                              logPutStrLn "refresh crashed with IO Error"
                              logError (T.pack (show except)))
                          (readIORef editorRef >>= refresh fs >> renderLoop))
                  (const $ return ())

        fs = FrontendState vty config endMain endInput endRender dirty editorRef

    inputThreadId <- forkIO inputLoop
    labelThread inputThreadId "VtyInput"
    renderThreadId <- forkIO renderLoop
    labelThread renderThreadId "VtyRender"

    return $! Common.dummyUI
        { Common.main = main fs
        , Common.end = end fs
        , Common.refresh = requestRefresh fs
        , Common.userForceRefresh = Vty.refresh vty
        , Common.layout = layout fs
        }

main :: FrontendState -> IO ()
main fs = do
    tid <- myThreadId
    labelThread tid "VtyMain"
    takeMVar (fsEndMain fs)

layout :: FrontendState -> Editor -> IO Editor
layout fs e = do
    (colCount, rowCount) <- Vty.displayBounds (Vty.outputIface (fsVty fs))
    let (e', _layout) = SL.layout colCount rowCount e
    return e'

end :: FrontendState -> Bool -> IO ()
end fs mustQuit = do
    -- setTerminalAttributes stdInput (oAttrs ui) Immediately
    void $ tryPutMVar (fsEndInputLoop fs) ()
    void $ tryPutMVar (fsEndRenderLoop fs) ()
    Vty.shutdown (fsVty fs)
    when mustQuit $
        void (tryPutMVar (fsEndMain fs) ())

requestRefresh :: FrontendState -> Editor -> IO ()
requestRefresh fs e = do
    writeIORef (fsEditorRef fs) e
    void $ tryPutMVar (fsDirty fs) ()

refresh :: FrontendState -> Editor -> IO ()
refresh fs e = do
    (colCount, rowCount) <- Vty.displayBounds (Vty.outputIface (fsVty fs))
    let (_e, SL.Layout tabbarRect winRects promptRect) = SL.layout colCount rowCount e
        ws = windows e
        (cmd, cmdSty) = statusLineInfo e
        niceCmd = arrangeItems cmd (SL.sizeX promptRect) (maxStatusHeight e)
        mkLine = T.justifyLeft colCount ' ' . T.take colCount
        formatCmdLine text = withAttributes statusBarStyle (mkLine text)
        winImage (win, hasFocus) =
            let (rect, nb) = winRects M.! wkey win
            in renderWindow (configUI $ fsConfig fs) e rect nb (win, hasFocus)
        windowsAndImages =
            fmap (\(w, f) -> (w, winImage (w, f))) (PL.withFocus ws)
        bigImages =
            map (picture . snd)
                (filter (not . isMini . fst) (toList windowsAndImages))
        miniImages =
            map (picture . snd)
                (filter (isMini . fst) (toList windowsAndImages))
        statusBarStyle =
            ((appEndo <$> cmdSty) <*> baseAttributes)
                (configStyle (configUI (fsConfig fs)))
        tabBarImage =
            renderTabBar tabbarRect (configStyle (configUI (fsConfig fs)))
                (map (\(TabDescr t f) -> (t, f)) (toList (tabBarDescr e)))
        cmdImage = if null cmd
                   then Vty.emptyImage
                   else Vty.translate
                           (SL.offsetX promptRect)
                           (SL.offsetY promptRect)
                           (Vty.vertCat (fmap formatCmdLine niceCmd))
        cursorPos =
            let (w, image) = PL._focus windowsAndImages
            in case (isMini w, cursor image) of
                  (False, Just (y, x)) ->
                      Vty.Cursor (toEnum x) (toEnum y)
                  (True, Just (_, x)) -> Vty.Cursor (toEnum x) (toEnum (rowCount - 1))
                  (_, Nothing) -> Vty.NoCursor
    logPutStrLn "refreshing screen."
    Vty.update (fsVty fs)
        (Vty.picForLayers ([tabBarImage, cmdImage] ++ bigImages ++ miniImages))
        { Vty.picCursor = cursorPos }

renderWindow :: UIConfig -> Editor -> SL.Rect -> HasNeighborWest -> (Window, Bool) -> Rendered
renderWindow cfg e (SL.Rect x y w h) nb (win, focused) =
    Rendered (Vty.translate x y $ if nb then vertSep Vty.<|> pict else pict)
             (fmap (\(i, j) -> (i + y, j + x')) cur)
    where
        x' = x + if nb then 1 else 0
        w' = w - if nb then 1 else 0
        b = findBufferWith (bufkey win) e
        sty = configStyle cfg

        notMini = not (isMini win)
        -- off reserves space for the mode line. The mini window does not have a mode line.
        off = if notMini then 1 else 0
        h' = h - off
        ground = baseAttributes sty
        wsty = attributesToAttr ground Vty.defAttr
        eofsty = appEndo (eofStyle sty) ground
        (point, _) = runBuffer win b pointB
        region = mkSizeRegion fromMarkPoint $ Size (w' * h')
        -- Work around a problem with the mini window never displaying it's contents due to a
        -- fromMark that is always equal to the end of the buffer contents.
        (Just (MarkSet fromM _ _), _) = runBuffer win b (getMarks win)
        fromMarkPoint = if notMini
                        then fst $ runBuffer win b $ use $ markPointA fromM
                        else Point 0
        (text, _) = runBuffer win b (indexedStreamB Forward fromMarkPoint)

        (attributes, _) = runBuffer win b $ attributesPictureAndSelB sty (currentRegex e) region
        -- TODO: I suspect that this costs quite a lot of CPU in the "dry run" which determines the window size;
        -- In that case, since attributes are also useless there, it might help to replace the call by a dummy value.
        -- This is also approximately valid of the call to "indexedAnnotatedStreamB".
        colors = map (fmap (($ Vty.defAttr) . attributesToAttr)) attributes
        bufData =  paintChars Vty.defAttr colors text
        tabWidth = tabSize . fst $ runBuffer win b indentSettingsB
        prompt = if isMini win then miniIdentString b else ""

        cur = (fmap (\(SL.Point2D curx cury) -> (cury, T.length prompt + curx)) . fst)
              (runBuffer win b
                         (SL.coordsOfCharacterB
                             (SL.Size2D w' h)
                             fromMarkPoint
                             point))

        rendered =
            drawText wsty h' w'
                     tabWidth
                     ([(c, wsty) | c <- T.unpack prompt] ++ bufData ++ [(' ', wsty)])
                     -- we always add one character which can be used to position the cursor at the end of file
        commonPref = T.pack <$> commonNamePrefix e
        (modeLine0, _) = runBuffer win b $ getModeLine commonPref
        modeLine = if notMini then Just modeLine0 else Nothing
        prepare = withAttributes modeStyle . T.justifyLeft w' ' ' . T.take w'
        modeLines = map prepare $ maybeToList modeLine
        modeStyle = (if focused then appEndo (modelineFocusStyle sty) else id) (modelineAttributes sty)

        filler :: T.Text
        filler = if w' == 0 -- justify would return a single char at w = 0
                 then T.empty
                 else T.justifyLeft w' ' ' $ T.singleton (configWindowFill cfg)

        pict = Vty.vertCat $ take h' (rendered <> repeat (withAttributes eofsty filler)) <> modeLines
        
        sepStyle = attributesToAttr (modelineAttributes sty) Vty.defAttr
        vertSep = Vty.charFill sepStyle ' ' 1 h

withAttributes :: Attributes -> T.Text -> Vty.Image
withAttributes sty = Vty.text' (attributesToAttr sty Vty.defAttr)

attributesToAttr :: Attributes -> Vty.Attr -> Vty.Attr
attributesToAttr (Attributes fg bg reverse bd _itlc underline') =
    (if reverse then (`Vty.withStyle` Vty.reverseVideo) else id) .
    (if bd then (`Vty.withStyle` Vty.bold) else id) .
    (if underline' then (`Vty.withStyle` Vty.underline) else id) .
    colorToAttr (flip Vty.withForeColor) fg .
    colorToAttr (flip Vty.withBackColor) bg

-- | Apply the attributes in @sty@ and @changes@ to @cs@.  If the
-- attributes are not used, @sty@ and @changes@ are not evaluated.
paintChars :: a -> [(Point, a)] -> [(Point, Char)] -> [(Char, a)]
paintChars sty changes cs = zip (fmap snd cs) attrs
    where attrs = stys sty changes cs

stys :: a -> [(Point, a)] -> [(Point, Char)] -> [a]
stys sty [] cs = [ sty | _ <- cs ]
stys sty ((endPos, sty') : xs) cs = [ sty | _ <- previous ] <> stys sty' xs later
    where (previous, later) = break ((endPos <=) . fst) cs

drawText :: Vty.Attr -- ^ "Ground" attribute.
         -> Int    -- ^ The height of the part of the window we are in
         -> Int    -- ^ The width of the part of the window we are in
         -> Int    -- ^ The number of spaces to represent a tab character with.
         -> [(Char, Vty.Attr)]  -- ^ The data to draw.
         -> [Vty.Image]
drawText wsty h w tabWidth bufData
    | h == 0 || w == 0 = []
    | otherwise        = renderedLines
    where

    -- the number of lines that taking wrapping into account,
    -- we use this to calculate the number of lines displayed.
    wrapped = concatMap (wrapLine w . addSpace . concatMap expandGraphic) $ take h $ lines' bufData
    lns0 = take h wrapped

    -- fill lines with blanks, so the selection looks ok.
    renderedLines = map fillColorLine lns0
    colorChar (c, a) = Vty.char a c

    fillColorLine :: [(Char, Vty.Attr)] -> Vty.Image
    fillColorLine [] = Vty.charFill Vty.defAttr ' ' w 1
    fillColorLine l = Vty.horizCat (map colorChar l)
                      Vty.<|>
                      Vty.charFill a ' ' (w - length l) 1
                      where (_, a) = last l


    addSpace :: [(Char, Vty.Attr)] -> [(Char, Vty.Attr)]
    addSpace [] = [(' ', wsty)]
    addSpace l = case mod lineLength w of
                    0 -> l
                    _ -> l ++ [(' ', wsty)]
                 where
                    lineLength = length l

    -- | Cut a string in lines separated by a '\n' char. Note
    -- that we remove the newline entirely since it is no longer
    -- significant for drawing text.

    lines' :: [(Char, a)] -> [[(Char, a)]]
    lines' [] =  []
    lines' s  = case s' of
                  []          -> [l]
                  ((_,_):s'') -> l : lines' s''
                where
                (l, s') = break ((== '\n') . fst) s

    wrapLine :: Int -> [x] -> [[x]]
    wrapLine _ [] = []
    wrapLine n l = let (x,rest) = splitAt n l in x : wrapLine n rest

    expandGraphic ('\t', p) = replicate tabWidth (' ', p)
    expandGraphic (c, p)
        | numeric < 32 = [('^', p), (chr (numeric + 64), p)]
        | otherwise = [(c, p)]
        where numeric = ord c

renderTabBar :: SL.Rect -> UIStyle -> [(T.Text, Bool)] -> Vty.Image
renderTabBar r uiStyle ts = (Vty.<|> padding) . Vty.horizCat $ fmap render ts
  where
    render (text, inFocus) = Vty.text' (tabAttr inFocus) (tabTitle text)
    tabTitle text   = ' ' `T.cons` text `T.snoc` ' '
    tabAttr b       = baseAttr b $ tabBarAttributes uiStyle
    baseAttr True sty =
        attributesToAttr (appEndo (tabInFocusStyle uiStyle) sty) Vty.defAttr
    baseAttr False sty =
        attributesToAttr (appEndo (tabNotFocusedStyle uiStyle) sty) Vty.defAttr
            `Vty.withStyle` Vty.underline
    padding = Vty.charFill (tabAttr False) ' ' (SL.sizeX r - width) 1
    width = sum . map ((+2) . T.length . fst) $ ts
