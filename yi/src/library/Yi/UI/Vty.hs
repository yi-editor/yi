{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Yi.UI.Vty
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines a user interface implemented using vty.
--
-- Originally derived from: riot/UI.hs Copyright (c) Tuomo Valkonen 2004.

module Yi.UI.Vty
    ( start
    ) where

import Prelude hiding (error, foldr1, concatMap, reverse)
import Control.Applicative hiding ((<|>))
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Char
import qualified Data.DList as D
import Data.Foldable (toList, foldr1, concatMap)
import Data.IORef
import qualified Data.List.PointedList.Circular as PL
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import GHC.Conc (labelThread)

import Yi.Buffer
import Yi.Config
import Yi.Debug
import Yi.Editor
import Yi.Event
import Yi.Style
import qualified Yi.UI.Common as Common
import qualified Yi.UI.SimpleLayout as SL
import Yi.UI.Vty.Conversions
import Yi.UI.TabBar
import Yi.UI.Utils
import Yi.Window

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

start :: UIBoot
start config submitEvents submitActions editor = do
    vty <- (Vty.mkVty . configVty . configUI) config
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
                                done <- isEmptyChan inputChan
                                if done
                                then submitEvents (D.toList (evs `D.snoc` e))
                                else go (evs `D.snoc` e)
                            go D.empty
                            inputLoop)
                          (const $ return ())

        -- | Read a key. UIs need to define a method for getting events.
        getEvent :: IO Yi.Event.Event
        getEvent = do
          event <- readChan inputChan
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
            let rect = winRects M.! wkey win
            in renderWindow (configUI $ fsConfig fs) e rect (win, hasFocus)
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
        tabBarImages = renderTabBar e fs (SL.sizeX tabbarRect)
        cmdImage = if null cmd
                   then Vty.emptyImage
                   else Vty.translate
                           (SL.offsetX promptRect)
                           (SL.offsetY promptRect)
                           (Vty.vertCat (fmap formatCmdLine niceCmd))
        tabBarImage = Vty.vertCat tabBarImages
        cursorPos =
            case (\(w, r) -> (isMini w, cursor r)) (PL._focus windowsAndImages) of
                (False, Just (y, x)) -> Vty.Cursor (toEnum x) (toEnum y)
                (True, Just (_, x)) -> Vty.Cursor (toEnum x) (toEnum (rowCount - 1))
                (_, Nothing) -> Vty.NoCursor
    logPutStrLn "refreshing screen."
    Vty.update (fsVty fs)
        (Vty.picForLayers ([tabBarImage, cmdImage] ++ bigImages ++ miniImages))
        { Vty.picCursor = cursorPos }

renderWindow :: UIConfig -> Editor -> SL.Rect -> (Window, Bool) -> Rendered
renderWindow cfg e (SL.Rect x y w h) (win, focused) =
    Rendered (Vty.translate x y pict)
             (fmap (\(i, j) -> (i + y, j + x)) cur)
    where
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
        (eofPoint, _) = runBuffer win b sizeB
        region = mkSizeRegion fromMarkPoint (Size (w*h'))
        -- Work around a problem with the mini window never displaying it's contents due to a
        -- fromMark that is always equal to the end of the buffer contents.
        (Just (MarkSet fromM _ _), _) = runBuffer win b (getMarks win)
        fromMarkPoint = if notMini
                            then fst $ runBuffer win b $ use $ markPointA fromM
                            else Point 0
        (text, _) = runBuffer win b (indexedAnnotatedStreamB fromMarkPoint) -- read chars from the buffer, lazily

        (attributes, _) = runBuffer win b $ attributesPictureAndSelB sty (currentRegex e) region
        -- TODO: I suspect that this costs quite a lot of CPU in the "dry run" which determines the window size;
        -- In that case, since attributes are also useless there, it might help to replace the call by a dummy value.
        -- This is also approximately valid of the call to "indexedAnnotatedStreamB".
        colors = map (fmap (($ Vty.defAttr) . attributesToAttr)) attributes
        bufData = -- trace (unlines (map show text) ++ unlines (map show $ concat strokes)) $
                  paintChars Vty.defAttr colors text
        tabWidth = tabSize . fst $ runBuffer win b indentSettingsB
        prompt = if isMini win then miniIdentString b else ""

        (rendered, cur) =
            drawText h' w
                     point
                     tabWidth
                     ([(c,(wsty, -1)) | c <- T.unpack prompt] ++ bufData ++ [(' ',(wsty, eofPoint))])
                     -- we always add one character which can be used to position the cursor at the end of file
        commonPref = T.pack <$> commonNamePrefix e
        (modeLine0, _) = runBuffer win b $ getModeLine commonPref
        modeLine = if notMini then Just modeLine0 else Nothing
        prepare = withAttributes modeStyle . T.justifyLeft w ' ' . T.take w
        modeLines = map prepare $ maybeToList modeLine
        modeStyle = (if focused then appEndo (modelineFocusStyle sty) else id) (modelineAttributes sty)

        filler :: T.Text
        filler = if w == 0 -- justify would return a single char at w = 0
                 then T.empty
                 else T.justifyLeft w ' ' $ T.singleton (configWindowFill cfg)

        pict = Vty.vertCat (take h' (rendered <> repeat (withAttributes eofsty filler)) <> modeLines)

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
paintChars :: a -> [(Point,a)] -> [(Point,Char)] -> [(Char, (a,Point))]
paintChars sty changes cs = [(c,(s,p)) | ((p,c),s) <- zip cs attrs]
    where attrs = stys sty changes cs

stys :: a -> [(Point,a)] -> [(Point,Char)] -> [a]
stys sty [] cs = [ sty | _ <- cs ]
stys sty ((endPos,sty'):xs) cs = [ sty | _ <- previous ] <> stys sty' xs later
    where (previous, later) = break ((endPos <=) . fst) cs

drawText :: Int    -- ^ The height of the part of the window we are in
         -> Int    -- ^ The width of the part of the window we are in
         -> Point  -- ^ The position of the cursor
         -> Int    -- ^ The number of spaces to represent a tab character with.
         -> [(Char, (Vty.Attr, Point))]  -- ^ The data to draw.
         -> ([Vty.Image], Maybe (Int, Int))
drawText h w point tabWidth bufData
    | h == 0 || w == 0 = ([], Nothing)
    | otherwise        = (renderedLines, pntpos)
    where

    -- the number of lines that taking wrapping into account,
    -- we use this to calculate the number of lines displayed.
    wrapped = concatMap (wrapLine w) $ map (concatMap expandGraphic) $ take h $ lines' bufData
    lns0 = take h wrapped

    pntpos = listToMaybe
        [ (y, x)
        | (y, l) <- zip [0..] lns0
        , (x, (_char, (_attr, p))) <- zip [0..] l
        , p == point]

    -- fill lines with blanks, so the selection looks ok.
    renderedLines = map fillColorLine lns0
    colorChar (c, (a, _aPoint)) = Vty.char a c

    fillColorLine :: [(Char, (Vty.Attr, Point))] -> Vty.Image
    fillColorLine [] = Vty.charFill Vty.defAttr ' ' w 1
    fillColorLine l = Vty.horizCat (map colorChar l)
                      Vty.<|>
                      Vty.charFill a ' ' (w - length l) 1
                      where (_,(a,_x)) = last l

    -- | Cut a string in lines separated by a '\n' char. Note
    -- that we add a blank character where the \n was, so the
    -- cursor can be positioned there.

    lines' :: [(Char,a)] -> [[(Char,a)]]
    lines' [] =  []
    lines' s  = case s' of
                  []          -> [l]
                  ((_,x):s'') -> (l++[(' ',x)]) : lines' s''
                where
                (l, s') = break ((== '\n') . fst) s

    wrapLine :: Int -> [x] -> [[x]]
    wrapLine _ [] = []
    wrapLine n l = let (x,rest) = splitAt n l in x : wrapLine n rest

    expandGraphic ('\t', p) = replicate tabWidth (' ', p)
    expandGraphic (c,p)
        | ord c < 32 = [('^',p),(chr (ord c + 64),p)]
        | otherwise = [(c,p)]

-- | Construct images for the tabbar if at least one tab exists.
renderTabBar :: Editor -> FrontendState -> Int -> [Vty.Image]
renderTabBar e fs xss = [tabImages Vty.<|> extraImage]
  where tabImages       = foldr1 (Vty.<|>) $ fmap tabToVtyImage $ tabBarDescr e
        imagePad = T.replicate (xss - fromEnum totalTabWidth) $ T.singleton ' '
        extraImage      = withAttributes (tabBarAttributes uiStyle) imagePad
        totalTabWidth   = Vty.imageWidth tabImages
        uiStyle         = configStyle $ configUI $ fsConfig fs
        tabTitle text   = ' ' `T.cons` text `T.snoc` ' '
        tabAttr b       = baseAttr b $ tabBarAttributes uiStyle
        baseAttr True  sty = attributesToAttr (appEndo (tabInFocusStyle uiStyle) sty) Vty.defAttr
        baseAttr False sty = attributesToAttr (appEndo (tabNotFocusedStyle uiStyle) sty) Vty.defAttr `Vty.withStyle` Vty.underline
        tabToVtyImage _tab@(TabDescr text inFocus) = Vty.text' (tabAttr inFocus) (tabTitle text)
