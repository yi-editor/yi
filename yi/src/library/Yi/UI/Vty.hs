{-# LANGUAGE ScopedTypeVariables #-}
-- Copyright (C) 2007-8 JP Bernardy
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Originally derived from: riot/UI.hs Copyright (c) Tuomo Valkonen 2004.

module Yi.UI.Vty
    ( start
    ) where

import Prelude hiding (error, mapM, foldr1, concatMap, reverse)
import Control.Applicative hiding ((<|>))
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad hiding (mapM)
import Control.Monad.State (evalState, get, put)
import Data.Char
import Data.Foldable (toList, foldr1, concatMap)
import Data.IORef
import Data.List (nub, sort)
import qualified Data.List.PointedList.Circular as PL
import Data.Maybe
import Data.Monoid
import Data.Traversable
import qualified Graphics.Vty as Vty
import GHC.Conc (labelThread)

import Yi.Buffer
import Yi.Config
import Yi.Debug
import Yi.Editor
import Yi.Event
import Yi.Keymap
import Yi.Style
import qualified Yi.UI.Common as Common
import qualified Yi.UI.SimpleLayout as SL
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
start config submitEvent submitActions editor = do
    vty <- (Vty.mkVty . configVty . configUI) config
    endInput <- newEmptyMVar
    endMain <- newEmptyMVar
    endRender <- newEmptyMVar
    dirty <- newEmptyMVar
    editorRef <- newIORef editor
    let -- | Action to read characters into a channel
        inputLoop :: IO ()
        inputLoop = tryTakeMVar endInput >>=
                    maybe (getEvent >>= submitEvent >> inputLoop)
                          (const $ return ())

        -- | Read a key. UIs need to define a method for getting events.
        getEvent :: IO Yi.Event.Event
        getEvent = do
          event <- Vty.nextEvent vty
          case event of
            (Vty.EvResize _ _) -> do
                submitActions [makeAction (return () :: YiM ())]
                -- since any action will force a refresh, return () is probably
                -- sufficient instead of "layoutAction ui"
                getEvent
            _ -> return (fromVtyEvent event)

        renderLoop :: IO ()
        renderLoop = do
          takeMVar dirty
          tryTakeMVar endRender >>=
            maybe (handle (\(except :: IOException) -> do
                              logPutStrLn "refresh crashed with IO Error"
                              logError $ show except)
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
main = takeMVar . fsEndMain

layout :: FrontendState -> Editor -> IO Editor
layout fs e = do
    (colCount, rowCount) <- Vty.displayBounds (Vty.outputIface (fsVty fs))
    return (SL.layout colCount rowCount e)

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

fromVtyEvent :: Vty.Event -> Yi.Event.Event
fromVtyEvent (Vty.EvKey Vty.KBackTab mods) = Event Yi.Event.KTab (sort $ nub $ Yi.Event.MShift : map fromVtyMod mods)
fromVtyEvent (Vty.EvKey k mods) = Event (fromVtyKey k) (sort $ map fromVtyMod mods)
fromVtyEvent _ = error "fromVtyEvent: unsupported event encountered."

fromVtyKey :: Vty.Key -> Yi.Event.Key
fromVtyKey (Vty.KEsc      ) = Yi.Event.KEsc
fromVtyKey (Vty.KFun x    ) = Yi.Event.KFun x
fromVtyKey (Vty.KPrtScr   ) = Yi.Event.KPrtScr
fromVtyKey (Vty.KPause    ) = Yi.Event.KPause
fromVtyKey (Vty.KChar '\t') = Yi.Event.KTab
fromVtyKey (Vty.KChar c   ) = Yi.Event.KASCII c
fromVtyKey (Vty.KBS       ) = Yi.Event.KBS
fromVtyKey (Vty.KIns      ) = Yi.Event.KIns
fromVtyKey (Vty.KHome     ) = Yi.Event.KHome
fromVtyKey (Vty.KPageUp   ) = Yi.Event.KPageUp
fromVtyKey (Vty.KDel      ) = Yi.Event.KDel
fromVtyKey (Vty.KEnd      ) = Yi.Event.KEnd
fromVtyKey (Vty.KPageDown ) = Yi.Event.KPageDown
fromVtyKey (Vty.KCenter   ) = Yi.Event.KNP5
fromVtyKey (Vty.KUp       ) = Yi.Event.KUp
fromVtyKey (Vty.KMenu     ) = Yi.Event.KMenu
fromVtyKey (Vty.KLeft     ) = Yi.Event.KLeft
fromVtyKey (Vty.KDown     ) = Yi.Event.KDown
fromVtyKey (Vty.KRight    ) = Yi.Event.KRight
fromVtyKey (Vty.KEnter    ) = Yi.Event.KEnter
fromVtyKey (Vty.KBackTab  ) = error "This should be handled in fromVtyEvent"
fromVtyKey (Vty.KBegin    ) = error "Yi.UI.Vty.fromVtyKey: can't handle KBegin"

fromVtyMod :: Vty.Modifier -> Yi.Event.Modifier
fromVtyMod Vty.MShift = Yi.Event.MShift
fromVtyMod Vty.MCtrl  = Yi.Event.MCtrl
fromVtyMod Vty.MMeta  = Yi.Event.MMeta
fromVtyMod Vty.MAlt   = Yi.Event.MMeta

refresh :: FrontendState -> Editor -> IO ()
refresh fs e = do
    (colCount, rowCount) <- Vty.displayBounds (Vty.outputIface (fsVty fs))
    let ws = windows e
        windowStartY = 1
        (cmd, cmdSty) = statusLineInfo e
        niceCmd = arrangeItems cmd colCount (maxStatusHeight e)
        formatCmdLine text = withAttributes statusBarStyle (take colCount $ text ++ repeat ' ')
        winImage (win, hasFocus) =
            renderWindow (configUI $ fsConfig fs) e colCount (win, hasFocus)
        windowsAndImages = fmap (\(w, f) -> (w, winImage (w, f))) (PL.withFocus ws)
        bigImages = map (picture . snd) (filter (not . isMini . fst) (toList windowsAndImages))
        miniImages = map (picture . snd) (filter (isMini . fst) (toList windowsAndImages))
        startXs = scanrT (+) windowStartY (fmap (\w -> if isMini w then 0 else height w) ws)
        statusBarStyle = ((appEndo <$> cmdSty) <*> baseAttributes) $ configStyle $ configUI $ fsConfig fs
        tabBarImages = renderTabBar e fs colCount
    logPutStrLn "refreshing screen."
    logPutStrLn $ "startXs: " ++ show startXs
    Vty.update (fsVty fs)
        ( Vty.picForImage ( Vty.vertCat tabBarImages
                            Vty.<->
                            Vty.vertCat bigImages
                            Vty.<->
                            (if null cmd then Vty.emptyImage else Vty.vertCat (fmap formatCmdLine niceCmd))
                            Vty.<->
                            Vty.vertCat miniImages
                          ))
        { Vty.picCursor =
            case (\(w, r) -> (isMini w, cursor r)) (PL._focus windowsAndImages) of
                (False, Just (y, x)) -> Vty.Cursor (toEnum x) (toEnum $ y + PL._focus startXs)
                (True, Just (_, x)) -> Vty.Cursor (toEnum x) (toEnum $ rowCount - 1)
                -- Add the position of the window to the position of the cursor
                (_, Nothing) -> Vty.NoCursor
                -- This case can occur if the user resizes the window.
                -- Not really nice, but upon the next refresh the cursor will show.
        }

renderWindow :: UIConfig -> Editor -> Int -> (Window, Bool) -> Rendered
renderWindow cfg e w (win, focused) = Rendered pict cur
    where
        h = height win
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
                     ([(c,(wsty, -1)) | c <- prompt] ++ bufData ++ [(' ',(wsty, eofPoint))])
                     -- we always add one character which can be used to position the cursor at the end of file
        (modeLine0, _) = runBuffer win b $ getModeLine (commonNamePrefix e)
        modeLine = if notMini then Just modeLine0 else Nothing
        modeLines = map (withAttributes modeStyle . take w . (++ repeat ' ')) $ maybeToList modeLine
        modeStyle = (if focused then appEndo (modelineFocusStyle sty) else id) (modelineAttributes sty)
        filler = take w (configWindowFill cfg : repeat ' ')

        pict = Vty.vertCat (take h' (rendered ++ repeat (withAttributes eofsty filler)) ++ modeLines)

withAttributes :: Attributes -> String -> Vty.Image
withAttributes sty = Vty.string (attributesToAttr sty Vty.defAttr)

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
stys sty ((endPos,sty'):xs) cs = [ sty | _ <- previous ] ++ stys sty' xs later
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

-- | Convert a Yi Attr into a Vty attribute change.
colorToAttr :: (Vty.Color -> Vty.Attr -> Vty.Attr) -> Yi.Style.Color -> Vty.Attr -> Vty.Attr
colorToAttr set c =
  case c of
    RGB 0 0 0         -> set Vty.black
    RGB 128 128 128   -> set Vty.brightBlack
    RGB 139 0 0       -> set Vty.red
    RGB 255 0 0       -> set Vty.brightRed
    RGB 0 100 0       -> set Vty.green
    RGB 0 128 0       -> set Vty.brightGreen
    RGB 165 42 42     -> set Vty.yellow
    RGB 255 255 0     -> set Vty.brightYellow
    RGB 0 0 139       -> set Vty.blue
    RGB 0 0 255       -> set Vty.brightBlue
    RGB 128 0 128     -> set Vty.magenta
    RGB 255 0 255     -> set Vty.brightMagenta
    RGB 0 139 139     -> set Vty.cyan
    RGB 0 255 255     -> set Vty.brightCyan
    RGB 165 165 165   -> set Vty.white
    RGB 255 255 255   -> set Vty.brightWhite
    Default           -> id
    RGB r g b         -> set (Vty.rgbColor r g b)

-- | Construct images for the tabbar if at least one tab exists.
renderTabBar :: Editor -> FrontendState -> Int -> [Vty.Image]
renderTabBar e fs xss = [tabImages Vty.<|> extraImage]
  where tabImages       = foldr1 (Vty.<|>) $ fmap tabToVtyImage $ tabBarDescr e
        extraImage      = withAttributes (tabBarAttributes uiStyle) (replicate (xss - fromEnum totalTabWidth) ' ')
        totalTabWidth   = Vty.imageWidth tabImages
        uiStyle         = configStyle $ configUI $ fsConfig fs
        tabTitle text   = " " ++ text ++ " "
        tabAttr b       = baseAttr b $ tabBarAttributes uiStyle
        baseAttr True  sty = attributesToAttr (appEndo (tabInFocusStyle uiStyle) sty) Vty.defAttr
        baseAttr False sty = attributesToAttr (appEndo (tabNotFocusedStyle uiStyle) sty) Vty.defAttr `Vty.withStyle` Vty.underline
        tabToVtyImage _tab@(TabDescr text inFocus) = Vty.string (tabAttr inFocus) (tabTitle text)

-- As scanr, but generalized to a traversable (TODO)
scanrT :: (Int -> Int -> Int) -> Int -> PL.PointedList Int -> PL.PointedList Int
scanrT (+*+) k t = evalState (mapM f t) k
    where f x = do s <- get
                   let s' = s +*+ x
                   put s'
                   return s
