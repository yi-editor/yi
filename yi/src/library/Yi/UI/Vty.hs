{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
-- Copyright (C) 2007-8 JP Bernardy
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Originally derived from: riot/UI.hs Copyright (c) Tuomo Valkonen 2004.


-- | This module defines a user interface implemented using vty.

module Yi.UI.Vty (start) where

import Prelude hiding (error,mapM,foldr1,concatMap,mapM_,reverse)
import Control.Applicative hiding ((<|>))
import Control.Arrow
import Control.Monad hiding (mapM,mapM_)
import Control.Concurrent
import Control.Exception
import Control.Monad.State (evalState, get, put)
import Control.Monad.Base
import Control.Lens hiding (wrapped,set)
import Data.Char (ord,chr)
import Data.IORef
import Data.List (partition, sort, nub)
import qualified Data.List.PointedList.Circular as PL
import Data.Foldable
import Data.Traversable
import Data.Maybe
import Data.Monoid
import GHC.Conc (labelThread)
import System.Exit
import System.Posix.Signals (raiseSignal, sigTSTP)
import System.Posix.Terminal
import System.Posix.IO (stdInput)
import Yi.Buffer
import Yi.Editor
import Yi.Event
import Yi.Style
import qualified Yi.UI.Common as Common
import Yi.Config
import Yi.Window
import Yi.Style as Style
import Graphics.Vty as Vty hiding (Config(..), refresh, Default, text)
import qualified Graphics.Vty as Vty
import Yi.Keymap (makeAction, YiM)
import Yi.Debug
import Yi.Utils
import Yi.Monad
import Yi.UI.Utils
import Yi.UI.TabBar

data Rendered =
    Rendered { picture :: !Image             -- ^ the picture currently displayed.
             , cursor  :: !(Maybe (Int,Int)) -- ^ cursor point on the above
             }

data UI = UI {  vty            :: Vty             -- ^ Vty
             , scrsize         :: IORef (Int,Int) -- ^ screen size
             , uiThread        :: ThreadId
             , uiEndInputLoop  :: MVar ()
             , uiEndRenderLoop :: MVar ()
             , uiEditor        :: IORef Editor    -- ^ Copy of the editor state, local to the UI, used to show stuff when the window is resized.
             , uiDirty         :: MVar ()         -- ^ used to trigger redraw in renderLoop
             , config          :: Config
             , oAttrs          :: TerminalAttributes
             }

mkUI :: UI -> Common.UI
mkUI ui = Common.dummyUI
  {
   Common.main           = main ui,
   Common.end            = end ui,
   Common.suspend        = raiseSignal sigTSTP,
   Common.refresh        = requestRefresh ui,
   Common.layout         = layout ui,
   Common.userForceRefresh = userForceRefresh ui
  }

-- | Initialise the ui
start :: UIBoot
start cfg ch outCh editor =
  liftBase $ do
          oattr <- getTerminalAttributes stdInput
          v <- mkVty $ configVty $ configUI $ cfg
          nattr <- getTerminalAttributes stdInput
          setTerminalAttributes stdInput (withoutMode nattr ExtendedFunctions) Immediately
          -- remove the above call to setTerminalAttributes when vty does it.
          (x0,y0) <- Vty.displayBounds $ Vty.outputIface v
          sz <- newIORef (fromEnum y0, fromEnum x0)
          -- fork input-reading thread. important to block *thread* on getKey
          -- otherwise all threads will block waiting for input
          tid <- myThreadId
          labelThread tid "UI"
          endInput <- newEmptyMVar
          endRender <- newEmptyMVar
          editorRef <- newIORef editor
          dirty <- newEmptyMVar
          let ui = UI v sz tid endInput endRender editorRef dirty cfg oattr

              -- | Action to read characters into a channel
              inputLoop :: IO ()
              inputLoop = tryTakeMVar endInput >>=
                          maybe (getKey >>= ch >> inputLoop)
                                (const $ return ())

              -- | Read a key. UIs need to define a method for getting events.
              getKey :: IO Yi.Event.Event
              getKey = do
                event <- Vty.nextEvent v
                case event of
                  (EvResize x y) -> do
                      logPutStrLn $ "UI: EvResize: " ++ show (x,y)
                      writeIORef sz (y,x)
                      outCh [makeAction (layoutAction ui :: YiM ())]
                      -- since any action will force a refresh, return () is probably
                      -- sufficient instead of "layoutAction ui"
                      getKey
                  _ -> return (fromVtyEvent event)

              renderLoop :: IO ()
              renderLoop = do
                takeMVar dirty
                tryTakeMVar endRender >>=
                  maybe (do logPutStrLn "time to render"
                            handle (\(except :: IOException) -> do
                                       logPutStrLn "refresh crashed with IO Error"
                                       logError $ show except)
                                   (readIORef editorRef >>= refresh ui >> renderLoop))
                        (const $ return ())

          inputThreadId <- forkIO inputLoop
          labelThread inputThreadId "VtyInput"
          renderThreadId <- forkIO renderLoop
          labelThread renderThreadId "VtyRender"

          return (mkUI ui)

-- Is there something else to do here?
-- Previous version said "block on MVar forever" in rather obfuscated way
main :: UI -> IO ()
main _ui = forever $ threadDelay maxBound

-- | Clean up and go home
end :: UI -> Bool -> IO ()
end ui reallyQuit = do
  Vty.shutdown (vty ui)
  setTerminalAttributes stdInput (oAttrs ui) Immediately
  void $ tryPutMVar (uiEndInputLoop ui) ()
  void $ tryPutMVar (uiEndRenderLoop ui) ()
  when reallyQuit $ throwTo (uiThread ui) ExitSuccess
  return ()

fromVtyEvent :: Vty.Event -> Yi.Event.Event
fromVtyEvent (EvKey Vty.KBackTab mods) = Event Yi.Event.KTab (sort $ nub $ Yi.Event.MShift : map fromVtyMod mods)
fromVtyEvent (EvKey k mods) = Event (fromVtyKey k) (sort $ map fromVtyMod mods)
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

-- This re-computes the heights and widths of all the windows.
layout :: UI -> Editor -> IO Editor
layout ui e = do
  (rows,cols) <- readIORef (scrsize ui)
  let ws = windows e
      tabBarHeight = if hasTabBar e ui then 1 else 0
      (cmd, _) = statusLineInfo e
      niceCmd = arrangeItems cmd cols (maxStatusHeight e)
      cmdHeight = length niceCmd
      ws' = applyHeights (computeHeights (rows - tabBarHeight - cmdHeight + 1) ws) ws
      discardOldRegion w = w { winRegion = emptyRegion }
                           -- Discard this field, otherwise we keep retaining reference to
                           -- old Window objects (leak)

  let apply :: Window -> IO Window
      apply win = do
        let uiconfig = configUI $ config ui
        newWinRegion <- return $! getRegionImpl win uiconfig e cols (height win)
        newActualLines <- return $! windowLinesDisp win uiconfig e cols (height win)
        return $! win { winRegion = newWinRegion, actualLines = newActualLines }

  ws'' <- mapM (apply . discardOldRegion) ws'
  return $ windowsA .~ ws'' $ e
  -- return $ windowsA ^= forcePL ws'' $ e

-- Do Vty layout inside the Yi event loop
layoutAction :: (MonadEditor m, MonadBase IO m) => UI -> m ()
layoutAction ui = do
    withEditor . put =<< io . layout ui =<< withEditor get
    withEditor $ mapM_ (`withWindowE` snapInsB) =<< use windowsA

requestRefresh :: UI -> Editor -> IO ()
requestRefresh ui e = do
  writeIORef (uiEditor ui) e
  void $ tryPutMVar (uiDirty ui) ()

-- | Redraw the entire terminal from the UI.
refresh :: UI -> Editor -> IO ()
refresh ui e = do
  (_,xss) <- readRef (scrsize ui)
  let ws = windows e
      tabBarHeight = if hasTabBar e ui then 1 else 0
      windowStartY = tabBarHeight
      (cmd, cmdSty) = statusLineInfo e
      niceCmd = arrangeItems cmd xss (maxStatusHeight e)
      formatCmdLine text = withAttributes statusBarStyle (take xss $ text ++ repeat ' ')
      renders = fmap (renderWindow (configUI $ config ui) e xss) (PL.withFocus ws)
      startXs = scanrT (+) windowStartY (fmap height ws)
      wImages = fmap picture renders
      statusBarStyle = ((appEndo <$> cmdSty) <*> baseAttributes) $ configStyle $ configUI $ config ui
      tabBarImages = renderTabBar e ui xss
  logPutStrLn "refreshing screen."
  logPutStrLn $ "startXs: " ++ show startXs
  Vty.update (vty ui)
      ( picForImage ( vertCat tabBarImages
                      <->
                      vertCat (toList wImages)
                      <->
                      vertCat (fmap formatCmdLine niceCmd)
                    )
      ) { picCursor = case cursor (PL._focus renders) of
                        Just (y,x) -> Cursor (toEnum x) (toEnum $ y + PL._focus startXs)
                        -- Add the position of the window to the position of the cursor
                        Nothing -> NoCursor
                        -- This case can occur if the user resizes the window.
                        -- Not really nice, but upon the next refresh the cursor will show.
        }

  return ()

-- | Construct images for the tabbar if at least one tab exists.
renderTabBar :: Editor -> UI -> Int -> [Image]
renderTabBar e ui xss = [tabImages <|> extraImage | hasTabBar e ui]
  where tabImages       = foldr1 (<|>) $ fmap tabToVtyImage $ tabBarDescr e
        extraImage      = withAttributes (tabBarAttributes uiStyle) (replicate (xss - fromEnum totalTabWidth) ' ')

        totalTabWidth   = Vty.imageWidth tabImages
        uiStyle         = configStyle $ configUI $ config ui
        tabTitle text   = " " ++ text ++ " "
        tabAttr b       = baseAttr b $ tabBarAttributes uiStyle
        baseAttr True  sty = attributesToAttr (appEndo (tabInFocusStyle uiStyle) sty) Vty.defAttr
        baseAttr False sty = attributesToAttr (appEndo (tabNotFocusedStyle uiStyle) sty) Vty.defAttr `Vty.withStyle` Vty.underline
        tabToVtyImage _tab@(TabDescr text inFocus) = Vty.string (tabAttr inFocus) (tabTitle text)

-- | Determine whether it is necessary to render the tab bar
hasTabBar :: Editor -> UI -> Bool
hasTabBar e ui = (not . configAutoHideTabBar . configUI . config $ ui) || PL.length (e ^. tabsA) > 1

-- As scanr, but generalized to a traversable (TODO)
scanrT :: (Int -> Int -> Int) -> Int -> PL.PointedList Int -> PL.PointedList Int
scanrT (+*+) k t = evalState (mapM f t) k
    where f x = do s <- get
                   let s' = s +*+ x
                   put s'
                   return s

-- | Calculate the lines a window can display from a buffer.
windowLinesDisp :: Window -> UIConfig -> Editor -> Int -> Int -> Int
windowLinesDisp win cfg e w h = dispCount
  where (_,_,dispCount) = drawWindow cfg e (error "focus must not be used")  win w h

getRegionImpl :: Window -> UIConfig -> Editor -> Int -> Int -> Region
getRegionImpl win cfg e w h = region
  where (_,region,_) = drawWindow cfg e (error "focus must not be used") win w h

-- | Return a rendered view of the window.
renderWindow :: UIConfig -> Editor -> Int -> (Window, Bool) -> Rendered
renderWindow cfg e width (win,hasFocus) =
    let (rendered,_,_) = drawWindow cfg e hasFocus win width (height win)
    in rendered

-- | Draw a window
--
-- TODO: horizontal scrolling.
drawWindow :: UIConfig -> Editor -> Bool -> Window -> Int -> Int -> (Rendered, Region, Int)
drawWindow cfg e focused win w h = (Rendered { picture = pict,cursor = cur}, mkRegion fromMarkPoint toMarkPoint', dispLnCount)
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
        (text, _)    = runBuffer win b (indexedAnnotatedStreamB fromMarkPoint) -- read chars from the buffer, lazily

        (attributes, _) = runBuffer win b $ attributesPictureAndSelB sty (currentRegex e) region
        -- TODO: I suspect that this costs quite a lot of CPU in the "dry run" which determines the window size;
        -- In that case, since attributes are also useless there, it might help to replace the call by a dummy value.
        -- This is also approximately valid of the call to "indexedAnnotatedStreamB".
        colors = map (second (($ Vty.defAttr) . attributesToAttr)) attributes
        bufData = -- trace (unlines (map show text) ++ unlines (map show $ concat strokes)) $
                  paintChars Vty.defAttr colors text
        tabWidth = tabSize . fst $ runBuffer win b indentSettingsB
        prompt = if isMini win then miniIdentString b else ""

        (rendered,toMarkPoint',cur,dispLnCount) = drawText h' w
                                fromMarkPoint
                                point
                                tabWidth
                                ([(c,(wsty, -1)) | c <- prompt] ++ bufData ++ [(' ',(wsty, eofPoint))])
                             -- we always add one character which can be used to position the cursor at the end of file
        (modeLine0, _) = runBuffer win b $ getModeLine (commonNamePrefix e)
        modeLine = if notMini then Just modeLine0 else Nothing
        modeLines = map (withAttributes modeStyle . take w . (++ repeat ' ')) $ maybeToList modeLine
        modeStyle = (if focused then appEndo (modelineFocusStyle sty) else id) (modelineAttributes sty)
        filler = take w (configWindowFill cfg : repeat ' ')

        pict = vertCat (take h' (rendered ++ repeat (withAttributes eofsty filler)) ++ modeLines)

-- | Renders text in a rectangle.
-- This also returns
-- * the index of the last character fitting in the rectangle
-- * the position of the Point in (x,y) coordinates, if in the window,
-- * the number of display lines for this drawing.
--
-- We calculate the number of lines displayed for this window so that line
-- wrapping doesn't break scrolling.
drawText :: Int    -- ^ The height of the part of the window we are in
         -> Int    -- ^ The width of the part of the window we are in
         -> Point  -- ^ The position of the first character to draw
         -> Point  -- ^ The position of the cursor
         -> Int    -- ^ The number of spaces to represent a tab character with.
         -> [(Char,(Vty.Attr,Point))]  -- ^ The data to draw.
         -> ([Image], Point, Maybe (Int,Int), Int)
drawText h w topPoint point tabWidth bufData
    | h == 0 || w == 0 = ([], topPoint, Nothing, 0)
    | otherwise        = (renderedLines, bottomPoint, pntpos, h - (length wrapped - h))
  where

  -- the number of lines that taking wrapping into account,
  -- we use this to calculate the number of lines displayed.
  wrapped = concatMap (wrapLine w) $ map (concatMap expandGraphic) $ take h $ lines' bufData
  lns0 = take h wrapped

  bottomPoint = case lns0 of
                 [] -> topPoint
                 _ -> snd $ snd $ last $ last lns0

  pntpos = listToMaybe [(y,x) | (y,l) <- zip [0..] lns0, (x,(_char,(_attr,p))) <- zip [0..] l, p == point]

  -- fill lines with blanks, so the selection looks ok.
  renderedLines = map fillColorLine lns0
  colorChar (c, (a, _aPoint)) = Vty.char a c

  fillColorLine :: [(Char, (Vty.Attr, Point))] -> Image
  fillColorLine [] = charFill Vty.defAttr ' ' w 1
  fillColorLine l = horizCat (map colorChar l)
                    <|>
                    charFill a ' ' (w - length l) 1
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

withAttributes :: Attributes -> String -> Image
withAttributes sty = Vty.string (attributesToAttr sty Vty.defAttr)

------------------------------------------------------------------------

userForceRefresh :: UI -> IO ()
userForceRefresh = Vty.refresh . vty

-- | Calculate window heights, given all the windows and current height.
-- (No specific code for modelines)
computeHeights :: Int -> PL.PointedList Window -> [Int]
computeHeights totalHeight ws = y+r-1 : repeat y
  where (mwls, wls) = partition isMini (toList ws)
        (y,r) = getY (totalHeight - length mwls) (length wls)

getY :: Int -> Int -> (Int,Int)
getY screenHeight 0               = (screenHeight, 0)
getY screenHeight numberOfWindows = screenHeight `quotRem` numberOfWindows

------------------------------
-- Low-level stuff

------------------------------------------------------------------------

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

attributesToAttr :: Attributes -> Vty.Attr -> Vty.Attr
attributesToAttr (Attributes fg bg reverse bd _itlc underline') =
    (if reverse then (`Vty.withStyle` Vty.reverseVideo) else id) .
    (if bd then (`Vty.withStyle` Vty.bold) else id) .
    (if underline' then (`Vty.withStyle` Vty.underline) else id) .
    colorToAttr (flip Vty.withForeColor) fg .
    colorToAttr (flip Vty.withBackColor) bg


---------------------------------


-- | Apply the attributes in @sty@ and @changes@ to @cs@.  If the
-- attributes are not used, @sty@ and @changes@ are not evaluated.
paintChars :: a -> [(Point,a)] -> [(Point,Char)] -> [(Char, (a,Point))]
paintChars sty changes cs = [(c,(s,p)) | ((p,c),s) <- zip cs attrs]
    where attrs = stys sty changes cs

stys :: a -> [(Point,a)] -> [(Point,Char)] -> [a]
stys sty [] cs = [ sty | _ <- cs ]
stys sty ((endPos,sty'):xs) cs = [ sty | _ <- previous ] ++ stys sty' xs later
    where (previous, later) = break ((endPos <=) . fst) cs
