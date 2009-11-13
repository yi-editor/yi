{-# LANGUAGE ScopedTypeVariables #-}
-- Copyright (C) 2007-8 JP Bernardy
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Originally derived from: riot/UI.hs Copyright (c) Tuomo Valkonen 2004.


-- | This module defines a user interface implemented using vty.

module Yi.UI.Vty (start) where

import Yi.Prelude hiding ((<|>))
import Prelude (map, take, zip, repeat, length, break, splitAt)
import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad (forever)
import Control.Monad.State (runState, get, put)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Char (ord,chr)
import Data.Foldable
import Data.IORef
import Data.List (partition, sort, nub)
import qualified Data.List.PointedList.Circular as PL
import Data.Maybe
import Data.Monoid
import Data.Traversable
import System.Exit
import System.Posix.Signals (raiseSignal, sigTSTP)
import System.Posix.Terminal
import System.Posix.IO (stdInput)
import Yi.Buffer
import Yi.Editor
import Yi.Event
import Yi.Monad
import Yi.Style
import qualified Yi.UI.Common as Common
import Yi.Config
import Yi.Window
import Yi.Style as Style
import Graphics.Vty as Vty hiding (refresh, Default)
import qualified Graphics.Vty as Vty
import Yi.Keymap (makeAction, YiM)

import Yi.UI.Utils
import Yi.UI.TabBar

data Rendered = 
    Rendered { picture :: !Image             -- ^ the picture currently displayed.
             , cursor  :: !(Maybe (Int,Int)) -- ^ cursor point on the above
             }

data UI = UI {  vty       :: Vty             -- ^ Vty
             , scrsize    :: IORef (Int,Int) -- ^ screen size
             , uiThread   :: ThreadId
             , uiEnd      :: MVar ()
             , uiRefresh  :: MVar ()
             , uiEditor   :: IORef Editor    -- ^ Copy of the editor state, local to the UI, used to show stuff when the window is resized.
             , config     :: Config
             , oAttrs     :: TerminalAttributes
             }

mkUI :: UI -> Common.UI
mkUI ui = Common.dummyUI 
  {
   Common.main           = main ui,
   Common.end            = end ui,
   Common.suspend        = raiseSignal sigTSTP,
   Common.refresh        = refresh ui,
   Common.layout         = layout ui,
   Common.userForceRefresh = userForceRefresh ui
  }

-- | Initialise the ui
start :: UIBoot
start cfg ch outCh editor = do
  liftIO $ do 
          oattr <- getTerminalAttributes stdInput
          v <- mkVtyEscDelay $ configVtyEscDelay $ configUI $ cfg
          nattr <- getTerminalAttributes stdInput
          setTerminalAttributes stdInput (withoutMode nattr ExtendedFunctions) Immediately
          -- remove the above call to setTerminalAttributes when vty does it.
          Vty.DisplayRegion x0 y0 <- Vty.display_bounds $ Vty.terminal v
          sz <- newIORef (fromEnum y0, fromEnum x0)
          -- fork input-reading thread. important to block *thread* on getKey
          -- otherwise all threads will block waiting for input
          tid <- myThreadId
          endUI <- newEmptyMVar
          tuiRefresh <- newEmptyMVar
          editorRef <- newIORef editor
          let result = UI v sz tid endUI tuiRefresh editorRef cfg oattr
              -- | Action to read characters into a channel
              getcLoop = maybe (getKey >>= ch >> getcLoop) (const (return ())) =<< tryTakeMVar endUI

              -- | Read a key. UIs need to define a method for getting events.
              getKey = do 
                event <- Vty.next_event v
                case event of 
                  (EvResize x y) -> do
                      logPutStrLn $ "UI: EvResize: " ++ show (x,y)
                      writeIORef sz (y,x)
                      outCh [makeAction (layoutAction result :: YiM ())] 
                      -- since any action will force a refresh, return () is probably 
                      -- sufficient instead of "layoutAction result"
                      getKey
                  _ -> return (fromVtyEvent event)
          forkIO getcLoop
          return (mkUI result)

main :: UI -> IO ()
main ui = do
  let
      -- | When the editor state isn't being modified, refresh, then wait for
      -- it to be modified again.
      refreshLoop :: IO ()
      refreshLoop = forever $ do
                      logPutStrLn "waiting for refresh"
                      takeMVar (uiRefresh ui)
                      handle (\(except :: IOException) -> do
                                 logPutStrLn "refresh crashed with IO Error"
                                 logError $ show $ except)
                             (readRef (uiEditor ui) >>= refresh ui >> return ())
  logPutStrLn "refreshLoop started"
  refreshLoop

-- | Clean up and go home
end :: UI -> Bool -> IO ()
end i reallyQuit = do  
  Vty.shutdown (vty i)
  setTerminalAttributes stdInput (oAttrs i) Immediately
  tryPutMVar (uiEnd i) ()
  when reallyQuit $ throwTo (uiThread i) ExitSuccess
  return ()

fromVtyEvent :: Vty.Event -> Yi.Event.Event
fromVtyEvent (EvKey Vty.KBackTab mods) = Event Yi.Event.KTab (sort $ nub $ Yi.Event.MShift : map fromVtyMod mods)
fromVtyEvent (EvKey k mods) = Event (fromVtyKey k) (sort $ map fromVtyMod mods)
fromVtyEvent _ = error "fromVtyEvent: unsupported event encountered."


fromVtyKey :: Vty.Key -> Yi.Event.Key
fromVtyKey (Vty.KEsc     ) = Yi.Event.KEsc      
fromVtyKey (Vty.KFun x   ) = Yi.Event.KFun x    
fromVtyKey (Vty.KPrtScr  ) = Yi.Event.KPrtScr   
fromVtyKey (Vty.KPause   ) = Yi.Event.KPause    
fromVtyKey (Vty.KASCII '\t') = Yi.Event.KTab
fromVtyKey (Vty.KASCII c ) = Yi.Event.KASCII c  
fromVtyKey (Vty.KBS      ) = Yi.Event.KBS       
fromVtyKey (Vty.KIns     ) = Yi.Event.KIns      
fromVtyKey (Vty.KHome    ) = Yi.Event.KHome     
fromVtyKey (Vty.KPageUp  ) = Yi.Event.KPageUp   
fromVtyKey (Vty.KDel     ) = Yi.Event.KDel      
fromVtyKey (Vty.KEnd     ) = Yi.Event.KEnd      
fromVtyKey (Vty.KPageDown) = Yi.Event.KPageDown 
fromVtyKey (Vty.KNP5     ) = Yi.Event.KNP5      
fromVtyKey (Vty.KUp      ) = Yi.Event.KUp       
fromVtyKey (Vty.KMenu    ) = Yi.Event.KMenu     
fromVtyKey (Vty.KLeft    ) = Yi.Event.KLeft     
fromVtyKey (Vty.KDown    ) = Yi.Event.KDown     
fromVtyKey (Vty.KRight   ) = Yi.Event.KRight    
fromVtyKey (Vty.KEnter   ) = Yi.Event.KEnter    
fromVtyKey (Vty.KBackTab ) = error "This should be handled in fromVtyEvent"

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
      ws'' = fmap (apply . discardOldRegion) ws'
      discardOldRegion w = w { winRegion = emptyRegion }
                           -- Discard this field, otherwise we keep retaining reference to
                           -- old Window objects (leak)
      apply win = win {
          winRegion = getRegionImpl win (configUI $ config ui) e cols (height win)
        }

  return $ windowsA ^= ws'' $ e

-- Do Vty layout inside the Yi event loop
layoutAction :: (MonadEditor m, MonadIO m) => UI -> m ()
layoutAction ui = do
    withEditor . put =<< io . layout ui =<< withEditor get
    withEditor $ mapM_ (flip withWindowE snapInsB) =<< getA windowsA

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
      statusBarStyle = ((appEndo <$> cmdSty) <*> baseAttributes) $ configStyle $ configUI $ config $ ui
      tabBarImages = renderTabBar e ui xss
  logPutStrLn "refreshing screen."
  logPutStrLn $ "startXs: " ++ show startXs
  Vty.update (vty $ ui) 
      ( pic_for_image ( vert_cat tabBarImages
                        <->
                        vert_cat (toList wImages) 
                        <-> 
                        vert_cat (fmap formatCmdLine niceCmd)
                      )
      ) { pic_cursor = case cursor (PL.focus renders) of
                        Just (y,x) -> Cursor (toEnum x) (toEnum $ y + PL.focus startXs) 
                        -- Add the position of the window to the position of the cursor
                        Nothing -> NoCursor
                        -- This case can occur if the user resizes the window. 
                        -- Not really nice, but upon the next refresh the cursor will show.
        }

  return ()

-- | Construct images for the tabbar if at least one tab exists.
renderTabBar :: Editor -> UI -> Int -> [Image]
renderTabBar e ui xss =
  if hasTabBar e ui
    then [tabImages <|> extraImage]
    else []
  where tabImages       = foldr1 (<|>) $ fmap tabToVtyImage $ tabBarDescr e
        extraImage      = withAttributes (tabBarAttributes uiStyle) (replicate (xss - fromEnum totalTabWidth) ' ')

        totalTabWidth   = Vty.image_width tabImages
        uiStyle         = configStyle $ configUI $ config $ ui
        tabTitle text   = " " ++ text ++ " "
        baseAttr b sty  = if b then attributesToAttr (appEndo (tabInFocusStyle uiStyle) sty) Vty.def_attr
                               else attributesToAttr (appEndo (tabNotFocusedStyle uiStyle) sty) Vty.def_attr `Vty.with_style` Vty.underline
        tabAttr b       = baseAttr b $ tabBarAttributes uiStyle
        tabToVtyImage _tab@(TabDescr text inFocus) = Vty.string (tabAttr inFocus) (tabTitle text)

-- | Determine whether it is necessary to render the tab bar
hasTabBar :: Editor -> UI -> Bool
hasTabBar e ui = (not . configAutoHideTabBar . configUI . config $ ui) || (PL.length $ e ^. tabsA) > 1

-- As scanr, but generalized to a traversable (TODO)
scanrT :: (Int -> Int -> Int) -> Int -> PL.PointedList Int -> PL.PointedList Int
scanrT (+*+) k t = fst $ runState (mapM f t) k
    where f x = do s <- get
                   let s' = s +*+ x
                   put s'
                   return s

getRegionImpl :: Window -> UIConfig -> Editor -> Int -> Int -> Region
getRegionImpl win cfg e w h = snd $
                              drawWindow cfg e (error "focus must not be used")  win w h

-- | Return a rendered wiew of the window.
renderWindow :: UIConfig -> Editor -> Int -> (Window, Bool) -> Rendered
renderWindow cfg e width (win,hasFocus) =
    let (rendered,_) = drawWindow cfg e hasFocus win width (height win)
    in rendered

-- | Draw a window
-- TODO: horizontal scrolling.
drawWindow :: UIConfig -> Editor -> Bool -> Window -> Int -> Int -> (Rendered, Region)
drawWindow cfg e focused win w h = (Rendered { picture = pict,cursor = cur}, mkRegion fromMarkPoint toMarkPoint')
    where
        b = findBufferWith (bufkey win) e
        sty = configStyle cfg
        
        notMini = not (isMini win)
        -- off reserves space for the mode line. The mini window does not have a mode line.
        off = if notMini then 1 else 0
        h' = h - off
        ground = baseAttributes sty
        wsty = attributesToAttr ground Vty.def_attr
        eofsty = appEndo (eofStyle sty) ground
        (point, _) = runBuffer win b pointB
        (eofPoint, _) = runBuffer win b sizeB
        region = mkSizeRegion fromMarkPoint (Size (w*h'))
        -- Work around a problem with the mini window never displaying it's contents due to a
        -- fromMark that is always equal to the end of the buffer contents.
        (Just (MarkSet fromM _ _), _) = runBuffer win b (getMarks win)
        fromMarkPoint = if notMini
                            then fst $ runBuffer win b (getMarkPointB fromM)
                            else Point 0
        (text, _)    = runBuffer win b (indexedAnnotatedStreamB fromMarkPoint) -- read chars from the buffer, lazily
        
        (attributes, _) = runBuffer win b $ attributesPictureAndSelB sty (currentRegex e) region 
        -- TODO: I suspect that this costs quite a lot of CPU in the "dry run" which determines the window size;
        -- In that case, since attributes are also useless there, it might help to replace the call by a dummy value.
        -- This is also approximately valid of the call to "indexedAnnotatedStreamB".
        colors = map (second (($ Vty.def_attr) . attributesToAttr)) attributes
        bufData = -- trace (unlines (map show text) ++ unlines (map show $ concat strokes)) $ 
                  paintChars Vty.def_attr colors text
        tabWidth = tabSize . fst $ runBuffer win b indentSettingsB
        prompt = if isMini win then miniIdentString b else ""

        (rendered,toMarkPoint',cur) = drawText h' w
                                fromMarkPoint
                                point 
                                tabWidth
                                ([(c,(wsty, (-1))) | c <- prompt] ++ bufData ++ [(' ',(wsty, eofPoint))])
                             -- we always add one character which can be used to position the cursor at the end of file
        (modeLine0, _) = runBuffer win b $ getModeLine (commonNamePrefix e)
        modeLine = if notMini then Just modeLine0 else Nothing
        modeLines = map (withAttributes modeStyle . take w . (++ repeat ' ')) $ maybeToList $ modeLine
        modeStyle = (if focused then appEndo (modelineFocusStyle sty) else id) (modelineAttributes sty)
        filler = take w (configWindowFill cfg : repeat ' ')
    
        pict = vert_cat (take h' (rendered ++ repeat (withAttributes eofsty filler)) ++ modeLines)
  
-- | Renders text in a rectangle.
-- This also returns 
-- * the index of the last character fitting in the rectangle
-- * the position of the Point in (x,y) coordinates, if in the window.
drawText :: Int    -- ^ The height of the part of the window we are in
         -> Int    -- ^ The width of the part of the window we are in
         -> Point  -- ^ The position of the first character to draw
         -> Point  -- ^ The position of the cursor
         -> Int    -- ^ The number of spaces to represent a tab character with.
         -> [(Char,(Vty.Attr,Point))]  -- ^ The data to draw.
         -> ([Image], Point, Maybe (Int,Int))
drawText h w topPoint point tabWidth bufData
    | h == 0 || w == 0 = ([], topPoint, Nothing)
    | otherwise        = (rendered_lines, bottomPoint, pntpos)
  where 

  lns0 = take h $ concatMap (wrapLine w) $ map (concatMap expandGraphic) $ take h $ lines' $ bufData

  bottomPoint = case lns0 of 
                 [] -> topPoint 
                 _ -> snd $ snd $ last $ last $ lns0

  pntpos = listToMaybe [(y,x) | (y,l) <- zip [0..] lns0, (x,(_char,(_attr,p))) <- zip [0..] l, p == point]

  -- fill lines with blanks, so the selection looks ok.
  rendered_lines = map fillColorLine lns0
  colorChar (c, (a, _aPoint)) = Vty.char a c

  fillColorLine :: [(Char, (Vty.Attr, Point))] -> Image
  fillColorLine [] = char_fill Vty.def_attr ' ' w 1
  fillColorLine l = horiz_cat (map colorChar l) 
                    <|>
                    char_fill a ' ' (w - length l) 1
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
withAttributes sty str = Vty.string (attributesToAttr sty Vty.def_attr) str

------------------------------------------------------------------------

userForceRefresh :: UI -> IO ()
userForceRefresh = Vty.refresh . vty

-- | Schedule a refresh of the UI.
scheduleRefresh :: UI -> Editor -> IO ()
scheduleRefresh ui e = do
  writeRef (uiEditor ui) e
  logPutStrLn "scheduleRefresh"
  tryPutMVar (uiRefresh ui) ()
  return ()

-- | Calculate window heights, given all the windows and current height.
-- (No specific code for modelines)
computeHeights :: Int -> PL.PointedList Window -> [Int]
computeHeights totalHeight ws = ((y+r-1) : repeat y)
  where (mwls, wls) = partition isMini (toList ws)
        (y,r) = getY (totalHeight - length mwls) (length wls)

getY :: Int -> Int -> (Int,Int)
getY screenHeight 0               = (screenHeight, 0)
getY screenHeight numberOfWindows = screenHeight `quotRem` numberOfWindows

------------------------------
-- Low-level stuff

------------------------------------------------------------------------

-- | Convert a Yi Attr into a Vty attribute change.
colorToAttr :: (Vty.Color -> Vty.Attr -> Vty.Attr) -> Vty.Color -> Style.Color -> (Vty.Attr -> Vty.Attr)
colorToAttr set unknown c =
  case c of 
    RGB 0 0 0         -> set Vty.black
    RGB 128 128 128   -> set Vty.bright_black
    RGB 139 0 0       -> set Vty.red
    RGB 255 0 0       -> set Vty.bright_red
    RGB 0 100 0       -> set Vty.green
    RGB 0 128 0       -> set Vty.bright_green
    RGB 165 42 42     -> set Vty.yellow
    RGB 255 255 0     -> set Vty.bright_yellow
    RGB 0 0 139       -> set Vty.blue
    RGB 0 0 255       -> set Vty.bright_blue
    RGB 128 0 128     -> set Vty.magenta
    RGB 255 0 255     -> set Vty.bright_magenta
    RGB 0 139 139     -> set Vty.cyan
    RGB 0 255 255     -> set Vty.bright_cyan
    RGB 165 165 165   -> set Vty.white
    RGB 255 255 255   -> set Vty.bright_white
    Default           -> id
    _                 -> set unknown -- NB

attributesToAttr :: Attributes -> (Vty.Attr -> Vty.Attr)
attributesToAttr (Attributes fg bg reverse bd _itlc underline') =
    (if reverse then (flip Vty.with_style Vty.reverse_video)  else id) .
    (if bd then (flip Vty.with_style Vty.bold) else id) .
    (if underline' then (flip Vty.with_style Vty.underline) else id) .
    colorToAttr (flip Vty.with_fore_color) Vty.black fg . 
    colorToAttr (flip Vty.with_back_color) Vty.white bg

---------------------------------


-- | Apply the attributes in @sty@ and @changes@ to @cs@.  If the
-- attributes are not used, @sty@ and @changes@ are not evaluated.
paintChars :: a -> [(Point,a)] -> [(Point,Char)] -> [(Char, (a,Point))]
paintChars sty changes cs = [(c,(s,p)) | ((p,c),s) <- zip cs attrs]
    where attrs = lazy (stys sty changes cs)

lazy :: [a] -> [a]
lazy l = head l : lazy (tail l)

stys :: a -> [(Point,a)] -> [(Point,Char)] -> [a]
stys sty [] cs = [ sty | _ <- cs ]
stys sty ((endPos,sty'):xs) cs = [ sty | _ <- previous ] ++ stys sty' xs later
    where (previous, later) = break ((endPos <=) . fst) cs
