-- Copyright (C) 2007-8 JP Bernardy
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Originally derived from: riot/UI.hs Copyright (c) Tuomo Valkonen 2004.


-- | This module defines a user interface implemented using vty.

module Yi.UI.Vty (start) where

import Yi.Prelude hiding ((<|>))
import Prelude (map, take, zip, repeat, length, break, splitAt)
import Control.Arrow
import Control.Concurrent
import Control.OldException
import Control.Monad (forever)
import Control.Monad.State (runState, State, gets, modify, get, put)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Char (ord,chr)
import Data.Foldable
import Data.IORef
import Data.List (partition, sort, nub)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Traversable
import System.Exit
import System.Posix.Signals (raiseSignal, sigTSTP)
import Yi.Buffer
import Yi.Config
import Yi.Editor
import Yi.Event
import Yi.Monad
import Yi.Style
import Yi.WindowSet as WS
import qualified Data.ByteString.Char8 as B
import qualified Yi.UI.Common as Common
import Yi.Config
import Yi.Window
import Yi.Style as Style
import Graphics.Vty as Vty hiding (refresh)
import qualified Graphics.Vty as Vty

import Yi.UI.Utils
import Yi.UI.TabBar

data Rendered = 
    Rendered { picture :: !Image             -- ^ the picture currently displayed.
             , cursor  :: !(Maybe (Int,Int)) -- ^ cursor point on the above
             }

data UI = UI {  vty       :: Vty             -- ^ Vty
             , scrsize    :: IORef (Int,Int) -- ^ screen size
             , uiThread   :: ThreadId
             , uiRefresh  :: MVar ()
             , uiEditor   :: IORef Editor    -- ^ Copy of the editor state, local to the UI
             , config     :: Config
             }

mkUI :: UI -> Common.UI
mkUI ui = Common.dummyUI 
  {
   Common.main           = main ui,
   Common.end            = end ui,
   Common.suspend        = raiseSignal sigTSTP,
   Common.refresh        = scheduleRefresh ui,
   Common.prepareAction  = prepareAction ui,
   Common.userForceRefresh = userForceRefresh ui
  }

-- | Initialise the ui
start :: UIBoot
start cfg ch _outCh editor = do
  liftIO $ do 
          v <- mkVtyEscDelay $ configVtyEscDelay $ configUI $ cfg
          (x0,y0) <- Vty.getSize v
          sz <- newIORef (y0,x0)
          -- fork input-reading thread. important to block *thread* on getKey
          -- otherwise all threads will block waiting for input
          t <- myThreadId
          tuiRefresh <- newEmptyMVar
          editorRef <- newIORef editor
          let result = UI v sz t tuiRefresh editorRef cfg
              -- | Action to read characters into a channel
              getcLoop = forever $ getKey >>= ch

              -- | Read a key. UIs need to define a method for getting events.
              getKey = do 
                event <- getEvent v
                case event of 
                  (EvResize x y) -> do logPutStrLn $ "UI: EvResize: " ++ show (x,y)
                                       writeIORef sz (y,x) >> readRef (uiEditor result) >>= Yi.UI.Vty.refresh result >> getKey
                  _ -> return (fromVtyEvent event)
          forkIO $ getcLoop
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
                      handleJust ioErrors (\except -> do 
                                             logPutStrLn "refresh crashed with IO Error"
                                             logError $ show $ except)
                                     (readRef (uiEditor ui) >>= refresh ui >> return ())
  readRef (uiEditor ui) >>= scheduleRefresh ui
  logPutStrLn "refreshLoop started"
  refreshLoop
  

-- | Clean up and go home
end :: UI -> IO ()
end i = do  
  Vty.shutdown (vty i)
  throwTo (uiThread i) (ExitException ExitSuccess)

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

prepareAction :: UI -> IO (EditorM ())
prepareAction ui = do
  (yss,xss) <- readRef (scrsize ui)
  return $ do
    e <- get
    modA windowsA (computeHeights (yss - (if hasTabBar e ui then 1 else 0)))
    let ws = windows e
        renderSeq = fmap (scrollAndRenderWindow (configUI $ config ui) xss) (WS.withFocus ws)
    sequence_ renderSeq


-- | Redraw the entire terminal from the UI.
-- Among others, this re-computes the heights and widths of all the windows.
refresh :: UI -> Editor -> IO Editor
refresh ui e = do
  let ws = windows e
      tabBarHeight = if hasTabBar e ui then 1 else 0
      windowStartY = if hasTabBar e ui then 1 else 0
  logPutStrLn "refreshing screen."
  (yss,xss) <- readRef (scrsize ui)
  let ws' = computeHeights (yss - tabBarHeight) ws
      (cmd, cmdSty) = statusLineInfo e
      renderSeq = fmap (scrollAndRenderWindow (configUI $ config ui) xss) (WS.withFocus ws')
      (e', renders) = runEditor (config ui) (sequence renderSeq) e

  let startXs = scanrT (+) windowStartY (fmap height ws')
      wImages = fmap picture renders
      statusBarStyle = ((appEndo <$> cmdSty) <*> baseAttributes) $ configStyle $ configUI $ config $ ui
      tabBarImages = renderTabBar e' ui xss
  WS.debug "Drawing: " ws'
  logPutStrLn $ "startXs: " ++ show startXs
  Vty.update (vty $ ui) 
      pic { pImage  = vertcat tabBarImages
                      <->
                      vertcat (toList wImages) 
                      <-> 
                      withAttributes statusBarStyle (take xss $ cmd ++ repeat ' ')
          , pCursor = case cursor (WS.current renders) of
                        Just (y,x) -> Cursor x (y + WS.current startXs) 
                        -- Add the position of the window to the position of the cursor
                        Nothing -> NoCursor
                        -- This case can occur if the user resizes the window. 
                        -- Not really nice, but upon the next refresh the cursor will show.
          }

  return e'

-- | Construct images for the tabbar if at least one tab exists.
renderTabBar :: Editor -> UI -> Int -> [Image]
renderTabBar e ui xss =
  if hasTabBar e ui
    then [tabImages <|> extraImage]
    else []
  where tabImages       = foldr1 (<|>) $ fmap tabToVtyImage $ tabBarDescr e
        extraImage      = withAttributes (tabBarAttributes uiStyle) (replicate (xss-totalTabWidth) ' ')

        totalTabWidth   = imgWidth tabImages
        uiStyle         = configStyle $ configUI $ config $ ui
        tabTitle text   = " " ++ text ++ " "
        tabAttributes f = appEndo ((if f then tabInFocusStyle else tabNotFocusedStyle) uiStyle) (tabBarAttributes uiStyle)
        tabToVtyImage _tab@(TabDescr text inFocus) = withAttributes (tabAttributes inFocus) (tabTitle text)

-- | Determine whether it is necessary to render the tab bar
hasTabBar :: Editor -> UI -> Bool
hasTabBar e ui = (not . configAutoHideTabBar . configUI . config $ ui) || (WS.size $ e ^. tabsA) > 1

scanrT :: (Int -> Int -> Int) -> Int -> WindowSet Int -> WindowSet Int
scanrT (+*+) k t = fst $ runState (mapM f t) k
    where f x = do s <- get
                   let s' = s +*+ x
                   put s'
                   return s

-- | Scrolls the window to show the point if needed, and return a rendered wiew of the window.
scrollAndRenderWindow :: UIConfig -> Int -> (Window, Bool) -> EditorM Rendered
scrollAndRenderWindow cfg width (win,hasFocus) = do 
    e <- get
    let sty = configStyle cfg
        b0 = findBufferWith (bufkey win) e
        (_,b1) = drawWindow cfg e b0 sty hasFocus width win
        -- this is merely to recompute the bos point.
        ((pointDriven, inWindow), _) = runBuffer win b1 $ do point <- pointB
                                                             (,) <$> getA pointDriveA <*> pointInWindowB point
        -- | Move the point inside the window.
        showPoint buf = snd $ runBuffer win buf $ do r <- winRegionB
                                                     p <- pointB
                                                     moveTo $ max (regionStart r) $ min (regionEnd r - 1) $ p
                                                     putA pointDriveA True -- revert to a point-driven behaviour
        b2 = if inWindow then b1 else 
                if pointDriven then moveWinTosShowPoint b1 win else showPoint b1
        (rendered, b3) = drawWindow cfg e b2 sty hasFocus width win
    put e { buffers = M.insert (bufkey win) b3 (buffers e) }
    return rendered

-- | Draw a window
-- TODO: horizontal scrolling.
drawWindow :: UIConfig -> Editor -> FBuffer -> UIStyle -> Bool -> Int -> Window -> (Rendered, FBuffer)
drawWindow cfg e b sty focused w win = (Rendered { picture = pict,cursor = cur}, b')
    where
        
        notMini = not (isMini win)
        -- off reserves space for the mode line. The mini window does not have a mode line.
        off = if notMini then 1 else 0
        h' = height win - off
        ground = baseAttributes sty
        wsty = attributesToAttr ground attr
        eofsty = appEndo (eofStyle sty) ground
        (point, _) = runBuffer win b pointB
        (eofPoint, _) = runBuffer win b sizeB
        region = mkSizeRegion fromMarkPoint (Size (w*h'))
        -- Work around a problem with the mini window never displaying it's contents due to a
        -- fromMark that is always equal to the end of the buffer contents.
        (Just (MarkSet fromM _ _ toM), _) = runBuffer win b (getMarks win)
        fromMarkPoint = if notMini
                            then fst $ runBuffer win b (getMarkPointB fromM)
                            else Point 0
        (text, _)    = runBuffer win b (indexedAnnotatedStreamB fromMarkPoint) -- read chars from the buffer, lazily
        
        (attributes, _) = runBuffer win b $ attributesPictureAndSelB sty (regex e) region 
        colors = map (second (($ attr) . attributesToAttr)) attributes
        bufData = -- trace (unlines (map show text) ++ unlines (map show $ concat strokes)) $ 
                  paintChars attr colors text
        tabWidth = tabSize . fst $ runBuffer win b indentSettingsB
        prompt = if isMini win then case b ^. identA of
                Right _ -> "MINIFILE:"
                Left bufName -> bufName
            else ""

        (rendered,toMarkPoint',cur) = drawText h' w
                                fromMarkPoint
                                point 
                                tabWidth
                                ([(c,(wsty, (-1))) | c <- prompt] ++ bufData ++ [(' ',(wsty, eofPoint))])
                             -- we always add one character which can be used to position the cursor at the end of file
        (_, b') = runBuffer win b (setMarkPointB toM toMarkPoint')
        (modeLine0, _) = runBuffer win b $ getModeLine (commonNamePrefix e)
        modeLine = if notMini then Just modeLine0 else Nothing
        modeLines = map (withAttributes modeStyle . take w . (++ repeat ' ')) $ maybeToList $ modeLine
        modeStyle = (if focused then appEndo (modelineFocusStyle sty) else id) (modelineAttributes sty)
        filler = take w (configWindowFill cfg : repeat ' ')
    
        pict = vertcat (take h' (rendered ++ repeat (withAttributes eofsty filler)) ++ modeLines)
  
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
  colorChar (c, (a, _aPoint)) = renderChar a c

  fillColorLine :: [(Char, (Vty.Attr, Point))] -> Image
  fillColorLine [] = renderHFill attr ' ' w
  fillColorLine l = horzcat (map colorChar l) 
                    <|>
                    renderHFill a ' ' (w - length l)
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
withAttributes sty str = renderBS (attributesToAttr sty attr) (B.pack str)

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
-- The non-blocking behviour was set up with this in mind: if the display
-- thread is not able to catch up with the editor updates (possible since
-- display is much more time consuming than simple editor operations),
-- then there will be fewer display refreshes. 

-- | Calculate window heights, given all the windows and current height.
-- (No specific code for modelines)
computeHeights :: Int -> WindowSet Window  -> WindowSet Window
computeHeights totalHeight ws = result
  where (mwls, wls) = partition isMini (toList ws)
        (y,r) = getY (totalHeight - length mwls) (length wls) 
        (result, _) = runState (Data.Traversable.mapM distribute ws) ((y+r-1) : repeat y)

distribute :: Window -> State [Int] Window
distribute win = case isMini win of
                 True -> return win {height = 1}
                 False -> do h <- gets head
                             modify tail
                             return win {height = h}

getY :: Int -> Int -> (Int,Int)
getY screenHeight 0               = (screenHeight, 0)
getY screenHeight numberOfWindows = screenHeight `quotRem` numberOfWindows

------------------------------
-- Low-level stuff

------------------------------------------------------------------------

-- | Convert a Yi Attr into a Vty attribute change.
colorToAttr :: (Vty.Color -> Vty.Attr -> Vty.Attr) -> (Vty.Color -> Vty.Attr -> Vty.Attr) -> Vty.Color -> Style.Color -> (Vty.Attr -> Vty.Attr)
colorToAttr setVivid set unknown c =
  case c of 
    RGB 0 0 0         -> set      Vty.black
    RGB 128 128 128   -> setVivid Vty.black
    RGB 139 0 0       -> set      Vty.red
    RGB 255 0 0       -> setVivid Vty.red
    RGB 0 100 0       -> set      Vty.green
    RGB 0 128 0       -> setVivid Vty.green
    RGB 165 42 42     -> set      Vty.yellow
    RGB 255 255 0     -> setVivid Vty.yellow
    RGB 0 0 139       -> set      Vty.blue
    RGB 0 0 255       -> setVivid Vty.blue
    RGB 128 0 128     -> set      Vty.magenta
    RGB 255 0 255     -> setVivid Vty.magenta
    RGB 0 139 139     -> set      Vty.cyan
    RGB 0 255 255     -> setVivid Vty.cyan
    RGB 165 165 165   -> set      Vty.white
    RGB 255 255 255   -> setVivid Vty.white
    Default           -> set      Vty.def
    _                 -> set      unknown -- NB

attributesToAttr :: Attributes -> (Vty.Attr -> Vty.Attr)
attributesToAttr (Attributes fg bg reverse) =
  (if reverse then setRV else id) .
  colorToAttr setFGVivid setFG Vty.black fg .
  colorToAttr setBGVivid setBG Vty.white bg


---------------------------------


-- | Return @n@ elems starting at @i@ of the buffer as a list.
-- This routine also does syntax highlighting and applies overlays.
paintChars :: a -> [(Point,a)] -> [(Point,Char)] -> [(Char, (a,Point))]
paintChars sty [] cs = setSty sty cs
paintChars sty ((endPos,sty'):xs) cs = setSty sty previous ++ paintChars sty' xs later
        where (previous, later) = break ((endPos <=) . fst) cs

setSty :: a -> [(Point,Char)] -> [(Char, (a,Point))]
setSty sty cs = [(c,(sty,p)) | (p,c) <- cs]
