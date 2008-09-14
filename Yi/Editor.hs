{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, StandaloneDeriving #-}

-- Copyright (c) 2004-5, Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2007-8, JP Bernardy

-- | The top level editor state, and operations on it.

module Yi.Editor where

import Yi.Buffer                ( BufferRef
                                , FBuffer (..)
                                , BufferM
                                , newB
                                , runBufferFull
                                , insertN
                                , setAnyMode)
import Yi.Buffer.Implementation (Update(..), updateIsDelete)
import Yi.Buffer.HighLevel (botB)
import Yi.Buffer.Basic
import Yi.Regex (SearchExp)
import Yi.Config
import Yi.Monad
import Yi.Dynamic
import Yi.KillRing
import Yi.Window
import Yi.WindowSet (WindowSet)
import qualified Yi.WindowSet as WS
import Yi.Event (Event)


import Prelude (map, filter, (!!), takeWhile, length, reverse, zip)
import Yi.Prelude

import Data.Binary
import Data.List (nub, delete)
import qualified Data.DelayList as DelayList
import qualified Data.Map as M
import Data.Typeable
import Control.Monad.RWS hiding (get, put)
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8

-- | The Editor state
data Editor = Editor {
        bufferStack   :: ![BufferRef]               -- ^ Stack of all the buffers. Never empty;
                                                    -- first buffer is the current one.
       ,buffers       :: !(M.Map BufferRef FBuffer)
       ,refSupply     :: !Int  -- ^ Supply for buffer and window ids.

       ,tabs          :: !(WindowSet (WindowSet Window))

       ,dynamic       :: !(DynamicValues)              -- ^ dynamic components

       ,tabwidth      :: !Int                       -- ^ width of tabs

       ,statusLines   :: !(DelayList.DelayList String)
       ,killring      :: !Killring
       ,regex         :: !(Maybe SearchExp) -- ^ most recent regex
       ,searchDirection :: !Direction
       ,pendingEvents :: ![Event]                   -- ^ Processed events that didn't yield any action yet.
    }
    deriving Typeable

instance Binary Editor where
    put (Editor bss bs supply ts _dv sl kr _re _dir _ev) = put bss >> put bs >> put supply >> put ts >> put sl >> put kr 
    get = Editor <$> get <*> get <*> get <*> get <*> pure emptyDV <*> get <*> get <*> pure Nothing <*> pure Forward <*> pure []

windows :: Editor -> WindowSet Window
windows editor =
    WS.current $ tabs editor

newtype EditorM a = EditorM {fromEditorM :: RWS Config () Editor a}
    deriving (Monad, MonadState Editor, MonadReader Config, Functor)

deriving instance Typeable1 EditorM

class (Monad m, MonadState Editor m) => MonadEditor m
    where askCfg :: m Config
          withEditor :: EditorM a -> m a
          withEditor f = do
              cfg <- askCfg
              getsAndModify (runEditor cfg f) 
    
liftEditor :: MonadEditor m => EditorM a -> m a
liftEditor = withEditor

instance MonadEditor EditorM where
    askCfg = ask
    withEditor = id

pendingEventsA :: Accessor Editor [Event]
pendingEventsA = Accessor pendingEvents (\f e -> e {pendingEvents = f (pendingEvents e)})

statusLinesA :: Accessor Editor (DelayList.DelayList String)
statusLinesA = Accessor statusLines (\f e -> e {statusLines = f (statusLines e)})


refSupplyA :: Accessor Editor Int
refSupplyA = Accessor refSupply (\f e -> e {refSupply = f (refSupply e)})

buffersA :: Accessor Editor (M.Map BufferRef FBuffer)
buffersA = Accessor buffers (\f e -> e {buffers = f (buffers e)})

dynamicA :: Accessor Editor DynamicValues
dynamicA = Accessor dynamic (\f e -> e {dynamic = f (dynamic e)})

windowsA :: Accessor Editor (WindowSet Window)
windowsA = WS.currentA .> tabsA

tabsA :: Accessor Editor (WindowSet (WindowSet Window))
tabsA = Accessor tabs (\f e -> e {tabs = f (tabs e)})

killringA :: Accessor Editor Killring
killringA = Accessor killring (\f e -> e {killring = f (killring e)})

dynA :: Initializable a => Accessor Editor a
dynA = dynamicValueA .> dynamicA

regexA :: Accessor Editor (Maybe SearchExp)
regexA = Accessor regex (\f e -> e{regex = f (regex e)})

searchDirectionA :: Accessor Editor Direction
searchDirectionA = Accessor searchDirection (\f e -> e{searchDirection = f (searchDirection e)})


-- | The initial state
emptyEditor :: Config -> Editor
emptyEditor cfg = Editor {
        buffers      = M.singleton (bkey buf) buf
       ,tabs         = WS.new (WS.new win)
       ,bufferStack  = [bkey buf]
       ,refSupply = 2
       ,tabwidth     = 8
       ,regex        = Nothing
       ,searchDirection = Forward
       ,dynamic      = M.empty
       ,statusLines  = DelayList.insert (maxBound, "") []
       ,killring     = krEmpty
       ,pendingEvents = []
       }
        where buf = newB cfg 0 "*console*" (LazyUTF8.fromString "")
              win = (dummyWindow (bkey buf)) {wkey = 1, isMini = False}

-- ---------------------------------------------------------------------

runEditor :: Config -> EditorM a -> Editor -> (Editor, a)
runEditor cfg f e = let (a, e',()) = runRWS (fromEditorM f) cfg e in (e',a)

-- ---------------------------------------------------------------------
-- Buffer operations

newRef :: EditorM Int
newRef = do
  modifyA refSupplyA (+ 1)
  getA refSupplyA

newBufRef :: EditorM BufferRef
newBufRef = BufferRef <$> newRef

-- | Create and fill a new buffer, using contents of string.
stringToNewBuffer :: String -- ^ The buffer name (*not* the associated file)
                  -> LazyUTF8.ByteString -- ^ The contents with which to populate the buffer
                  -> EditorM BufferRef
stringToNewBuffer nm cs = do
    cfg <- ask
    u <- newBufRef
    b <- insertBuffer (newB cfg u nm cs)
    m <- asks configFundamentalMode
    withGivenBuffer0 b $ setAnyMode m
    return b

insertBuffer :: FBuffer -> EditorM BufferRef
insertBuffer b = getsAndModify $
                 \e -> (e { bufferStack = nub (bkey b : bufferStack e),
                            buffers = M.insert (bkey b) b (buffers e)
                          }, bkey b)

-- | Delete a buffer (and release resources associated with it).
deleteBuffer :: BufferRef -> EditorM ()
deleteBuffer k = do
  bs <- gets bufferStack
  case bs of
      (b0:nextB:_) -> do
          let pickOther = \w -> if bufkey w == k then w {bufkey = other} else w
              other = head (delete k bs)
          when (b0 == k) $ do
              -- we delete the currently selected buffer: the next buffer will become active in
              -- the main window, therefore it must be assigned a new window.
              switchToBufferE nextB
          modify $ \e -> e {
                            bufferStack = filter (k /=) $ bufferStack e,
                            buffers = M.delete k (buffers e),
                            tabs = fmap (fmap pickOther) (tabs e)
                            -- all windows open on that buffer must switch to another buffer.
                           }
      _ -> return () -- Don't delete the last buffer.

-- | Return the buffers we have
getBuffers :: EditorM [FBuffer]
getBuffers = gets (M.elems . buffers)

getBufferStack :: EditorM [FBuffer]
getBufferStack = do
  bufMap <- gets buffers
  gets (fmap (bufMap M.!) . bufferStack)

findBuffer :: BufferRef -> EditorM (Maybe FBuffer)
findBuffer k = gets (M.lookup k . buffers)

-- | Find buffer with this key
findBufferWith :: BufferRef -> Editor -> FBuffer
findBufferWith k e =
    case M.lookup k (buffers e) of
        Just b  -> b
        Nothing -> error "Editor.findBufferWith: no buffer has this key"


-- | Find buffer with this name
findBufferWithName :: String -> Editor -> [BufferRef]
findBufferWithName n e = map bkey $ filter (\b -> name b == n) (M.elems $ buffers e)

-- | Find buffer with given name. Fail if not found.
getBufferWithName :: String -> EditorM BufferRef
getBufferWithName bufName = do
  bs <- gets $ findBufferWithName bufName
  case bs of
    [] -> fail ("Buffer not found: " ++ bufName)
    (b:_) -> return b

------------------------------------------------------------------------

-- | Return the next buffer
nextBuffer :: EditorM BufferRef
nextBuffer = shiftBuffer 1

-- | Return the prev buffer
prevBuffer :: EditorM BufferRef
prevBuffer = shiftBuffer (negate 1)

-- | Return the buffer using a function applied to the current window's
-- buffer's index.
shiftBuffer :: Int -> EditorM BufferRef
shiftBuffer shift = gets $ \e ->
    let bs  = bufferStack e
        n   = shift `mod` length bs
    in (bs !! n)

------------------------------------------------------------------------

-- | Perform action with any given buffer
withGivenBuffer0 :: BufferRef -> BufferM a -> EditorM a
withGivenBuffer0 k f = withGivenBufferAndWindow0 (dummyWindow k) k f

-- | Perform action with any given buffer
withGivenBufferAndWindow0 :: Window -> BufferRef -> BufferM a -> EditorM a
withGivenBufferAndWindow0 w k f = do
  accum <- asks configKillringAccumulate
  getsAndModify $ \e ->
                        let b = findBufferWith k e
                            (v, us, b') = runBufferFull w b f
                            
                        in (e {buffers = mapAdjust' (const b') k (buffers e),
                               killring = (if accum && all updateIsDelete us
                                           then foldl (.) id 
                                                (reverse [krPut dir (LazyUTF8.toString s) | Delete _ dir s <- us])
                                           else id) 
                                          (killring e)
                              },v)


-- | Perform action with current window's buffer
withBuffer0 :: BufferM a -> EditorM a
withBuffer0 f = do
  w <- getA currentWindowA
  withGivenBufferAndWindow0 w (bufkey w) f

currentWindowA :: Accessor Editor Window
currentWindowA = WS.currentA .> windowsA

-- | Return the current buffer
getBuffer :: EditorM BufferRef
getBuffer = gets (head . bufferStack)

-- | Set the current buffer
setBuffer :: BufferRef -> EditorM BufferRef
setBuffer k = do
  b <- gets $ findBufferWith k
  insertBuffer b -- a bit of a hack.

--------------

-- | Display a transient message
printMsg :: String -> EditorM ()
printMsg = setTmpStatus 1

-- | Set the "background" status line 
setStatus :: String -> EditorM ()
setStatus = setTmpStatus maxBound

-- | Clear the status line
msgClr :: EditorM ()
msgClr = setStatus ""

statusLine :: Editor -> String
statusLine = snd . head . statusLines

setTmpStatus :: Int -> String -> EditorM ()
setTmpStatus delay s = do
  modifyA statusLinesA $ DelayList.insert (delay, 
                                           takeWhile (/= '\n') s)
  -- also show in the messages buffer, so we don't loose any message
  bs <- gets $ findBufferWithName "*messages*"
  b <- case bs of
         (b':_) -> return b'
         [] -> newBufferE "*messages*" (fromString "")
  withGivenBuffer0 b $ do botB; insertN (s ++ "\n")


-- ---------------------------------------------------------------------
-- Register interface to killring.

-- | Put string into yank register
setRegE :: String -> EditorM ()
setRegE s = modifyA killringA $ krSet s

-- | Return the contents of the yank register
getRegE :: EditorM String
getRegE = getsA killringA krGet

-- ---------------------------------------------------------------------
-- | Dynamically-extensible state components.
--
-- These hooks are used by keymaps to store values that result from
-- Actions (i.e. that restult from IO), as opposed to the pure values
-- they generate themselves, and can be stored internally.
--
-- The `dynamic' field is a type-indexed map.
--

-- | Retrieve a value from the extensible state
getDynamic :: Initializable a => EditorM a
getDynamic = getA (dynamicValueA .> dynamicA)

-- | Insert a value into the extensible state, keyed by its type
setDynamic :: Initializable a => a -> EditorM ()
setDynamic x = setA (dynamicValueA .> dynamicA) x

-- | Attach the next buffer in the buffer list
-- to the current window.
nextBufW :: EditorM ()
nextBufW = nextBuffer >>= switchToBufferE

-- | edit the previous buffer in the buffer list
prevBufW :: EditorM ()
prevBufW = prevBuffer >>= switchToBufferE

-- | Like fnewE, create a new buffer filled with the String @s@,
-- Switch the current window to this buffer. Doesn't associate any file
-- with the buffer (unlike fnewE) and so is good for popup internal
-- buffers (like scratch)
newBufferE :: String   -- ^ buffer name
              -> LazyUTF8.ByteString -- ^ buffer contents
              -> EditorM BufferRef
newBufferE f s = do
    b <- stringToNewBuffer f s
    switchToBufferE b
    return b

-- | Create a new window onto the current buffer.
newWindowE :: Bool -> BufferRef -> EditorM Window
newWindowE mini bk = do
  k <- newRef
  return $ Window mini bk 0 k

-- | Attach the specified buffer to the current window
switchToBufferE :: BufferRef -> EditorM ()
switchToBufferE bk = do
    modifyWindows (modifier WS.currentA (\w -> w { bufkey = bk }))

-- | Attach the specified buffer to some other window than the current one
switchToBufferOtherWindowE :: BufferRef -> EditorM ()
switchToBufferOtherWindowE b = shiftOtherWindow >> switchToBufferE b

-- | Switch to the buffer specified as parameter. If the buffer name is empty, switch to the next buffer.
switchToBufferWithNameE :: String -> EditorM ()
switchToBufferWithNameE "" = nextBufW
switchToBufferWithNameE bufName = switchToBufferE =<< getBufferWithName bufName

-- | Return a list of all buffers, and their indicies
listBuffersE :: EditorM [(String,Int)]
listBuffersE = do
        bs  <- getBuffers
        return $ zip (map name bs) [0..]

-- | Close a buffer.
-- Note: close the current buffer if the empty string is given
closeBufferE :: String -> EditorM ()
closeBufferE nm = deleteBuffer =<< getBufferWithNameOrCurrent nm

getBufferWithNameOrCurrent :: String -> EditorM BufferRef
getBufferWithNameOrCurrent nm = if null nm then getBuffer else getBufferWithName nm


------------------------------------------------------------------------

-- | Close current buffer and window, unless it's the last one.
closeBufferAndWindowE :: EditorM ()
closeBufferAndWindowE = do
  deleteBuffer =<< getBuffer
  tryCloseE

-- | Rotate focus to the next window
nextWinE :: EditorM ()
nextWinE = modifyWindows WS.forward

-- | Rotate focus to the previous window
prevWinE :: EditorM ()
prevWinE = modifyWindows WS.backward

-- | Apply a function to the windowset.
modifyWindows :: (WindowSet Window -> WindowSet Window) -> EditorM ()
modifyWindows f = do
  b <- getsAndModifyA windowsA $ \ws -> let ws' = f ws in (ws', bufkey $ WS.current ws')
  -- TODO: push this fiddling with current buffer into windowsA
  setBuffer b
  return ()

withWindows :: (WindowSet Window -> a) -> EditorM a
withWindows = getsA windowsA

withWindow :: (Window -> a) -> EditorM a
withWindow f = getsA (WS.currentA .> windowsA) f

-- | Split the current window, opening a second window onto current buffer.
-- TODO: unfold newWindowE here?
splitE :: EditorM ()
splitE = do
  b <- getBuffer
  w <- newWindowE False b
  modifyWindows (WS.add w)


-- | Enlarge the current window
enlargeWinE :: EditorM ()
enlargeWinE = error "enlargeWinE: not implemented"

-- | Shrink the current window
shrinkWinE :: EditorM ()
shrinkWinE = error "shrinkWinE: not implemented"

-- | Creates a new tab containing a window that views the current buffer.
newTabE :: EditorM ()
newTabE = do
    bk <- getBuffer
    k <- newRef
    let win = Window False bk 0 k
    modifyA tabsA (WS.add (WS.new win))

-- | Moves to the next tab in the round robin set of tabs
nextTabE :: EditorM ()
nextTabE = modifyA tabsA WS.forward

-- | Moves to the previous tab in the round robin set of tabs
previousTabE :: EditorM ()
previousTabE = modifyA tabsA WS.backward

-- | Close the current window. If there is only one tab open and the tab 
-- contains only one window then do nothing.
tryCloseE :: EditorM ()
tryCloseE = do
    n <- getsA windowsA WS.size
    if n == 1
        then modifyA tabsA WS.delete
        else modifyA windowsA WS.delete

-- | Make the current window the only window on the screen
closeOtherE :: EditorM ()
closeOtherE = modifyWindows WS.deleteOthers

-- | Switch focus to some other window. If none is available, create one.
shiftOtherWindow :: MonadEditor m => m ()
shiftOtherWindow = liftEditor $ do
  len <- withWindows WS.size
  if (len == 1) 
    then splitE
    else nextWinE

-- | Execute the argument in the context of an other window. Create
-- one if necessary. The current window is re-focused after the
-- argument has completed.
withOtherWindow :: MonadEditor m => m () -> m ()
withOtherWindow f = do
  shiftOtherWindow
  f
  liftEditor prevWinE


