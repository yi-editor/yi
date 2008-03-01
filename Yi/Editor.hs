{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Copyright (c) 2004-5, 8, Don Stewart - http://www.cse.unsw.edu.au/~dons

-- | The top level editor state, and operations on it.

module Yi.Editor

{-
        -- * Buffer only stuff
        newBufferE,     -- :: String -> String -> YiM ()
        listBuffersE,   -- :: YiM ()
        closeBufferE,   -- :: String -> YiM ()

        -- * Buffer/Window
        closeBufferAndWindowE,
        switchToBufferE,
        switchToBufferOtherWindowE,
        switchToBufferWithNameE,
        nextBufW,       -- :: YiM ()
        prevBufW,       -- :: YiM ()

        -- * Basic registers
        setRegE,        -- :: String -> YiM ()
        getRegE,        -- :: EditorM String

        -- * Dynamically extensible state
        getDynamic,
        setDynamic,

        setWindowStyleE,-- :: UIStyle -> EditorM ()
-}

where

import Yi.Buffer                ( BufferRef, FBuffer (..), BufferM, newB, runBuffer, insertN )
import Yi.Buffer.HighLevel (botB)
import Text.Regex.Posix.Wrap    ( Regex )
import Yi.Style                 ( uiStyle, UIStyle )

import Yi.Debug
import Yi.Monad
import Yi.Accessor
import Yi.Dynamic
import Yi.KillRing
import Yi.Window
import Yi.WindowSet (WindowSet)
import qualified Yi.WindowSet as WS

import Prelude hiding (error)

import Data.List                ( nub )
import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Writer


-- | The Editor state
data Editor = Editor {
        bufferStack   :: ![BufferRef]               -- ^ Stack of all the buffers. Never empty;
                                                    -- first buffer is the current one.
       ,buffers       :: M.Map BufferRef FBuffer
       ,bufferRefSupply :: BufferRef

       ,windows       :: WindowSet Window

       ,uistyle       :: !UIStyle                   -- ^ ui colours
       ,dynamic       :: DynamicValues              -- ^ dynamic components

       ,windowfill    :: !Char                      -- ^ char to fill empty window space with
       ,tabwidth      :: !Int                       -- ^ width of tabs

       ,statusLine    :: !String
       ,killring      :: !Killring
       ,regex         :: !(Maybe (String,Regex))    -- ^ most recent regex
    }


bufferRefSupplyA :: Accessor Editor BufferRef
bufferRefSupplyA = Accessor bufferRefSupply (\f e -> e {bufferRefSupply = f (bufferRefSupply e)})

buffersA :: Accessor Editor (M.Map BufferRef FBuffer)
buffersA = Accessor buffers (\f e -> e {buffers = f (buffers e)})

dynamicA :: Accessor Editor DynamicValues
dynamicA = Accessor dynamic (\f e -> e {dynamic = f (dynamic e)})

windowsA :: Accessor Editor (WindowSet Window)
windowsA = Accessor windows (\f e -> e {windows = f (windows e)})

killringA :: Accessor Editor Killring
killringA = Accessor killring (\f e -> e {killring = f (killring e)})

dynA :: Initializable a => Accessor Editor a
dynA = dynamicValueA .> dynamicA

-- | The initial state
emptyEditor :: Editor
emptyEditor = Editor {
        buffers      = M.singleton (bkey buf) buf
       ,windows      = WS.new (dummyWindow $ bkey buf)
       ,bufferStack  = [bkey buf]
       ,bufferRefSupply = 1
       ,windowfill   = ' '
       ,tabwidth     = 8
       ,regex        = Nothing
       ,uistyle      = Yi.Style.uiStyle
       ,dynamic      = M.empty
       ,statusLine   = ""
       ,killring     = krEmpty
       }
        where buf = newB 0 "*console*" ""

-- ---------------------------------------------------------------------

runEditor :: EditorM a -> Editor -> (a, Editor)
runEditor = runState . fromEditorM

-- ---------------------------------------------------------------------
-- Buffer operations

newBufferRef :: EditorM BufferRef
newBufferRef = do
  modifyA bufferRefSupplyA (+ 1)
  getA bufferRefSupplyA

-- | Create and fill a new buffer, using contents of string.
stringToNewBuffer :: String -- ^ The buffer name (*not* the associated file)
                  -> String -- ^ The contents with which to populate the buffer
                  -> EditorM BufferRef
stringToNewBuffer nm cs = do
    u <- newBufferRef
    insertBuffer (newB u nm cs)

insertBuffer :: FBuffer -> EditorM BufferRef
insertBuffer b = getsAndModify $
                 \e -> (e { bufferStack = nub (bkey b : bufferStack e),
                            buffers = M.insert (bkey b) b (buffers e)
                          }, bkey b)

deleteBuffer :: BufferRef -> EditorM ()
deleteBuffer k = do
  bs <- gets bufferStack
  when (length bs > 1) $ do -- never delete the last buffer.
    modify $ \e -> e { bufferStack = filter (k /=) $ bufferStack e,
                       buffers = M.delete k (buffers e)
                     }

-- | Return the buffers we have
getBuffers :: EditorM [FBuffer]
getBuffers = gets (M.elems . buffers)


-- | Check that the buffer has not been deleted
bufferExists :: BufferRef -> EditorM Bool
bufferExists k = gets ((M.member k) . buffers)

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
withGivenBufferAndWindow0 w k f = getsAndModify $ \e ->
                        let b = findBufferWith k e
                            (v, b') = runBuffer w b f
                        in (e {buffers = M.adjust (const b') k (buffers e)},v)


-- | Perform action with current window's buffer
withBuffer0 :: BufferM a -> EditorM a
withBuffer0 f = do
  w <- getA (WS.currentA .> windowsA)
  withGivenBufferAndWindow0 w (bufkey w) f

-- | Return the current buffer
getBuffer :: EditorM BufferRef
getBuffer = gets (head . bufferStack)

-- | Set the current buffer
setBuffer :: BufferRef -> EditorM BufferRef
setBuffer k = do
  b <- gets $ findBufferWith k
  insertBuffer b -- a bit of a hack.

-- ---------------------------------------------------------------------

newtype EditorM a = EditorM {fromEditorM :: State Editor a}
    deriving (Monad, MonadState Editor)

--------------

-- | Set the cmd buffer, and draw message at bottom of screen
printMsg :: String -> EditorM ()
printMsg s = do
  modify $ \e -> e { statusLine = takeWhile (/= '\n') s }
  -- also show in the messages buffer, so we don't loose any message
  bs <- gets $ findBufferWithName "*messages*"
  b <- case bs of
         (b':_) -> return b'
         [] -> newBufferE "*messages*" ""
  withGivenBuffer0 b $ do botB; insertN (s ++ "\n")


-- ---------------------------------------------------------------------
-- registers (TODO these may be redundant now that it is easy to thread
-- state in key bindings, or maybe not.
--

-- | Put string into yank register
setRegE :: String -> EditorM ()
setRegE s = modifyA killringA $ krPut s

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

-- | A character to fill blank lines in windows with. Usually '~' for
-- vi-like editors, ' ' for everything else
setWindowFillE :: Char -> EditorM ()
setWindowFillE c = modify $ \e -> e { windowfill = c }

-- | Sets the window style.
setWindowStyleE :: UIStyle -> EditorM ()
setWindowStyleE sty = modify $ \e -> e { uistyle = sty }


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
newBufferE :: String -> -- ^ buffer name
              String -> -- ^ buffer contents
                  EditorM BufferRef
newBufferE f s = do
    b <- stringToNewBuffer f s
    switchToBufferE b
    return b

-- | Attach the specified buffer to the current window
switchToBufferE :: BufferRef -> EditorM ()
switchToBufferE b = modifyWindows (modifier WS.currentA (\w -> w {bufkey = b}))

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

-- | Release resources associated with buffer
-- Note: close the current buffer if the empty string is given
closeBufferE :: String -> EditorM ()
closeBufferE bufName = do
  nextB <- nextBuffer
  b <- getBuffer
  b' <- if null bufName then return b else getBufferWithName bufName
  switchToBufferE nextB
  deleteBuffer b'

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
splitE :: EditorM ()
splitE = do
  b <- getBuffer
  modifyWindows (WS.add $ dummyWindow b)


-- | Enlarge the current window
enlargeWinE :: EditorM ()
enlargeWinE = error "enlargeWinE: not implemented"

-- | Shrink the current window
shrinkWinE :: EditorM ()
shrinkWinE = error "shrinkWinE: not implemented"


-- | Close the current window, unless it is the last window open.
tryCloseE :: EditorM ()
tryCloseE = modifyWindows WS.delete

-- | Make the current window the only window on the screen
closeOtherE :: EditorM ()
closeOtherE = modifyWindows WS.deleteOthers

-- | Switch focus to some other window. If none is available, create one.
shiftOtherWindow :: EditorM ()
shiftOtherWindow = do
  len <- withWindows WS.size
  when (len == 1) splitE
  nextWinE
