{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Copyright (c) 2004-5, 8, Don Stewart - http://www.cse.unsw.edu.au/~dons

-- | The top level editor state, and operations on it.

module Yi.Editor where

import Yi.Buffer                ( BufferRef, FBuffer (..), BufferM, newB, runBuffer )
import Text.Regex.Posix.Wrap    ( Regex )
import Yi.Style                 ( uiStyle, UIStyle )

import Yi.Debug
import Yi.Monad
import Yi.Accessor
import Yi.Dynamic
import Yi.Window
import Yi.WindowSet

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


       -- consider make the below fields part of dynamic component
       ,statusLine    :: !String
       ,yreg          :: !String                    -- ^ yank register
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

-- | The initial state
emptyEditor :: Editor
emptyEditor = Editor {
        buffers      = M.singleton (bkey buf) buf
       ,windows      = new (dummyWindow $ bkey buf)
       ,bufferStack  = [bkey buf]
       ,bufferRefSupply = 1
       ,windowfill   = ' '
       ,tabwidth     = 8
       ,yreg         = []
       ,regex        = Nothing
       ,uistyle      = Yi.Style.uiStyle
       ,dynamic      = M.empty
       ,statusLine   = ""
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
                 \e -> (e { bufferStack = nub $ (bkey b : bufferStack e),
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

-- | Find buffer with this key
findBufferWith :: BufferRef -> Editor -> FBuffer
findBufferWith k e =
    case M.lookup k (buffers e) of
        Just b  -> b
        Nothing -> error "Editor.findBufferWith: no buffer has this key"

-- | Find buffer with this name
findBufferWithName :: String -> Editor -> [BufferRef]
findBufferWithName n e = map bkey $ filter (\b -> name b == n) (M.elems $ buffers e)


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
  w <- getA (currentA .> windowsA)
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


