--
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--

--
-- | The top level editor state, and operations on it.
--

module Yi.Editor where

import Yi.Buffer                ( BufferRef, FBuffer (..), BufferM, newB, hNewB, runBuffer )
import Text.Regex.Posix.Wrap    ( Regex )
import Yi.Style                 ( uiStyle, UIStyle )

import Yi.Debug
import Yi.Undo
import Prelude hiding (error)

import Data.List                ( nub )
import Data.Unique             
import Data.Dynamic
import Data.IORef
import qualified Data.Map as M

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Yi.Monad

------------------------------------------------------------------------

-- | The Editor state
data Editor = Editor {
        bufferStack   :: ![BufferRef]               -- ^ Stack of all the buffers. Never empty; 
                                                    -- first buffer is the current one.
       ,buffers       :: M.Map BufferRef FBuffer
       ,uistyle       :: !UIStyle                   -- ^ ui colours
       ,dynamic       :: !(M.Map String Dynamic)    -- ^ dynamic components

       ,windowfill    :: !Char                      -- ^ char to fill empty window space with
       ,tabwidth      :: !Int                       -- ^ width of tabs

       ,yreg          :: !String                    -- ^ yank register
       ,regex         :: !(Maybe (String,Regex))    -- ^ most recent regex
       -- should be moved into dynamic component, perhaps

       ,editorUpdates :: [(Unique, Update)]
    }

--
-- | The initial state
--

emptyEditor :: FBuffer -> Editor
emptyEditor buf = Editor {
        buffers      = M.singleton (bkey buf) buf
       ,bufferStack  = [bkey buf]
       ,windowfill   = ' '
       ,tabwidth     = 8 
       ,yreg         = []
       ,regex        = Nothing
       ,uistyle      = Yi.Style.uiStyle
       ,dynamic      = M.empty
       
       ,editorUpdates = []
    }

-- ---------------------------------------------------------------------

--
-- | Read the editor state, with a pure action
--
readEditor :: (Editor -> b) -> EditorM b
readEditor f = do
  e <- ask
  liftM f (readRef e)

-- | Modify the contents, using an IO action.
modifyEditor_ :: (Editor -> IO Editor) -> EditorM ()
modifyEditor_ f = do
  e <- ask
  e' <- lift (f =<< readIORef e)
  writeRef e e'  

-- | Variation on modifyEditor_ that lets you return a value
modifyEditor :: (Editor -> IO (Editor,b)) -> EditorM b
modifyEditor f = do
  e <- ask
  (e',result) <- lift (f =<< readIORef e)
  writeRef e e'
  return result

withEditor0 :: (Editor -> IO a) -> EditorM a
withEditor0 f = do
  e <- ask
  lift $ f =<< readIORef e

-- ---------------------------------------------------------------------
-- Buffer operations
--
-- | Create a new buffer filling with contents of file.
--
hNewBuffer :: FilePath -> EditorM FBuffer
hNewBuffer f = do
    b <- lift $ hNewB f
    insertBuffer b

-- | Create and fill a new buffer, using contents of string.
stringToNewBuffer :: String -> String -> EditorM FBuffer
stringToNewBuffer nm cs = do
    logPutStrLn $ "stringToNewBuffer: " ++ show nm
    u <- liftIO $ newUnique
    let b = newB u nm cs
    insertBuffer b

insertBuffer :: FBuffer -> EditorM FBuffer
insertBuffer b = do
  modifyEditor $ \e -> do
                     return (e { bufferStack = nub $ (bkey b : bufferStack e),
                                 buffers = M.insert (bkey b) b (buffers e)
                               }, b)

deleteBuffer :: BufferRef -> EditorM ()
deleteBuffer k = do
  bs <- readEditor bufferStack
  when (length bs > 0) $ do -- never delete the last buffer.
    modifyEditor_ $ \e-> return e { bufferStack = filter (k /=) $ bufferStack e,
                                    buffers = M.delete k (buffers e)
                                  }

-- | Return the buffers we have
getBuffers :: EditorM [FBuffer]
getBuffers = readEditor (M.elems . buffers)

-- | Find buffer with this key
findBufferWith :: Editor -> BufferRef -> FBuffer
findBufferWith e k =
    case M.lookup k (buffers e) of
        Just b  -> b
        Nothing -> error "Editor.findBufferWith: no buffer has this key"

-- | Find buffer with this name
findBufferWithName :: Editor -> String -> [BufferRef]
findBufferWithName e n = map bkey $ filter (\b -> name b == n) (M.elems $ buffers e)


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
shiftBuffer shift = readEditor $ \e ->
    let bs  = bufferStack e
        n   = shift `mod` length bs
    in (bs !! n)

------------------------------------------------------------------------

-- | Perform action with any given buffer    
withGivenBuffer0 :: BufferRef -> BufferM a -> EditorM a
withGivenBuffer0 k f = modifyEditor $ \e -> do
                        let b = findBufferWith e k
                        let (v, b', updates) = runBuffer b f 
                        return (e {editorUpdates = editorUpdates e ++ [(bkey b,u) | u <- updates],
                                   buffers = M.adjust (const b') k (buffers e)
                                  },v)

-- | Perform action with current window's buffer
withBuffer0 :: BufferM a -> EditorM a
withBuffer0 f = do 
  b <- readEditor $ \e -> head $ bufferStack e
  withGivenBuffer0 b f
                                                
-- | Return the current buffer
getBuffer :: EditorM BufferRef
getBuffer = withEditor0 $ \e -> return (head . bufferStack $ e)

-- | Set the current buffer
setBuffer :: Unique -> EditorM FBuffer
setBuffer k = do
  b <- withEditor0 $ \e -> return $ findBufferWith e k
  insertBuffer b -- a bit of a hack.

-- ---------------------------------------------------------------------

type EditorM = ReaderT (IORef Editor) IO


