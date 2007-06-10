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

import Yi.Buffer                ( FBuffer (..), BufferM, newB, keyB, hNewB, runBuffer, finaliseB )
import Text.Regex.Posix.Wrap    ( Regex )
import Yi.Window
import Yi.Style                 ( uiStyle, UIStyle )

import Yi.Debug
import Yi.Undo
import Prelude hiding (error)

import Data.List                ( nub, delete, find )
import Data.Unique              ( Unique )

import Data.Dynamic
import Data.IORef
import qualified Data.Map as M

import Control.Monad.Reader
import Control.Monad.Writer
import Yi.Monad

------------------------------------------------------------------------

-- | The Editor state
data Editor = Editor {
        buffers       :: ![FBuffer]                 -- ^ all the buffers. Never empty; first buffer is the current one.
       ,uistyle       :: !UIStyle                   -- ^ ui colours
       ,dynamic       :: !(M.Map String Dynamic)    -- ^ dynamic components

       ,windowfill    :: !Char                      -- ^ char to fill empty window space with
       ,tabwidth      :: !Int                       -- ^ width of tabs

       ,yreg          :: !String                    -- ^ yank register
       ,regex         :: !(Maybe (String,Regex))    -- ^ most recent regex
       -- should be moved into dynamic component, perhaps

       ,editorUpdates :: [(Unique, URAction)]
    }

--
-- | The initial state
--

emptyEditor :: FBuffer -> Editor
emptyEditor buf = Editor {
        buffers      = [buf]

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
    b <- lift $ newB nm cs
    insertBuffer b

insertBuffer :: FBuffer -> EditorM FBuffer
insertBuffer b = do
  modifyEditor $ \e@(Editor{buffers=bs}) -> do
                     return (e { buffers = nub $ (b:bs) }, b)

deleteBuffer :: FBuffer -> EditorM ()
deleteBuffer b = do
  bs <- readEditor buffers
  when (length bs > 0) $ do -- never delete the last buffer.
    lift $ runBuffer b finaliseB
    modifyEditor_ $ \e-> return e { buffers = delete b $ buffers e}

-- | Return the buffers we have
getBuffers :: EditorM [FBuffer]
getBuffers = readEditor buffers

-- | Find buffer with this key
findBufferWith :: Editor -> Unique -> FBuffer
findBufferWith e k =
    case find ((== k) . keyB) (buffers e) of
        Just b  -> b
        Nothing -> error "Editor.findBufferWith: no buffer has this key"

-- | Find buffer with this name
findBufferWithName :: Editor -> String -> [FBuffer]
findBufferWithName e n = filter (\b -> name b == n) (buffers e)


------------------------------------------------------------------------

-- | Return the next buffer
nextBuffer :: EditorM FBuffer
nextBuffer = shiftBuffer 1

-- | Return the prev buffer
prevBuffer :: EditorM FBuffer
prevBuffer = shiftBuffer (negate 1)

-- | Return the buffer using a function applied to the current window's
-- buffer's index.
shiftBuffer :: Int -> EditorM FBuffer
shiftBuffer shift = readEditor $ \e ->
    let bs  = buffers e
        n   = shift `mod` length bs
    in (bs !! n)

------------------------------------------------------------------------
    
killAllBuffers :: IO ()
killAllBuffers = error "killAllBuffers undefined"


-- | turn a list of windows into an association list suitable for fromList
mkAssoc :: [Window] -> [(Unique,Window)]
mkAssoc []     = []
mkAssoc (w:ws) = (key w, w) : mkAssoc ws

-- | Perform action with current window's buffer
withGivenBuffer0 :: FBuffer -> BufferM a -> EditorM a
withGivenBuffer0 b f = modifyEditor $ \e -> do
                        (v,updates) <- runBuffer b f
                        return (e {editorUpdates = editorUpdates e ++ [(bkey b,u) | u <- updates]},v)

withBuffer0 :: BufferM a -> EditorM a
withBuffer0 f = do 
  b <- readEditor $ \e -> head $ buffers e
  withGivenBuffer0 b f                                                

-- | Return the current buffer
getBuffer :: EditorM FBuffer
getBuffer = withBuffer0 ask

setBuffer :: Unique -> EditorM FBuffer
setBuffer k = do
  b <- withEditor0 $ \e -> return $ findBufferWith e k
  insertBuffer b

-- ---------------------------------------------------------------------

type EditorM = ReaderT (IORef Editor) IO


