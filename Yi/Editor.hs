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
import Yi.Event
import Yi.Debug
import Yi.Undo
import Prelude hiding (error)

import Data.List                ( elemIndex )
import Data.Unique              ( Unique, hashUnique )
import Data.Dynamic
import Data.IORef
import qualified Data.Map as M

import Control.Concurrent       ( killThread, ThreadId )
import Control.Monad.Reader
import Control.Monad.Writer

------------------------------------------------------------------------

--
-- | The Editor state
--
data Editor = Editor {
        buffers       :: !(M.Map Unique FBuffer)    -- ^ all the buffers
       ,windows       :: !(M.Map Unique Window)     -- ^ all the windows

       ,curwin        :: !(Maybe Unique)            -- ^ the window with focus
       ,uistyle       :: !UIStyle                   -- ^ ui colours
       ,reboot        :: (Maybe Editor) -> IO ()    -- ^ our reboot function
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
emptyEditor :: Editor
emptyEditor = Editor {
        buffers      = M.empty
       ,windows      = M.empty

       ,windowfill   = ' '
       ,tabwidth     = 8        -- has to be for now
       ,yreg         = []
       ,regex        = Nothing
       ,curwin       = Nothing
       ,uistyle      = Yi.Style.uiStyle
       ,reboot       = const $ return ()
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
  lift $ liftM f (readIORef e)

-- | Modify the contents, using an IO action.
modifyEditor_ :: (Editor -> IO Editor) -> EditorM ()
modifyEditor_ f = do
  e <- ask
  e' <- lift $ (f =<< readIORef e)
  lift $ writeIORef e e'  

-- | Variation on modifyEditor_ that lets you return a value
modifyEditor :: (Editor -> IO (Editor,b)) -> EditorM b
modifyEditor f = do
  e <- ask
  (e',result) <- lift (f =<< readIORef e)
  lift $ writeIORef e e'
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
    lift $ logPutStrLn $ "stringToNewBuffer: " ++ show nm
    b <- lift $ newB nm cs
    insertBuffer b

insertBuffer :: FBuffer -> EditorM FBuffer
insertBuffer b = do
  modifyEditor $ \e@(Editor{buffers=bs}) -> do
                     let e' = e { buffers = M.insert (keyB b) b bs } :: Editor
                     return (e', b)

deleteBuffer :: FBuffer -> EditorM ()
deleteBuffer b = do
  lift $ runBuffer b finaliseB
  modifyEditor_ $ \e-> return e { buffers = M.delete (bkey b) (buffers e)}

------------------------------------------------------------------------
--
-- | return the buffers we have
-- TODO we need to order the buffers some how.
--
getBuffers :: EditorM [FBuffer]
getBuffers = readEditor $ M.elems . buffers


--
-- | Find buffer with this key
--
findBufferWith :: Editor -> Unique -> FBuffer
findBufferWith e k =
    case M.lookup k (buffers e) of
        Just b  -> b
        Nothing -> error "Editor.findBufferWith: no buffer has this key"

-- | Find buffer with this name
findBufferWithName :: Editor -> String -> [FBuffer]
findBufferWithName e n = filter (\b -> name b == n) (M.elems $ buffers e)

--
-- | Find the buffer connected to this window
--
win2buf :: Window -> Editor -> FBuffer
win2buf w e = findBufferWith e (bufkey w)

--
-- | Safely lookup buffer using it's key.
--
getBufferWith :: Unique -> EditorM FBuffer
getBufferWith u = readEditor $ \e -> findBufferWith (e :: Editor) u

------------------------------------------------------------------------

-- | Return the next buffer
nextBuffer :: EditorM FBuffer
nextBuffer = shiftBuffer (+1)

-- | Return the prev buffer
prevBuffer :: EditorM FBuffer
prevBuffer = shiftBuffer (subtract 1)

-- | Return the nth buffer in the buffer list, module buffer count
bufferAt :: Int -> EditorM FBuffer
bufferAt n = shiftBuffer (const n)

-- | Return the buffer using a function applied to the current window's
-- buffer's index.
shiftBuffer :: (Int -> Int) -> EditorM FBuffer
shiftBuffer f = readEditor $ \e ->
    let bs  = M.elems $ buffers (e :: Editor)
        win = findWindowWith e (curwin e)
        buf = findBufferWith e (bufkey win)
    in case elemIndex buf bs of
        Nothing -> error "Editor: current buffer has been lost."
        Just i -> let l = length bs in bs !! ((f i) `mod` l)


------------------------------------------------------------------------
    
deleteWindow' :: Window -> EditorM ()
deleteWindow' win = (modifyEditor_ $ \e -> do
    logPutStrLn $ "Deleting window #" ++ show (hashUnique $ key win)
    let ws = M.delete (key win) (windows e) -- delete window
    return e { windows = ws }) >> debugWindows "After deletion"
  

debugWindows :: String -> EditorM ()
debugWindows msg = do 
  ws <- readEditor getWindows
  w <- readEditor curwin
  lift $ logPutStrLn $ msg ++ ": editor windows: " ++ show ws ++ " current window " ++ show (fmap hashUnique w)

killAllBuffers :: IO ()
killAllBuffers = error "killAllBuffers undefined"


-- | turn a list of windows into an association list suitable for fromList
mkAssoc :: [Window] -> [(Unique,Window)]
mkAssoc []     = []
mkAssoc (w:ws) = (key w, w) : mkAssoc ws

-- ---------------------------------------------------------------------
-- | Get all the windows
-- TODO by key
--
getWindows :: Editor -> [Window]
getWindows = M.elems . windows

-- | Get current window
getWindow :: EditorM (Maybe Window)
getWindow = readEditor getWindowOf


--
-- | Get window, from the given editor state.
--
getWindowOf :: Editor -> (Maybe Window)
getWindowOf e = case curwin e of
                    Nothing -> Nothing
                    k       -> Just $ findWindowWith e k

--
-- | How many windows do we have
--
sizeWindows :: EditorM Int
sizeWindows = readEditor $ \e -> length $ M.elems (windows e)

--
-- | Find the window with this key
--
findWindowWith :: Editor -> (Maybe Unique) -> Window
findWindowWith _ Nothing  = error "Editor: no current window!"
findWindowWith e (Just k) =
    case M.lookup k (windows e) of
            Just w  -> w
            Nothing -> error $ "Editor: no window has key #" ++ (show (hashUnique k))

------------------------------------------------------------------------
--
-- | Perform action with current window
--
withWindow0 :: (Window -> FBuffer -> IO a) -> EditorM a
withWindow0 f = modifyEditor $ \e -> do
        let w = findWindowWith e (curwin e)
            b = findBufferWith e (bufkey w)
        v <- f w b
        return (e,v)


-- | Perform action with current window's buffer
withGivenBuffer0 :: FBuffer -> BufferM a -> EditorM a
withGivenBuffer0 b f = modifyEditor $ \e -> do
                        (v,updates) <- runBuffer b f
                        return (e {editorUpdates = editorUpdates e ++ [(bkey b,u) | u <- updates]},v)

withBuffer0 :: BufferM a -> EditorM a
withBuffer0 f = do 
  b <- readEditor $ \e -> findBufferWith e (bufkey $ findWindowWith e (curwin e))
  withGivenBuffer0 b f                                                

-- | Perform action with current window's buffer
withBuffer' :: (FBuffer -> IO a) -> EditorM a
withBuffer' f = withWindow0 (const f)

-- | Return the current buffer
getBuffer :: EditorM FBuffer
getBuffer = withBuffer0 ask


-- ---------------------------------------------------------------------




type EditorM = ReaderT (IORef Editor) IO


