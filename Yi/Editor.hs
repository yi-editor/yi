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

import Yi.Buffer                ( FBuffer (..), newB, keyB, hNewB, nameB, finaliseB )
import Text.Regex.Posix.Wrap    ( Regex )
import Yi.Window
import Yi.Style                 ( uiStyle, UIStyle )
import Yi.Event
import Yi.Debug
import Yi.Kernel
import qualified Yi.Interact as I
import Prelude hiding (error)

import Data.List                ( elemIndex )
import Data.Unique              ( Unique, hashUnique )
import Data.Dynamic
import Data.IORef
import qualified Data.Map as M

import Control.Concurrent       ( killThread, ThreadId )
import Control.Concurrent.Chan
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Concurrent   ( forkIO )
import Control.Exception

import {-# source #-} Yi.UI as UI ( UI )

------------------------------------------------------------------------

--
-- | The Editor state
--
data Editor = Editor {
        buffers       :: !(M.Map Unique FBuffer)    -- ^ all the buffers
       ,windows       :: !(M.Map Unique Window)     -- ^ all the windows

       ,ui            :: UI

       ,curwin        :: !(Maybe Unique)            -- ^ the window with focus
       ,uistyle       :: !UIStyle                   -- ^ ui colours
       ,input         :: Chan Event                 -- ^ input stream
       ,output        :: Chan Action                -- ^ output stream
       ,threads       :: [ThreadId]                 -- ^ all our threads
       ,reboot        :: (Maybe Editor) -> IO ()    -- ^ our reboot function
       ,dynamic       :: !(M.Map String Dynamic)    -- ^ dynamic components

       ,windowfill    :: !Char                      -- ^ char to fill empty window space with
       ,tabwidth      :: !Int                       -- ^ width of tabs

       ,yreg          :: !String                    -- ^ yank register
       ,regex         :: !(Maybe (String,Regex))    -- ^ most recent regex
       -- should be moved into dynamic component, perhaps

       ,defaultKeymap :: Keymap

       ,editorKernel  :: Kernel
       ,editorModules :: [String] -- ^ modules requested by user: (e.g. ["YiConfig", "Hoogle"])
    }

type EditorM = ReaderT (IORef Editor) IO

--
-- | The initial state
--
emptyEditor :: Editor
emptyEditor = Editor {
        buffers      = M.empty
       ,windows      = M.empty

       ,ui           = error "UI not initialized"

       ,windowfill   = ' '
       ,tabwidth     = 8        -- has to be for now
       ,yreg         = []
       ,regex        = Nothing
       ,curwin       = Nothing
       ,defaultKeymap = error "No keymap defined."
       ,uistyle      = Yi.Style.uiStyle
       ,input        = error "No input channel open"
       ,output       = error "No output channel open"
       ,threads      = []
       ,reboot       = const $ return ()
       ,dynamic      = M.empty

       ,editorKernel = error "GHC Kernel not initialized"
       ,editorModules = []
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

withEditor :: (Editor -> IO a) -> EditorM a
withEditor f = do
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
    insertBuffer b id

-- | Create and fill a new buffer, using contents of string.
stringToNewBuffer :: FilePath -> String -> KeymapMod -> EditorM FBuffer
stringToNewBuffer f cs modKm = do
    lift $ logPutStrLn $ "stringToNewBuffer: " ++ show f
    b <- lift $ newB f cs
    insertBuffer b modKm

insertBuffer :: FBuffer -> KeymapMod -> EditorM FBuffer
insertBuffer b km = do
  editor <- ask
  thread <- lift $ forkIO (bufferEventLoop editor b km) -- FIXME: kill this thread when the buffer dies.  
  let b' = b {bufferThread = Just thread}
  modifyEditor $ \e@(Editor{buffers=bs}) -> do
                     let e' = e { buffers = M.insert (keyB b) b' bs } :: Editor
                     return (e', b')

bufferEventLoop :: IORef Editor -> FBuffer -> KeymapMod -> IO ()
bufferEventLoop e b modKm = eventLoop 
  where
    handler exception = logPutStrLn $ "Buffer event loop crashed with: " ++ (show exception)

    run bkm = do
      -- logStream ("Event for " ++ show b) (bufferInput b)
      out <- liftM output $ readIORef e
      writeList2Chan out . bkm =<< getChanContents (bufferInput b)
      logPutStrLn "Keymap execution ended"

    -- | The buffer's main loop. Read key strokes from the ui and interpret
    -- them using the current key map. Keys are bound to core actions.
    eventLoop :: IO ()
    eventLoop = do
      repeatM_ $ do -- get the new version of the keymap every time we need to start it.
                    defaultKm <- liftM defaultKeymap $ readIORef e                     
                    handle handler (run $ runKeymap $ I.forever (modKm defaultKm))

deleteBuffer :: FBuffer -> EditorM ()
deleteBuffer b = modifyEditor_ $ \e-> do
                   finaliseB b
                   return e { buffers = M.delete (bkey b) (buffers e)}

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
findBufferWithName e n = filter (\b -> nameB b == n) (M.elems $ buffers e)

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
withWindow :: (Window -> FBuffer -> IO a) -> EditorM a
withWindow f = modifyEditor $ \e -> do
        let w = findWindowWith e (curwin e)
            b = findBufferWith e (bufkey w)
        v <- f w b
        return (e,v)

-- | Perform action with current window's buffer
withBuffer :: (FBuffer -> IO a) -> EditorM a
withBuffer f = withWindow (const f)

-- | Return the current buffer
getBuffer :: EditorM FBuffer
getBuffer = withBuffer return

withUI :: (UI -> IO a) -> EditorM a
withUI f = do
  e <- ask
  lift $ f . ui =<< readIORef e 

withKernel :: (Kernel -> IO a) -> EditorM a
withKernel f = withEditor $ \e -> f (editorKernel e)

-- ---------------------------------------------------------------------

--
-- | Shut down all of our threads. Should free buffers etc.
--
shutdown :: EditorM ()
shutdown = do ts <- readEditor threads
              lift $ mapM_ killThread ts
              modifyEditor_ $ const (return emptyEditor)

-- ---------------------------------------------------------------------
-- | Class of values that can go in the extensible state component
--
class Typeable a => Initializable a where initial :: IO a


-- | Repeat indefinitely the parameter.
repeatM_ :: forall m a. Monad m => m a -> m ()
repeatM_ a = a >> repeatM_ a
{-# SPECIALIZE repeatM_ :: IO a -> IO () #-}
{-# INLINE repeatM_ #-}


-- ---------------------------------------------------------------------
-- | The type of user-bindable functions
--
type Action = EditorM ()

type Interact ev a = I.Interact ev (Writer [Action]) a

type Keymap = Interact Event ()

runKeymap :: Interact ev () -> [ev] -> [Action]
runKeymap p evs = snd $ runWriter (I.runProcess p evs)


catchJustE :: (Exception -> Maybe b) -- ^ Predicate to select exceptions
           -> EditorM a	-- ^ Computation to run
           -> (b -> EditorM a) -- ^	Handler
           -> EditorM a
catchJustE p c h = ReaderT (\r -> catchJust p (runReaderT c r) (\b -> runReaderT (h b) r))

handleJustE :: (Exception -> Maybe b) -> (b -> EditorM a) -> EditorM a -> EditorM a
handleJustE p h c = catchJustE p c h

catchDynE :: Typeable exception => EditorM a -> (exception -> EditorM a) -> EditorM a
catchDynE inner handler = ReaderT (\r -> catchDyn (runReaderT inner r) (\e -> runReaderT (handler e) r))
