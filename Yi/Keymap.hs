-- Copyright (c) Jean-Philippe Bernardy 2007.
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

module Yi.Keymap where

import Prelude hiding (error)
import Yi.CommonUI
import qualified Yi.Editor as Editor
import Yi.Editor (EditorM, Editor, getBuffer, runEditor)
import Yi.Debug
import qualified Data.Map as M
import Yi.Kernel
import Control.Monad.Reader
import Data.Typeable
import Data.IORef
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Yi.Buffer
import qualified Yi.Interact as I
import Yi.Monad
import Control.Monad.Writer
import Control.Monad.State
import Yi.Event
import Yi.WindowSet as WS

data Action = forall a. Show a => YiA (YiM a)
            | forall a. Show a => EditorA (EditorM a)
            | forall a. Show a => BufferA (BufferM a)
--            | InsertA String
--             | TextA Direction Unit Operation          


type Interact ev a = I.Interact ev (Writer [Action]) a

type Keymap = Interact Event ()
 
type KeymapMod = Keymap -> Keymap


data BufferKeymap = BufferKeymap 
    { bufferInput  :: !(Chan Event)      -- ^ input stream
    , bufferThread :: !(Maybe ThreadId)  -- ^ Id of the thread running the buffer's keymap. 
    , bufferKeymap :: !(IORef KeymapMod) -- ^ Buffer's local keymap modification
    , bufferKeymapRestartable :: !(MVar ()) -- ^ Put () in this MVar to mark the buffer ready to restart.
                                            -- FIXME: the bufferKeymap should really be an MVar, and that can be used to sync.
    -- In general , this is way more complicated than it should
    -- be. Just killing the thread and restarting another one looks
    -- like a better approach. 
    }

data Yi = Yi {yiEditor :: IORef Editor,
              yiWindows :: MVar (WindowSet Window),
              yiUi          :: UI,
              threads       :: IORef [ThreadId],           -- ^ all our threads
              input         :: Chan Event,                 -- ^ input stream
              output        :: Chan Action,                -- ^ output stream
              defaultKeymap :: IORef Keymap,
              bufferKeymaps :: IORef (M.Map BufferRef BufferKeymap),
              -- FIXME: there is a latent bug here: the bufferkeymaps
              -- can be modified concurrently by the dispatcher thread
              -- and the worker thread.

              yiKernel  :: Kernel,
              editorModules :: IORef [String] -- ^ modules requested by user: (e.g. ["YiConfig", "Yi.Dired"])
             }

-- | The type of user-bindable functions
type YiM = ReaderT Yi IO

-----------------------
-- Keymap basics

runKeymap :: Interact ev () -> [ev] -> [Action]
runKeymap p evs = snd $ runWriter (I.runProcess p evs)

write :: (I.MonadInteract m (Writer [Action]) ev, YiAction a) => a () -> m ()
write x = I.write (tell [makeAction x])


-----------------------
-- Keymap thread handling


setBufferKeymap :: BufferRef -> KeymapMod -> YiM ()
setBufferKeymap b km = do 
  bkm <- getBufferKeymap b
  writeRef (bufferKeymap bkm) km
  restartBufferThread b
  logPutStrLn $ "Changed keymap for buffer " ++ show b
 
restartBufferThread :: BufferRef -> YiM ()
restartBufferThread b = do
  bkm <- getBufferKeymap b
  lift $ do logPutStrLn $ "Waiting for buffer thread to start: " ++ show b
            takeMVar (bufferKeymapRestartable bkm) 
            maybe (return ()) (flip throwDynTo "Keymap change") (bufferThread bkm)
  logPutStrLn $ "Restart signal sent: " ++ show b
            
deleteBufferKeymap :: BufferRef -> YiM ()
deleteBufferKeymap b = do
  bkm <- getBufferKeymap b
  lift $ do logPutStrLn $ "Waiting for buffer thread to start: " ++ show b
            takeMVar (bufferKeymapRestartable bkm) 
            maybe (return ()) killThread (bufferThread bkm)
  modifiesRef bufferKeymaps (M.delete b)

startBufferKeymap :: BufferRef -> YiM BufferKeymap
startBufferKeymap b = do
  logPutStrLn $ "Starting buffer keymap: " ++ show b
  yi <- ask
  bkm <- lift $ 
         do r <- newEmptyMVar
            ch <- newChan
            km <- newIORef id
            let bkm = BufferKeymap { bufferInput = ch
                                   , bufferThread = Nothing
                                   , bufferKeymap = km
                                   , bufferKeymapRestartable = r
                                   }
            t <- forkIO $ bufferEventLoop yi b bkm
            return bkm {bufferThread = Just t}
  modifiesRef bufferKeymaps (M.insert b bkm)
  return bkm

getBufferKeymap :: BufferRef -> YiM BufferKeymap
getBufferKeymap b = do
  kms <- readsRef bufferKeymaps
  case M.lookup b kms of
    Just bkm -> return bkm 
    Nothing -> startBufferKeymap b
                           
bufferEventLoop :: Yi -> BufferRef -> BufferKeymap -> IO ()
bufferEventLoop yi buf b = eventLoop 
  where
    handler exception = logPutStrLn $ "Buffer event loop crashed with: " ++ (show exception)

    run bkm = do
      -- logStream ("Event for " ++ show b) (bufferInput b)
      logPutStrLn $ "Starting keymap thread for " ++ show buf
      tryPutMVar (bufferKeymapRestartable b) ()
      writeList2Chan (output yi) . bkm =<< getChanContents (bufferInput b)
      takeMVar (bufferKeymapRestartable b)
      logPutStrLn "Keymap execution ended"

    -- | The buffer's main loop. Read key strokes from the ui and interpret
    -- them using the current key map. Keys are bound to core actions.
    eventLoop :: IO ()
    eventLoop = do
      repeatM_ $ do -- get the new version of the keymap every time we need to start it.
                    defaultKm <- readIORef (defaultKeymap yi)
                    modKm <- readIORef (bufferKeymap b)
                    handle handler (run $ runKeymap $ forever (modKm defaultKm))

dispatch :: Event -> YiM ()
dispatch ev = do b <- withEditor getBuffer
                 bkm <- getBufferKeymap b
                 lift $ writeChan (bufferInput bkm) ev


--------------------------------
-- Uninteresting glue code


withKernel :: (Kernel -> IO a) -> YiM a
withKernel = with yiKernel 


withUI :: (UI -> IO a) -> YiM a
withUI = with yiUi

withUI2 :: (UI -> x -> EditorM a) -> (x -> YiM a)
withUI2 f x = do
  e <- ask
  withEditor $ f (yiUi e) x

withEditor :: EditorM a -> YiM a
withEditor f = do
  r <- asks yiEditor
  e <- readRef r
  let (a,e') = runEditor f e
  writeRef r e'
  return a

withGivenBuffer :: BufferRef -> BufferM a -> YiM a
withGivenBuffer b f = withEditor (Editor.withGivenBuffer0 b f)

withBuffer :: BufferM a -> YiM a
withBuffer f = withEditor (Editor.withBuffer0 f)

readEditor :: (Editor -> a) -> YiM a
readEditor f = withEditor (gets f)

catchDynE :: Typeable exception => YiM a -> (exception -> YiM a) -> YiM a
catchDynE inner handler = ReaderT (\r -> catchDyn (runReaderT inner r) (\e -> runReaderT (handler e) r))

catchJustE :: (Exception -> Maybe b) -- ^ Predicate to select exceptions
           -> YiM a	-- ^ Computation to run
           -> (b -> YiM a) -- ^	Handler
           -> YiM a
catchJustE p c h = ReaderT (\r -> catchJust p (runReaderT c r) (\b -> runReaderT (h b) r))

handleJustE :: (Exception -> Maybe b) -> (b -> YiM a) -> YiM a -> YiM a
handleJustE p h c = catchJustE p c h

-- | Shut down all of our threads. Should free buffers etc.
shutdown :: YiM ()
shutdown = do ts <- readsRef threads
              lift $ mapM_ killThread ts

-- -------------------------------------------

class YiAction a where
    makeAction :: Show x => a x -> Action

instance YiAction YiM where
    makeAction = YiA


instance YiAction EditorM where
    makeAction = EditorA

instance YiAction BufferM where
    makeAction = BufferA

