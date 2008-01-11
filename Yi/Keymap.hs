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
import Yi.UI.Common
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
import Control.Monad.State
import Yi.Event
import Yi.WindowSet as WS

data Action = forall a. Show a => YiA (YiM a)
            | forall a. Show a => EditorA (EditorM a)
            | forall a. Show a => BufferA (BufferM a)
--            | InsertA String
--             | TextA Direction Unit Operation

instance I.PEq Action where
    equiv _ _ = False

instance Show Action where
    show (YiA _) = "@Y"
    show (EditorA _) = "@E"
    show (BufferA _) = "@B"

type Interact ev a = I.I ev Action a

type Keymap = Interact Event ()

type KeymapProcess = I.P Event Action

type KeymapMod = Keymap -> Keymap


data BufferKeymap = BufferKeymap
    { bufferKeymap :: KeymapMod -- ^ Buffer's local keymap modification
    , bufferKeymapProcess :: KeymapProcess -- ^ Current state of the keymap automaton
    }

data Yi = Yi {yiEditor :: IORef Editor,
              yiWindows :: MVar (WindowSet Window),
              yiUi          :: UI,
              threads       :: IORef [ThreadId],           -- ^ all our threads
              input         :: Chan Event,                 -- ^ input stream
              output        :: Chan Action,                -- ^ output stream
              defaultKeymap :: IORef Keymap,
              bufferKeymaps :: IORef (M.Map BufferRef BufferKeymap),
              yiKernel  :: Kernel,
              editorModules :: IORef [String] -- ^ modules requested by user: (e.g. ["YiConfig", "Yi.Dired"])
             }

-- | The type of user-bindable functions
type YiM = ReaderT Yi IO

-----------------------
-- Keymap basics

write :: (I.MonadInteract m Action ev, YiAction a) => a () -> m ()
write x = I.write (makeAction x)


-----------------------
-- Keymap thread handling

setBufferKeymap :: BufferRef -> KeymapMod -> YiM ()
setBufferKeymap b km = do
  bkm <- getBufferKeymap b
  modifiesRef bufferKeymaps (M.insert b bkm {bufferKeymap = km, bufferKeymapProcess = I.Fail})

restartBufferThread :: BufferRef -> YiM ()
restartBufferThread b = do
  bkm <- getBufferKeymap b
  modifiesRef bufferKeymaps (M.insert b bkm {bufferKeymapProcess = I.Fail})

deleteBufferKeymap :: BufferRef -> YiM ()
deleteBufferKeymap b = modifiesRef bufferKeymaps (M.delete b)

getBufferKeymap :: BufferRef -> YiM BufferKeymap
getBufferKeymap b = do
  kms <- readsRef bufferKeymaps
  return $ case M.lookup b kms of
    Just bkm -> bkm
    Nothing -> BufferKeymap {bufferKeymap = id, bufferKeymapProcess = I.Fail}

-- | Process an event by advancing the current keymap automaton an
-- execing the generated actions
dispatch :: Event -> YiM ()
dispatch ev =
    do yi <- ask
       b <- withEditor getBuffer
       bkm <- getBufferKeymap b
       defKm <- readRef (defaultKeymap yi)
       let p0 = bufferKeymapProcess bkm
           freshP = I.mkAutomaton $ bufferKeymap bkm $ defKm
           p = case p0 of
                 I.End -> freshP
                 I.Fail -> freshP -- TODO: output error message about unhandled input
                 _ -> p0
           (actions, p') = I.processOneEvent p ev
       logPutStrLn $ "Processing: " ++ show ev
       logPutStrLn $ "Actions posted:" ++ show actions
       logPutStrLn $ "New automation: " ++ show p'
       lift $ writeList2Chan (output yi) actions
       modifiesRef bufferKeymaps (M.insert b bkm { bufferKeymapProcess = p' })

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
  -- logPutStrLn $ "Buffers = " ++ (show $ M.elems $ buffers e')
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
           -> YiM a      -- ^ Computation to run
           -> (b -> YiM a) -- ^   Handler
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

