-- 
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
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
-- | The editor state. This the the machine that Core instructions manipulate.
-- The editor just manages buffers. 1 buffer is always in focus, and the
-- editor knows which one that is.
--

module Yi.Editor where

import Yi.Buffer

-- Get at the user defined settings
import {-# SOURCE #-} qualified Yi.Config as Config ( settings )

import Data.FiniteMap
import Data.Unique
import Data.IORef

import Control.Concurrent.MVar  ( MVar(), newMVar, withMVar )
import System.IO.Unsafe         ( unsafePerformIO )

--
-- | First stab at a simple editor state, manipulated by Core
-- instructions.  The 'Editor' just knows about what buffers there are.
-- The UI knows how to draw things of 'Buffer' type.
--
-- Parameterised on the buffer type
--
data Buffer a => GEditor a = Editor {
        buffers       :: FiniteMap Unique a     -- ^ multiple buffers
       ,focusKey      :: (Maybe Unique)         -- ^ one buffer has the focus
       ,editSettings  :: Config                 -- ^ user supplied settings
   }

--
-- What kind of buffer is this editor going to have?
--
type EBuffer = FBuffer
type Editor  = GEditor EBuffer

-- ---------------------------------------------------------------------
-- | The initial state
--
initialState :: Editor
initialState = Editor {
        buffers         = emptyFM,
        focusKey        = Nothing,
        editSettings    = Config.settings   -- static user settings
    }

--
-- | The actual editor state
-- TODO get rid of big lock on state -- individual buffers are already
-- synced.
--
environment :: MVar (IORef Editor)
environment = unsafePerformIO $ do
                ref  <- newIORef initialState
                newMVar ref
{-# NOINLINE environment #-}

-- ---------------------------------------------------------------------
-- 
-- | Grab editor state read-only
-- 
readEditor :: (Editor -> b) -> IO b
readEditor f = withMVar environment $ \r -> return . f =<< readIORef r

--
-- | Grab the editor state, manipulate it, and write it back
--
modifyEditor :: (Editor -> Editor) -> IO ()
modifyEditor f = withMVar environment $ \r -> modifyIORef r f

--
-- | Grab the editor state, maybe do some IO too
--
modifyEditorIO :: (Editor -> IO Editor) -> IO ()
modifyEditorIO f = withMVar environment $ \r-> readIORef r >>= f >>= writeIORef r

-- ---------------------------------------------------------------------
--
-- | return the buffers we have
getBuffers :: IO [EBuffer]
getBuffers = readEditor $ eltsFM . buffers

------------------------------------------------------------------------
--
-- | get current buffer
--
getCurrentBuffer :: IO EBuffer
getCurrentBuffer = readEditor $ \(e :: Editor) -> 
    case focusKey e of
        Just k  -> findBufferWithKey e k
        Nothing -> error "Editor.pwBuffer: no current buffer to get"

--
-- | with the current buffer, perform action
--
modifyCurrentBuffer :: (EBuffer -> IO EBuffer) -> IO ()
modifyCurrentBuffer f = modifyEditorIO $ \(e :: Editor) ->
    case focusKey e of
        Nothing -> error "Editor.getCurrentBuffer: no current buffer"
        Just k  -> do b' <- f $ findBufferWithKey e k
                      let bs'= addToFM (buffers e) k b'
                      return $! e { buffers = bs' }

--
-- Private: Find buffer with this key
-- 
findBufferWithKey :: Editor -> Unique -> EBuffer
findBufferWithKey e k = 
    case lookupFM (buffers e) k of
            Just b  -> b
            Nothing -> error "Editor.findBufferWithKey: no buffer has this key"

--
-- | set new focused buffer
-- Should assert this buffer is in @buffers e@.
-- Generalise the type
--
setCurrentBuffer :: EBuffer -> IO ()
setCurrentBuffer b = modifyEditor $ \(e::Editor) -> e{focusKey = Just $ (key b)}

------------------------------------------------------------------------
--
-- | some useful things
--
nextBuffer :: IO EBuffer
nextBuffer = undefined

prevBuffer :: IO EBuffer
prevBuffer = undefined

--
-- | get the number of buffers we have
--
lengthBuffers :: IO Int                        
lengthBuffers = readEditor $ \((Editor{buffers=bs}) :: Editor) -> sizeFM bs

-- ---------------------------------------------------------------------
-- | Create a new buffer, add it to the set, make it the current buffer,
-- and fill it with contents of @f@.
--
fillNewBuffer :: FilePath -> IO ()
fillNewBuffer f = 
    modifyEditorIO $ \(e@(Editor{buffers=bs}) :: Editor)-> do
        b <- newBuffer f
        return $! e { buffers = addToFM bs (key b) b, focusKey = Just (key b) }

-- todo: no inline?

-- ---------------------------------------------------------------------
-- | set the user-defineable key map
--
setUserSettings :: Config -> IO ()
setUserSettings cs = modifyEditor $ \(e :: Editor) -> e { editSettings = cs }

--
-- | retrieve the user-defineable key map
--
getKeyMap :: IO (Key -> Action)
getKeyMap = readEditor $ \(e :: Editor) -> keyMap (editSettings e)

-- ---------------------------------------------------------------------
-- | The type of user-bindable functions
--

type Action = IO ()
type KeyMap = Key -> Action

-- ---------------------------------------------------------------------
-- | The 'Key' type is the abstract syntax for keys. user interfaces
-- should define a decodeKey function that maps their idea of a key to
-- an abstract 'Key'.
--

data Key= Key  !Char 
        | KeyF !Int 
        | KeyEnter
        | KeyBreak  | KeyDown   | KeyUp     | KeyLeft   | KeyRight
        | KeyHome   | KeyBackspace 
        | KeyDL     | KeyIL     | KeyDC     | KeyIC     | KeyEIC 
        | KeyClear  | KeyEOS    | KeyEOL    | KeySF     | KeySR
        | KeyNPage  | KeyPPage  | KeySTab   | KeyCTab   | KeyCATab 
        | KeySReset | KeyReset  | KeyPrint  | KeyLL     | KeyA1 
        | KeyA3     | KeyB2     | KeyC1     | KeyC3     | KeyBTab 
        | KeyBeg    | KeyCancel | KeyClose  | KeyCommand| KeyCopy 
        | KeyCreate | KeyEnd    | KeyExit   | KeyFind   | KeyHelp 
        | KeyMark   | KeyMessage| KeyMove   | KeyNext   | KeyOpen
        | KeyOptions| KeyPrevious| KeyRedo  | KeyReference
        | KeyRefresh| KeyReplace| KeyRestart| KeyResume | KeySave   
        | KeySBeg   | KeySCancel| KeySCommand| KeySCopy | KeySCreate
        | KeySDC    | KeySDL    | KeySelect | KeySEnd   | KeySEOL   
        | KeySExit  | KeySFind  | KeySHelp  | KeySHome  | KeySIC 
        | KeySLeft  | KeySMessage| KeySMove | KeySNext  | KeySOptions
        | KeySPrevious| KeySPrint| KeySRedo | KeySReplace| KeySRight 
        | KeySRsume | KeySSave | KeySSuspend| KeySUndo  | KeySuspend
        | KeyUndo | KeyResize | KeyMouse 
        | KeyUnknown !Int
    deriving (Eq, Show)

------------------------------------------------------------------------
--
-- | All the user-defineable settings
--
data Config = Config {
            keyMap       :: Key -> Action       -- ^ bind keys to editor actions
        -- ,styles       :: [StyleSpec]
    }

