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
-- | The editor state. This is the machine that Core instructions
-- manipulate.  The editor manages buffers. One buffer is always in
-- focus.
--

module Yi.Editor where

import Yi.Buffer

-- | recursive module stuff
import {-# SOURCE #-} qualified Yi.Config as Config ( settings )
import {-# SOURCE #-} qualified Yi.Keymap as Keymap ( keymap )

import Data.List                ( elemIndex )
import Data.FiniteMap
import Data.IORef
import Data.Unique              ( Unique )

import Control.Concurrent.MVar
import System.IO
import System.IO.Unsafe         ( unsafePerformIO )

--
-- | First stab at a simple editor state, manipulated by Core
-- instructions.  The 'Editor' just knows about what buffers there are.
-- The UI knows how to draw things of 'Buffer' type.
--
-- Parameterised on the buffer type
--
data Buffer a => GenEditor a = Editor {
        buffers      :: FiniteMap Unique a -- ^ multiple buffers
       ,curkey       :: Unique             -- ^ one buffer has focus
       ,editSettings :: Config             -- ^ user settings
   }

--
-- Instantiate the editor with a basic buffer type
--
type Buffer' = FBuffer
type Editor  = GenEditor Buffer'

-- ---------------------------------------------------------------------
--
-- | The actual editor state
-- TODO get rid of big lock on state (buffers themselves are locked)
--
-- state :: Buffer a => MVar (IORef (GenEditor a))
state :: MVar (IORef Editor)
state = unsafePerformIO $ do
            ref  <- newIORef newEmptyEditor
            newMVar ref
{-# NOINLINE state #-}

--
-- | The initial state
--
-- newEmptyEditor :: Buffer a => GenEditor a
newEmptyEditor :: Editor
newEmptyEditor = Editor {
        buffers      = emptyFM,
        curkey       = error "Editor: no buffer in focus",
        editSettings = Config.settings   -- static user settings
    }

-- 
-- | Read the editor state
-- 
-- readEditor :: Buffer a => (GenEditor a -> b) -> IO b
readEditor :: (Editor -> b) -> IO b
readEditor f = withMVar state $ \ref -> return . f =<< readIORef ref

--
-- | Grab the editor state, mutate the contents, and write it back
--
-- modifyEditor_ :: Buffer a => (GenEditor a -> IO (GenEditor a)) -> IO ()
modifyEditor_ :: (Editor -> IO Editor) -> IO ()
modifyEditor_ f = modifyMVar_ state $ \r -> do
                    readIORef r >>= f >>= writeIORef r
                    return r    -- :: a -> IO a

--
-- | Variation on modifyEditor_ that lets you return a value
--
modifyEditor :: (Editor -> IO (Editor,b)) -> IO b
modifyEditor f = modifyMVar state $ \r -> do
                    v  <- readIORef r
                    (v',b) <- f v
                    writeIORef r v'
                    return (r,b)

-- ---------------------------------------------------------------------
--
-- | Create a new buffer, filling with contents of file.
--
hNewBuffer :: FilePath -> IO ()
hNewBuffer f = 
    modifyEditor_ $ \e@(Editor{buffers=bs} :: Editor) -> do
        b <- hNewB f
        return $! e { buffers = addToFM bs (keyB b) b, curkey = (keyB b) }

--
-- | Create and fill a new buffer. Gets an \n if it is empty (?)
--
stringToNewBuffer :: FilePath -> String -> IO ()
stringToNewBuffer f [] = stringToNewBuffer f ['\n']
stringToNewBuffer f cs =
    modifyEditor_ $ \e@(Editor{buffers=bs} :: Editor) -> do
        b <- newB f cs
        return $! e { buffers = addToFM bs (keyB b) b, curkey = (keyB b) }

--
-- | return the buffers we have
--
-- getBuffers :: Buffer a => IO [a]
--
getBuffers :: IO [Buffer']
getBuffers = readEditor $ eltsFM . buffers

--
-- | get the number of buffers we have
--
lengthBuffers :: IO Int                        
lengthBuffers = readEditor $ \(Editor {buffers=bs} :: Editor) -> sizeFM bs

--
-- | get current buffer
--
-- getCurrentBuffer :: Buffer a => IO a
getCurrentBuffer :: IO Buffer'
getCurrentBuffer = readEditor $ \e -> lookupBuffers e (curkey e)

--
-- Find buffer with this key
-- 
-- lookupBuffers :: Buffer a => GenEditor a -> Unique -> a
lookupBuffers :: Editor -> Unique -> Buffer'
lookupBuffers e k = 
    case lookupFM (buffers e) k of
        Just b  -> b
        Nothing -> error "Editor.lookupBuffers: no buffer has this key"

--
-- | Mutate the current buffer
--
-- If we want to do touch anything not inside an ioref, we'll have to
-- return a modified @e@ (see setBuffer)
--
-- withBuffer :: Buffer a => (a -> IO ()) -> IO ()
withBuffer_ :: (Buffer' -> IO ()) -> IO ()
withBuffer_ f = modifyEditor_ $ \e -> do 
        f $ lookupBuffers e (curkey e) -- :: IO ()
        return e                       -- nothing else changed

--
-- | Variation on withBuffer_ that lets you return a value
--
withBuffer :: (Buffer' -> IO b) -> IO b
withBuffer f = modifyEditor $ \e -> do
    b <- f $ lookupBuffers e (curkey e)
    return (e, b)

--
-- | Set current buffer
-- Should assert this buffer is in @buffers e@.
--
-- setBuffer :: Buffer a => a -> IO ()
setBuffer :: Buffer' -> IO ()
setBuffer b = modifyEditor_ $ \(e :: Editor) -> return $ e {curkey = keyB b}

--
-- | Rotate focus to the next buffer
--
nextBuffer :: IO ()
nextBuffer = shiftFocus (\i -> i+1)

--
-- | Rotate focus to the previous buffer
--
prevBuffer :: IO ()
prevBuffer = shiftFocus (\i -> i-1)

--
-- | Shift focus to the nth buffer, modulo the number of buffers
--
bufferAt :: Int -> IO ()
bufferAt n = shiftFocus (\_ -> n)

--
-- | Set the new current buffer using a function applied to the old
-- current buffer's index
--
shiftFocus :: (Int -> Int) -> IO ()
shiftFocus f = modifyEditor_ $ \(e :: Editor) -> do
    let bs  = eltsFM (buffers e)
        key = curkey e
    case lookupFM (buffers e) key of
        Nothing -> error "Editor.setBufferAt: no current buffer"
        Just cb -> case elemIndex cb bs of
            Nothing -> error "Error.setBufferAt: current buffer has gone"
            Just i -> let l = length bs
                          b = bs !! ((f i) `mod` l)
                      in return $ e { curkey = keyB b }

-- ---------------------------------------------------------------------
-- | set the user-defineable key map
--
setUserSettings :: Config -> IO ()
setUserSettings cs = modifyEditor_ $ \(e :: Editor) -> return $ e { editSettings = cs }

--
-- | retrieve the user-defineable key map
--
getKeyMap :: IO (Char -> Action)
-- getKeyMap = readEditor $ \(e :: Editor) -> keyMap (editSettings e)
getKeyMap = return $ Keymap.keymap

-- ---------------------------------------------------------------------
-- | The type of user-bindable functions
--
type Action = IO ()

------------------------------------------------------------------------
--
-- | All the user-defineable settings
--
data Config = Config {
            keyMap       :: Char -> Action       -- ^ bind keys to editor actions
        -- ,styles       :: [StyleSpec]
    }

