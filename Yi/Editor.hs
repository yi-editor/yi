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
import Yi.Window

import Data.List                ( elemIndex )
import Data.FiniteMap
import Data.IORef
import Data.Unique              ( Unique )

import Control.Concurrent.MVar
import System.IO
import System.IO.Unsafe         ( unsafePerformIO )

--
-- | The editor state, manipulated by Core instructoins.
-- The editor stores all the buffers, which correspond to opened files.
-- Windows are views (or port holes) on to buffers, and multiple windows
-- may be opened onto the one buffer. A distinguished /window/ is stored
-- explicitly: the command line.
--
-- Some instructions manipulate buffers, and some just manipulate
-- windows (e.g. scrolling and splitting).
--
-- The order windows are displayed on the screen is encoded in their
-- order in the @windows@ list.
--
-- TODO Windows should be MVar'd as well.
--
data Buffer a => GenEditor a = 
    Editor {
        buffers   :: FiniteMap Unique a         -- ^ all the buffers
       ,windows   :: FiniteMap Unique Window    -- ^ all the windows
       ,cmdline   :: !String                    -- ^ the command line
       ,curwin    :: Unique                     -- ^ the window with focus
       ,curkeymap :: (Char -> IO())             -- ^ user-configurable keymap
    }

--
-- Instantiate the editor with a basic buffer type
--
type Buffer' = FBuffer
type Editor  = GenEditor Buffer'

-- ---------------------------------------------------------------------
--
-- | The actual editor state
--
-- TODO get rid of big lock on state (buffers themselves are locked)
-- We'd have to lock individual components of the state, however...
--
-- state :: Buffer a => MVar (IORef (GenEditor a))
state :: MVar (IORef Editor)
state = unsafePerformIO $ do
            ref  <- newIORef emptyEditor
            newMVar ref
{-# NOINLINE state #-}

--
-- | The initial state
--
emptyEditor :: Editor
emptyEditor = Editor {
        buffers      = emptyFM 
       ,windows      = emptyFM
       ,cmdline      = []           -- for now
       ,curwin       = error "No focused window"
       ,curkeymap    = error "No keymap set"
    }

-- 
-- | Read the editor state, with a pure action
-- 
readEditor :: (Editor -> b) -> IO b
readEditor f = withMVar state $ \ref -> return . f =<< readIORef ref

--
-- | Modify the contents, using an IO action.
--
modifyEditor_ :: (Editor -> IO Editor) -> IO ()
modifyEditor_ f = modifyMVar_ state $ \r ->
                    readIORef r >>= f >>= writeIORef r >> return r

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
-- Buffer operations
--
-- | Create a new buffer filling with contents of file.
--
hNewBuffer :: FilePath -> IO ()
hNewBuffer f = 
    modifyEditor_ $ \e@(Editor{buffers=bs} :: Editor) -> do
        b <- hNewB f
        return $! e { buffers = addToFM bs (keyB b) b }

--
-- | Create and fill a new buffer, using contents of string.
--
stringToNewBuffer :: FilePath -> String -> IO ()
stringToNewBuffer f cs =
    modifyEditor_ $ \e@(Editor{buffers=bs} :: Editor) -> do
        b <- newB f cs
        return $! e { buffers = addToFM bs (keyB b) b }

--
-- | return the buffers we have
-- TODO we need to order the buffers some how.
--
-- getBuffers :: Buffer a => IO [a]
--
getBuffers :: IO [Buffer']
getBuffers = readEditor $ eltsFM . buffers

--
-- | get the number of buffers we have
--
sizeBuffers :: IO Int                        
sizeBuffers = readEditor $ \(e :: Editor) -> sizeFM (buffers e)

--
-- | Find buffer with this key
-- 
findBufferWith :: Editor -> Unique -> Buffer'
findBufferWith e k = 
    case lookupFM (buffers e) k of
        Just b  -> b
        Nothing -> error "Editor.findBufferWith: no buffer has this key"

--
-- | Safely lookup buffer using it's key.
--
getBufferWith :: Unique -> IO Buffer'
getBufferWith u = readEditor $ \(e :: Editor) -> findBufferWith e u

------------------------------------------------------------------------
-- | Window manipulation

-- | Create a new window onto this buffer
-- TODO totally fails to take into account the position of other windows
--
newWindow :: Unique -> (Int,Int) -> IO Window
newWindow u sz = modifyEditor $ \e -> do
    let b = findBufferWith e u
    w <- emptyWindow b sz
    let e' = e { windows = addToFM (windows e) (key w) w }
    return (e', w)

--
-- | Delete a window. 
-- TODO should close the underlying buffer if this is the last window
-- onto that buffer. If this is the focused window, switch to another
-- one.
--
deleteWindow :: Unique -> IO ()
deleteWindow u = modifyEditor_ $ \e -> do
    let w  = findWindowWith e u
        ws = delFromFM (windows e) (key w)
    return $ e { windows = ws }

-- ---------------------------------------------------------------------
-- | Get all the windows
-- TODO sort by key
--
getWindows :: IO [Window]
getWindows = readEditor $ \e -> eltsFM $ windows e

--
-- | Get current window
--
getWindow :: IO Window
getWindow = readEditor $ \e -> findWindowWith e (curwin e)

--
-- | Set current window
--
setWindow :: Window -> IO ()
setWindow w = modifyEditor_ $ \(e :: Editor) -> return $ e {curwin = key w}

--
-- | How many windows do we have
--
sizeWindows :: IO Int
sizeWindows = readEditor $ \e -> length $ eltsFM (windows e)

--
-- | Find the window with this key
--
findWindowWith :: Editor -> Unique -> Window
findWindowWith e k = 
    case lookupFM (windows e) k of
            Just w  -> w
            Nothing -> error "Editor: no window has this key"

------------------------------------------------------------------------
--
-- | Perform action with current window
--
withWindow_ :: (Window -> Buffer' -> IO Window) -> IO ()
withWindow_ f = modifyEditor_ $ \e -> do
        let w = findWindowWith e (curwin e)
            b = findBufferWith e (bufkey w)
        w' <- f w b
        m'     <- updateModeLine b (width w') (winpnt w')
        let w'' = w' { mode = m' }
            ws = windows e
            e' = e { windows = addToFM ws (key w'') w'' }
        return e'

--
-- | Variation on withWindow_ that can return a value
--
withWindow :: (Window -> Buffer' -> IO (Window,b)) -> IO b
withWindow f = modifyEditor $ \e -> do
        let w = findWindowWith e (curwin e)
            b = findBufferWith e (bufkey w)
        (w',v) <- f w b
        m'     <- updateModeLine b (width w') (winpnt w')
        let w'' = w' { mode = m' }
            ws = windows e
            e' = e { windows = addToFM ws (key w'') w'' }
        return (e',v)

-- ---------------------------------------------------------------------
-- | Rotate focus to the next window
--
nextWindow :: IO ()
nextWindow = shiftFocus (+1)

--
-- | Rotate focus to the previous window
--
prevWindow :: IO ()
prevWindow = shiftFocus (subtract 1)

--
-- | Shift focus to the nth window, modulo the number of windows
--
windowAt :: Int -> IO ()
windowAt n = shiftFocus (const n)

--
-- | Set the new current window using a function applied to the old
-- window's index
--
shiftFocus :: (Int -> Int) -> IO ()
shiftFocus f = modifyEditor_ $ \(e :: Editor) -> do
    let ws  = eltsFM $ windows e    -- hack
        k   = curwin e
        win = findWindowWith e k
    case elemIndex win ws of
        Nothing -> error "Editor: current buffer has been lost."
        Just i -> let l = length ws
                      w = ws !! ((f i) `mod` l)
                  in return $ e { curwin = key w }

-- ---------------------------------------------------------------------
-- | Given a keymap function, set the user-defineable key map to that function
--
setUserSettings :: Config -> IO ()
setUserSettings (Config km) = 
    modifyEditor_ $ \(e :: Editor) -> return $ e { curkeymap = km }

--
-- | retrieve the user-defineable key map
--
getKeyBinds :: IO (Char -> IO ())
getKeyBinds = readEditor curkeymap

-- ---------------------------------------------------------------------
--
-- | All the user-defineable settings. This is the type of the data in
-- ~/.yi/Config.hs. All user defineable values will eventually be in
-- this structure. A value of this type is passed from Boot.hs to Yi.hs
-- in the dynamically loaded edition of yi.
--
data Config = Config {
            keymap :: Char -> IO ()        -- ^ bind keys to editor actions
    }

