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
--

module HEmacs.Editor (

        -- * the editor type
        Editor,                 -- abstract
        -- Buffer,              -- abstract

        -- * access to editor state
        screenWidth,            -- :: Int
        screenHeight,           -- :: Int
        setScreenSize,          -- :: (Int,Int) -> IO ()
        getScreenSize,          -- :: IO (Int, Int)

        setBuffer,              -- :: [String] -> IO ()
        getBuffer,              -- :: IO -> [String]
        getBufSize,
        setBufSize,             -- :: (Int, Int) -> IO ()
        getBufOrigin,

        getUnsavedStatus,       -- :: Bool
        setUnsaved,             -- :: IO ()

        setUserSettings,        -- :: Config -> IO ()
        getKeyMap,              -- :: IO (Key -> Action)

        -- * a type for user defineable settings
        Config(..),

        -- * abstract syntax for keys
        Key(..),

        -- * user bindable actions
        Action,         -- = Key -> IO EditStatus
        KeyMap,
        EditStatus(..)

   ) where

import Control.Concurrent.MVar  ( MVar(), newMVar, withMVar )
import Data.IORef               ( writeIORef, readIORef, newIORef, IORef() )
import System.IO.Unsafe         ( unsafePerformIO )

--
-- Get at the user defined settings
--
import {-# SOURCE #-} qualified HEmacs.Config as Config ( settings )

--
-- | First stab at a simple editor state, manipulated by Core
-- instructions. Keep the machine as simple as possible for all sorts of
-- UIs
--
-- TODO generalise to n buffers
--
data Editor = Editor {
        s_width         :: !Int,        -- ^ total screen width 
        s_height        :: !Int,        -- ^ total screen height        

        buffer          :: Buffer,      -- ^ simple buffer

-- should be a sub-structure:
        buffer_x        :: !Int,        -- ^ buffer top left x origin
        buffer_y        :: !Int,        -- ^ buffer top left y origin
        buffer_height   :: !Int,        -- ^ height in rows
        buffer_width    :: !Int,        -- ^ width  in columns

        mode_line       :: !String,     -- ^ contents of mode lines
        mode_height     :: !Int,        -- ^ height of modeline

        unsaved_changes :: Bool,        -- ^ are there unsaved changes?
        user_settings   :: Config       -- ^ user supplied settings
   }

------------------------------------------------------------------------
--
-- | The initial state
--
initialState :: Editor
initialState = Editor {
        s_width         = 80,
        s_height        = 24,

        buffer          = emptyBuffer,
        buffer_x        = 0,
        buffer_y        = 0,
        buffer_width    = 80,
        buffer_height   = 24 - 2, -- s_height - mode_height

        mode_line       = "(hemacs)",
        mode_height     = 2,

        unsaved_changes = False,
        user_settings   = Config.settings
    }

--
-- | The editor state stored in an IORef, locked with an MVar
--
environment :: MVar (IORef Editor)
environment = unsafePerformIO $ do
                ref  <- newIORef initialState
                newMVar ref
{-# NOINLINE environment #-}

-- ---------------------------------------------------------------------
-- | Grab the editor state, manipulate it, and write it back
--
modifyEditor :: (Editor -> IO Editor) -> IO ()
modifyEditor f = withMVar environment $ \ref ->
                        readIORef ref >>= f >>= writeIORef ref

-- 
-- | Grab editor state read-only
-- 
withEditor :: (Editor -> IO a) -> IO a
withEditor f = withMVar environment $ \ref -> readIORef ref >>= f

-- ---------------------------------------------------------------------
-- | Functions to get at editor state fields

screenWidth    :: IO Int
screenWidth = withEditor $ \e -> return $ s_width e
 
screenHeight   :: IO Int
screenHeight = withEditor $ \e -> return $ s_height e

--
-- | Set the dimensions of the screen to height and width
--
setScreenSize :: (Int,Int) -> IO ()
setScreenSize (height,width) = 
    modifyEditor $ \e -> return $ e { s_width = width, s_height = height }

--
-- | get the screen dimensions (y,x)
--
getScreenSize :: IO (Int,Int)
getScreenSize = withEditor $ \e -> return $ (s_height e, s_width e)

--
-- | set the buffer dimensions
--
setBufSize :: (Int, Int) -> IO ()
setBufSize (h, w)= 
    modifyEditor $ \e ->
        return $ e { buffer_height = h - mode_height e, buffer_width = w }

--
-- | get buffer dimensions (y,x)
--
getBufSize :: IO (Int, Int)
getBufSize = withEditor $ \e -> return $ (buffer_height e, buffer_width e)

--
-- | get buffer origin (y,x)
--
getBufOrigin :: IO (Int, Int)
getBufOrigin = withEditor $ \e -> return $ (buffer_y e, buffer_x e)

--
-- | Are there any unsaved changes?
--
getUnsavedStatus :: IO Bool
getUnsavedStatus = withEditor $ \e -> return $ unsaved_changes e

--
-- | set the flag indicating there are unsaved changes in the buffer
--
setUnsaved :: IO ()
setUnsaved = modifyEditor $ \e -> return $ e { unsaved_changes = True }

--
-- | reset the flag indicating all changes have been saved
--
setSaved :: IO ()
setSaved = modifyEditor $ \e -> return $ e { unsaved_changes = False }

-- ---------------------------------------------------------------------
-- | load the buffer with [String]
--
setBuffer :: [String] -> IO ()
setBuffer ss = modifyEditor $ \e -> return $ e { buffer = ss }

--
-- | get the buffer in a form easy to draw
--
getBuffer :: IO [String]
getBuffer = withEditor $ \e -> return $ buffer e

-- ---------------------------------------------------------------------
-- | set the user-defineable key map
--
setUserSettings :: Config -> IO ()
setUserSettings cs = modifyEditor $ \e -> return $ e { user_settings = cs }

--
-- | retrieve the user-defineable key map
--
getKeyMap :: IO (Key -> Action)
getKeyMap = withEditor $ \e -> return $ keyMap (user_settings e)

-- --------------------------------------------------------------------- 
-- Buffer type and operations

type Buffer = [String]

emptyBuffer :: Buffer
emptyBuffer = []

-- ---------------------------------------------------------------------
-- | The type of user-bindable functions
--

type Action = IO EditStatus
type KeyMap = Key -> Action

data EditStatus 
    = EOk | EQuit

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

