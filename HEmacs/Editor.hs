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
        Editor,             -- abstract?

        -- * access to editor state
        screenWidth,            -- :: IO Int
        screenHeight,           -- :: IO Int
        getScreenSize,          -- :: IO (Int, Int)
        setScreenSize,          -- :: (Int,Int) -> IO ()

        getAllBuffers,          -- :: [BasicBuffer]
        getBuffers,             -- :: IO (Maybe BasicBuffer, [BasicBuffer])
        getBufCount,            -- :: IO Int                        

        newBuffer,              -- :: FilePath -> [String] -> IO ()
        delBuffer,              -- :: IO ()

        setUserSettings,        -- :: Config -> IO ()
        getKeyMap,              -- :: IO (Key -> Action)

        -- * a type for user defineable settings
        Config(..),

        -- * abstract syntax for keys
        Key(..),

        -- * user bindable actions
        Action,         -- = Key -> IO EditStatus
        KeyMap,
        EventStatus(..)

   ) where

import HEmacs.Buffer

-- Get at the user defined settings
import {-# SOURCE #-} qualified HEmacs.Config as Config ( settings )

import Control.Concurrent.MVar  ( MVar(), newMVar, withMVar )
import Data.IORef               ( writeIORef, readIORef, newIORef, IORef() )
import System.IO.Unsafe         ( unsafePerformIO )

--
-- | First stab at a simple editor state, manipulated by Core
-- instructions. Keep the machine as simple as possible for all sorts of
-- UIs
--
-- Parameterised on the buffer type
--
data Buffer a => GenericEditor a = Editor {
        s_width       :: !Int       -- ^ total screen width 
       ,s_height      :: !Int       -- ^ total screen height        
       ,buffers       :: [a]        -- ^ multiple buffers
       ,cur_buf_ind   :: Maybe Int  -- ^ current buffer, index into buffers
       ,buf_count     :: !Int       -- ^ number of buffers
       ,user_settings :: Config     -- ^ user supplied settings
   }

--
-- What kind of buffer is this editor going to have?
--
type Editor = GenericEditor BasicBuffer

-- ---------------------------------------------------------------------
-- | The initial state
--
initialState :: Editor
initialState = Editor {
        s_width         = 80,
        s_height        = 24,
        buffers         = [],
        cur_buf_ind     = Nothing,
        buf_count       = 0,
        user_settings   = Config.settings       -- global user settings
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
modifyEditor f = 
    withMVar environment $ \ref -> readIORef ref >>= f >>= writeIORef ref

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
-- | get the screen dimensions (y,x)
--
getScreenSize :: IO (Int,Int)
getScreenSize = withEditor $ \e -> return $ (s_height e, s_width e)

--
-- | Set the dimensions of the screen to height and width
--
setScreenSize :: (Int,Int) -> IO ()
setScreenSize (h,w) = 
    modifyEditor $ \e -> return $ e { s_width = w, s_height = h }

--
-- | return the buffers we have
--
getAllBuffers :: IO [BasicBuffer]
getAllBuffers = withEditor $ \e -> return $ buffers e

--
-- | get current buffer, and the rest
--
getBuffers :: IO (Maybe BasicBuffer, [BasicBuffer])
getBuffers = withEditor $ \e -> do
    let bs = buffers e
    case cur_buf_ind e of
        Nothing -> return (Nothing, bs)
        Just i  -> let (a,  b) = splitAt (i+1) bs
                       ([c],d) = splitAt 1     (reverse a)
                   in return $! (Just c, d ++ b)

--
-- | get the number of buffers we have
--
getBufCount :: IO Int                        
getBufCount = withEditor $ \e -> return $ buf_count e

-- ---------------------------------------------------------------------
-- | Create a new buffer, add it to the set, make it the current buffer,
-- and fill it with [String]. Inherit size from editor screen size.
--
newBuffer :: FilePath -> [String] -> IO ()
newBuffer f ss = do
    modifyEditor $ \(e :: Editor) -> do
        let buf = let a = new
                      b = setname a f
                      c = setsize b (s_width e , s_height e)
                      d = setcontents c ss in d
        let bb  = buffers e 
            i   = buf_count e
        return $! e { buffers = buf:bb, cur_buf_ind = Just 0, buf_count = i+1 }

--
-- | delete a buffer
--
delBuffer :: IO ()
delBuffer = error "delBuffer unimplemented"

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
-- | The type of user-bindable functions
--

type Action = IO EventStatus
type KeyMap = Key -> Action

--
-- | core instruction return values
--
data EventStatus = EOk | EQuit

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

