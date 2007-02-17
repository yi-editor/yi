--
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2007 Jean-Philippe Bernardy
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

-- UI-related code common between all frontends.

module Yi.CoreUI where

import Prelude hiding (error)
import Yi.UI
import Yi.Editor
import Yi.Debug
import Yi.Buffer
import Yi.Window
import qualified Data.Map as M
import Data.List



-- | Rotate focus to the next window
nextWindow :: IO ()
nextWindow = shiftFocus (+1)

-- | Rotate focus to the previous window
prevWindow :: IO ()
prevWindow = shiftFocus (subtract 1)

-- | Shift focus to the nth window, modulo the number of windows
windowAt :: Int -> IO ()
windowAt n = shiftFocus (const n)

-- | Set the new current window using a function applied to the old
-- window's index
shiftFocus :: (Int -> Int) -> IO ()
shiftFocus f = do
  ws <- readEditor getWindows
  mw <- getWindow
  case mw of
    Just w | Just i <- elemIndex w ws
          -> setWindow (ws !! ((f i) `mod` (length ws)))
    _     -> error "Editor: current window has been lost."

-- | Delete the focused window
deleteThisWindow :: IO ()
deleteThisWindow = logPutStrLn "deleting current window" >> getWindow >>= deleteWindow  

-- | Close any windows onto the buffer b, then free the buffer
killBufferWindows :: FBuffer -> IO ()
killBufferWindows b = do
  logPutStrLn $ "KillBufferWindows: " ++ nameB b
  ws <- readEditor getWindows
  mapM_ deleteWindow $ map Just $ filter (\w -> bufkey w == keyB b) ws

-- | Close any windows onto the buffer associated with name 'n', then free the buffer
killBufferAndWindows :: String -> IO ()
killBufferAndWindows n = do
  bs <- readEditor $ \e -> findBufferWithName e n
  case bs of
    [] -> return ()     -- no buffer to kill, so nothing to do
    _ -> mapM_ killB bs
    where
        killB b = do killBufferWindows b
                     finaliseB b  
                     modifyEditor_ $ \e -> return $ e { buffers = M.delete (keyB b) (buffers e) }
