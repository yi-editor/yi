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
import Yi.CommonUI
import Yi.Editor hiding (readEditor)
import Yi.Debug
import Yi.Buffer
import Yi.Window
import Data.List
import Yi.Keymap
import Control.Monad.Reader
import Yi.WindowSet as WS
--------------------------------------------------------


-- | Rotate focus to the next window
nextWindow :: YiM ()
nextWindow = shiftFocus (+1)

-- | Rotate focus to the previous window
prevWindow :: YiM ()
prevWindow = shiftFocus (subtract 1)

-- | Shift focus to the nth window, modulo the number of windows
windowAt :: Int -> YiM ()
windowAt n = shiftFocus (const n)

-- | Set the new current window using a function applied to the old
-- window's index
shiftFocus :: (Int -> Int) -> YiM ()
shiftFocus f = do
  wset <- withUI getWindows
  let ws = WS.contents wset
  w <- withUI getWindow
  case elemIndex w ws of
    Just i -> withUI2 setWindow (ws !! ((f i) `mod` (length ws)))
    _      -> error "Editor: current window has been lost."

-- | Delete the focused window
deleteThisWindow :: YiM ()
deleteThisWindow = do
  logPutStrLn "deleting current window"
  ui <- asks yiUi
  withEditor (getWindow ui >>= deleteWindow ui)

-- | Close any windows onto the buffer b, then free the buffer
killBufferWindows :: FBuffer -> YiM ()
killBufferWindows b = do 
  logPutStrLn $ "KillBufferWindows: " ++ name b
  ui <- asks yiUi
  ws <- getWindows ui
  withEditor $ mapM_ (deleteWindow ui) $ filter (\w -> bufkey w == keyB b) (WS.contents ws)

-- | Close any windows onto the buffer associated with name 'n', then free the buffer
killBufferAndWindows :: String -> YiM ()
killBufferAndWindows n = do
  bs <- readEditor $ \e -> findBufferWithName e n
  case bs of
    [] -> return ()     -- no buffer to kill, so nothing to do
    _ -> mapM_ killB bs
    where
        killB b = do killBufferWindows b
                     deleteBufferKeymap b
                     lift $ runBuffer b finaliseB
                     withEditor $ deleteBuffer b

-- | Split the current window, opening a second window onto this buffer.
-- Windows smaller than 3 lines cannot be split.
splitWindow :: YiM ()
splitWindow = do 
  b <- withEditor $ getBuffer
  ui <- asks yiUi
  withEditor $ setWindow ui =<< newWindow ui False b

-- | Switch focus to some other window. If none is available, create one.
shiftOtherWindow :: YiM ()
shiftOtherWindow = do
  ws <- withUI getWindows
  if length (WS.contents ws) == 1 then splitWindow else nextWindow


withOtherWindow :: YiM () -> YiM ()
withOtherWindow f = do
  w <- withUI $ getWindow
  shiftOtherWindow
  f
  withUI2 setWindow w
