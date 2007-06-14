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
import Data.Foldable (toList)
--------------------------------------------------------


-- | Rotate focus to the next window
nextWindow :: YiM ()
nextWindow = shiftFocus WS.forward

-- | Rotate focus to the previous window
prevWindow :: YiM ()
prevWindow = shiftFocus WS.backward

-- | Shift the focus to some other window.
shiftFocus f = do
  ui <- asks yiUi  
  withEditor $ do ws <- getWindows ui; setWindows ui (f ws)
  withEditor $ getWindow ui >>= setWindow ui


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
  withEditor $ mapM_ (deleteWindow ui) $ filter (\w -> bufkey w == keyB b) (toList ws)

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
  if length (toList ws) == 1 then splitWindow else nextWindow


withOtherWindow :: YiM () -> YiM ()
withOtherWindow f = do
  w <- withUI $ getWindow
  shiftOtherWindow
  f
  withUI2 setWindow w
