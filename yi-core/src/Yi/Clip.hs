{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Clip
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A proxy of clipboard.

module Yi.Clip ( setClipboard
               , getClipboard
               )
    where

import qualified System.Hclip        as H (getClipboard, setClipboard)
import           Data.IORef
import           System.IO.Unsafe

import           Yi.Types            (configDisableSystemClipboard, askCfg)
import           Yi.Utils            (io)
import           Yi.Keymap           (YiM)

clipboard :: IORef String
clipboard = unsafePerformIO $ newIORef ""

getClipboard' :: IO String
getClipboard' = readIORef clipboard

setClipboard' :: String -> IO ()
setClipboard' = writeIORef clipboard

getClipboard :: YiM String
getClipboard = do
  config <- askCfg
  if configDisableSystemClipboard config
  then io getClipboard'
  else io H.getClipboard

setClipboard :: String -> YiM ()
setClipboard text = do
  config <- askCfg
  if configDisableSystemClipboard config
  then io $ setClipboard' text
  else io $ H.setClipboard text
