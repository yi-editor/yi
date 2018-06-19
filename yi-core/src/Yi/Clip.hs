{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Yi.Rope             as R (YiString, fromString, toString)


-- |
-- This is just used for emulating system clipbord.
-- Do not release this IORef variable outside this module.
clipboard :: IORef R.YiString
clipboard = unsafePerformIO $ newIORef ""

getClipboard' :: IO R.YiString
getClipboard' = readIORef clipboard

setClipboard' :: R.YiString -> IO ()
setClipboard' = writeIORef clipboard

getClipboard :: YiM R.YiString
getClipboard = do
  config <- askCfg
  if configDisableSystemClipboard config
  then io getClipboard'
  else io $ fmap R.fromString $ H.getClipboard

setClipboard :: R.YiString -> YiM ()
setClipboard text = do
  config <- askCfg
  if configDisableSystemClipboard config
  then io $ setClipboard' text
  else io $ H.setClipboard $ R.toString text
