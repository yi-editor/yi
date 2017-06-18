{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Boot.Internal
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal use for Yi.Boot

module Yi.Boot.Internal where

import Config.Dyre.Relaunch (relaunchWithBinaryState)
import Control.Monad.Base   (liftBase)
import Control.Monad.State  (get)
import Yi.Keymap            (YiM, withUI)
import Yi.Types             (withEditor)
import Yi.UI.Common         (end)

-- | "reloads" the configuration
--
-- Serializes the editor state and relaunches Yi using the serialized
-- state. The launch of Yi will result in recompilation of the user's
-- custom Yi. This, in effect, "reloads" the configuration.
reload :: YiM ()
reload = do
  editor <- withEditor get
  withUI (`end` False)
  liftBase $ relaunchWithBinaryState (Just editor) Nothing
