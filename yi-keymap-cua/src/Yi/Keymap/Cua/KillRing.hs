{-# OPTIONS_HADDOCK show-extensions #-}
{-# language RankNTypes #-}

-- |
-- Module      :  Yi.Keymap.Cua.KillRing
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Cua.KillRing ( clipboardToKillring
                              , killringToClipboard
                              ) where

import           Lens.Micro.Platform (use, (%=), Getting)
import           Control.Monad       (when)
import           Control.Monad.State.Class (MonadState)
import           Data.List.NonEmpty  (NonEmpty ((:|)))
import           Yi.Buffer
import           Yi.Editor           (EditorM, killringA)
import           Yi.Keymap           (YiM)
import           Yi.KillRing         (Killring (_krContents), krPut)
import qualified Yi.Rope             as R (YiString, fromString, toString)
import           Yi.Types            (withEditor)
import           Yi.Utils            (io)
import           System.Hclip        (getClipboard, setClipboard)


uses :: forall a b f s. MonadState s f => Getting a s a -> (a -> b) -> f b
uses l f = f <$> use l


-- * Killring actions

-- | Adds system clipboard's contents on top of the killring if not already there
clipboardToKillring :: YiM ()
clipboardToKillring = do
  text <- fmap R.fromString $ io getClipboard
  withEditor $ do
    text' <- killringGet
    when (text' /= text) $ killringPut Forward text

-- | Adds the top of the killring to the system clipboard
killringToClipboard :: YiM ()
killringToClipboard = do
  text <- withEditor killringGet
  io . setClipboard $ R.toString text


killringGet :: EditorM R.YiString
killringGet = do
  text :| _ <- uses killringA _krContents
  return text

killringPut :: Direction -> R.YiString -> EditorM ()
killringPut dir s = killringA %= krPut dir s

