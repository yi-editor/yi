{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Emacs.KillRing
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Emacs.KillRing where

import           Control.Lens
import           Control.Monad (replicateM_)
import           Data.List.NonEmpty
import           Prelude hiding (tail, head)
import           Yi.Buffer
import           Yi.Editor
import           Yi.Keymap
import           Yi.KillRing
import qualified Yi.Rope as R

-- * Killring actions


-- | C-w
--
-- This is like @kill-region-or-backward-word@.
killRegion :: BufferM ()
killRegion = getSelectRegionB >>= \r ->
  if regionStart r == regionEnd r then bkillWordB else deleteRegionB r

-- | C-k
killLineE :: Maybe Int -> YiM ()
killLineE Nothing  = withCurrentBuffer killRestOfLine
killLineE (Just n) = withCurrentBuffer $ replicateM_ (2*n) killRestOfLine

killringPut :: Direction -> R.YiString -> EditorM ()
killringPut dir s = killringA %= krPut dir s

-- | Kill the rest of line
killRestOfLine :: BufferM ()
killRestOfLine =
    do eol <- atEol
       if eol then deleteN 1 else deleteToEol

-- | C-y
yankE :: EditorM ()
yankE = do
  text :| _ <- uses killringA _krContents
  withCurrentBuffer $ pointB >>= setSelectionMarkPointB >> insertN text

-- | M-w
killRingSaveE :: EditorM ()
killRingSaveE = do
  (r, text) <- withCurrentBuffer $ do
    r <- getSelectRegionB
    text <- readRegionB r
    assign highlightSelectionA False
    return (r, text)
  killringPut (regionDirection r) text
-- | M-y

-- TODO: Handle argument, verify last command was a yank
yankPopE :: EditorM ()
yankPopE = do
  kr <- use killringA
  withCurrentBuffer (deleteRegionB =<< getRawestSelectRegionB)
  killringA .= let x :| xs = _krContents kr
               in kr { _krContents = case xs of
                          [] -> x :| []
                          y:ys -> y :| ys ++ [x]
                     }
  yankE

-- | C-M-w
appendNextKillE :: EditorM ()
appendNextKillE = killringA . krKilled .= True
