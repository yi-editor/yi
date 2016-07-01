{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Emacs.KillRing
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Emacs.KillRing where

import           Lens.Micro         (assign, use, uses, (%=), (.=))
import           Control.Monad      (replicateM_, when)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe         (fromMaybe)
import           Yi.Buffer
import           Yi.Editor          (EditorM, killringA, withCurrentBuffer)
import           Yi.Keymap          (YiM)
import           Yi.KillRing        (Killring (_krContents), krKilled, krPut)
import qualified Yi.Rope            as R (YiString, fromString, toString)
import           Yi.Types           (withEditor)
import           Yi.Utils           (io)
import           System.Hclip       (getClipboard, setClipboard)

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

-- This is like @kill-region-or-backward-word@.
killRegionB :: BufferM ()
killRegionB = getSelectRegionB >>= \r ->
  if regionStart r == regionEnd r then bkillWordB else deleteRegionB r

-- | C-w
-- Like `killRegionB`, but with system clipboard synchronization
killRegion :: YiM ()
killRegion = withCurrentBuffer killRegionB >> killringToClipboard

-- | Kills current line
killLineB :: Maybe Int -> BufferM ()
killLineB mbr = replicateM_ (fromMaybe 1 mbr) $ do
  eol <- atEol
  let tu = if eol then Character else Line
  deleteRegionB =<< regionOfPartNonEmptyB tu Forward

-- | C-k
-- | Like `killLineB`, but with system clipboard synchronization
killLine :: Maybe Int -> YiM ()
killLine mbr = withCurrentBuffer (killLineB mbr) >> killringToClipboard

killringGet :: EditorM R.YiString
killringGet = do
  text :| _ <- uses killringA _krContents
  return text

killringPut :: Direction -> R.YiString -> EditorM ()
killringPut dir s = killringA %= krPut dir s

-- | Yanks top of killbuffer
yankE :: EditorM ()
yankE = do
  text :| _ <- uses killringA _krContents
  withCurrentBuffer $ pointB >>= setSelectionMarkPointB >> insertN text

-- | C-y
-- Like `yankE`, but with system clipboard synchronization
yank :: YiM ()
yank = clipboardToKillring >> withEditor yankE

-- | Saves current selection to killring and then clears it
killRingSaveE :: EditorM ()
killRingSaveE = do
  (r, text) <- withCurrentBuffer $ do
    r <- getSelectRegionB
    text <- readRegionB r
    assign highlightSelectionA False
    return (r, text)
  killringPut (regionDirection r) text

-- | M-w
-- Like `killRingSaveE`, but with system clipboard synchronization
killRingSave :: YiM ()
killRingSave = withEditor killRingSaveE >> killringToClipboard

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
