-- Copyright (c) 2005,8 Jean-Philippe Bernardy

module Yi.Keymap.Emacs.KillRing where

import Control.Monad (replicateM_)
import Control.Lens
import Data.List.NonEmpty
import Prelude hiding (tail, head)
import Yi.Keymap
import Yi.Buffer
import Yi.Editor
import Yi.KillRing

-- * Killring actions


-- | C-w
killRegion :: BufferM ()
killRegion = deleteRegionB =<< getSelectRegionB

-- | C-k
killLineE :: Maybe Int -> YiM ()
killLineE a = withBuffer $ case a of
               Nothing -> killRestOfLine
               Just n -> replicateM_ (2*n) killRestOfLine

killringPut :: Direction -> String -> EditorM ()
killringPut dir s = (%=) killringA $ krPut dir s

-- | Kill the rest of line
killRestOfLine :: BufferM ()
killRestOfLine =
    do eol <- atEol
       if eol then deleteN 1 else deleteToEol

-- | C-y
yankE :: EditorM ()
yankE = do (text :| _) <- uses killringA krContents
           withBuffer0 $ do pointB >>= setSelectionMarkPointB
                            insertN text

-- | M-w
killRingSaveE :: EditorM ()
killRingSaveE = do (r, text) <- withBuffer0 $ do
                            r <- getSelectRegionB
                            text <- readRegionB r
                            assign highlightSelectionA False
                            return (r,text)
                   killringPut (regionDirection r) text
-- | M-y

-- TODO: Handle argument, verify last command was a yank
yankPopE :: EditorM ()
yankPopE = do
  kr <- use killringA
  withBuffer0 (deleteRegionB =<< getRawestSelectRegionB)
  killringA .= let x :| xs = krContents kr
               in kr { krContents = case xs of
                          [] -> x :| []
                          y:ys -> y :| (ys ++ [x])
                     }
  yankE

-- | C-M-w
appendNextKillE :: EditorM ()
appendNextKillE = (%=) killringA (\kr -> kr {krKilled=True})
