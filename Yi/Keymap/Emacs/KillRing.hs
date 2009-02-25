{-# LANGUAGE DeriveDataTypeable #-}

-- Copyright (c) 2005,8 Jean-Philippe Bernardy

module Yi.Keymap.Emacs.KillRing where

import Prelude ()
import Yi.Prelude 
import Yi.Monad
import Yi.Keymap
import Yi.Buffer
import Yi.Editor
import Control.Monad ( replicateM_ )
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
killringPut dir s = modA killringA $ krPut dir s

-- | Kill the rest of line
killRestOfLine :: BufferM ()
killRestOfLine =
    do eol <- atEol
       if eol then deleteN 1 else deleteToEol

-- | C-y
yankE :: EditorM ()
yankE = do (text:_) <- getsA killringA krContents
           withBuffer0 $ do pointB >>= setSelectionMarkPointB
                            insertN text

-- | M-w
killRingSaveE :: EditorM ()
killRingSaveE = do (r, text) <- withBuffer0 $ do
                            r <- getSelectRegionB
                            text <- readRegionB r
                            putA highlightSelectionA False
                            return (r,text)
                   killringPut (regionDirection r) text
-- | M-y

-- TODO: Handle argument, verify last command was a yank
yankPopE :: EditorM ()
yankPopE = do 
  kr <- getA killringA
  withBuffer0 (deleteRegionB =<< getRawestSelectRegionB)
  putA killringA $ let ring = krContents kr
                   in kr {krContents = tail ring ++ [head ring]}
  yankE

-- | C-M-w
appendNextKillE :: EditorM ()
appendNextKillE = modA killringA (\kr -> kr {krKilled=True})
