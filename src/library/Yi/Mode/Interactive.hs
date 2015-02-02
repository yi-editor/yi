{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Mode.Interactive
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Collection of 'Mode's for working with Haskell.

module Yi.Mode.Interactive where

import           Control.Applicative  ((<$>))
import           Control.Concurrent   (threadDelay)
import           Control.Lens         (use, (%~), (.=))
import           Data.Monoid          ((<>))
import qualified Data.Text            as T (Text)
import           Yi.Buffer
import           Yi.Core              (sendToProcess, startSubprocess, withSyntax)
import           Yi.Editor
import           Yi.History           (historyFinishGen, historyMoveGen, historyStartGen)
import           Yi.Keymap            (YiM, topKeymapA)
import           Yi.Keymap.Keys       (Key (KEnter, KHome), char, choice, meta, spec, (<||), (?>>!))
import           Yi.Lexer.Alex        (Tok)
import           Yi.Lexer.Compilation (Token)
import qualified Yi.Mode.Compilation  as Compilation (mode)
import           Yi.Modes             (lookupMode)
import           Yi.Monad             (gets)
import qualified Yi.Rope              as R (YiString, fromText, toString, toText)
import qualified Yi.Syntax.OnlineTree as OnlineTree (Tree)
import           Yi.Utils             (io)


mode :: Mode (OnlineTree.Tree (Tok Token))
mode = Compilation.mode
  { modeApplies = modeNeverApplies,
    modeName = "interactive",
    modeKeymap = topKeymapA %~ (<||)
     (choice
      [spec KHome ?>>! moveToSol,
       spec KEnter ?>>! do
          eof <- withCurrentBuffer atLastLine
          if eof
            then feedCommand
            else withSyntax modeFollow,
       meta (char 'p') ?>>! interactHistoryMove 1,
       meta (char 'n') ?>>! interactHistoryMove (-1)
      ])
  }

interactId :: T.Text
interactId = "Interact"

-- | TODO: we're just converting back and forth here, 'historyMoveGen'
-- and friends need to migrate to YiString it seems.
interactHistoryMove :: Int -> EditorM ()
interactHistoryMove delta =
  historyMoveGen interactId delta (R.toText <$> withCurrentBuffer getInput) >>= inp
  where
    inp = withCurrentBuffer . setInput . R.fromText

interactHistoryFinish :: EditorM ()
interactHistoryFinish =
  historyFinishGen interactId (R.toText <$> withCurrentBuffer getInput)

interactHistoryStart :: EditorM ()
interactHistoryStart = historyStartGen interactId

getInputRegion :: BufferM Region
getInputRegion = do mo <- getMarkB (Just "StdOUT")
                    p <- pointAt botB
                    q <- use $ markPointA mo
                    return $ mkRegion p q

getInput :: BufferM R.YiString
getInput = readRegionB =<< getInputRegion

setInput :: R.YiString -> BufferM ()
setInput val = flip replaceRegionB val =<< getInputRegion

-- | Open a new buffer for interaction with a process.
spawnProcess :: String -> [String] -> YiM BufferRef
spawnProcess = spawnProcessMode mode

-- | open a new buffer for interaction with a process, using any
-- interactive-derived mode
spawnProcessMode :: Mode syntax -> FilePath -> [String] -> YiM BufferRef
spawnProcessMode interMode cmd args = do
    b <- startSubprocess cmd args (const $ return ())
    withEditor interactHistoryStart
    mode' <- lookupMode $ AnyMode interMode
    withCurrentBuffer $ do
        m1 <- getMarkB (Just "StdERR")
        m2 <- getMarkB (Just "StdOUT")
        modifyMarkB m1 (\v -> v {markGravity = Backward})
        modifyMarkB m2 (\v -> v {markGravity = Backward})
        setAnyMode mode'
    return b



-- | Send the type command to the process
feedCommand :: YiM ()
feedCommand = do
  b <- gets currentBuffer
  withEditor interactHistoryFinish
  cmd <- withCurrentBuffer $ do
      botB
      newlineB
      me <- getMarkB (Just "StdERR")
      mo <- getMarkB (Just "StdOUT")
      p <- pointB
      q <- use $ markPointA mo
      cmd <- readRegionB $ mkRegion p q
      markPointA me .= p
      markPointA mo .= p
      return $ R.toString cmd
  withEditor interactHistoryStart
  sendToProcess b cmd

-- | Send command, recieve reply
queryReply :: BufferRef -> String -> YiM R.YiString
queryReply buf cmd = do
    start <- withGivenBuffer buf (botB >> pointB)
    sendToProcess buf (cmd <> "\n")
    io $ threadDelay 50000  -- Hack to let ghci finish writing its output.
    withGivenBuffer buf $ do
      botB
      moveToSol
      leftB -- There is probably a much better way to do this moving around, but it works
      end <- pointB
      result <- readRegionB (mkRegion start end)
      botB
      return result
