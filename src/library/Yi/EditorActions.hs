{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.EditorActions
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The base actions of Yi.

module Yi.EditorActions
  (
  -- * Construction and destruction
    startEditor
  , quitEditor          -- :: YiM ()

  -- * Global editor actions
  , closeWindow         -- :: YiM ()
  , closeWindowEmacs
  ) where

import           Control.Concurrent             (ThreadId, forkIO, forkOS,
                                                 modifyMVar, modifyMVar_,
                                                 newMVar, readMVar, threadDelay)
import           Control.Exception              (SomeException, handle)
import           Control.Lens                   (mapped, use, uses,
                                                 (%=))
import           Control.Monad                  (when)
import           Control.Monad.Reader           (MonadReader (ask), ReaderT (runReaderT), asks)
import qualified Data.List.PointedList.Circular as PL (PointedList (_focus), length)
import qualified Data.Map                       as M (empty)
import           Data.Maybe                     (fromMaybe, isNothing)
import           Data.List.NonEmpty             (NonEmpty (..))
import           Yi.Config
import           Yi.Core                        (recoverMode, postActions, showErrors,
                                                 errorEditor, refreshEditor, interactive,
                                                 dispatch, onYiVar, terminateSubprocesses)
import           Yi.Debug                       (logPutStrLn)
import           Yi.Editor
import           Yi.Keymap                      (makeAction, withUI)
import           Yi.PersistentState
import           Yi.String                      (showT)
import           Yi.Types                       (Yi(..), YiM(..), YiVar(..), IsRefreshNeeded(..))
import           Yi.UI.Common                   as UI (UI (main, end))
import           Yi.Window                      (isMini)


-- | Start up the editor, setting any state with the user preferences
-- and file names passed in, and turning on the UI
--
startEditor :: Config -> Maybe Editor -> IO ()
startEditor cfg st = do
    let uiStart = startFrontEnd cfg

    logPutStrLn "Starting Core"

    -- Use an empty state unless resuming from an earlier session and
    -- one is already available
    let editor = fromMaybe emptyEditor st
    -- here to add load history etc?

    -- Setting up the 1st window is a bit tricky because most
    -- functions assume there exists a "current window"
    newSt <- newMVar $ YiVar editor 1 M.empty
    (ui, runYi) <- mdo
      let handler (exception :: SomeException) =
            runYi $ errorEditor (showT exception) >> refreshEditor

          inF []     = return ()
          inF (e:es) = handle handler $ runYi $ dispatch (e :| es)

          outF refreshNeeded acts =
            handle handler $ runYi $ interactive refreshNeeded acts
          runYi f   = runReaderT (runYiM f) yi
          yi        = Yi ui inF outF cfg newSt
      ui <- uiStart cfg inF (outF MustRefresh) editor
      return (ui, runYi)

    runYi loadPersistentState

    runYi $ do
      if isNothing st
        -- process options if booting for the first time
        then postActions NoNeedToRefresh $ startActions cfg
        -- otherwise: recover the mode of buffers
        else withEditor $ buffersA.mapped %= recoverMode (modeTable cfg)
      postActions NoNeedToRefresh $ initialActions cfg ++ [makeAction showErrors]

    runYi refreshEditor

    UI.main ui -- transfer control to UI

-- | Quit.
quitEditor :: YiM ()
quitEditor = do
    savePersistentState
    onYiVar $ terminateSubprocesses (const True)
    withUI (`UI.end` True)

-- | Close the current window.
-- If this is the last window open, quit the program.
--
-- CONSIDER: call quitEditor when there are no other window in the
-- 'interactive' function. (Not possible since the windowset type
-- disallows it -- should it be relaxed?)
closeWindow :: YiM ()
closeWindow = do
    winCount <- withEditor $ uses windowsA PL.length
    tabCount <- withEditor $ uses tabsA PL.length
    when (winCount == 1 && tabCount == 1) quitEditor
    withEditor tryCloseE

-- | This is a like 'closeWindow' but with emacs behaviour of C-x 0:
-- if we're trying to close the minibuffer or last buffer in the
-- editor, then just print a message warning the user about it rather
-- closing mini or quitting editor.
closeWindowEmacs :: YiM ()
closeWindowEmacs = do
  wins <- withEditor $ use windowsA
  let winCount = PL.length wins
  tabCount <- withEditor $ uses tabsA PL.length

  case () of
   _ | winCount == 1 && tabCount == 1 ->
         printMsg "Attempt to delete sole ordinary window"
     | isMini (PL._focus wins) ->
         printMsg "Attempt to delete the minibuffer"
     | otherwise -> withEditor tryCloseE
