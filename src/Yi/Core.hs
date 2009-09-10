{-# LANGUAGE ScopedTypeVariables, RecursiveDo, Rank2Types #-}

-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) Don Stewart 2004-5. http://www.cse.unsw.edu.au/~dons
-- Copyright (c) Jean-Philippe Bernardy 2007-8 

-- | The core actions of yi. This module is the link between the editor
-- and the UI. Key bindings, and libraries should manipulate Yi through
-- the interface defined here.

module Yi.Core 
  ( module Yi.Dynamic
    -- * Keymap
  , module Yi.Keymap

  , module Yi.Prelude
  , module Yi.Editor
  , module Yi.Buffer
  , module Yi.Keymap.Keys

  -- * Construction and destruction
  , startEditor         
  , quitEditor          -- :: YiM ()

  -- * User interaction
  , refreshEditor       -- :: YiM ()
  , suspendEditor       -- :: YiM ()
  , userForceRefresh  

  -- * Global editor actions
  , msgEditor           -- :: String -> YiM ()
  , errorEditor         -- :: String -> YiM ()
  , closeWindow         -- :: YiM ()

  -- * Interacting with external commands
  , runProcessWithInput          -- :: String -> String -> YiM String
  , startSubprocess                 -- :: FilePath -> [String] -> YiM ()
  , sendToProcess

  -- * Misc
  , runAction
  , withSyntax

  ) 
where

import Prelude (realToFrac)

import Control.Concurrent
import Control.Monad (when, forever)
import Control.Monad.Error ()
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.Trans
import Control.OldException
import qualified Data.DelayList as DelayList
import Data.List (intercalate, partition)
import Data.List.Split (splitOn)
import qualified Data.List.PointedList.Circular as PL
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Rope as R
import System.Directory (doesFileExist)
import System.Exit
import System.FilePath
import System.IO (Handle, hWaitForInput, hPutStr)
import qualified System.IO.UTF8 as UTF8
import System.PosixCompat.Files
import System.Process (terminateProcess, getProcessExitCode, ProcessHandle)

import Yi.Buffer
import Yi.Config
import Yi.Dynamic
import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Keys
import Yi.KillRing (krEndCmd)
import Yi.Prelude
import Yi.Process (popen, createSubprocess, readAvailable, SubprocessId, SubprocessInfo(..))
import Yi.String
import Yi.Style (errorStyle, strongHintStyle)
import Yi.UI.Common as UI (UI)
import qualified Yi.UI.Common as UI
import Yi.Window (dummyWindow, bufkey, wkey, winRegion)

-- | Make an action suitable for an interactive run.
-- UI will be refreshed.
interactive :: [Action] -> YiM ()
interactive action = do
  evs <- withEditor $ getA pendingEventsA
  logPutStrLn $ ">>> interactively" ++ showEvs evs
  withEditor $ modA buffersA (fmap $  undosA ^: addChangeU InteractivePoint)
  mapM_ runAction action
  withEditor $ modA killringA krEndCmd
  refreshEditor
  logPutStrLn "<<<"
  return ()

-- ---------------------------------------------------------------------
-- | Start up the editor, setting any state with the user preferences
-- and file names passed in, and turning on the UI
--
startEditor :: Config -> Maybe Editor -> IO ()
startEditor cfg st = do
    let uiStart = startFrontEnd cfg

    logPutStrLn "Starting Core"

    -- Use an empty state unless resuming from an earlier session and one is already available
    let editor = maybe emptyEditor id st

    -- Setting up the 1st window is a bit tricky because most functions assume there exists a "current window"
    newSt <- newMVar $ YiVar editor [] 1 M.empty
    (ui, runYi) <- mdo let handler exception = runYi $ (errorEditor (show exception) >> refreshEditor)
                           inF  ev  = handle handler (runYi (dispatch ev))
                           outF acts = handle handler (runYi (interactive acts))
                       ui <- uiStart cfg inF outF editor
                       let runYi f = runReaderT (runYiM f) yi
                           yi = Yi ui inF outF cfg newSt 
                       return (ui, runYi)
  
    runYi $ do
      if isNothing st 
         then postActions $ startActions cfg ++ [makeAction showErrors] -- process options if booting for the first time
         else withEditor $ modA buffersA (fmap (recoverMode (modeTable cfg))) -- otherwise: recover the mode of buffers

    runYi refreshEditor

    UI.main ui -- transfer control to UI

recoverMode :: [AnyMode] -> FBuffer -> FBuffer
recoverMode tbl buffer  = case fromMaybe (AnyMode emptyMode) (find (\(AnyMode m) -> modeName m == oldName) tbl) of
    AnyMode m -> setMode0 m buffer
  where oldName = case buffer of FBuffer {bmode = m} -> modeName m

postActions :: [Action] -> YiM ()
postActions actions = do yi <- ask; liftIO $ output yi actions

-- | Display the errors buffer if it is not already visible.
showErrors :: YiM ()
showErrors = withEditor $ do
               bs <- gets $ findBufferWithName ".yi/yi.errors"
               case bs of
                 [] -> return ()
                 _  -> do splitE
                          switchToBufferWithNameE ".yi/yi.errors"

-- | Process an event by advancing the current keymap automaton an
-- execing the generated actions.
dispatch :: Event -> YiM ()
dispatch ev =
    do yi <- ask
       entryEvs <- withEditor $ getA pendingEventsA
       logPutStrLn $ "pending events: " ++ showEvs entryEvs
       (userActions,_p') <- withBuffer $ do
         keymap <- gets (withMode0 modeKeymap)
         p0 <- getA keymapProcessA
         let km = extractTopKeymap $ keymap $ defaultKm $ yiConfig $ yi
         let freshP = Chain (configInputPreprocess $ yiConfig $ yi) (mkAutomaton km)
             p = case computeState p0 of
                   Dead  -> freshP
                   _     -> p0
             (actions, p') = processOneEvent p ev
             state = computeState p'
             ambiguous = case state of 
                 Ambiguous _ -> True
                 _ -> False
         putA keymapProcessA (if ambiguous then freshP else p')
         let actions0 = case state of 
                          Dead -> [makeAction $ do
                                         evs <- getA pendingEventsA
                                         printMsg ("Unrecognized input: " ++ showEvs (evs ++ [ev]))]
                          _ -> actions
             actions1 = if ambiguous 
                          then [makeAction $ printMsg "Keymap was in an ambiguous state! Resetting it."]
                          else []
         return (actions0 ++ actions1,p')
       -- logPutStrLn $ "Processing: " ++ show ev
       -- logPutStrLn $ "Actions posted:" ++ show userActions
       -- logPutStrLn $ "New automation: " ++ show _p'
       let decay, pendingFeedback :: EditorM ()
           decay = modA statusLinesA (DelayList.decrease 1)
           pendingFeedback = do modA pendingEventsA (++ [ev])
                                if null userActions
                                    then printMsg . showEvs =<< getA pendingEventsA
                                    else putA pendingEventsA []
       postActions $ [makeAction decay] ++ userActions ++ [makeAction pendingFeedback]

showEvs = intercalate " " . fmap prettyEvent
showEvs :: [Event] -> String

-- ---------------------------------------------------------------------
-- Meta operations

-- | Quit.
quitEditor :: YiM ()
quitEditor = do
    onYiVar $ terminateSubprocesses (const True)
    withUI (flip UI.end True)

-- | Redraw
refreshEditor :: YiM ()
refreshEditor = onYiVar $ \yi var -> do
        let e0 = yiEditor var 
            msg1 = (1, (["File was changed by a concurrent process, reloaded!"], strongHintStyle))
            msg2 = (1, (["Disk version changed by a concurrent process"], strongHintStyle))
            visibleBuffers = fmap bufkey $ windows e0
        -- Find out if any file was modified "behind our back" by other processes.            
        -- FIXME: since we do IO here we must catch exceptions!

        -- Update (visible) buffers if they have changed on disk.
        now <- getCurrentTime
        newBuffers <- forM (buffers e0) $ \b -> 
          let nothing = return (b, Nothing) 
          in if bkey b `elem` visibleBuffers
          then do
            case b ^.identA of
               Right fname -> do 
                  fe <- doesFileExist fname
                  if not fe then nothing else do
                  modTime <- fileModTime fname
                  if b ^. lastSyncTimeA < modTime
                     then if isUnchangedBuffer b
                       then do newContents <- UTF8.readFile fname
                               return (snd $ runBuffer (dummyWindow $ bkey b) b (revertB newContents now), Just msg1)
                       else do return (b, Just msg2)
                     else nothing
               _ -> nothing
          else nothing  
    
        -- TODO: rewrite this sequence as a pipeline, in order to get rid of the editor names. (e1..e8)
        -- show appropriate update message if applicable
        let e1 = case getFirst (foldMap (First . snd) newBuffers) of
               Just msg -> (statusLinesA ^: DelayList.insert msg) e0 {buffers = fmap fst newBuffers}
               Nothing -> e0
            -- Hide selection, clear "syntax dirty" flag (as appropriate).
            e2 = buffersA ^: (fmap (clearSyntax . clearHighlight)) $ e1

        -- Adjust window sizes according to UI info
        let runOnWins a = runEditor (yiConfig yi)
                                    (do ws <- getA windowsA
                                        forM ws $ flip withWindowE a)

        e3 <- UI.layout (yiUi yi) e2
        -- Scroll windows to show current points as appropriate
        let (e4, relayout) = runOnWins snapScreenB e3
        -- Do another layout pass if there was any scrolling;
        e5 <- (if or relayout then UI.layout (yiUi yi) else return) e4
        -- Adjust point according to the current layout;
        let e6 = fst $ runOnWins snapInsB e5
        -- Focus syntax tree on the current window.
            regions b = M.fromList [(wkey w, winRegion w) | w <- toList $ windows e6, bufkey w == bkey b]
            e7 = buffersA ^: (fmap (\b -> focusSyntax (regions b) b)) $ e6
        -- Display the new state of the editor
        UI.refresh (yiUi yi) e7
        -- Clear "pending updates" and "followUp" from buffers.
        let e8 = buffersA ^: (fmap (clearUpdates . clearFollow)) $ e7
        -- Terminate stale processes.
        terminateSubprocesses (staleProcess $ buffers e5) yi var {yiEditor = e8}
  where
    clearUpdates = pendingUpdatesA ^= []
    clearFollow = pointFollowsWindowA ^= const False
    clearHighlight fb =
      -- if there were updates, then hide the selection.
      let h = getVal highlightSelectionA fb
          us = getVal pendingUpdatesA fb
      in highlightSelectionA ^= (h && null us) $ fb
    fileModTime f = posixSecondsToUTCTime . realToFrac . modificationTime <$> getFileStatus f
    -- | Is this process stale? (associated with a deleted buffer)
    staleProcess bs p = not (bufRef p `M.member` bs)
    

-- | Suspend the program
suspendEditor :: YiM ()
suspendEditor = withUI UI.suspend

------------------------------------------------------------------------

------------------------------------------------------------------------
-- | Pipe a string through an external command, returning the stdout
-- chomp any trailing newline (is this desirable?)
--
-- Todo: varients with marks?
--
runProcessWithInput :: String -> String -> YiM String
runProcessWithInput cmd inp = do
    let (f:args) = splitOn " " cmd
    (out,_err,_) <- liftIO $ popen f args (Just inp)
    return (chomp "\n" out)


------------------------------------------------------------------------

-- | Same as msgEditor, but do nothing instead of printing @()@
msgEditor' :: String -> YiM ()
msgEditor' "()" = return ()
msgEditor' s = msgEditor s

runAction :: Action -> YiM ()
runAction (YiA act) = do
  act >>= msgEditor' . show
  return ()
runAction (EditorA act) = do
  withEditor act >>= msgEditor' . show
  return ()
runAction (BufferA act) = do
  withBuffer act >>= msgEditor' . show
  return ()
runAction (TaggedA _ act) = runAction act

msgEditor :: String -> YiM ()
msgEditor = withEditor . printMsg

-- | Show an error on the status line and log it.
errorEditor :: String -> YiM ()
errorEditor s = do withEditor $ printStatus (["error: " ++ s], errorStyle)
                   logPutStrLn $ "errorEditor: " ++ s

-- | Close the current window.
-- If this is the last window open, quit the program.
-- CONSIDER: call quitEditor when there are no other window in the 'interactive' function.
-- (Not possible since the windowset type disallows it -- should it be relaxed?)
closeWindow :: YiM ()
closeWindow = do
    winCount <- withEditor $ getsA windowsA PL.length
    tabCount <- withEditor $ getsA tabsA PL.length
    when (winCount == 1 && tabCount == 1) quitEditor
    withEditor $ tryCloseE


onYiVar :: (Yi -> YiVar -> IO (YiVar, a)) -> YiM a
onYiVar f = do
    yi <- ask
    io $ modifyMVar (yiVar yi) (f yi)

-- | Kill a given subprocess
terminateSubprocesses :: (SubprocessInfo -> Bool) -> Yi -> YiVar -> IO (YiVar, ())
terminateSubprocesses shouldTerminate _yi var = do
        let (toKill, toKeep) = partition (shouldTerminate . snd) $ M.assocs $ yiSubprocesses var
        forM toKill $ terminateProcess . procHandle . snd
        return (var {yiSubprocesses = M.fromList toKeep}, ())

-- | Start a subprocess with the given command and arguments.
startSubprocess :: FilePath -> [String] -> (Either Exception ExitCode -> YiM x) -> YiM BufferRef
startSubprocess cmd args onExit = onYiVar $ \yi var -> do
        let (e', bufref) = runEditor 
                              (yiConfig yi) 
                              (printMsg ("Launched process: " ++ cmd) >> newBufferE (Left bufferName) (R.fromString ""))
                              (yiEditor var)
            procid = yiSubprocessIdSupply var + 1
        procinfo <- createSubprocess cmd args bufref
        startSubprocessWatchers procid procinfo yi onExit
        return (var {yiEditor = e', 
                     yiSubprocessIdSupply = procid,
                     yiSubprocesses = M.insert procid procinfo (yiSubprocesses var)
                    }, bufref)
  where bufferName = "output from " ++ cmd ++ " " ++ show args

startSubprocessWatchers :: SubprocessId -> SubprocessInfo -> Yi -> (Either Exception ExitCode -> YiM x) -> IO ()
startSubprocessWatchers procid procinfo yi onExit = do
    mapM_ forkOS ([pipeToBuffer (hErr procinfo) (send . append True) | separateStdErr procinfo] ++
                  [pipeToBuffer (hOut procinfo) (send . append False),
                   waitForExit (procHandle procinfo) >>= reportExit])
  where send a = output yi [makeAction a]
        append :: Bool -> String -> YiM ()
        append atMark s = withEditor $ appendToBuffer atMark (bufRef procinfo) s
        reportExit ec = send $ do append True ("Process exited with " ++ show ec)
                                  removeSubprocess procid
                                  onExit ec
                                  return ()

removeSubprocess :: SubprocessId -> YiM ()
removeSubprocess procid = modifiesRef yiVar (\v -> v {yiSubprocesses = M.delete procid $ yiSubprocesses v})

appendToBuffer :: Bool -> BufferRef -> String -> EditorM ()
appendToBuffer atErr bufref s = withGivenBuffer0 bufref $ do
    -- We make sure stdout is always after stderr. This ensures that the output of the
    -- two pipe do not get interleaved. More importantly, GHCi prompt should always
    -- come after the error messages.
    me <- getMarkB (Just "StdERR")
    mo <- getMarkB (Just "StdOUT")
    let mms = if atErr then [mo,me] else [mo]
    forM_ mms (flip modifyMarkB (\v -> v {markGravity = Forward}))
    insertNAt s =<< getMarkPointB (if atErr then me else mo)
    forM_ mms (flip modifyMarkB (\v -> v {markGravity = Backward}))

sendToProcess :: BufferRef -> String -> YiM ()
sendToProcess bufref s = do
    yi <- ask
    Just subProcessInfo <- find ((== bufref) . bufRef) . yiSubprocesses <$> readRef (yiVar yi)
    io $ hPutStr (hIn subProcessInfo) s

pipeToBuffer :: Handle -> (String -> IO ()) -> IO ()
pipeToBuffer h append = 
  handle (const $ return ()) $ forever $ (hWaitForInput h (-1) >> readAvailable h >>= append)


waitForExit :: ProcessHandle -> IO (Either Exception ExitCode)
waitForExit ph = 
    handle (\e -> return (Left e)) $ do 
      mec <- getProcessExitCode ph
      case mec of
          Nothing -> threadDelay (500*1000) >> waitForExit ph
          Just ec -> return (Right ec)

withSyntax :: (Show x, YiAction a x) => (forall syntax. Mode syntax -> syntax -> a) -> YiM ()
withSyntax f = do
            b <- gets currentBuffer
            act <- withGivenBuffer b $ withSyntaxB f
            runAction $ makeAction $ act

userForceRefresh :: YiM ()
userForceRefresh = withUI UI.userForceRefresh
