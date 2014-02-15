{-# LANGUAGE
  ScopedTypeVariables,
  RecursiveDo,
  Rank2Types,
  OverloadedStrings #-}

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
  , focusAllSyntax

  )
where

import Prelude hiding (elem,or,mapM_)
import Control.Concurrent
import Control.Monad hiding (mapM_,forM_,forM)
import Control.Monad.Error ()
import Control.Monad.Reader hiding (mapM_,forM_,forM)
import Control.Monad.Base
import Control.Exception
import Control.Exc
import Control.Applicative
import Control.Lens hiding (Action,act,acts)
import Data.Foldable
import Data.Traversable
import qualified Data.DelayList as DelayList
import Data.List (partition)
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
import System.IO (Handle, hWaitForInput, hPutStr)
import System.PosixCompat.Files
import System.Process (terminateProcess, getProcessExitCode, ProcessHandle, readProcessWithExitCode)

import Yi.Buffer
import Yi.Config
import Yi.Dynamic
import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Keys
import Yi.KillRing (krEndCmd)
import Yi.Utils
import Yi.Process (createSubprocess, readAvailable, SubprocessId, SubprocessInfo(..))
import Yi.String
import Yi.Style (errorStyle, strongHintStyle)
import Yi.Monad
import Yi.Debug
import qualified Yi.UI.Common as UI
import Yi.Window (dummyWindow, bufkey, wkey, winRegion)
import {-# source #-} Yi.PersistentState(loadPersistentState, savePersistentState)

-- | Make an action suitable for an interactive run.
-- UI will be refreshed.
interactive :: [Action] -> YiM ()
interactive action = do
  evs <- withEditor $ use pendingEventsA
  logPutStrLn $ ">>> interactively" ++ showEvs evs
  withEditor $ (%=) buffersA (fmap $  undosA %~ addChangeU InteractivePoint)
  mapM_ runAction action
  withEditor $ (%=) killringA krEndCmd
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
    let editor = fromMaybe emptyEditor st
    -- here to add load history etc?

    -- Setting up the 1st window is a bit tricky because most functions assume there exists a "current window"
    newSt <- newMVar $ YiVar editor [] 1 M.empty
    (ui, runYi) <- mdo
        let handler (exception :: SomeException) = runYi $ errorEditor (show exception) >> refreshEditor
            inF ev    = handle handler $ runYi $ dispatch ev
            outF acts = handle handler $ runYi $ interactive acts
            runYi f   = runReaderT (runYiM f) yi
            yi        = Yi ui inF outF cfg newSt
        ui <- uiStart cfg inF outF editor
        return (ui, runYi)

    runYi loadPersistentState

    runYi $ do if isNothing st
                    then postActions $ startActions cfg -- process options if booting for the first time
                    else withEditor $ (%=) buffersA (fmap (recoverMode (modeTable cfg))) -- otherwise: recover the mode of buffers
               postActions $ initialActions cfg ++ [makeAction showErrors]

    runYi refreshEditor

    UI.main ui -- transfer control to UI

recoverMode :: [AnyMode] -> FBuffer -> FBuffer
recoverMode tbl buffer  = case fromMaybe (AnyMode emptyMode) (find (\(AnyMode m) -> modeName m == oldName) tbl) of
    AnyMode m -> setMode0 m buffer
  where oldName = case buffer of FBuffer {bmode = m} -> modeName m

postActions :: [Action] -> YiM ()
postActions actions = do yi <- ask; liftBase $ yiOutput yi actions

-- | Display the errors buffer if it is not already visible.
showErrors :: YiM ()
showErrors = withEditor $ do
               bs <- gets $ findBufferWithName "*errors*"
               case bs of
                 [] -> return ()
                 _  -> do splitE
                          switchToBufferWithNameE "*errors*"

-- | Process an event by advancing the current keymap automaton an
-- execing the generated actions.
dispatch :: Event -> YiM ()
dispatch ev =
    do yi <- ask
       entryEvs <- withEditor $ use pendingEventsA
       logPutStrLn $ "pending events: " ++ showEvs entryEvs
       (userActions,_p') <- withBuffer $ do
         keymap <- gets (withMode0 modeKeymap)
         p0 <- use keymapProcessA
         let km = extractTopKeymap $ keymap $ defaultKm $ yiConfig yi
         let freshP = Chain (configInputPreprocess $ yiConfig yi) (mkAutomaton km)
             p = case computeState p0 of
                   Dead  -> freshP
                   _     -> p0
             (actions, p') = processOneEvent p ev
             state = computeState p'
             ambiguous = case state of
                 Ambiguous _ -> True
                 _ -> False
         assign keymapProcessA (if ambiguous then freshP else p')
         let actions0 = case state of
                          Dead -> [makeAction $ do
                                         evs <- use pendingEventsA
                                         printMsg ("Unrecognized input: " ++ showEvs (evs ++ [ev]))]
                          _ -> actions
             actions1 = [makeAction $ printMsg "Keymap was in an ambiguous state! Resetting it." | ambiguous]
         return (actions0 ++ actions1,p')
       -- logPutStrLn $ "Processing: " ++ show ev
       -- logPutStrLn $ "Actions posted:" ++ show userActions
       -- logPutStrLn $ "New automation: " ++ show _p'
       let decay, pendingFeedback :: EditorM ()
           decay = (%=) statusLinesA (DelayList.decrease 1)
           pendingFeedback = do (%=) pendingEventsA (++ [ev])
                                if null userActions
                                    then printMsg . showEvs =<< use pendingEventsA
                                    else assign pendingEventsA []
       postActions $ [makeAction decay] ++ userActions ++ [makeAction pendingFeedback]

showEvs = unwords . fmap prettyEvent
showEvs :: [Event] -> String

-- ---------------------------------------------------------------------
-- Meta operations

-- | Quit.
quitEditor :: YiM ()
quitEditor = do
    savePersistentState
    onYiVar $ terminateSubprocesses (const True)
    withUI (`UI.end` True)

-- | Update (visible) buffers if they have changed on disk.
-- FIXME: since we do IO here we must catch exceptions!
checkFileChanges :: Editor -> IO Editor
checkFileChanges e0 = do
        now <- getCurrentTime
        -- Find out if any file was modified "behind our back" by other processes.
        newBuffers <- forM (buffers e0) $ \b ->
          let nothing = return (b, Nothing)
          in if bkey b `elem` visibleBuffers
          then
            case b ^.identA of
               Right fname -> do
                  fe <- doesFileExist fname
                  if not fe then nothing else do
                  modTime <- fileModTime fname
                  if b ^. lastSyncTimeA < modTime
                     then if isUnchangedBuffer b
                       then do newContents <- R.readFile fname
                               return (snd $ runBuffer (dummyWindow $ bkey b) b (revertB newContents now), Just msg1)
                       else return (b, Just msg2)
                     else nothing
               _ -> nothing
          else nothing
        -- show appropriate update message if applicable
        return $ case getFirst (foldMap (First . snd) newBuffers) of
               Just msg -> (statusLinesA %~ DelayList.insert msg) e0 {buffers = fmap fst newBuffers}
               Nothing -> e0
    where msg1 = (1, (["File was changed by a concurrent process, reloaded!"], strongHintStyle))
          msg2 = (1, (["Disk version changed by a concurrent process"], strongHintStyle))
          visibleBuffers = fmap bufkey $ windows e0
          fileModTime f = posixSecondsToUTCTime . realToFrac . modificationTime <$> getFileStatus f


-- | Hide selection, clear "syntax dirty" flag (as appropriate).
clearAllSyntaxAndHideSelection :: Editor -> Editor
clearAllSyntaxAndHideSelection = buffersA %~ fmap (clearSyntax . clearHighlight)
  where
    clearHighlight fb =
      -- if there were updates, then hide the selection.
      let h = view highlightSelectionA fb
          us = view pendingUpdatesA fb
      in highlightSelectionA .~ (h && null us) $ fb


-- Focus syntax tree on the current window, for all visible buffers.
focusAllSyntax :: Editor -> Editor
focusAllSyntax e6 = buffersA %~ fmap (\b -> focusSyntax (regions b) b) $ e6
    where regions b = M.fromList [(wkey w, winRegion w) | w <- toList $ windows e6, bufkey w == bkey b]
          -- Why bother filtering the region list? After all the trees are lazily computed.
          -- Answer: focusing is an incremental algorithm. Each "focused" path depends on the previous one.
          -- If we left unforced focused paths, we'd create a long list of thunks: a memory leak.

-- | Redraw
refreshEditor :: YiM ()
refreshEditor = onYiVar $ \yi var -> do
        let cfg = yiConfig yi
            runOnWins a = runEditor cfg
                                    (do ws <- use windowsA
                                        forM ws $ flip withWindowE a)
            style = configScrollStyle $ configUI cfg
        let scroll e3 = let (e4, relayout) = runOnWins (snapScreenB style) e3 in
                -- Scroll windows to show current points as appropriate
                -- Do another layout pass if there was any scrolling;
                (if or relayout then UI.layout (yiUi yi) else return) e4

        e7 <- (if configCheckExternalChangesObsessively cfg then checkFileChanges else return) (yiEditor var) >>=
             return . clearAllSyntaxAndHideSelection >>=
             -- Adjust window sizes according to UI info
             UI.layout (yiUi yi) >>=
             scroll >>=
             -- Adjust point according to the current layout;
             return . (fst . runOnWins snapInsB) >>=
             return . focusAllSyntax >>=
             -- Clear "pending updates" and "followUp" from buffers.
             return . (buffersA %~ fmap (clearUpdates . clearFollow))
        -- Display the new state of the editor
        UI.refresh (yiUi yi) e7
        -- Terminate stale processes.
        terminateSubprocesses (staleProcess $ buffers e7) yi var {yiEditor = e7}
  where
    clearUpdates = pendingUpdatesA .~ []
    clearFollow = pointFollowsWindowA .~ const False
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
    (_,out,_err) <- liftBase $ readProcessWithExitCode f args inp
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
    winCount <- withEditor $ uses windowsA PL.length
    tabCount <- withEditor $ uses tabsA PL.length
    when (winCount == 1 && tabCount == 1) quitEditor
    withEditor tryCloseE


onYiVar :: (Yi -> YiVar -> IO (YiVar, a)) -> YiM a
onYiVar f = do
    yi <- ask
    io $ modifyMVar (yiVar yi) (f yi)

-- | Kill a given subprocess
terminateSubprocesses :: (SubprocessInfo -> Bool) -> Yi -> YiVar -> IO (YiVar, ())
terminateSubprocesses shouldTerminate _yi var = do
        let (toKill, toKeep) = partition (shouldTerminate . snd) $ M.assocs $ yiSubprocesses var
        void $ forM toKill $ terminateProcess . procHandle . snd
        return (var {yiSubprocesses = M.fromList toKeep}, ())

-- | Start a subprocess with the given command and arguments.
startSubprocess :: FilePath -> [String] -> (Either SomeException ExitCode -> YiM x) -> YiM BufferRef
startSubprocess cmd args onExit = onYiVar $ \yi var -> do
        let (e', bufref) = runEditor
                              (yiConfig yi)
                              (printMsg ("Launched process: " ++ cmd) >> newBufferE (Left bufferName) "")
                              (yiEditor var)
            procid = yiSubprocessIdSupply var + 1
        procinfo <- createSubprocess cmd args bufref
        startSubprocessWatchers procid procinfo yi onExit
        return (var {yiEditor = e',
                     yiSubprocessIdSupply = procid,
                     yiSubprocesses = M.insert procid procinfo (yiSubprocesses var)
                    }, bufref)
  where bufferName = "output from " ++ cmd ++ " " ++ show args

startSubprocessWatchers :: SubprocessId -> SubprocessInfo -> Yi -> (Either SomeException ExitCode -> YiM x) -> IO ()
startSubprocessWatchers procid procinfo yi onExit =
    mapM_ forkOS ([pipeToBuffer (hErr procinfo) (send . append True) | separateStdErr procinfo] ++
                  [pipeToBuffer (hOut procinfo) (send . append False),
                   waitForExit (procHandle procinfo) >>= reportExit])
  where send a = yiOutput yi [makeAction a]
        append :: Bool -> String -> YiM ()
        append atMark s = withEditor $ appendToBuffer atMark (bufRef procinfo) s
        reportExit ec = send $ do append True ("Process exited with " ++ show ec)
                                  removeSubprocess procid
                                  void $ onExit ec

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
    forM_ mms (`modifyMarkB` (\ v -> v{markGravity = Forward}))
    insertNAt s =<< use (markPointA (if atErr then me else mo))
    forM_ mms (`modifyMarkB` (\ v -> v{markGravity = Backward}))

sendToProcess :: BufferRef -> String -> YiM ()
sendToProcess bufref s = do
    yi <- ask
    Just subProcessInfo <- find ((== bufref) . bufRef) . yiSubprocesses <$> readRef (yiVar yi)
    io $ hPutStr (hIn subProcessInfo) s

pipeToBuffer :: Handle -> (String -> IO ()) -> IO ()
pipeToBuffer h append =
  do _ <- ignoringException $ forever (do _ <- hWaitForInput h (-1)
                                          r <- readAvailable h
                                          _ <- append r
                                          return ())
     return ()



waitForExit :: ProcessHandle -> IO (Either SomeException ExitCode)
waitForExit ph =
    handle (\e -> return (Left (e :: SomeException))) $ do
      mec <- getProcessExitCode ph
      case mec of
          Nothing -> threadDelay (500*1000) >> waitForExit ph
          Just ec -> return (Right ec)

withSyntax :: (Show x, YiAction a x) => (forall syntax. Mode syntax -> syntax -> a) -> YiM ()
withSyntax f = do
            b <- gets currentBuffer
            act <- withGivenBuffer b $ withSyntaxB f
            runAction $ makeAction act

userForceRefresh :: YiM ()
userForceRefresh = withUI UI.userForceRefresh
