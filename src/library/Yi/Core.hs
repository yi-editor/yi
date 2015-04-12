{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Core
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The core actions of Yi. This module is the link between the editor
-- and the UI. Key bindings, and libraries should manipulate Yi
-- through the interface defined here.

module Yi.Core
  (
  -- * Construction and destruction
    startEditor
  , quitEditor          -- :: YiM ()

  -- * User interaction
  , refreshEditor       -- :: YiM ()
  , suspendEditor       -- :: YiM ()
  , userForceRefresh

  -- * Global editor actions
  , errorEditor         -- :: String -> YiM ()
  , closeWindow         -- :: YiM ()
  , closeWindowEmacs

  -- * Interacting with external commands
  , runProcessWithInput          -- :: String -> String -> YiM String
  , startSubprocess                 -- :: FilePath -> [String] -> YiM ()
  , sendToProcess

  -- * Misc
  , runAction
  , withSyntax
  , focusAllSyntax
  , forkAction
  ) where

import           Prelude                        hiding (elem, mapM_, or)

import           Control.Applicative            (Applicative (pure), (<$>))
import           Control.Concurrent             (ThreadId, forkIO, forkOS,
                                                 modifyMVar, modifyMVar_,
                                                 newMVar, readMVar, threadDelay)
import           Control.Exc                    (ignoringException)
import           Control.Exception              (SomeException, handle)
import           Control.Lens                   (assign, mapped, use, uses,
                                                 view, (%=), (%~), (&), (.=),
                                                 (.~), (^.))
import           Control.Monad                  (forever, void, when)
import           Control.Monad.Base             (MonadBase (liftBase))
import           Control.Monad.Error            ()
import           Control.Monad.Reader           (MonadReader (ask), ReaderT (runReaderT), asks)
import qualified Data.DelayList                 as DelayList (decrease, insert)
import           Data.Foldable                  (Foldable (foldMap), elem, find, forM_, mapM_, or, toList)
import           Data.List                      (partition)
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.List.PointedList.Circular as PL (PointedList (_focus), length)
import           Data.List.Split                (splitOn)
import qualified Data.Map                       as M (assocs, delete, empty, fromList, insert, member)
import           Data.Maybe                     (fromMaybe, isNothing)
import           Data.Monoid                    (First (First, getFirst), (<>))
import qualified Data.Text                      as T (Text, pack, unwords)
import           Data.Time                      (getCurrentTime)
import           Data.Time.Clock.POSIX          (posixSecondsToUTCTime)
import           Data.Traversable               (forM)
import           GHC.Conc                       (labelThread)
import           System.Directory               (doesFileExist)
import           System.Exit                    (ExitCode)
import           System.IO                      (Handle, hPutStr, hWaitForInput)
import           System.PosixCompat.Files       (getFileStatus, modificationTime)
import           System.Process                 (ProcessHandle,
                                                 getProcessExitCode,
                                                 readProcessWithExitCode,
                                                 terminateProcess)
import           Yi.Buffer
import           Yi.Config
import           Yi.Debug                       (logPutStrLn)
import           Yi.Editor
import           Yi.Keymap
import           Yi.Keymap.Keys
import           Yi.KillRing                    (krEndCmd)
import           Yi.Monad                       (gets)
import           Yi.PersistentState             (loadPersistentState, savePersistentState)
import           Yi.Process
import qualified Yi.Rope                        as R (YiString, fromString, readFile)
import           Yi.String                      (chomp, showT)
import           Yi.Style                       (errorStyle, strongHintStyle)
import qualified Yi.UI.Common                   as UI (UI (end, layout, main, refresh, suspend, userForceRefresh))
import           Yi.Utils                       (io)
import           Yi.Window                      (bufkey, dummyWindow, isMini, winRegion, wkey)

-- | Make an action suitable for an interactive run.
-- UI will be refreshed.
interactive :: IsRefreshNeeded -> [Action] -> YiM ()
interactive isRefreshNeeded action = do
  evs <- withEditor $ use pendingEventsA
  logPutStrLn $ ">>> interactively" <> showEvs evs
  withEditor $ buffersA %= (fmap $ undosA %~ addChangeU InteractivePoint)
  mapM_ runAction action
  withEditor $ killringA %= krEndCmd
  when (isRefreshNeeded == MustRefresh) refreshEditor
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

-- | Runs a 'YiM' action in a separate thread.
--
-- Notes:
--
-- * It seems to work but I don't know why
--
-- * Maybe deadlocks?
--
-- * If you're outputting into the Yi window, you should really limit
-- the rate at which you do so: for example, the Pango front-end will
-- quite happily segfault/double-free if you output too fast.
--
-- I am exporting this for those adventurous to play with but I have
-- only discovered how to do this a night before the release so it's
-- rather experimental. A simple function that prints a message once a
-- second, 5 times, could be written like this:
--
-- @
-- printer :: YiM ThreadId
-- printer = do
--   mv <- io $ newMVar (0 :: Int)
--   forkAction (suicide mv) MustRefresh $ do
--     c <- io $ do
--       modifyMVar_ mv (return . succ)
--       tryReadMVar mv
--     case c of
--       Nothing -> printMsg "messaging unknown time"
--       Just x -> printMsg $ "message #" <> showT x
--   where
--     suicide mv = tryReadMVar mv >>= \case
--       Just i | i >= 5 -> return True
--       _ -> threadDelay 1000000 >> return False
-- @
forkAction :: (YiAction a x, Show x)
           => IO Bool
              -- ^ runs after we insert the action: this may be a
              -- thread delay or a thread suicide or whatever else;
              -- when delay returns False, that's our signal to
              -- terminate the thread.
           -> IsRefreshNeeded
              -- ^ should we refresh after each action
           -> a
              -- ^ The action to actually run
           -> YiM ThreadId
forkAction delay ref ym = onYiVar $ \yi yv -> do
  let loop = do
        yiOutput yi ref [makeAction ym]
        delay >>= \b -> when b loop
  t <- forkIO loop
  return (yv, t)

recoverMode :: [AnyMode] -> FBuffer -> FBuffer
recoverMode tbl buffer  = case fromMaybe (AnyMode emptyMode) (find (\(AnyMode m) -> modeName m == oldName) tbl) of
    AnyMode m -> setMode0 m buffer
  where oldName = case buffer of FBuffer {bmode = m} -> modeName m

postActions :: IsRefreshNeeded -> [Action] -> YiM ()
postActions refreshNeeded actions = do yi <- ask; liftBase $ yiOutput yi refreshNeeded actions

-- | Display the errors buffer if it is not already visible.
showErrors :: YiM ()
showErrors = withEditor $ do
               bs <- gets $ findBufferWithName "*errors*"
               case bs of
                 [] -> return ()
                 _  -> do splitE
                          switchToBufferWithNameE "*errors*"

-- | Process events by advancing the current keymap automaton and
-- executing the generated actions.
dispatch :: NonEmpty Event -> YiM ()
dispatch (ev :| evs) = do
  yi <- ask
  (userActions, _p') <- withCurrentBuffer $ do
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
          Dead -> [EditorA $ do
                      evs' <- use pendingEventsA
                      printMsg ("Unrecognized input: " <> showEvs (evs' ++ [ev]))]
          _ -> actions

        actions1 = [ EditorA (printMsg "Keymap was in an ambiguous state! Resetting it.")
                   | ambiguous]

    return (actions0 ++ actions1, p')

  let decay, pendingFeedback :: EditorM ()
      decay = statusLinesA %= DelayList.decrease 1
      pendingFeedback = do pendingEventsA %= (++ [ev])
                           if null userActions
                               then printMsg . showEvs =<< use pendingEventsA
                               else assign pendingEventsA []
      allActions = [makeAction decay] ++ userActions ++ [makeAction pendingFeedback]

  case evs of
    [] -> postActions MustRefresh allActions
    (e:es) -> postActions NoNeedToRefresh allActions >> dispatch (e :| es)


showEvs :: [Event] -> T.Text
showEvs = T.unwords . fmap (T.pack . prettyEvent)

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
  -- Find out if any file was modified "behind our back" by
  -- other processes.
  newBuffers <- forM (buffers e0) $ \b ->
    let nothing = return (b, Nothing)
    in if bkey b `elem` visibleBuffers
    then
      case b ^. identA of
         FileBuffer fname -> do
            fe <- doesFileExist fname
            if not fe then nothing else do
            modTime <- fileModTime fname
            if b ^. lastSyncTimeA < modTime
               then if isUnchangedBuffer b
                 then R.readFile fname >>= return . \case
                        Left m ->
                          (runDummy b (readOnlyA .= True), Just $ msg3 m)
                        Right (newContents, c) ->
                          (runDummy b (revertB newContents (Just c) now), Just msg1)
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
        msg3 x = (1, (["File changed on disk to unknown encoding, not updating buffer: " <> x], strongHintStyle))
        visibleBuffers = bufkey <$> windows e0
        fileModTime f = posixSecondsToUTCTime . realToFrac . modificationTime <$> getFileStatus f
        runDummy b act = snd $ runBuffer (dummyWindow $ bkey b) b act

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
          -- Why bother filtering the region list? After all the trees
          -- are lazily computed. Answer: focusing is an incremental
          -- algorithm. Each "focused" path depends on the previous
          -- one. If we left unforced focused paths, we'd create a
          -- long list of thunks: a memory leak.

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

        e7 <- (if configCheckExternalChangesObsessively cfg
               then checkFileChanges
               else return) (yiEditor var) >>=
             return . clearAllSyntaxAndHideSelection >>=
             -- Adjust window sizes according to UI info
             UI.layout (yiUi yi) >>=
             scroll >>=
             -- Adjust point according to the current layout;
             return . fst . runOnWins snapInsB >>=
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
    -- Is this process stale? (associated with a deleted buffer)
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

-- | Same as 'Yi.Editor.printMsg', but do nothing instead of printing @()@
msgEditor :: T.Text -> YiM ()
msgEditor "()" = return ()
msgEditor s = printMsg s

runAction :: Action -> YiM ()
runAction (YiA act) = act >>= msgEditor . showT
runAction (EditorA act) = withEditor act >>= msgEditor . showT
runAction (BufferA act) = withCurrentBuffer act >>= msgEditor . showT

-- | Show an error on the status line and log it.
errorEditor :: T.Text -> YiM ()
errorEditor s = do
  printStatus (["error: " <> s], errorStyle)
  logPutStrLn $ "errorEditor: " <> s

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

onYiVar :: (Yi -> YiVar -> IO (YiVar, a)) -> YiM a
onYiVar f = do
  yi <- ask
  io $ modifyMVar (yiVar yi) (f yi)

-- | Kill a given subprocess
terminateSubprocesses :: (SubprocessInfo -> Bool) -> Yi -> YiVar -> IO (YiVar, ())
terminateSubprocesses shouldTerminate _yi var = do
  let (toKill, toKeep) =
        partition (shouldTerminate . snd) $ M.assocs $ yiSubprocesses var
  void $ forM toKill $ terminateProcess . procHandle . snd
  return (var & yiSubprocessesA .~ M.fromList toKeep, ())

-- | Start a subprocess with the given command and arguments.
startSubprocess :: FilePath
                -> [String]
                -> (Either SomeException ExitCode -> YiM x)
                -> YiM BufferRef
startSubprocess cmd args onExit = onYiVar $ \yi var -> do
        let (e', bufref) = runEditor
                              (yiConfig yi)
                              (printMsg ("Launched process: " <> T.pack cmd)
                               >> newEmptyBufferE (MemBuffer bufferName))
                              (yiEditor var)
            procid = yiSubprocessIdSupply var + 1
        procinfo <- createSubprocess cmd args bufref
        startSubprocessWatchers procid procinfo yi onExit
        return (var & yiEditorA .~ e'
                    & yiSubprocessIdSupplyA .~ procid
                    & yiSubprocessesA %~ M.insert procid procinfo
               , bufref)
  where
    bufferName = T.unwords [ "output from", T.pack cmd, showT args ]

startSubprocessWatchers :: SubprocessId
                        -> SubprocessInfo
                        -> Yi
                        -> (Either SomeException ExitCode -> YiM x)
                        -> IO ()
startSubprocessWatchers procid procinfo yi onExit =
    mapM_ (\(labelSuffix, run) -> do
              threadId <- forkOS run
              labelThread threadId (procCmd procinfo ++ labelSuffix))
          ([("Err", pipeToBuffer (hErr procinfo) (send . append True)) | separateStdErr procinfo] ++
           [("Out", pipeToBuffer (hOut procinfo) (send . append False)),
            ("Exit", waitForExit (procHandle procinfo) >>= reportExit)])
  where
    send :: YiM () -> IO ()
    send a = yiOutput yi MustRefresh [makeAction a]

    -- TODO: This 'String' here is due to 'pipeToBuffer' but I don't
    -- know how viable it would be to read from a process as Text.
    -- Probably not worse than String but needs benchmarking.
    append :: Bool -> String -> YiM ()
    append atMark =
      withEditor . appendToBuffer atMark (bufRef procinfo) . R.fromString

    reportExit :: Either SomeException ExitCode -> IO ()
    reportExit ec = send $ do
      append True $ "Process exited with " <> show ec
      removeSubprocess procid
      void $ onExit ec

removeSubprocess :: SubprocessId -> YiM ()
removeSubprocess procid = asks yiVar >>= liftBase . flip modifyMVar_ (pure . (yiSubprocessesA %~ M.delete procid))

-- | Appends a 'R.YiString' to the given buffer.
--
-- TODO: Figure out and document the Bool here. Probably to do with
-- 'startSubprocessWatchers'.
appendToBuffer :: Bool      -- Something to do with stdout/stderr?
               -> BufferRef -- ^ Buffer to append to
               -> R.YiString  -- ^ Text to append
               -> EditorM ()
appendToBuffer atErr bufref s = withGivenBuffer bufref $ do
    -- We make sure stdout is always after stderr. This ensures that
    -- the output of the two pipe do not get interleaved. More
    -- importantly, GHCi prompt should always come after the error
    -- messages.
    me <- getMarkB (Just "StdERR")
    mo <- getMarkB (Just "StdOUT")
    let mms = if atErr then [mo, me] else [mo]
    forM_ mms (`modifyMarkB` (markGravityAA .~ Forward))
    insertNAt s =<< use (markPointA (if atErr then me else mo))
    forM_ mms (`modifyMarkB` (markGravityAA .~ Backward))

sendToProcess :: BufferRef -> String -> YiM ()
sendToProcess bufref s = do
    yi <- ask
    find ((== bufref) . bufRef) . yiSubprocesses <$> liftBase (readMVar (yiVar yi)) >>= \case
      Just subProcessInfo -> io $ hPutStr (hIn subProcessInfo) s
      Nothing -> printMsg "Could not get subProcessInfo in sendToProcess"

pipeToBuffer :: Handle -> (String -> IO ()) -> IO ()
pipeToBuffer h append = void . ignoringException . forever $ do
  _ <- hWaitForInput h (-1)
  r <- readAvailable h
  append r

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
