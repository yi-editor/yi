{-# LANGUAGE PatternSignatures, RecursiveDo, Rank2Types #-}

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

  -- * Construction and destruction
  , startEditor         
  , quitEditor          -- :: YiM ()

  , reloadEditor        -- :: YiM ()
  , getAllNamesInScope

  , refreshEditor       -- :: YiM ()
  , suspendEditor       -- :: YiM ()

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
  , withMode
  , withSyntax
  ) 
where

import Prelude ()
import Yi.Prelude

import Yi.Config
import Yi.Debug
import Yi.Undo
import Yi.Buffer
import Yi.Dynamic
import Yi.String
import Yi.Process ( popen, createSubprocess, readAvailable, SubprocessId, SubprocessInfo(..) )
import Yi.Editor
import Yi.Event (Event, prettyEvent)
import Yi.Keymap
import Yi.KillRing (krEndCmd)
import qualified Yi.Interact as I
import Yi.Monad
import Yi.Accessor
import qualified Yi.WindowSet as WS
import qualified Yi.Editor as Editor
import qualified Yi.UI.Common as UI
import Yi.UI.Common as UI (UI)
import qualified Data.DelayList as DelayList

import Data.List (intercalate)
import Data.Maybe
import qualified Data.Map as M
import Data.Foldable (mapM_, all)

import System.IO (Handle, hWaitForInput, hPutStr)
import System.FilePath
import System.Process ( getProcessExitCode, ProcessHandle )

import Control.Monad (when,forever)
import Control.Monad.Reader (runReaderT, ask, asks)
import Control.Monad.Trans
import Control.Monad.Error ()
import Control.Monad.State (gets, get, put)
import Control.Exception
import Control.Concurrent

-- | Make an action suitable for an interactive run.
-- UI will be refreshed.
interactive :: [Action] -> YiM ()
interactive action = do
  evs <- withEditor $ getA pendingEventsA
  logPutStrLn $ ">>> interactively" ++ showEvs evs
  prepAction <- withUI UI.prepareAction
  withEditor $ do prepAction
                  modifyAllA buffersA undosA (addChangeU InteractivePoint)
  mapM_ runAction action
  withEditor $ modifyA killringA krEndCmd
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

    -- restore the old state
    let initEditor = maybe (emptyEditor cfg) id st
    -- Setting up the 1st window is a bit tricky because most functions assume there exists a "current window"
    newSt <- newMVar $ YiVar initEditor [] 1 M.empty
    (ui, runYi) <- mdo let handler exception = runYi $ (errorEditor (show exception) >> refreshEditor)
                           inF  ev  = handle handler (runYi (dispatch ev))
                           outF acts = handle handler (runYi (interactive acts))
                       ui <- uiStart cfg inF outF initEditor
                       let runYi f = runReaderT (runYiM f) yi
                           yi = Yi ui inF outF cfg newSt 
                       return (ui, runYi)
  
    runYi $ do

      withEditor $ newBufferE "*messages*" (fromString "") >> return ()

      when (isNothing st) $ do -- process options if booting for the first time
        postActions $ startActions cfg

    runYi refreshEditor

    UI.main ui -- transfer control to UI

postActions :: [Action] -> YiM ()
postActions actions = do yi <- ask; liftIO $ output yi actions

-- | Process an event by advancing the current keymap automaton an
-- execing the generated actions
dispatch :: Event -> YiM ()
dispatch ev =
    do yi <- ask
       entryEvs <- withEditor $ getA pendingEventsA
       logPutStrLn $ "pending events: " ++ showEvs entryEvs
       (userActions,_p') <- withBuffer $ do
         keymap <- withModeB modeKeymap
         p0 <- getA keymapProcessA
         let defKm = defaultKm $ yiConfig $ yi
         let freshP = I.mkAutomaton $ forever $ keymap $ defKm
             -- Note the use of "forever": this has quite subtle implications, as it means that
             -- failures in one iteration can yield to jump to the next iteration seamlessly.
             -- eg. in emacs keybinding, failures in incremental search, like <left>, will "exit"
             -- incremental search and immediately move to the left.
             p = case p0 of
                   I.End  -> freshP

                   I.Fail -> freshP
                   _      -> p0
             (actions, p') = I.processOneEvent p ev
             possibilities = I.possibleActions p'
             ambiguous = not (null possibilities) && all isJust possibilities
         setA keymapProcessA (if ambiguous then freshP else p')
         let actions0 = case p' of 
                          I.Fail -> [makeAction $ do
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
           decay = modifyA statusLinesA (DelayList.decrease 1)
           pendingFeedback = do modifyA pendingEventsA (++ [ev])
                                if null userActions
                                    then printMsg . showEvs =<< getA pendingEventsA
                                    else setA pendingEventsA []
       postActions $ [makeAction decay] ++ userActions ++ [makeAction pendingFeedback]

showEvs = intercalate " " . fmap prettyEvent
showEvs :: [Event] -> String

-- ---------------------------------------------------------------------
-- Meta operations

-- | Quit.
quitEditor :: YiM ()
quitEditor = withUI UI.end

-- | Redraw
refreshEditor :: YiM ()
refreshEditor = do 
    yi <- ask
    io $ modifyMVar_ (yiVar yi) $ \var -> do
        let e0 = yiEditor var 
            e1 = modifier buffersA (fmap (clearSyntax . clearHighlight)) e0
            e2 = modifier buffersA (fmap clearUpdates)  e1
        UI.refresh (yiUi yi) e2
        return var {yiEditor = e2}
    where clearHighlight fb@FBuffer {pendingUpdates = us, highlightSelection = h} 
              = modifier highlightSelectionA (const (h && null us)) fb
          -- if there were updates, then hide the selection.
          clearUpdates fb = modifier pendingUpdatesA (const []) fb
          

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
    let (f:args) = split " " cmd
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

msgEditor :: String -> YiM ()
msgEditor = withEditor . printMsg

-- | Show an error on the status line and log it.
errorEditor :: String -> YiM ()
errorEditor s = do msgEditor ("error: " ++ s)
                   logPutStrLn $ "errorEditor: " ++ s

-- | Close the current window.
-- If this is the last window open, quit the program.
-- FIXME: call quitEditor when there are no other window in the interactive command.
closeWindow :: YiM ()
closeWindow = do
    n <- withEditor $ withWindows WS.size
    when (n == 1) quitEditor
    withEditor $ tryCloseE

reloadEditor :: YiM ()
reloadEditor = msgEditor "reloadEditor: Not supported"

  
getAllNamesInScope :: YiM [String]
getAllNamesInScope = do 
  acts <- asks (publishedActions . yiConfig)
  return (M.keys acts)

-- | Start a subprocess with the given command and arguments.
startSubprocess :: FilePath -> [String] -> YiM BufferRef
startSubprocess cmd args = do
    yi <- ask
    io $ modifyMVar (yiVar yi) $ \var -> do        
        let (e', bufref) = runEditor 
                              (yiConfig yi) 
                              (printMsg ("Launched process: " ++ cmd) >> newBufferE bufferName (fromString ""))
                              (yiEditor var)
            procid = yiSubprocessIdSupply var + 1
        procinfo <- createSubprocess cmd args bufref
        startSubprocessWatchers procid procinfo yi
        return (var {yiEditor = e', 
                     yiSubprocessIdSupply = procid,
                     yiSubprocesses = M.insert procid procinfo (yiSubprocesses var)
                    }, bufref)
  where bufferName = "output from " ++ cmd ++ " " ++ show args

startSubprocessWatchers :: SubprocessId -> SubprocessInfo -> Yi -> IO ()
startSubprocessWatchers procid procinfo yi = do
  let send a = output yi [makeAction a]
      append s = send $ appendToBuffer (bufRef procinfo) s
      reportExit s = append s >> (send $ removeSubprocess procid)
  mapM_ forkOS [pipeToBuffer (hOut procinfo) append,
                pipeToBuffer (hErr procinfo) append,
                waitForExit (procHandle procinfo) >>= reportExit]

removeSubprocess :: SubprocessId -> YiM ()
removeSubprocess procid = modifiesRef yiVar (\v -> v {yiSubprocesses = M.delete procid $ yiSubprocesses v})

appendToBuffer :: BufferRef -> String -> EditorM ()
appendToBuffer bufref s = withGivenBuffer0 bufref $ do
    m <- getMarkB (Just "Prompt")
    modifyMarkB m (\v -> v {markGravity = Forward})
    insertNAt s =<< getMarkPointB m 
    modifyMarkB m (\v -> v {markGravity = Backward})

sendToProcess :: BufferRef -> String -> YiM ()
sendToProcess bufref s = do
    yi <- ask
    Just subProcessInfo <- find ((== bufref) . bufRef) . yiSubprocesses <$> readRef (yiVar yi)
    io $ hPutStr (hIn subProcessInfo) s

pipeToBuffer :: Handle -> (String -> IO ()) -> IO ()
pipeToBuffer h append = 
  handle (\_ -> return ()) $ forever $ (hWaitForInput h (-1) >> readAvailable h >>= append)


waitForExit :: ProcessHandle -> IO String
waitForExit ph = 
    handle (\_ -> return "Process killed") $ do 
      ec <- getProcessExitCode ph
      if (isJust ec) then return "Process exited"
                     else threadDelay (500*1000) >> waitForExit ph


withMode :: (Show x, YiAction a x) => (forall syntax. Mode syntax -> a) -> YiM ()
withMode f = do
            b <- withEditor Editor.getBuffer
            act <- withBufferMode b f
            runAction $ makeAction $ act

withSyntax :: (Show x, YiAction a x) => (forall syntax. Mode syntax -> syntax -> a) -> YiM ()
withSyntax f = do
            b <- withEditor Editor.getBuffer
            act <- withGivenBuffer b $ gets (withSyntax0 f)
            runAction $ makeAction $ act
