{-# LANGUAGE PatternSignatures, RecursiveDo, Rank2Types #-}

-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) Don Stewart 2004-5. http://www.cse.unsw.edu.au/~dons

--
-- | The core actions of yi. This module is the link between the editor
-- and the UI. Key bindings, and libraries should manipulate Yi through
-- the interface defined here.

module Yi.Core 
  ( module Yi.Dynamic
    -- * Keymap
  , module Yi.Keymap

  -- * Construction and destruction
  , startEditor         -- :: StartConfig -> Kernel -> Maybe Editor -> [YiM ()] -> IO ()
  , quitEditor          -- :: YiM ()

  , reloadEditor        -- :: YiM ()
  , getAllNamesInScope
  , execEditorAction

  , refreshEditor       -- :: YiM ()
  , suspendEditor       -- :: YiM ()

  -- * Global editor actions
  , msgEditor           -- :: String -> YiM ()
  , errorEditor         -- :: String -> YiM ()
  , msgClr        -- :: YiM ()

  -- * Window manipulation
  , closeWindow         -- :: YiM ()
  , withOtherWindow

  -- * Interacting with external commands
  , runProcessWithInput          -- :: String -> String -> YiM String
  , startSubprocess                 -- :: FilePath -> [String] -> YiM ()

  -- * Misc
  , runAction
  , withMode
  , withSyntax
  ) 
where

import Prelude hiding (error, sequence_, mapM_, elem, concat, all)

import Yi.Debug
import Yi.Undo
import Yi.Buffer
import Yi.Dynamic
import Yi.String
import Yi.Process ( popen, createSubprocess, readAvailable, SubprocessId, SubprocessInfo(..) )
import Yi.Editor
import Yi.Event (Event)
import Yi.Keymap
import Yi.KillRing (krEndCmd)
import qualified Yi.Interact as I
import Yi.Monad
import Yi.Accessor
import qualified Yi.WindowSet as WS
import qualified Yi.Editor as Editor
import qualified Yi.UI.Common as UI
import Yi.UI.Common as UI (UI)
import Yi.Interpreter

import Data.Maybe
import qualified Data.Map as M
import Data.Dynamic
import Data.IORef
import Data.Foldable (mapM_, all)

import System.IO ( Handle, hWaitForInput )
import System.FilePath
import System.Process ( getProcessExitCode, ProcessHandle )

import Control.Monad (when,forever)
import Control.Monad.Reader (runReaderT, ask, asks)
import Control.Monad.Trans
import Control.Monad.Error ()
import Control.Monad.State (gets)
import Control.Exception
import Control.Concurrent
#ifdef SHIM
import System.Directory ( findExecutable )
import Shim.Hsinfo
import Shim.SHM
#endif

-- | Make an action suitable for an interactive run.
-- UI will be refreshed.
interactive :: Action -> YiM ()
interactive action = do
  logPutStrLn ">>> interactively"
  prepAction <- withUI UI.prepareAction
  withEditor $ do prepAction
                  modifyAllA buffersA undosA (addChangeU InteractivePoint)
  runAction action
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
    let initEditor = maybe emptyEditor id st
    newSt <- newIORef initEditor
    -- Setting up the 1st window is a bit tricky because most functions assume there exists a "current window"
    startKm <- newIORef (defaultKm cfg)
    startThreads <- newIORef []
    startSubprocessId <- newIORef 1
    startSubprocesses <- newIORef M.empty
    keymaps <- newIORef M.empty
#ifdef SHIM
    ghc <- findExecutable "ghc" -- FIXME: Add version constraint
           >>= maybe (error "Could not find ghc executable in path.") return
    session <- ghcInit ghc
    shim <- newIORef ShimState
               { ghcProgram = ghc,
                 tempSession = session,
                 sessionMap = M.empty,
                 compBuffer = M.empty }
#endif
    (ui, runYi) <- mdo let handler e = runYi $ errorEditor (show e)
                           inF  ev  = handle handler (runYi (dispatch ev))
                           outF act = handle handler (runYi (interactive act))
                       ui <- uiStart (configUI cfg) inF outF initEditor
                       let yi = Yi newSt ui startThreads inF outF keymaps startSubprocessId startSubprocesses cfg 
#ifdef SHIM 
                                   shim
#endif
                           runYi f = runReaderT (runYiM f) yi
                       return (ui, runYi)
  
    runYi $ do

      withEditor $ newBufferE "*messages*" "" >> return ()

      when (isNothing st) $ do -- process options if booting for the first time
        startAction cfg
        postActions $ startQueuedActions cfg

    runYi refreshEditor

    UI.main ui -- transfer control to UI: GTK must run in the main thread, or else it's not happy.

postActions :: [Action] -> YiM ()
postActions actions = do yi <- ask; liftIO $ mapM_ (output yi) actions

-- | Process an event by advancing the current keymap automaton an
-- execing the generated actions
dispatch :: Event -> YiM ()
dispatch ev =
    do yi <- ask
       b <- withEditor getBuffer
       keymap <- withBufferMode b modeKeymap
       p0 <- getBufferProcess b
       let defKm = defaultKm $ yiConfig $ yi
       let freshP = I.mkAutomaton $ keymap $ defKm
           p = case p0 of
                 I.End  -> freshP
                 I.Fail -> freshP
                 _      -> p0
           (actions, p') = I.processOneEvent p ev
           possibilities = I.possibleActions p'
           ambiguous = not (null possibilities) && all isJust possibilities
       logPutStrLn $ "Processing: " ++ show ev
       logPutStrLn $ "Actions posted:" ++ show actions
       logPutStrLn $ "New automation: " ++ show p'
       -- TODO: if no action is posted, accumulate the input and give feedback to the user.

       postActions $ case p' of 
                       I.Fail -> [makeAction $ msgEditor "Unrecognized input"]
                       _ -> actions
       when ambiguous $
            postActions [makeAction $ msgEditor "Keymap was in an ambiguous state! Resetting it."]
       modifiesRef bufferProcesses (M.insert b (if ambiguous then freshP else p'))

-- ---------------------------------------------------------------------
-- Meta operations

-- | Quit.
quitEditor :: YiM ()
quitEditor = withUI UI.end

-- | Redraw
refreshEditor :: YiM ()
refreshEditor = do e0 <- with yiEditor readRef
                   let e1 = modifier buffersA (fmap (clearSyntax . clearHighlight)) e0
                       e2 = modifier buffersA (fmap clearUpdates)   e1
                   withUI $ flip UI.refresh e1
                   with yiEditor (flip writeRef e2)
    where clearHighlight fb@FBuffer {pendingUpdates = us, highlightSelection = h} 
              = modifier highlightSelectionA (const (h && null us)) fb
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

-- | Clear the message line at bottom of screen
msgClr :: YiM ()
msgClr = msgEditor ""

-- | Close the current window.
-- If this is the last window open, quit the program.
closeWindow :: YiM ()
closeWindow = do
    n <- withEditor $ withWindows WS.size
    when (n == 1) quitEditor
    withEditor $ tryCloseE

-- | Execute the argument in the context of an other window. Create
-- one if necessary.
withOtherWindow :: EditorM () -> EditorM ()
withOtherWindow f = do
  shiftOtherWindow
  f
  prevWinE

reloadEditor :: YiM ()
reloadEditor = msgEditor "reloadEditor: Not supported"

execEditorAction :: String -> YiM ()
execEditorAction s = do 
  env <- asks (publishedActions . yiConfig)
  case toMono =<< interpret =<< addMakeAction =<< rename env =<< parse s of
    Left err -> errorEditor err
    Right a -> postActions [a]
  where addMakeAction expr = return $ UApp (UVal mkAct) expr
        mkAct = [
                 toDyn (makeAction :: BufferM () -> Action),
                 toDyn (makeAction :: BufferM Int -> Action),
                 toDyn (makeAction :: EditorM () -> Action),
                 toDyn (makeAction :: YiM () -> Action)
                ]
            
  
getAllNamesInScope :: YiM [String]
getAllNamesInScope = do 
  acts <- asks (publishedActions . yiConfig)
  return (M.keys acts)

-- | Start a subprocess with the given command and arguments.
startSubprocess :: FilePath -> [String] -> YiM ()
startSubprocess cmd args = do
  let buffer_name = "output from " ++ cmd
  bufref <- withEditor $ newBufferE buffer_name ""

  procid <- modifiesThenReadsRef yiSubprocessIdSupply (+1)
  procinfo <- liftIO $ createSubprocess cmd args bufref

  yi <- ask

  startSubprocessWatchers (output yi) procid procinfo

  modifiesRef yiSubprocesses $ M.insert procid procinfo
  msgEditor ("Launched process: " ++ cmd )

startSubprocessWatchers :: (Action -> IO ()) -> SubprocessId -> SubprocessInfo -> YiM ()
startSubprocessWatchers chan procid procinfo = do
  mapM_ (liftIO . forkIO) [ pipeToBuffer (hOut procinfo) append,
                          pipeToBuffer (hErr procinfo) append,
                          waitForExit (procHandle procinfo) >>= reportExit ]
  where append s = send $ appendToBuffer (bufRef procinfo) s
        reportExit s = append s >> (send $ removeSubprocess procid)
        send a = chan $ makeAction a

removeSubprocess :: SubprocessId -> YiM ()
removeSubprocess procid = modifiesRef yiSubprocesses $ M.delete procid

appendToBuffer :: BufferRef -> String -> YiM ()
appendToBuffer bufref s = withGivenBuffer bufref $ savingExcursionB $ (sizeB >>= insertNAt s)

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
