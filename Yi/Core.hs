-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) Don Stewart 2004-5. http://www.cse.unsw.edu.au/~dons
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--
-- Derived from: riot/UI.hs
--

--
-- | The core actions of yi. This module is the link between the editor
-- and the UI. Key bindings, and libraries should manipulate Yi through
-- the interface defined here.
--

module Yi.Core (
                module Yi.Dynamic,
        -- * Keymap
        module Yi.Keymap,
        
        -- * Construction and destruction
        startE,         -- :: Kernel -> Maybe Editor -> [Action] -> IO ()
        quitE,          -- :: Action
        reloadE,        -- :: Action
        reconfigE,
        loadE,
        unloadE,
        refreshE,       -- :: Action
        suspendE,       -- :: Action

        -- * Global editor actions
        msgE,           -- :: String -> Action
        errorE,         -- :: String -> Action
        msgClrE,        -- :: Action
        setWindowFillE, -- :: Char -> Action
        setWindowStyleE,-- :: UIStyle -> Action

        -- * Window manipulation
        closeE,         -- :: Action

        -- * File-based actions
        fnewE,          -- :: FilePath -> Action
        fwriteE,        -- :: Action
        fwriteAllE,     -- :: Action
        fwriteToE,      -- :: String -> Action
        backupE,        -- :: FilePath -> Action

        -- * Buffer only stuff
        newBufferE,     -- :: String -> String -> Action
        listBuffersE,   -- :: Action
        closeBufferE,   -- :: String -> Action
        getBufferWithName,

        -- * Buffer/Window
        switchToBufferE,
        switchToBufferOtherWindowE,
        switchToBufferWithNameE,
        nextBufW,       -- :: Action
        prevBufW,       -- :: Action

        -- * Window-based movement
        upScreenE,      -- :: Action
        upScreensE,     -- :: Int -> Action
        downScreenE,    -- :: Action
        downScreensE,   -- :: Int -> Action
        downFromTosE,   -- :: Int -> Action
        upFromBosE,     -- :: Int -> Action
        middleE,        -- :: Action

        -- * Buffer editing
        revertE,        -- :: Action

        -- * Basic registers
        setRegE,        -- :: String -> Action
        getRegE,        -- :: EditorM String

        -- * Dynamically extensible state
        getDynamic,
        setDynamic,

        -- * Interacting with external commands
        pipeE,                   -- :: String -> String -> EditorM String
 
        -- * Minibuffer
        spawnMinibufferE,

        -- * Misc
        changeKeymapE,
        getNamesInScopeE,
        execE
   ) where

import Prelude hiding (error, sequence_, mapM_)

import Yi.Debug
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Dynamic
import Yi.String
import Yi.Process           ( popen )
import Yi.Editor
import Yi.CoreUI
import Yi.Kernel
import Yi.Event (eventToChar, Event)
import Yi.Keymap
import Yi.Interact (anyEvent)
import Yi.Monad
import qualified Yi.WindowSet as WS
import qualified Yi.Editor as Editor
import qualified Yi.Style as Style
import qualified Yi.CommonUI as UI
import Yi.CommonUI as UI (Window (..), UI)

import Data.Maybe
import qualified Data.Map as M
import Data.List
import Data.IORef
import Data.Foldable

import System.Directory     ( doesFileExist, doesDirectoryExist )
import System.FilePath      

import Control.Monad (when, replicateM_)
import Control.Monad.Reader (runReaderT, asks)
import Control.Monad.Trans
import Control.Monad.State (gets, modify)
import Control.Exception
import Control.Concurrent 
import Control.Concurrent.Chan

import qualified GHC
import qualified DynFlags
import qualified SrcLoc
import qualified ErrUtils
import Outputable

import GHC.Exts ( unsafeCoerce# )

-- | Make an action suitable for an interactive run.
-- UI will be refreshed.
interactive :: YiM a -> YiM a
interactive action = do 
  logPutStrLn ">>>>>>> interactively"
  prepAction <- withUI UI.prepareAction
  withEditor prepAction
  x <- action
  refreshE 
  logPutStrLn "<<<<<<<"
  return x

nilKeymap :: Keymap
nilKeymap = do c <- anyEvent
               write $ case eventToChar c of
                         'q' -> quitE 
                         'r' -> reconfigE
                         'h' -> (configHelp >> return ())
                         _ -> errorE $ "Keymap not defined, type 'r' to reload config, 'q' to quit, 'h' for help."
    where configHelp = newBufferE "*configuration help*" $ unlines $
                         ["To get a standard reasonable keymap, you can run yi with either --as=vim or --as=emacs.",
                          "You can also create your own ~/.yi/YiConfig.hs file,",
                          "see http://haskell.org/haskellwiki/Yi#How_to_Configure_Yi for help on how to do that."]

-- ---------------------------------------------------------------------
-- | Start up the editor, setting any state with the user preferences
-- and file names passed in, and turning on the UI
--
startE :: String -> Kernel -> Maybe Editor -> [Action] -> IO ()
startE frontend kernel st commandLineActions = do
    let frontendModule = "Yi." ++ capitalize frontend ++ ".UI"
    logPutStrLn $ "Starting frontend: " ++ frontend

    targets <- mapM (\m -> guessTarget kernel m Nothing) [frontendModule]
    setTargets kernel targets
    result <- loadAllTargets kernel
    case result of
      GHC.Failed -> error $ "Panic: could not load " ++ frontend ++ " frontend. Use -fgtk or -fvty, or install the needed packages."
      _ -> return ()
    uiStartM <- compileExpr kernel (frontendModule ++ ".start") 
    let uiStart :: (forall action. 
                    Chan Yi.Event.Event -> 
                    Chan action ->
                    Editor -> (EditorM () -> action) -> 
                    MVar (WS.WindowSet Window) -> 
                    IO UI) = case uiStartM of
             Nothing -> do error "Could not compile frontend!"
             Just x -> unsafeCoerce# x

    logPutStrLn "Starting Core"


    -- restore the old state
    let initEditor = maybe emptyEditor id st
    let [consoleB] = flip findBufferWithName "*console*" initEditor
    newSt <- newIORef initEditor
    -- Setting up the 1st window is a bit tricky because most functions assume there exists a "current window"
    wins <- newMVar (WS.new $ Window False (consoleB) 0 0 0)
    inCh <- newChan
    outCh :: Chan Action <- newChan
    ui <- uiStart inCh outCh initEditor withEditor wins
    startKm <- newIORef nilKeymap
    startModules <- newIORef ["Yi.Yi"] -- this module re-exports all useful stuff, so we want it loaded at all times.
    startThreads <- newIORef []
    keymaps <- newIORef M.empty
    let yi = Yi newSt wins ui startThreads inCh outCh startKm keymaps kernel startModules
        runYi f = runReaderT f yi

    runYi $ do 

      newBufferE "*messages*" "" >> return ()

      withKernel $ \k -> do
        dflags <- getSessionDynFlags k
        setSessionDynFlags k dflags { GHC.log_action = ghcErrorReporter yi }

      -- run user configuration
      loadE "YiConfig"
      runConfig

      when (isNothing st) $ do -- process options if booting for the first time
        sequence_ commandLineActions

    logPutStrLn "Starting event handler"
    let
        handler e = runYi $ errorE (show e)
        -- | The editor's input main loop. 
        -- Read key strokes from the ui and dispatches them to the buffer with focus.
        eventLoop :: IO ()
        eventLoop = do
            let run = mapM_ (\ev -> runYi (dispatch ev)) =<< getChanContents inCh
            repeatM_ $ (handle handler run >> logPutStrLn "Dispatching loop ended")
                     

        -- | The editor's output main loop. 
        execLoop :: IO ()
        execLoop = do
            runYi refreshE
            let loop = sequence_ . map runYi . map interactive =<< getChanContents outCh
            repeatM_ $ (handle handler loop >> logPutStrLn "Execing loop ended")
      
    t1 <- forkIO eventLoop 
    t2 <- forkIO execLoop
    runYi $ modifiesRef threads (\ts -> t1 : t2 : ts)

    UI.main ui -- transfer control to UI: GTK must run in the main thread, or else it's not happy.
                
changeKeymapE :: Keymap -> Action
changeKeymapE km = do
  modifiesRef defaultKeymap (const km)
  bs <- withEditor getBuffers
  mapM_ (restartBufferThread . bkey) bs
  return ()

-- ---------------------------------------------------------------------
-- Meta operations

-- | Quit.
quitE :: Action
quitE = withUI UI.end


-- | (Re)compile and reload the user's config files
reloadE :: YiM [String]
reloadE = do
  modules <- readsRef editorModules
  withKernel $ \kernel -> do
    targets <- mapM (\m -> guessTarget kernel m Nothing) modules
    setTargets kernel targets
  -- lift $ rts_revertCAFs -- FIXME: GHCi does this; It currently has undesired effects on logging; investigate.
  result <- withKernel loadAllTargets
  loaded <- withKernel setContextAfterLoad
  case result of
    GHC.Failed -> withOtherWindow (switchToBufferE =<< getBufferWithName "*console*")
    _ -> return ()
  return $ map (moduleNameString . moduleName) loaded
  -- lift $ rts_revertCAFs

--foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()
	-- Make it "safe", just in case


-- | Force a complete redraw
refreshE :: Action
refreshE = do editor <- with yiEditor readRef
              withUI $ flip UI.scheduleRefresh editor
              withEditor $ modify $ \e -> e {editorUpdates = []}


-- | Suspend the program
suspendE :: Action
suspendE = withUI UI.suspend

------------------------------------------------------------------------

-- | Scroll up 1 screen
upScreenE :: Action
upScreenE = upScreensE 1

-- | Scroll up n screens
upScreensE :: Int -> Action
upScreensE n = withWindowAndBuffer $ \w -> do
                 gotoLnFrom (- (n * (height w - 1)))
                 moveToSol

-- | Scroll down 1 screen
downScreenE :: Action
downScreenE = downScreensE 1

-- | Scroll down n screens
downScreensE :: Int -> Action
downScreensE n = withWindowAndBuffer $ \w -> do
                   gotoLnFrom (n * (height w - 1))
                   return ()

-- | Move to @n@ lines down from top of screen
downFromTosE :: Int -> Action
downFromTosE n = withWindowAndBuffer $ \w -> do
                   moveTo (tospnt w)
                   replicateM_ n lineDown

-- | Move to @n@ lines up from the bottom of the screen
upFromBosE :: Int -> Action
upFromBosE n = withWindowAndBuffer $ \w -> do
                   moveTo (bospnt w)
                   moveToSol
                   replicateM_ n lineUp

-- | Move to middle line in screen
middleE :: Action
middleE = withWindowAndBuffer $ \w -> do
                   moveTo (tospnt w)
                   replicateM_ (height w `div` 2) lineDown


-- ---------------------------------------------------------------------
-- Window based operations
--

{-
-- | scroll window up
scrollUpE :: Action
scrollUpE = withWindow_ scrollUpW

-- | scroll window down
scrollDownE :: Action
scrollDownE = withWindow_ scrollDownW
-}

-- ---------------------------------------------------------------------
-- registers (TODO these may be redundant now that it is easy to thread
-- state in key bindings, or maybe not.
--

-- | Put string into yank register
setRegE :: String -> Action
setRegE s = withEditor $ modify $ \e -> e { yreg = s }

-- | Return the contents of the yank register
getRegE :: YiM String
getRegE = withEditor $ gets $ yreg

-- ---------------------------------------------------------------------
-- | Dynamically-extensible state components.
--
-- These hooks are used by keymaps to store values that result from
-- Actions (i.e. that restult from IO), as opposed to the pure values
-- they generate themselves, and can be stored internally.
--
-- The `dynamic' field is a type-indexed map.
--

-- | Retrieve a value from the extensible state
getDynamic :: Initializable a => YiM a
getDynamic = do 
  ps <- readEditor dynamic
  return $ getDynamicValue ps

-- | Insert a value into the extensible state, keyed by its type
setDynamic :: Initializable a => a -> Action
setDynamic x = withEditor $ modify $ \e -> e { dynamic = setDynamicValue x (dynamic e) }

------------------------------------------------------------------------
-- | Pipe a string through an external command, returning the stdout
-- chomp any trailing newline (is this desirable?)
--
-- Todo: varients with marks?
--
pipeE :: String -> String -> YiM String
pipeE cmd inp = do
    let (f:args) = split " " cmd
    (out,_err,_) <- lift $ popen f args (Just inp)
    return (chomp "\n" out)

------------------------------------------------------------------------

-- | Set the cmd buffer, and draw message at bottom of screen
msgE :: String -> Action
msgE s = do 
  ui <- asks yiUi
  lift $ UI.setCmdLine ui s
  -- also show in the messages buffer, so we don't loose any message
  b <- getBufferWithName "*messages*"
  withGivenBuffer b $ do botB; insertN (s ++ "\n")

-- | Set the cmd buffer, and draw a pretty error message
errorE :: String -> Action
errorE s = do msgE ("error: " ++ s)
              logPutStrLn $ "errorE: " ++ s

-- | Clear the message line at bottom of screen
msgClrE :: Action
msgClrE = msgE ""


-- | A character to fill blank lines in windows with. Usually '~' for
-- vi-like editors, ' ' for everything else
setWindowFillE :: Char -> Action
setWindowFillE c = withEditor $ modify $ \e -> e { windowfill = c }

-- | Sets the window style.
setWindowStyleE :: Style.UIStyle -> Action
setWindowStyleE sty = withEditor $ modify $ \e -> e { uistyle = sty }


-- | Attach the next buffer in the buffer list
-- to the current window.
nextBufW :: Action
nextBufW = withEditor Editor.nextBuffer >>= switchToBufferE

-- | edit the previous buffer in the buffer list
prevBufW :: Action
prevBufW = withEditor Editor.prevBuffer >>= switchToBufferE

-- | If file exists, read contents of file into a new buffer, otherwise
-- creating a new empty buffer. Replace the current window with a new
-- window onto the new buffer.
--
-- Need to clean up semantics for when buffers exist, and how to attach
-- windows to buffers.
--
fnewE  :: FilePath -> Action
fnewE f = do
    bufs <- withEditor getBuffers
    let bufsWithThisFilename = filter ((== Just f) . file) bufs
    b <- case bufsWithThisFilename of
             [] -> do
                   fe  <- lift $ doesFileExist f
                   de  <- lift $ doesDirectoryExist f
                   newBufferForPath fe de
             _  -> return (bkey $ head bufsWithThisFilename)
    withGivenBuffer b $ setfileB f        -- associate buffer with file
    switchToBufferE b
    where
    newBufferForPath :: Bool -> Bool -> YiM BufferRef
    newBufferForPath True _      = fileToNewBuffer f                 -- Load the file into a new buffer
    newBufferForPath False True  = do           -- Open the dir in Dired
            loadE "Yi.Dired"
            execE $ "Yi.Dired.diredDirBufferE " ++ show f
            withEditor getBuffer
    newBufferForPath False False = withEditor $ stringToNewBuffer f []                       -- Create new empty buffer


fileToNewBuffer :: FilePath -> YiM BufferRef
fileToNewBuffer path = do
  contents <- liftIO $ readFile path
  withEditor $ stringToNewBuffer (takeFileName path) contents
  -- FIXME: we should not create 2 buffers with the same name.

-- | Revert to the contents of the file on disk
revertE :: Action
revertE = do
            mfp <- withBuffer getfileB
            case mfp of
                     Just fp -> do
                             s <- liftIO $ readFile fp
                             withBuffer $ do
                                  end <- sizeB
                                  p <- pointB
                                  moveTo 0
                                  deleteN end
                                  insertN s
                                  moveTo p
                                  clearUndosB
                             msgE ("Reverted from " ++ show fp)
                     Nothing -> do
                                msgE "Can't revert, no file associated with buffer."
                                return ()

-- | Like fnewE, create a new buffer filled with the String @s@,
-- Open up a new window onto this buffer. Doesn't associate any file
-- with the buffer (unlike fnewE) and so is good for popup internal
-- buffers (like scratch)
newBufferE :: String -> String -> YiM BufferRef
newBufferE f s = do
    b <- withEditor $ stringToNewBuffer f s
    switchToBufferE b
    logPutStrLn "newBufferE ended"
    return b

-- | Attach the specified buffer to the current window
switchToBufferE :: BufferRef -> Action
switchToBufferE b = modifyWindows (WS.modifyCurrent (\w -> w {bufkey = b}))

-- | Attach the specified buffer to some other window than the current one
switchToBufferOtherWindowE :: BufferRef -> Action
switchToBufferOtherWindowE b = shiftOtherWindow >> switchToBufferE b

-- | Find buffer with given name. Raise exception if not found.
getBufferWithName :: String -> YiM BufferRef
getBufferWithName bufName = do
  bs <- readEditor $ \e -> findBufferWithName e bufName
  case bs of
    [] -> fail ("Buffer not found: " ++ bufName)
    (b:_) -> return b

-- | Switch to the buffer specified as parameter. If the buffer name is empty, switch to the next buffer.
switchToBufferWithNameE :: String -> Action
switchToBufferWithNameE "" = nextBufW
switchToBufferWithNameE bufName = switchToBufferE =<< getBufferWithName bufName

-- | Open a minibuffer window with the given prompt and keymap
spawnMinibufferE :: String -> KeymapMod -> Action -> Action
spawnMinibufferE prompt kmMod initialAction =
    do b <- withEditor $ stringToNewBuffer prompt []
       setBufferKeymap b kmMod
       modifyWindows (WS.add $ Window True b 0 0 0)
       initialAction

-- | Write current buffer to disk, if this buffer is associated with a file
fwriteE :: Action
fwriteE = do contents <- withBuffer elemsB
             fname <- withBuffer (gets file)
             case fname of
               Just n -> liftIO $ writeFile n contents
               Nothing -> msgE "Buffer not associated with a file."

-- | Write current buffer to disk as @f@. If this buffer doesn't
-- currently have a file associated with it, the file is set to @f@
fwriteToE :: String -> Action
fwriteToE f = do withBuffer $ setfileB f
                 fwriteE

-- | Write all open buffers
fwriteAllE :: Action
fwriteAllE = error "fwriteAllE not implemented"

-- | Make a backup copy of file
backupE :: FilePath -> Action
backupE = error "backupE not implemented"

-- | Return a list of all buffers, and their indicies
listBuffersE :: YiM [(String,Int)]
listBuffersE = do
        bs  <- withEditor getBuffers
        return $ zip (map name bs) [0..]

-- | Release resources associated with buffer

closeBufferE :: String -> Action
closeBufferE bufName = do
  nextB <- withEditor nextBuffer
  b <- withEditor getBuffer
  b' <- if null bufName then return b else getBufferWithName bufName
  switchToBufferE nextB
  withEditor $ deleteBuffer b'

------------------------------------------------------------------------

-- | Close the current window.
-- If this is the last window open, quit the program.
closeE :: Action
closeE = do
    n <- withWindows (length . toList)
    when (n == 1) quitE
    tryCloseE

reconfigE :: Action
reconfigE = reloadE >> runConfig

runConfig :: Action
runConfig = do
  loaded <- withKernel $ \kernel -> do
              let cfgMod = mkModuleName kernel "YiConfig"
              isLoaded kernel cfgMod
  if loaded 
   then do result <- withKernel $ \kernel -> compileExpr kernel "YiConfig.yiMain :: Yi.Yi.YiM ()"
           case result of
             Nothing -> errorE "Could not run YiConfig.yiMain :: Yi.Yi.YiM ()"
             Just x -> (unsafeCoerce# x)
   else errorE "YiConfig not loaded"

loadE :: String -> YiM [String]
loadE modul = do
  modifiesRef editorModules (\ms -> if Data.List.notElem modul ms then ms++[modul] else ms)
  reloadE

unloadE :: String -> YiM [String]
unloadE modul = do
  modifiesRef editorModules $ delete modul
  reloadE

getNamesInScopeE :: YiM [String]
getNamesInScopeE = do
  withKernel $ \k -> do
      rdrNames <- getRdrNamesInScope k
      names <- getNamesInScope k
      return $ map (nameToString k) rdrNames ++ map (nameToString k) names

ghcErrorReporter :: Yi -> GHC.Severity -> SrcLoc.SrcSpan -> Outputable.PprStyle -> ErrUtils.Message -> IO () 
ghcErrorReporter yi severity srcSpan pprStyle message = 
    -- the following is written in very bad style.
    flip runReaderT yi $ do
      e <- readEditor id
      let [b] = findBufferWithName e "*console*"
      withGivenBuffer b $ savingExcursionB $ do 
        moveTo =<< getMarkPointB =<< getMarkB (Just "errorInsert")
        insertN msg
        insertN "\n"
    where msg = case severity of
                  GHC.SevInfo -> show (message pprStyle)
                  GHC.SevFatal -> show (message pprStyle)
                  _ -> show ((ErrUtils.mkLocMessage srcSpan message) pprStyle)


-- | Run a (dynamically specified) editor command.
execE :: String -> YiM ()
execE s = do
  ghcErrorHandlerE $ do
            result <- withKernel $ \kernel -> do
                               logPutStrLn $ "execing " ++ s
                               compileExpr kernel ("makeAction (" ++ s ++ ") >>= msgE' . show :: YiM ()")
            case result of
              Nothing -> errorE ("Could not compile: " ++ s)
              Just x -> do let (x' :: YiM ()) = unsafeCoerce# x
                           x'
                           return ()

-- | Install some default exception handlers and run the inner computation.
ghcErrorHandlerE :: YiM () -> YiM ()
ghcErrorHandlerE inner = do
  flip catchDynE (\dyn -> do
  		    case dyn of
		     GHC.PhaseFailed _ code -> errorE $ "Exitted with " ++ show code
		     GHC.Interrupted -> errorE $ "Interrupted!"
		     _ -> do errorE $ "GHC exeption: " ++ (show (dyn :: GHC.GhcException))
			     
	    ) $
            inner
