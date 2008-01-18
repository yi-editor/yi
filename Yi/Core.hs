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
-- | The core actions of yi. This module is the link between the editor
-- and the UI. Key bindings, and libraries should manipulate Yi through
-- the interface defined here.
--

module Yi.Core (
                module Yi.Dynamic,
        -- * Keymap
        module Yi.Keymap,
        
        -- * Construction and destruction
        StartConfig    ( .. ), -- Must be passed as the first argument to 'startE'
        startE,         -- :: StartConfig -> Kernel -> Maybe Editor -> [YiM ()] -> IO ()
        quitE,          -- :: YiM ()
        reloadE,        -- :: YiM ()
        reconfigE,
        loadE,
        unloadE,
        refreshE,       -- :: YiM ()
        suspendE,       -- :: YiM ()

        -- * Global editor actions
        msgE,           -- :: String -> YiM ()
        errorE,         -- :: String -> YiM ()
        msgClrE,        -- :: YiM ()
        setWindowFillE, -- :: Char -> YiM ()
        setWindowStyleE,-- :: UIStyle -> YiM ()

        -- * Window manipulation
        closeE,         -- :: YiM ()

        -- * File-based actions
        fnewE,          -- :: FilePath -> YiM ()
        fwriteE,        -- :: YiM ()
        fwriteAllE,     -- :: YiM ()
        fwriteToE,      -- :: String -> YiM ()
        backupE,        -- :: FilePath -> YiM ()

        -- * Buffer only stuff
        newBufferE,     -- :: String -> String -> YiM ()
        listBuffersE,   -- :: YiM ()
        closeBufferE,   -- :: String -> YiM ()
        getBufferWithName,

        -- * Buffer/Window
        closeBufferAndWindowE,
        switchToBufferE,
        switchToBufferOtherWindowE,
        switchToBufferWithNameE,
        nextBufW,       -- :: YiM ()
        prevBufW,       -- :: YiM ()

        -- * Window-based movement
        upScreenE,      -- :: YiM ()
        upScreensE,     -- :: Int -> YiM ()
        downScreenE,    -- :: YiM ()
        downScreensE,   -- :: Int -> YiM ()
        downFromTosE,   -- :: Int -> YiM ()
        upFromBosE,     -- :: Int -> YiM ()
        middleE,        -- :: YiM ()

        -- * Buffer editing
        revertE,        -- :: YiM ()

        -- * Basic registers
        setRegE,        -- :: String -> YiM ()
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
        execE,
        runAction
   ) where

import Prelude hiding (error, sequence_, mapM_, elem, concat)

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
import qualified Yi.Interact as I
import Yi.Monad
import Yi.Accessor
import qualified Yi.WindowSet as WS
import qualified Yi.Editor as Editor
import qualified Yi.Style as Style
import qualified Yi.UI.Common as UI
import Yi.UI.Common as UI (UI)

import Data.Maybe
import qualified Data.Map as M
import Data.List
  ( notElem
  , delete
  )
import Data.IORef
import Data.Foldable

import System.Directory     ( doesFileExist, doesDirectoryExist )
import System.FilePath      

import Control.Monad (when, forever, replicateM_)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.Trans
import Control.Monad.State (gets, modify)
import Control.Monad.Error ()
import Control.Exception
import Control.Concurrent 
import Control.Concurrent.Chan

import qualified GHC
import qualified DynFlags
import qualified SrcLoc
import qualified ErrUtils
import Outputable


-- | Make an action suitable for an interactive run.
-- UI will be refreshed.
interactive :: Action -> YiM ()
interactive action = do 
  logPutStrLn ">>>>>>> interactively"
  prepAction <- withUI UI.prepareAction
  withEditor prepAction
  runAction action
  refreshE 
  logPutStrLn "<<<<<<<"
  return ()

nilKeymap :: Keymap
nilKeymap = do c <- I.anyEvent
               write $ case eventToChar c of
                         'q' -> quitE 
                         'r' -> reconfigE
                         'h' -> (configHelp >> return ())
                         _ -> errorE $ "Keymap not defined, type 'r' to reload config, 'q' to quit, 'h' for help."
    where configHelp = newBufferE "*configuration help*" $ unlines $
                         ["To get a standard reasonable keymap, you can run yi with either --as=vim or --as=emacs.",
                          "You can also create your own ~/.yi/YiConfig.hs file,",
                          "see http://haskell.org/haskellwiki/Yi#How_to_Configure_Yi for help on how to do that."]


data StartConfig = StartConfig { startFrontEnd   :: UI.UIBoot
                               , startConfigFile :: FilePath
                               }

-- ---------------------------------------------------------------------
-- | Start up the editor, setting any state with the user preferences
-- and file names passed in, and turning on the UI
--
startE :: StartConfig -> Kernel -> Maybe Editor -> [YiM ()] -> IO ()
startE startConfig kernel st commandLineActions = do
    let yiConfigFile   = startConfigFile startConfig
        uiStart        = startFrontEnd startConfig

    logPutStrLn "Starting Core"

    -- restore the old state
    let initEditor = maybe emptyEditor id st
    newSt <- newIORef initEditor
    -- Setting up the 1st window is a bit tricky because most functions assume there exists a "current window"
    inCh <- newChan
    outCh :: Chan Action <- newChan
    ui <- uiStart inCh outCh initEditor makeAction
    startKm <- newIORef nilKeymap
    startModules <- newIORef ["Yi.Yi"] -- this module re-exports all useful stuff, so we want it loaded at all times.
    startThreads <- newIORef []
    keymaps <- newIORef M.empty
    let yi = Yi newSt ui startThreads inCh outCh startKm keymaps kernel startModules
        runYi f = runReaderT f yi

    runYi $ do 

      newBufferE "*messages*" "" >> return ()

      withKernel $ \k -> do
        dflags <- getSessionDynFlags k
        setSessionDynFlags k dflags { GHC.log_action = ghcErrorReporter yi }

      -- run user configuration
      loadE yiConfigFile -- "YiConfig"
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
            forever $ (handle handler run >> logPutStrLn "Dispatching loop ended")
                     

        -- | The editor's output main loop. 
        execLoop :: IO ()
        execLoop = do
            runYi refreshE
            let loop = sequence_ . map runYi . map interactive =<< getChanContents outCh
            forever $ (handle handler loop >> logPutStrLn "Execing loop ended")
      
    t1 <- forkIO eventLoop 
    t2 <- forkIO execLoop
    runYi $ modifiesRef threads (\ts -> t1 : t2 : ts)

    UI.main ui -- transfer control to UI: GTK must run in the main thread, or else it's not happy.

postActions :: [Action] -> YiM ()
postActions actions = do yi <- ask; lift $ writeList2Chan (output yi) actions

-- | Process an event by advancing the current keymap automaton an
-- execing the generated actions
dispatch :: Event -> YiM ()
dispatch ev =
    do yi <- ask
       b <- withEditor getBuffer
       bkm <- getBufferKeymap b
       defKm <- readRef (defaultKeymap yi)
       let p0 = bufferKeymapProcess bkm
           freshP = I.mkAutomaton $ bufferKeymap bkm $ defKm
           p = case p0 of
                 I.End -> freshP
                 I.Fail -> freshP -- TODO: output error message about unhandled input
                 _ -> p0
           (actions, p') = I.processOneEvent p ev
       logPutStrLn $ "Processing: " ++ show ev
       logPutStrLn $ "Actions posted:" ++ show actions
       logPutStrLn $ "New automation: " ++ show p'
       logPutStrLn $ "Ambact: " ++ show (I.ambiguousActions p')
       postActions actions
       when (not $ null $ I.ambiguousActions p') $ 
            postActions [makeAction $ msgE "Keymap is in an ambiguous state!"]
       modifiesRef bufferKeymaps (M.insert b bkm { bufferKeymapProcess = p' })


changeKeymapE :: Keymap -> YiM ()
changeKeymapE km = do
  modifiesRef defaultKeymap (const km)
  bs <- withEditor getBuffers
  mapM_ (restartBufferThread . bkey) bs
  return ()

-- ---------------------------------------------------------------------
-- Meta operations

-- | Quit.
quitE :: YiM ()
quitE = withUI UI.end

loadModulesE :: [String] -> YiM (Bool, [String])
loadModulesE modules = do
  withKernel $ \kernel -> do
    targets <- mapM (\m -> guessTarget kernel m Nothing) modules
    setTargets kernel targets
  -- lift $ rts_revertCAFs -- FIXME: GHCi does this; It currently has undesired effects on logging; investigate.
  logPutStrLn $ "Loading targets..."
  result <- withKernel loadAllTargets
  loaded <- withKernel setContextAfterLoad
  ok <- case result of
    GHC.Failed -> withOtherWindow (switchToBufferE =<< getBufferWithName "*console*") >> return False
    _ -> return True
  let newModules = map (moduleNameString . moduleName) loaded
  writesRef editorModules newModules
  logPutStrLn $ "loadModulesE: " ++ show modules ++ " -> " ++ show (ok, newModules)
  return (ok, newModules)

--foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()
	-- Make it "safe", just in case

tryLoadModulesE :: [String] -> YiM [String]
tryLoadModulesE [] = return []
tryLoadModulesE  modules = do
  (ok, newModules) <- loadModulesE modules
  if ok 
    then return newModules 
    else tryLoadModulesE (init modules) 
    -- when failed, try to drop the most recently loaded module.
    -- We do this because GHC stops trying to load modules upon the 1st failing modules.
    -- This allows to load more modules if we ever try loading a wrong module.

-- | (Re)compile 
reloadE :: YiM [String]
reloadE = tryLoadModulesE =<< readsRef editorModules

-- | Redraw
refreshE :: YiM ()
refreshE = do editor <- with yiEditor readRef
              withUI $ flip UI.refresh editor
              withEditor $ modify $ \e -> e {editorUpdates = []}

-- | Suspend the program
suspendE :: YiM ()
suspendE = withUI UI.suspend

------------------------------------------------------------------------

-- | Scroll up 1 screen
upScreenE :: YiM ()
upScreenE = upScreensE 1

-- | Scroll up n screens
upScreensE :: Int -> YiM ()
upScreensE n = withEditor $ withWindowAndBuffer $ \w -> do
                 gotoLnFrom (- (n * (height w - 1)))
                 moveToSol

-- | Scroll down 1 screen
downScreenE :: YiM ()
downScreenE = downScreensE 1

-- | Scroll down n screens
downScreensE :: Int -> YiM ()
downScreensE n = withEditor $ withWindowAndBuffer $ \w -> do
                   gotoLnFrom (n * (height w - 1))
                   return ()

-- | Move to @n@ lines down from top of screen
downFromTosE :: Int -> YiM ()
downFromTosE n = withEditor $ withWindowAndBuffer $ \w -> do
                   moveTo (tospnt w)
                   replicateM_ n lineDown

-- | Move to @n@ lines up from the bottom of the screen
upFromBosE :: Int -> YiM ()
upFromBosE n = withEditor $ withWindowAndBuffer $ \w -> do
                   moveTo (bospnt w)
                   moveToSol
                   replicateM_ n lineUp

-- | Move to middle line in screen
middleE :: YiM ()
middleE = withEditor $ withWindowAndBuffer $ \w -> do
                   moveTo (tospnt w)
                   replicateM_ (height w `div` 2) lineDown


-- ---------------------------------------------------------------------
-- Window based operations
--

{-
-- | scroll window up
scrollUpE :: YiM ()
scrollUpE = withWindow_ scrollUpW

-- | scroll window down
scrollDownE :: YiM ()
scrollDownE = withWindow_ scrollDownW
-}

-- ---------------------------------------------------------------------
-- registers (TODO these may be redundant now that it is easy to thread
-- state in key bindings, or maybe not.
--

-- | Put string into yank register
setRegE :: String -> YiM ()
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
getDynamic = withEditor $ getA (dynamicValueA .> dynamicA)

-- | Insert a value into the extensible state, keyed by its type
setDynamic :: Initializable a => a -> YiM ()
setDynamic x = withEditor $ setA (dynamicValueA .> dynamicA) x

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

-- | Same as msgE, but do nothing instead of printing @()@
msgE' :: String -> YiM ()
msgE' "()" = return ()
msgE' s = msgE s

runAction :: Action -> YiM ()
runAction (YiA act) = do 
  act >>= msgE' . show
  return ()
runAction (EditorA act) = do 
  withEditor act >>= msgE' . show
  return ()
runAction (BufferA act) = do 
  withBuffer act >>= msgE' . show
  return ()


-- | Set the cmd buffer, and draw message at bottom of screen
msgE :: String -> YiM ()
msgE s = do 
  withEditor $ modify $ \e -> e { statusLine = s}
  -- also show in the messages buffer, so we don't loose any message
  b <- getBufferWithName "*messages*"
  withGivenBuffer b $ do botB; insertN (s ++ "\n")

-- | Show an error on the status line and log it.
errorE :: String -> YiM ()
errorE s = do msgE ("error: " ++ s)
              logPutStrLn $ "errorE: " ++ s

-- | Clear the message line at bottom of screen
msgClrE :: YiM ()
msgClrE = msgE ""


-- | A character to fill blank lines in windows with. Usually '~' for
-- vi-like editors, ' ' for everything else
setWindowFillE :: Char -> YiM ()
setWindowFillE c = withEditor $ modify $ \e -> e { windowfill = c }

-- | Sets the window style.
setWindowStyleE :: Style.UIStyle -> YiM ()
setWindowStyleE sty = withEditor $ modify $ \e -> e { uistyle = sty }


-- | Attach the next buffer in the buffer list
-- to the current window.
nextBufW :: YiM ()
nextBufW = withEditor Editor.nextBuffer >>= switchToBufferE

-- | edit the previous buffer in the buffer list
prevBufW :: YiM ()
prevBufW = withEditor Editor.prevBuffer >>= switchToBufferE

-- | If file exists, read contents of file into a new buffer, otherwise
-- creating a new empty buffer. Replace the current window with a new
-- window onto the new buffer.
--
-- Need to clean up semantics for when buffers exist, and how to attach
-- windows to buffers.
--
fnewE  :: FilePath -> YiM ()
fnewE f = do
    bufs <- withEditor getBuffers
        -- The file names associated with the list of current buffers
    let bufsWithThisFilename = filter ((== Just f) . file) bufs
        -- The names of the existing buffers
        currentBufferNames   = map name bufs
        -- The new name for the buffer
        bufferName           = bestNewName currentBufferNames
    b <- case bufsWithThisFilename of
             [] -> do
                   fe  <- lift $ doesFileExist f
                   de  <- lift $ doesDirectoryExist f
                   newBufferForPath bufferName fe de
             _  -> return (bkey $ head bufsWithThisFilename)
    withGivenBuffer b $ setfileB f        -- associate buffer with file
    withGivenBuffer b $ setSyntaxB (syntaxFromExtension $ takeExtension f)
    switchToBufferE b
    where
    -- The first argument is the buffer name the second argument is
    -- whether or not the file currently exists and the third argument
    -- is whether or not the file is a directory that exists.
    newBufferForPath :: String -> Bool -> Bool -> YiM BufferRef
    newBufferForPath bufferName True _       = 
      fileToNewBuffer bufferName f -- Load the file into a new buffer
    newBufferForPath _bufferName False True  =
      do -- Open the dir in Dired
         loadE "Yi.Dired"
         execE $ "Yi.Dired.diredDirBufferE " ++ show f
         withEditor getBuffer
    newBufferForPath bufferName False False  = 
      withEditor $ stringToNewBuffer bufferName []  -- Create new empty buffer

    {-
      Working out the name of the syntax from the extension of
      the file. Some of these are a little questionably haskell
      relatex. For example ".x" is an alex lexer specification
      I dare say that there are other file types that use ".x"
      as the file extension. 
      For now though this is probably okay given the users of
      'yi' are mostly haskell hackers, as of yet.
    -}
    syntaxFromExtension :: String -> String
    syntaxFromExtension ".hs"    = "haskell"
    syntaxFromExtension ".x"     = "haskell"
    -- anyone want to come up with a literate haskell syntax?
    syntaxFromExtension ".lhs"   = "haskell"
    -- haskell include files such as Yi/Syntax/alex.hsinc
    syntaxFromExtension ".hsinc" = "haskell"
    syntaxFromExtension ".cabal" = "cabal"
    syntaxFromExtension ".tex"   = "latex"
    syntaxFromExtension ".sty"   = "latex"
    syntaxFromExtension ".cxx"   = "cplusplus"
    syntaxFromExtension ".cpp"   = "cplusplus"
    syntaxFromExtension ".h"     = "cplusplus"
    -- I treat c file as cpp files, most users are smart enough
    -- to allow for that.
    syntaxFromExtension ".c"     = "cplusplus"
    -- pepa is a subset of srmc
    syntaxFromExtension ".pepa"  = "srmc"
    syntaxFromExtension ".srmc"  = "srmc"
    syntaxFromExtension _        = "none"

    -- The first argument is the buffer name
    fileToNewBuffer :: String -> FilePath -> YiM BufferRef
    fileToNewBuffer bufferName path = do
      contents <- liftIO $ readFile path
      withEditor $ stringToNewBuffer bufferName contents

    -- Given the desired buffer name, plus a list of current buffer
    -- names returns the best name for the new buffer. This will
    -- be the desired one in the case that it doesn't currently exist.
    -- Otherwise we will suffix it with <n> where n is one more than the
    -- current number of suffixed similar names.
    -- IOW if we want "file.hs" but one already exists then we'll create
    -- "file.hs<1>" but if that already exists then we'll create "file.hs<2>"
    -- and so on.
    desiredBufferName  = takeFileName f
    bestNewName :: [ String ] -> String
    bestNewName currentBufferNames 
      | elem desiredBufferName currentBufferNames = addSuffixBName 1
      | otherwise                                 = desiredBufferName
      where
      addSuffixBName :: Int -> String
      addSuffixBName i
        | elem possibleName currentBufferNames = addSuffixBName (i + 1)
        | otherwise                            = possibleName
        where
        possibleName = concat [ desiredBufferName
                              , "<"
                              , show i
                              , ">"
                              ]

-- | Revert to the contents of the file on disk
revertE :: YiM ()
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
switchToBufferE :: BufferRef -> YiM ()
switchToBufferE b = withEditor $ modifyWindows (modifier WS.currentA (\w -> w {bufkey = b}))

-- | Attach the specified buffer to some other window than the current one
switchToBufferOtherWindowE :: BufferRef -> YiM ()
switchToBufferOtherWindowE b = withEditor shiftOtherWindow >> switchToBufferE b

-- | Find buffer with given name. Raise exception if not found.
getBufferWithName :: String -> YiM BufferRef
getBufferWithName bufName = do
  bs <- withEditor $ gets $ findBufferWithName bufName
  case bs of
    [] -> fail ("Buffer not found: " ++ bufName)
    (b:_) -> return b

-- | Switch to the buffer specified as parameter. If the buffer name is empty, switch to the next buffer.
switchToBufferWithNameE :: String -> YiM ()
switchToBufferWithNameE "" = nextBufW
switchToBufferWithNameE bufName = switchToBufferE =<< getBufferWithName bufName

-- | Open a minibuffer window with the given prompt and keymap
spawnMinibufferE :: String -> KeymapMod -> YiM () -> YiM ()
spawnMinibufferE prompt kmMod initialAction =
    do b <- withEditor $ stringToNewBuffer prompt []
       setBufferKeymap b kmMod
       withEditor $ modifyWindows (WS.add $ Window True b 0 0 0)
       initialAction

-- | Write current buffer to disk, if this buffer is associated with a file
fwriteE :: YiM ()
fwriteE = do contents <- withBuffer elemsB
             fname <- withBuffer (gets file)
             withBuffer clearUndosB
             case fname of
               Just n -> liftIO $ writeFile n contents
               Nothing -> msgE "Buffer not associated with a file."

-- | Write current buffer to disk as @f@. If this buffer doesn't
-- currently have a file associated with it, the file is set to @f@
fwriteToE :: String -> YiM ()
fwriteToE f = do withBuffer $ setfileB f
                 fwriteE

-- | Write all open buffers
fwriteAllE :: YiM ()
fwriteAllE = error "fwriteAllE not implemented"

-- | Make a backup copy of file
backupE :: FilePath -> YiM ()
backupE = error "backupE not implemented"

-- | Return a list of all buffers, and their indicies
listBuffersE :: YiM [(String,Int)]
listBuffersE = do
        bs  <- withEditor getBuffers
        return $ zip (map name bs) [0..]

-- | Release resources associated with buffer
closeBufferE :: String -> YiM ()
closeBufferE bufName = do
  nextB <- withEditor nextBuffer
  b <- withEditor getBuffer
  b' <- if null bufName then return b else getBufferWithName bufName
  switchToBufferE nextB
  withEditor $ deleteBuffer b'

------------------------------------------------------------------------

-- | Close current buffer and window, unless it's the last one.
closeBufferAndWindowE :: EditorM ()
closeBufferAndWindowE = do
  deleteBuffer =<< getBuffer
  tryCloseE

-- | Close the current window.
-- If this is the last window open, quit the program.
closeE :: YiM ()
closeE = do
    n <- withEditor $ withWindows (length . toList)
    when (n == 1) quitE
    withEditor $ tryCloseE

-- | Recompile and reload the user's config files
reconfigE :: YiM ()
reconfigE = reloadE >> runConfig

runConfig :: YiM ()
runConfig = do
  loaded <- withKernel $ \kernel -> do
              let cfgMod = mkModuleName kernel "YiConfig"
              isLoaded kernel cfgMod
  if loaded 
   then do result <- withKernel $ \kernel -> evalMono kernel "YiConfig.yiMain :: Yi.Yi.YiM ()"
           case result of
             Nothing -> errorE "Could not run YiConfig.yiMain :: Yi.Yi.YiM ()"
             Just x -> x
   else errorE "YiConfig not loaded"

loadE :: String -> YiM [String]
loadE modul = do
  logPutStrLn $ "loadE: " ++ modul
  ms <- readsRef editorModules
  tryLoadModulesE (if Data.List.notElem modul ms then ms++[modul] else ms)

unloadE :: String -> YiM [String]
unloadE modul = do
  ms <- readsRef editorModules
  tryLoadModulesE $ delete modul ms

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
      let [b] = findBufferWithName "*console*" e
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
                               evalMono kernel ("makeAction (" ++ s ++ ") :: Yi.Yi.Action")
            case result of
              Left err -> errorE err
              Right x -> do runAction x
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

withOtherWindow :: YiM () -> YiM ()
withOtherWindow f = do
  withEditor $ shiftOtherWindow
  f
  withEditor $ prevWinE
