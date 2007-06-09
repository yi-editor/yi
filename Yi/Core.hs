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
        
        Direction (..),
        -- * Construction and destruction
        startE,         -- :: Kernel -> Maybe Editor -> [Action] -> IO ()
        --emptyE,         -- :: Action
        --runE,           -- :: String -> Action
        quitE,          -- :: Action
        --rebootE,        -- :: Action
        reloadE,        -- :: Action
        reconfigE,
        loadE,
        unloadE,
        refreshE,       -- :: Action
        suspendE,       -- :: Action

        -- * Global editor actions
        BufferFileInfo ( .. ), 

        nopE,           -- :: Action
        msgE,           -- :: String -> Action
        errorE,         -- :: String -> Action
        msgClrE,        -- :: Action
        bufInfoE,       -- :: EditorM BufferFileInfo
        fileNameE,      -- :: EditorM (Maybe FilePath)
        bufNameE,       -- :: EditorM String
        setWindowFillE, -- :: Char -> Action
        setWindowStyleE,-- :: UIStyle -> Action

        -- * Window manipulation
        nextWinE,       -- :: Action
        prevWinE,       -- :: Action
        setWinE,        -- :: Window -> Action
        closeE,         -- :: Action
        closeOtherE,    -- :: Action
        splitE,         -- :: Action
        enlargeWinE,    -- :: Action
        shrinkWinE,     -- :: Action

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
        isUnchangedE,   -- :: EditorM Bool
        setUnchangedE,  -- :: Action
        setSynE,        -- :: String -> Action
        getBufferWithName,

        -- * Buffer/Window
        switchToBufferE,
        switchToBufferOtherWindowE,
        switchToBufferWithNameE,
        nextBufW,       -- :: Action
        prevBufW,       -- :: Action

        -- * Buffer point movement
        topE,           -- :: Action
        botE,           -- :: Action
        solE,           -- :: Action
        eolE,           -- :: Action
        downE,          -- :: Action
        upE,            -- :: Action
        leftE,          -- :: Action
        rightE,         -- :: Action
        leftOrSolE,     -- :: Int -> Action
        rightOrEolE,    -- :: Int -> Action
        gotoLnE,        -- :: Int -> Action
        gotoLnFromE,    -- :: Int -> Action
        gotoPointE,     -- :: Int -> Action
        getPointE,      -- :: EditorM Int
        getLineAndColE, -- :: EditorM (Int, Int)

        atSolE,         -- :: EditorM Bool
        atEolE,         -- :: EditorM Bool
        atSofE,         -- :: EditorM Bool
        atEofE,         -- :: EditorM Bool

        -- * Window-based movement
        upScreenE,      -- :: Action
        upScreensE,     -- :: Int -> Action
        downScreenE,    -- :: Action
        downScreensE,   -- :: Int -> Action
        downFromTosE,   -- :: Int -> Action
        upFromBosE,     -- :: Int -> Action
        middleE,        -- :: Action

        -- * Buffer editing
        insertE,        -- :: Char -> Action
        insertNE,       -- :: String -> Action
        deleteE,        -- :: Action
        deleteNE,       -- :: Int -> Action
        killE,          -- :: Action
        deleteRegionE,  -- :: Region -> Action
        writeE,         -- :: Char -> Action
        undoE,          -- :: Action
        redoE,          -- :: Action
        revertE,        -- :: Action

        -- * Read parts of the buffer
        readE,          -- :: EditorM Char
        readRegionE,    -- :: Region -> EditorM String
        readLnE,        -- :: EditorM String
        readNM,         -- :: Int -> Int -> EditorM String
        readRestOfLnE,  -- :: EditorM String
        readAllE,       -- :: EditorM String

        swapE,          -- :: Action

        -- * Basic registers
        setRegE,        -- :: String -> Action
        getRegE,        -- :: EditorM String

        -- * Marks
        setMarkE,
        getMarkE,
        unsetMarkE,
        exchangePointAndMarkE,
        getBookmarkE,
        getBookmarkPointE,
        setBookmarkPointE,

        -- * Dynamically extensible state
        getDynamic,
        setDynamic,

        -- * higher level ops
        mapRangeE,              -- :: Int -> Int -> (Char -> Char) -> Action

        -- * Interacting with external commands
        pipeE,                   -- :: String -> String -> EditorM String
 
        -- * Minibuffer
        spawnMinibufferE,

        -- * Misc
        catchJustE,
        changeKeymapE,
        getNamesInScopeE,
        execE
   ) where

import Prelude hiding (error)

import Yi.Debug
import Yi.Buffer
import Yi.Dynamic
import Yi.Region
import Yi.Window
import Yi.String
import Yi.Process           ( popen )
import Yi.Editor hiding (readEditor)
import Yi.CoreUI
import Yi.Kernel
import Yi.Event
import Yi.Keymap
import Yi.Interact (anyEvent)
import Yi.Monad
import qualified Yi.Editor as Editor
import qualified Yi.Style as Style
import qualified Yi.UI as UI

import Data.Maybe
import qualified Data.Map as M
import Data.List
import Data.IORef

import System.Directory     ( doesFileExist, doesDirectoryExist )

import Control.Monad.Reader
import Control.Exception
import Control.Concurrent 
import Control.Concurrent.Chan

import qualified GHC
import qualified DynFlags
import qualified SrcLoc
import qualified ErrUtils
import Outputable

import GHC.Exts ( unsafeCoerce# )


-- | A 'Direction' is either left or right.
data Direction = GoLeft | GoRight

-- | Make an action suitable for an interactive run.
-- UI will be refreshed.
interactive :: YiM a -> YiM a
interactive action = do 
  lift $ logPutStrLn ">>>>>>> interactively"
  withUI UI.prepareAction
  x <- action
  withUI UI.scheduleRefresh
  lift $ logPutStrLn "<<<<<<<"
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
startE :: Kernel -> Maybe Editor -> [Action] -> IO ()
startE kernel st commandLineActions = do
    logPutStrLn "Starting Core"

    -- restore the old state
    newSt <- newIORef $ maybe emptyEditor id st
    let runEd f = runReaderT f newSt
    (inCh, ui) <- runEd UI.start
    outCh <- newChan
    startKm <- newIORef nilKeymap
    startModules <- newIORef []
    keymaps <- newIORef M.empty
    let yi = Yi newSt ui inCh outCh startKm keymaps kernel startModules
        runYi f = runReaderT f yi

    runYi $ do 

      -- Setting up the 1st buffer/window is a bit tricky because most functions assume there exists a "current window"
      -- or a "current buffer".
      consoleB <- withEditor $ stringToNewBuffer "*console*" "" 
      consoleW <- withUI $ UI.newWindow False consoleB
      withEditor $ UI.setWindow consoleW
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
            runYi $ withUI UI.scheduleRefresh
            let loop = sequence_ . map runYi . map interactive =<< getChanContents outCh
            repeatM_ $ (handle handler loop >> logPutStrLn "Execing loop ended")
      
    t1 <- forkIO eventLoop 
    t2 <- forkIO execLoop
    modifyIORef newSt $ \e -> e { threads = t1 : t2 : threads e }

    UI.main newSt ui -- transfer control to UI: GTK must run in the main thread, or else it's not happy.
                
changeKeymapE :: Keymap -> Action
changeKeymapE km = do
  modifyRef defaultKeymap (const km)
  bs <- withEditor getBuffers
  mapM_ restartBufferThread bs
  return ()

-- ---------------------------------------------------------------------
-- Meta operations

-- | Quit.
quitE :: Action
quitE = withUI' UI.end


-- | (Re)compile and reload the user's config files
reloadE :: YiM [String]
reloadE = do
  modules <- readRef editorModules
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


-- | Reset the size, and force a complete redraw
refreshE :: Action
refreshE = withUI UI.refreshAll

-- | Do nothing
nopE :: Action
nopE = return ()

-- | Suspend the program
suspendE :: Action
suspendE = withUI UI.suspend

-- ---------------------------------------------------------------------
-- Movement operations

-- | Move cursor to origin
topE :: Action
topE = withBuffer $ moveTo 0

-- | Move cursor to end of buffer
botE :: Action
botE = withBuffer $ do
            n <- sizeB
            moveTo n

-- | Move cursor to start of line
solE :: Action
solE = withBuffer moveToSol

-- | Move cursor to end of line
eolE :: Action
eolE = withBuffer moveToEol

-- | Move cursor down 1 line
downE :: Action
downE = withBuffer lineDown

-- | Move cursor up to the same point on the previous line
upE :: Action
upE = withBuffer lineUp

-- | Go to line number @n@
gotoLnE :: Int -> Action
gotoLnE n = withBuffer (gotoLn n >> return ())

-- | Go to line @n@ offset from current line
gotoLnFromE :: Int -> Action
gotoLnFromE n = withBuffer (gotoLnFrom n >> return ())

-- | Go to a particular point.
gotoPointE :: Int -> Action
gotoPointE p = withBuffer $ moveTo p

-- | Get the current point
getPointE :: YiM Int
getPointE = withBuffer pointB

-- | Get the current line and column number
getLineAndColE :: YiM (Int, Int)
getLineAndColE = withBuffer lineAndColumn
    where lineAndColumn :: BufferM (Int, Int)
	  lineAndColumn = 
	      do lineNo <- curLn
		 colNo  <- offsetFromSol
		 return (lineNo, colNo)

------------------------------------------------------------------------

-- | Is the point at the start of the line
atSolE :: YiM Bool
atSolE = withBuffer atSol

-- | Is the point at the end of the line
atEolE :: YiM Bool
atEolE = withBuffer atEol

-- | Is the point at the start of the file
atSofE :: YiM Bool
atSofE = withBuffer atSof

-- | Is the point at the end of the file
atEofE :: YiM Bool
atEofE = withBuffer atEof

------------------------------------------------------------------------

-- | Scroll up 1 screen
upScreenE :: Action
upScreenE = do
    (Just w) <- withEditor getWindow
    withBuffer (gotoLnFrom (- (height w - 1)))
    solE

-- | Scroll up n screens
upScreensE :: Int -> Action
upScreensE n = do
    (Just w) <- withEditor getWindow
    withBuffer (gotoLnFrom (- (n * (height w - 1))))
    solE

-- | Scroll down 1 screen
downScreenE :: Action
downScreenE = do
    (Just w) <- withEditor getWindow
    withBuffer (gotoLnFrom (height w - 1))
    return ()

-- | Scroll down n screens
downScreensE :: Int -> Action
downScreensE n = do
    (Just w) <- withEditor getWindow
    withBuffer (gotoLnFrom (n * (height w - 1)))
    return ()

-- | Move to @n@ lines down from top of screen
downFromTosE :: Int -> Action
downFromTosE n = do
    (i,fn) <- withWindow $ \w _ -> do
                    let y  = fst $ cursor w
                        n' = min n (height w - 1 - 1)
                        d  = n' - y
                    return (abs d, if d < 0 then upE else downE)
    replicateM_ i fn

-- | Move to @n@ lines up from the bottom of the screen
upFromBosE :: Int -> Action
upFromBosE n = (withWindow $ \w _ -> return (height w -1 -1 - n)) >>= downFromTosE

-- | Move to middle line in screen
middleE :: Action
middleE = (withWindow $ \w _ -> return ((height w -1-1) `div` 2)) >>= downFromTosE

-- ---------------------------------------------------------------------

-- | move the point left (backwards) in the buffer
leftE :: Action
leftE = withBuffer leftB

-- | move the point right (forwards) in the buffer
rightE :: Action
rightE = withBuffer rightB

-- | Move left @x@ or to start of line
leftOrSolE :: Int -> Action
leftOrSolE x = withBuffer $ moveXorSol x

-- | Move right @x@ or to end of line
rightOrEolE :: Int -> Action
rightOrEolE x = withBuffer $ moveXorEol x

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

------------------------------------------------------------------------

-- | Insert new character
insertE :: Char -> Action
insertE c = insertNE [c]

-- | Insert a string
insertNE :: String -> Action
insertNE str = withBuffer $ insertN str

-- | Delete character under cursor
deleteE :: Action
deleteE = withBuffer $ deleteN 1

-- | Delete @n@ characters from under the cursor
deleteNE :: Int -> Action
deleteNE i = withBuffer $ deleteN i

-- | Kill to end of line
killE :: Action
killE = withBuffer deleteToEol

-- | Delete an arbitrary part of the buffer
deleteRegionE :: Region -> Action
deleteRegionE r = withBuffer $
                    deleteNAt (regionEnd r - regionStart r) (regionStart r)


-- | Read the char under the cursor
readE :: YiM Char
readE = withBuffer readB


-- | Read an arbitrary part of the buffer
readRegionE :: Region -> YiM String
readRegionE r = readNM (regionStart r) (regionEnd r)

-- | Read the line the point is on
readLnE :: YiM String
readLnE = withBuffer $ do
    i <- indexOfSol
    j <- indexOfEol
    nelemsB (j-i) i

-- | Read from - to
readNM :: Int -> Int -> YiM String
readNM i j = withBuffer $ nelemsB (j-i) i

-- | Return the contents of the buffer as a string (note that this will
-- be very expensive on large (multi-megabyte) buffers)
readAllE :: YiM String
readAllE = withBuffer elemsB

-- | Read from point to end of line
readRestOfLnE :: YiM String
readRestOfLnE = withBuffer $ do
    p <- pointB
    j <- indexOfEol
    nelemsB (j-p) p

-- | Write char to point
writeE :: Char -> Action
writeE c = withBuffer $ writeB c

-- | Transpose two characters, (the Emacs C-t action)
swapE :: Action
swapE = do eol <- atEolE
           when eol leftE
           c <- readE
           deleteE
           leftE
           insertE c
           rightE

-- ---------------------------------------------------------------------

undoE :: Action
undoE = withBuffer undo

redoE :: Action
redoE = withBuffer redo

-- ---------------------------------------------------------------------
-- registers (TODO these may be redundant now that it is easy to thread
-- state in key bindings, or maybe not.
--

-- | Put string into yank register
setRegE :: String -> Action
setRegE s = withEditor $  modifyEditor_ $ \e -> return e { yreg = s }

-- | Return the contents of the yank register
getRegE :: YiM String
getRegE = readEditor yreg


-- ----------------------------------------------------
-- | Marks

-- | Set the current buffer mark
setMarkE :: Int -> Action
setMarkE pos = withBuffer $ do m <- getSelectionMarkB; setMarkPointB m pos

-- | Unset the current buffer mark so that there is no selection
unsetMarkE :: Action
unsetMarkE = withBuffer $ unsetMarkB

-- | Get the current buffer mark
getMarkE :: YiM Int
getMarkE = withBuffer $ do m <- getSelectionMarkB; getMarkPointB m

-- | Exchange point & mark.
-- Maybe this is better put in Emacs\/Mg common file
exchangePointAndMarkE :: Action
exchangePointAndMarkE = do m <- getMarkE
                           p <- getPointE
                           setMarkE p
                           gotoPointE m

getBookmarkE :: String -> YiM Mark
getBookmarkE nm = withBuffer $ getMarkB (Just nm)

setBookmarkPointE :: Mark -> Point -> Action
setBookmarkPointE bookmark pos = withBuffer $ setMarkPointB bookmark pos

getBookmarkPointE :: Mark -> YiM Point
getBookmarkPointE bookmark = withBuffer $ getMarkPointB bookmark

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
setDynamic x = withEditor $ modifyEditor_ $ \e ->
        return e { dynamic = setDynamicValue x (dynamic e) }

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
  withUI' $ UI.setCmdLine s
  withEditor $ modifyEditor_ $ \e -> do
              -- also show in the messages buffer, so we don't loose any message
              let [b] = findBufferWithName e "*messages*"
              runBuffer b $ do moveTo =<< sizeB; insertN (s ++ "\n")
              return e
            

-- | Set the cmd buffer, and draw a pretty error message
errorE :: String -> Action
errorE s = do msgE ("error: " ++ s)
              lift $ logPutStrLn $ "errorE: " ++ s

-- | Clear the message line at bottom of screen
msgClrE :: Action
msgClrE = msgE ""

-- ---------------------------------------------------------------------
-- Buffer operations

data BufferFileInfo =
    BufferFileInfo { bufInfoFileName :: FilePath
		   , bufInfoSize     :: Int
		   , bufInfoLineNo   :: Int
		   , bufInfoColNo    :: Int
		   , bufInfoCharNo   :: Int
		   , bufInfoPercent  :: String
		   , bufInfoModified :: Bool
		   }

-- | File info, size in chars, line no, col num, char num, percent
bufInfoE :: YiM BufferFileInfo
bufInfoE = withBuffer $ do
    s <- sizeB
    p <- pointB
    m <- isUnchangedB
    l <- curLn
    c <- offsetFromSol
    nm <- nameB
    let bufInfo = BufferFileInfo { bufInfoFileName = nm
				 , bufInfoSize     = s
				 , bufInfoLineNo   = l
				 , bufInfoColNo    = c
				 , bufInfoCharNo   = p
				 , bufInfoPercent  = getPercent p s 
				 , bufInfoModified = not m
				 }
    return bufInfo

-- | Maybe a file associated with this buffer
fileNameE :: YiM (Maybe FilePath)
fileNameE = withBuffer getfileB

-- | Name of this buffer
bufNameE :: YiM String
bufNameE = withBuffer $ nameB

-- | A character to fill blank lines in windows with. Usually '~' for
-- vi-like editors, ' ' for everything else
setWindowFillE :: Char -> Action
setWindowFillE c = withEditor $ modifyEditor_ $ \e -> return $ e { windowfill = c }

-- | Sets the window style.
setWindowStyleE :: Style.UIStyle -> Action
setWindowStyleE sty = withEditor $ modifyEditor_ $ \e -> return $ e { uistyle = sty }


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
    bufsWithThisFilename <- liftIO $ filterM (\b -> readMVar (file b) >>= return . (==Just f)) bufs
    b <- case bufsWithThisFilename of
             [] -> do
                   fe  <- lift $ doesFileExist f
                   de  <- lift $ doesDirectoryExist f
                   newBufferForPath fe de
             _  -> return (head bufsWithThisFilename)
    withGivenBuffer b $ setfileB f        -- associate buffer with file
    switchToBufferE b
    where
    newBufferForPath :: Bool -> Bool -> YiM FBuffer
    newBufferForPath True _      = do b <- withEditor $ hNewBuffer f                 -- Load the file into a new buffer
                                      return b
    newBufferForPath False True  = do           -- Open the dir in Dired
            loadE "Yi.Dired"
            execE $ "Yi.Dired.diredDirBufferE " ++ show f
            withEditor getBuffer
    newBufferForPath False False = do b <- withEditor $ stringToNewBuffer f []                       -- Create new empty buffer
                                      return b


-- | Revert to the contents of the file on disk
revertE :: Action
revertE = do
            mfp <- withBuffer getfileB
            case mfp of
                     Just fp -> do
                             s <- liftIO $ readFile fp
                             end <- withBuffer sizeB
                             p <- getPointE
                             deleteRegionE (mkRegion 0 end)
                             topE
                             insertNE s
                             gotoPointE p
                             withBuffer clearUndosB
                             msgE ("Reverted from " ++ show fp)
                     Nothing -> do
                                msgE "Can't revert, no file associated with buffer."
                                return ()

-- | Like fnewE, create a new buffer filled with the String @s@,
-- Open up a new window onto this buffer. Doesn't associate any file
-- with the buffer (unlike fnewE) and so is good for popup internal
-- buffers (like scratch)
newBufferE :: String -> String -> YiM FBuffer
newBufferE f s = do
    b <- withEditor $ stringToNewBuffer f s
    switchToBufferE b
    lift $ logPutStrLn "newBufferE ended"
    return b

-- | Attach the specified buffer to the current window
switchToBufferE :: FBuffer -> Action
switchToBufferE b = withEditor getWindow >>= \w -> (withUI $ UI.setWindowBuffer b w)

-- | Attach the specified buffer to some other window than the current one
switchToBufferOtherWindowE :: FBuffer -> Action
switchToBufferOtherWindowE b = shiftOtherWindow >> switchToBufferE b

-- | Find buffer with given name. Raise exception if not found.
getBufferWithName :: String -> YiM FBuffer
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
       w <- withUI $ UI.newWindow True b
       withEditor $ UI.setWindow w
       initialAction

-- | Write current buffer to disk, if this buffer is associated with a file
fwriteE :: Action
fwriteE = withBuffer hPutB

-- | Write current buffer to disk as @f@. If this buffer doesn't
-- currently have a file associated with it, the file is set to @f@
fwriteToE :: String -> Action
fwriteToE f = withBuffer $ do setfileB f
                              hPutB

-- | Write all open buffers
fwriteAllE :: Action
fwriteAllE = undefined

-- | Make a backup copy of file
backupE :: FilePath -> Action
backupE = undefined

-- | Return a list of all buffers, and their indicies
listBuffersE :: YiM [(String,Int)]
listBuffersE = do
        bs  <- withEditor getBuffers
        return $ zip (map name bs) [0..]

-- | Release resources associated with buffer, close any windows open
-- onto buffer.
closeBufferE :: String -> Action
closeBufferE f = killBufferAndWindows f

------------------------------------------------------------------------

-- | Is the current buffer unmodifed? (currently buggy, we need
-- bounaries in the undo list)
isUnchangedE :: YiM Bool
isUnchangedE = withBuffer isUnchangedB

-- | Set the current buffer to be unmodifed
setUnchangedE :: Action
setUnchangedE = undefined

-- | Set the current buffer's highlighting kind
setSynE :: String -> Action
setSynE sy = withBuffer $ setSyntaxB sy

------------------------------------------------------------------------
--
-- Window operations

-- | Make the next window (down the screen) the current window
nextWinE :: Action
nextWinE = nextWindow

-- | Make the previous window (up the screen) the current window
prevWinE :: Action
prevWinE = prevWindow

-- | Make window with key @k@ the current window
setWinE :: Window -> Action
setWinE w = withEditor $ UI.setWindow w

-- | Split the current window, opening a second window onto this buffer.
-- Windows smaller than 3 lines cannot be split.
splitE :: Action
splitE = do
    canSplit <- withUI UI.hasRoomForExtraWindow
    if canSplit 
      then splitWindow
      else errorE "Not enough room to split"

-- | Enlarge the current window
enlargeWinE :: Action
enlargeWinE = withEditor getWindow >>= \w -> withUI $ UI.enlargeWindow w

-- | Shrink the current window
shrinkWinE :: Action
shrinkWinE = withEditor getWindow >>= \w -> withUI $ UI.shrinkWindow w

-- | Close the current window.
-- If this is the last window open, quit the program. TODO this
-- behaviour may be undesirable
closeE :: Action
closeE = do
        deleteThisWindow
        i <- withEditor $ sizeWindows
        when (i == 0) quitE

-- | Make the current window the only window on the screen
closeOtherE :: Action
closeOtherE = do
        this   <- withEditor getWindow -- current window
        others <- withEditor $ modifyEditor $ \e -> do
                        let ws = getWindows e
                        return (e, (filter (/= this) (map Just ws)))
        sequence_ [withUI $ UI.deleteWindow w | w <- others]

------------------------------------------------------------------------
--
-- Map a char function over a range of the buffer.
--
-- Fold over a range is probably useful too..
--
-- !!!This is a very bad implementation; delete; apply; and insert the result.
mapRangeE :: Int -> Int -> (Char -> Char) -> Action
mapRangeE from to fn
    | from < 0  = nopE
    | otherwise = do
        withBuffer $ do
            eof <- sizeB
            when (to < eof) $ do
                let loop j | j <= 0    = return ()
                           | otherwise = do
                                readB >>= return . fn >>= writeB
                                rightB
                                loop (j-1)
                loop (max 0 (to - from))
            moveTo from


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
  modifyRef editorModules (++ [modul])
  reloadE

unloadE :: String -> YiM [String]
unloadE modul = do
  modifyRef editorModules $ delete modul
  reloadE

getNamesInScopeE :: YiM [String]
getNamesInScopeE = do
  withKernel $ \k -> do
      rdrNames <- getRdrNamesInScope k
      names <- getNamesInScope k
      return $ map (nameToString k) rdrNames ++ map (nameToString k) names

savingExcursion :: BufferM a -> BufferM a
savingExcursion f = do
    m <- getMarkB Nothing
    res <- f
    moveTo =<< getMarkPointB m
    return res


ghcErrorReporter :: Yi -> GHC.Severity -> SrcLoc.SrcSpan -> Outputable.PprStyle -> ErrUtils.Message -> IO () 
ghcErrorReporter yi severity srcSpan pprStyle message = 
    -- the following is written in very bad style.
    flip runReaderT yi $ do
      e <- readEditor id
      let [b] = findBufferWithName e "*console*"
      withGivenBuffer b $ savingExcursion $ do 
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
                               compileExpr kernel ("(" ++ s ++ ") >>= msgE' . show :: YiM ()")
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
