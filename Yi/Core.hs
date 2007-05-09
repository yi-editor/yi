-- -fglasgow-exts for deriving Typeable
--
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
        -- * Keymap
        module Yi.Keymap,

        Direction (..),
        -- * Construction and destruction
        startE,         -- :: Kernel -> Maybe Editor -> [Action] -> IO ()
        emptyE,         -- :: Action
        runE,           -- :: String -> Action
        quitE,          -- :: Action
        rebootE,        -- :: Action
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
        getNamesInScopeE
   ) where

import Prelude hiding (error)

import Yi.Debug
import Yi.Buffer
import Yi.Region
import Yi.Window
import Yi.String
import Yi.Process           ( popen )
import Yi.Editor
import Yi.CoreUI
import Yi.Kernel
import Yi.Event
import Yi.Keymap
import Yi.Interact (anyEvent)
import qualified Yi.Editor as Editor
import qualified Yi.Style as Style
import qualified Yi.UI as UI

import Data.Maybe
import Data.Dynamic
import Data.List
import qualified Data.Map as M        ( lookup, insert )
import Data.IORef

import System.Directory     ( doesFileExist )

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

nilKeymap :: Keymap
nilKeymap = do c <- anyEvent
               write $ case eventToChar c of
                         'q' -> quitE 
                         'r' -> reconfigE
                         _ -> errorE $ "Keymap not defined, type 'r' to reload config or 'q' to quit. " ++
                                          "README file may help you."

-- ---------------------------------------------------------------------
-- | Start up the editor, setting any state with the user preferences
-- and file names passed in, and turning on the UI
--
startE :: Kernel -> Maybe Editor -> [Action] -> IO ()
startE kernel st commandLineActions = do
    logPutStrLn "Starting Core"

    -- restore the old state
    newSt <- newIORef $ maybe emptyEditor id st
    outCh <- newChan
    flip runReaderT newSt $ do 
      modifyEditor_ $ \e -> return e { output = outCh, 
                                       editorKernel = kernel, 
                                       defaultKeymap = nilKeymap }
      UI.start  

      -- Setting up the 1st buffer/window is a bit tricky because most functions assume there exists a "current window"
      -- or a "current buffer".
      stringToNewBuffer "*console*" "" >>= UI.newWindow False >>= UI.setWindow
      newBufferE "*messages*" "" >> return ()

      withKernel $ \k -> do
        dflags <- getSessionDynFlags k
        setSessionDynFlags k dflags { GHC.log_action = ghcErrorReporter newSt }

      -- run user configuration
      loadE "YiConfig"
      runConfig

      when (isNothing st) $ do -- process options if booting for the first time
        sequence_ commandLineActions

    logPutStrLn "Starting event handler"
    let
        handler e = flip runReaderT newSt $ errorE (show e)
        -- | The editor's input main loop. 
        -- Read key strokes from the ui and dispatches them to the buffer with focus.
        eventLoop :: IO ()
        eventLoop = do
            ch <- liftM input $ readIORef newSt 
            let run = mapM_ dispatch =<< getChanContents ch
            repeatM_ $ (handle handler run >> logPutStrLn "Dispatching loop ended")
                     
            where
              dispatch action = flip runReaderT newSt $ withBuffer $ \b -> writeChan (bufferInput b) action


        -- | Make an action suitable for an interactive run.
        -- Editor state will be refreshed after
        interactive :: Action -> IO ()
        interactive action = do 
          logPutStrLn ">>>>>>> interactively"
          runReaderT (UI.prepareAction >> action >> UI.scheduleRefresh)  newSt
          logPutStrLn "<<<<<<<"

        -- | The editor's output main loop. 
        execLoop :: IO ()
        execLoop = do
            runReaderT UI.scheduleRefresh newSt
            let loop = sequence_ . map interactive =<< getChanContents outCh
            repeatM_ $ (handle handler loop >> logPutStrLn "Execing loop ended")
      
    t1 <- forkIO eventLoop 
    t2 <- forkIO execLoop
    modifyIORef newSt $ \e -> e { threads = t1 : t2 : threads e }

    UI.main newSt -- transfer control to UI: GTK must run in the main thread, or else it's not happy.
                
changeKeymapE :: Keymap -> Action
changeKeymapE km = do
  modifyEditor_ $ \e -> return e { defaultKeymap = km }
  bs <- getBuffers
  lift $ mapM_ restartBufferThread bs


-- ---------------------------------------------------------------------
-- | @emptyE@ and @runE@ are for automated testing purposes. The former
-- initialises the editor state, and returns, enabling the tester to
-- invoke various core commands from a program. The latter runs the
-- editor with string as input, and is useful for testing keymaps.
-- emptyE takes no input -- the ui blocks on stdin.
--
emptyE :: Action
emptyE = modifyEditor_ $ const $ return $ emptyEditor
    -- need to get it into a state where we can just run core commands
    -- to make it reinitialisable, lets blank out the state
    -- make up an abitrary screen size

    -- no ui thread
    -- no eventloop

-- for testing keymaps:
runE :: String -> Action
runE = undefined

-- ---------------------------------------------------------------------
-- Meta operations

-- | Quit.
quitE :: Action
quitE = withUI UI.end

-- | Reboot (!). Reboot the entire editor, reloading the Yi core.
rebootE :: Action
rebootE = do
    e  <- readEditor id
    fn <- readEditor reboot
    Editor.shutdown
    lift $ fn (Just e)

-- | (Re)compile and reload the user's config files
reloadE :: EditorM [String]
reloadE = do
  modules <- readEditor editorModules
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

foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()  
	-- Make it "safe", just in case


-- | Reset the size, and force a complete redraw
refreshE :: Action
refreshE = UI.refreshAll

-- | Do nothing
nopE :: Action
nopE = return ()

-- | Suspend the program
suspendE :: Action
suspendE = UI.suspend

-- ---------------------------------------------------------------------
-- Movement operations

-- | Move cursor to origin
topE :: Action
topE = withBuffer $ \b -> moveTo b 0

-- | Move cursor to end of buffer
botE :: Action
botE = withBuffer $ \b -> do
            n <- sizeB b
            moveTo b n

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
gotoLnE n = withBuffer (flip gotoLn n) >> return ()

-- | Go to line @n@ offset from current line
gotoLnFromE :: Int -> Action
gotoLnFromE n = withBuffer (flip gotoLnFrom n) >> return ()

-- | Go to a particular point.
gotoPointE :: Int -> Action
gotoPointE p = withBuffer $ flip moveTo p

-- | Get the current point
getPointE :: EditorM Int
getPointE = withBuffer pointB

-- | Get the current line and column number
getLineAndColE :: EditorM (Int, Int)
getLineAndColE = 
    withBuffer lineAndColumn
    where lineAndColumn :: FBuffer -> IO (Int, Int)
	  lineAndColumn b = 
	      do lineNo <- curLn b
		 colNo  <- offsetFromSol b
		 return (lineNo, colNo)

------------------------------------------------------------------------

-- | Is the point at the start of the line
atSolE :: EditorM Bool
atSolE = withBuffer atSol

-- | Is the point at the end of the line
atEolE :: EditorM Bool
atEolE = withBuffer atEol

-- | Is the point at the start of the file
atSofE :: EditorM Bool
atSofE = withBuffer atSof

-- | Is the point at the end of the file
atEofE :: EditorM Bool
atEofE = withBuffer atEof

------------------------------------------------------------------------

-- | Scroll up 1 screen
upScreenE :: Action
upScreenE = do
    (Just w) <- getWindow
    withBuffer (flip gotoLnFrom (- (height w - 1)))
    solE

-- | Scroll up n screens
upScreensE :: Int -> Action
upScreensE n = do
    (Just w) <- getWindow
    withBuffer (flip gotoLnFrom (- (n * (height w - 1))))
    solE

-- | Scroll down 1 screen
downScreenE :: Action
downScreenE = do
    (Just w) <- getWindow
    withBuffer (flip gotoLnFrom (height w - 1))
    return ()

-- | Scroll down n screens
downScreensE :: Int -> Action
downScreensE n = do
    (Just w) <- getWindow
    withBuffer (flip gotoLnFrom (n * (height w - 1)))
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
leftOrSolE x = withBuffer $ flip moveXorSol x

-- | Move right @x@ or to end of line
rightOrEolE :: Int -> Action
rightOrEolE x = withBuffer $ flip moveXorEol x

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
insertNE str = withBuffer $ \b -> insertN b str

-- | Delete character under cursor
deleteE :: Action
deleteE = withBuffer $ \b -> deleteN b 1

-- | Delete @n@ characters from under the cursor
deleteNE :: Int -> Action
deleteNE i = withBuffer $ \b -> deleteN b i

-- | Kill to end of line
killE :: Action
killE = withBuffer deleteToEol

-- | Delete an arbitrary part of the buffer
deleteRegionE :: Region -> Action
deleteRegionE r = withBuffer $ \b -> do
                    deleteNAt b (regionEnd r - regionStart r) (regionStart r)


-- | Read the char under the cursor
readE :: EditorM Char
readE = withBuffer readB


-- | Read an arbitrary part of the buffer
readRegionE :: Region -> EditorM String
readRegionE r = readNM (regionStart r) (regionEnd r)

-- | Read the line the point is on
readLnE :: EditorM String
readLnE = withBuffer $ \b -> do
    i <- indexOfSol b
    j <- indexOfEol b
    s <- nelemsB b (j-i) i
    return s

-- | Read from - to
readNM :: Int -> Int -> EditorM String
readNM i j = withBuffer $ \b -> nelemsB b (j-i) i

-- | Return the contents of the buffer as a string (note that this will
-- be very expensive on large (multi-megabyte) buffers)
readAllE :: EditorM String
readAllE = withBuffer $ \b -> elemsB b

-- | Read from point to end of line
readRestOfLnE :: EditorM String
readRestOfLnE = withBuffer $ \b -> do
    p <- pointB b
    j <- indexOfEol b
    s <- nelemsB b (j-p) p
    return s

-- | Write char to point
writeE :: Char -> Action
writeE c = withBuffer $ \b -> writeB b c

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
setRegE s = modifyEditor_ $ \e -> return e { yreg = s }

-- | Return the contents of the yank register
getRegE :: EditorM String
getRegE = readEditor yreg


-- ----------------------------------------------------
-- | Marks

-- | Set the current buffer mark
setMarkE :: Int -> Action
setMarkE pos = withBuffer $ \b -> do m <- getSelectionMarkB b; setMarkPointB b m pos

-- | Unset the current buffer mark so that there is no selection
unsetMarkE :: Action
unsetMarkE = withBuffer $ \b -> unsetMarkB b

-- | Get the current buffer mark
getMarkE :: EditorM Int
getMarkE = withBuffer $ \b -> do m <- getSelectionMarkB b; getMarkPointB b m

-- | Exchange point & mark.
-- Maybe this is better put in Emacs\/Mg common file
exchangePointAndMarkE :: Action
exchangePointAndMarkE = do m <- getMarkE
                           p <- getPointE
                           setMarkE p
                           gotoPointE m

getBookmarkE :: String -> EditorM Mark
getBookmarkE nm = withBuffer $ \b -> getMarkB b (Just nm)

setBookmarkPointE :: Mark -> Point -> Action
setBookmarkPointE bookmark pos = withBuffer $ \b -> setMarkPointB b bookmark pos

getBookmarkPointE :: Mark -> EditorM Point
getBookmarkPointE bookmark = withBuffer $ \b -> getMarkPointB b bookmark

-- ---------------------------------------------------------------------
-- | Dynamically-extensible state components.
--
-- These hooks are used by keymaps to store values that result from
-- Actions (i.e. that restult from IO), as opposed to the pure values
-- they generate themselves, and can be stored internally.
--
-- The `dynamic' field is a type-indexed map.
--

-- | Retrieve the extensible state

getDynamic :: Initializable a => EditorM a
getDynamic = getDynamic' (undefined :: a)
    where
        getDynamic' :: Initializable b => b -> EditorM b
        getDynamic' a = do
                ps <- readEditor dynamic
                case M.lookup (show $ typeOf a) ps of
                    Nothing -> lift $ initial
                    Just x -> return $ fromJust $ fromDynamic x

-- | Insert a value into the extensible state, keyed by its type
setDynamic :: Typeable a => a -> Action
setDynamic x = modifyEditor_ $ \e ->
        return e { dynamic = M.insert (show $ typeOf x) (toDyn x) (dynamic e) }


------------------------------------------------------------------------
-- | Pipe a string through an external command, returning the stdout
-- chomp any trailing newline (is this desirable?)
--
-- Todo: varients with marks?
--
pipeE :: String -> String -> EditorM String
pipeE cmd inp = do
    let (f:args) = split " " cmd
    (out,_err,_) <- lift $ popen f args (Just inp)
    return (chomp "\n" out)

------------------------------------------------------------------------

-- | Set the cmd buffer, and draw message at bottom of screen
msgE :: String -> Action
msgE s = do modifyEditor_ $ \e -> do
              UI.setCmdLine (ui e) s
              -- also show in the messages buffer, so we don't loose any message
              let [b] = findBufferWithName e "*messages*"
              moveTo b =<< sizeB b
              insertN b (s ++ "\n")
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
-- TODO: use the above data type
bufInfoE :: EditorM BufferFileInfo
bufInfoE = withWindow $ \w b -> do
    s <- sizeB b
    p <- pointB b
    m <- isUnchangedB b
    l <- curLn b
    let bufInfo = BufferFileInfo { bufInfoFileName = nameB b
				 , bufInfoSize     = s
				 , bufInfoLineNo   = l
				 , bufInfoColNo    = 1 + (snd $ cursor w)
				 , bufInfoCharNo   = p
				 , bufInfoPercent  = getPercent p s 
				 , bufInfoModified = not m
				 }
    return bufInfo

-- | Maybe a file associated with this buffer
fileNameE :: EditorM (Maybe FilePath)
fileNameE = withBuffer getfileB

-- | Name of this buffer
bufNameE :: EditorM String
bufNameE = withBuffer $ \b -> return (nameB b)

-- | A character to fill blank lines in windows with. Usually '~' for
-- vi-like editors, ' ' for everything else
setWindowFillE :: Char -> Action
setWindowFillE c = modifyEditor_ $ \e -> return $ e { windowfill = c }

-- | Sets the window style.
setWindowStyleE :: Style.UIStyle -> Action
setWindowStyleE sty = modifyEditor_ $ \e -> return $ e { uistyle = sty }


-- | Attach the next buffer in the buffer list
-- to the current window.
nextBufW :: Action
nextBufW = Editor.nextBuffer >>= switchToBufferE

-- | edit the previous buffer in the buffer list
prevBufW :: Action
prevBufW = Editor.prevBuffer >>= switchToBufferE

-- | If file exists, read contents of file into a new buffer, otherwise
-- creating a new empty buffer. Replace the current window with a new
-- window onto the new buffer.
--
-- Need to clean up semantics for when buffers exist, and how to attach
-- windows to buffers.
--
fnewE  :: FilePath -> Action
fnewE f = do
    e  <- lift $ doesFileExist f
    b  <- if e then hNewBuffer f else stringToNewBuffer f []
    lift $ setfileB b f        -- and associate with file f
    switchToBufferE b

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
newBufferE :: String -> String -> EditorM FBuffer
newBufferE f s = do
    b <- stringToNewBuffer f s
    switchToBufferE b
    lift $ logPutStrLn "newBufferE ended"
    return b

-- | Attach the specified buffer to the current window
switchToBufferE :: FBuffer -> Action
switchToBufferE b = getWindow >>= UI.setWindowBuffer b

-- | Attach the specified buffer to some other window than the current one
switchToBufferOtherWindowE :: FBuffer -> Action
switchToBufferOtherWindowE b = shiftOtherWindow >> switchToBufferE b

-- | Find buffer with given name. Raise exception if not found.
getBufferWithName :: String -> EditorM FBuffer
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
    do b <- stringToNewBuffer prompt []
       lift $ setBufferKeymap b kmMod
       w <- UI.newWindow True b
       UI.setWindow w
       initialAction

-- | Write current buffer to disk, if this buffer is associated with a file
fwriteE :: Action
fwriteE = withBuffer $ \b -> do
        mf <- getfileB b
        case mf of
                Nothing -> error "buffer not associated with a file"
                Just f  -> hPutB b f

-- | Write current buffer to disk as @f@. If this buffer doesn't
-- currently have a file associated with it, the file is set to @f@
fwriteToE :: String -> Action
fwriteToE f = withBuffer $ \b -> do
        hPutB b f
        mf <- getfileB b
        case mf of
                Nothing -> setfileB b f
                Just _  -> return ()

-- | Write all open buffers
fwriteAllE :: Action
fwriteAllE = undefined

-- | Make a backup copy of file
backupE :: FilePath -> Action
backupE = undefined

-- | Return a list of all buffers, and their indicies
listBuffersE :: EditorM [(String,Int)]
listBuffersE = do
        bs  <- getBuffers
        return $ zip (map nameB bs) [0..]

-- | Release resources associated with buffer, close any windows open
-- onto buffer.
closeBufferE :: String -> Action
closeBufferE f = killBufferAndWindows f

------------------------------------------------------------------------

-- | Is the current buffer unmodifed? (currently buggy, we need
-- bounaries in the undo list)
isUnchangedE :: EditorM Bool
isUnchangedE = withBuffer isUnchangedB

-- | Set the current buffer to be unmodifed
setUnchangedE :: Action
setUnchangedE = undefined

-- | Set the current buffer's highlighting kind
setSynE :: String -> Action
setSynE sy = withBuffer (\b -> setSyntaxB b sy)

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
setWinE = UI.setWindow

-- | Split the current window, opening a second window onto this buffer.
-- Windows smaller than 3 lines cannot be split.
splitE :: Action
splitE = do
    canSplit <- UI.hasRoomForExtraWindow
    if canSplit 
      then splitWindow
      else errorE "Not enough room to split"

-- | Enlarge the current window
enlargeWinE :: Action
enlargeWinE = getWindow >>= UI.enlargeWindow

-- | Shrink the current window
shrinkWinE :: Action
shrinkWinE = getWindow >>= UI.shrinkWindow

-- | Close the current window.
-- If this is the last window open, quit the program. TODO this
-- behaviour may be undesirable
closeE :: Action
closeE = do
        deleteThisWindow
        i <- sizeWindows
        when (i == 0) quitE

-- | Make the current window the only window on the screen
closeOtherE :: Action
closeOtherE = do
        this   <- getWindow -- current window
        others <- modifyEditor $ \e -> do
                        let ws = getWindows e
                        return (e, (filter (/= this) (map Just ws)))
        mapM_ UI.deleteWindow others

------------------------------------------------------------------------
--
-- Map a char function over a range of the buffer.
--
-- Fold over a range is probably useful too..
--
mapRangeE :: Int -> Int -> (Char -> Char) -> Action
mapRangeE from to fn
    | from < 0  = nopE
    | otherwise = do
        withBuffer $ \b -> do
            eof <- sizeB b
            when (to < eof) $ do
                let loop j | j <= 0    = return ()
                           | otherwise = do
                                readB b >>= return . fn >>= writeB b
                                rightB b
                                loop (j-1)
                loop (max 0 (to - from))
            moveTo b from


reconfigE :: Action
reconfigE = reloadE >> runConfig

runConfig :: Action
runConfig = do
  loaded <- withKernel $ \kernel -> do
              let cfgMod = mkModuleName kernel "YiConfig"
              isLoaded kernel cfgMod
  if loaded 
   then do result <- withKernel $ \kernel -> compileExpr kernel "YiConfig.yiMain :: Yi.Yi.EditorM ()"
           case result of
             Nothing -> errorE "Could not run YiConfig.yiMain :: Yi.Yi.EditorM ()"
             Just x -> (unsafeCoerce# x)
   else errorE "YiConfig not loaded"

loadE :: String -> EditorM [String]
loadE modul = do
  modifyEditor_ $ \e -> return e { editorModules = nub (editorModules e ++ [modul]) }
  reloadE

unloadE :: String -> EditorM [String]
unloadE modul = do
  modifyEditor_ $ \e -> return e { editorModules = delete modul (editorModules e) }
  reloadE

getNamesInScopeE :: EditorM [String]
getNamesInScopeE = do
  withKernel $ \k -> do
      names <- getRdrNamesInScope k
      return (map (nameToString k) names)

savingExcursion :: FBuffer -> IO a -> IO a
savingExcursion b f = do
    m <- getMarkB b Nothing
    res <- f
    moveTo b =<< getMarkPointB b m
    return res


ghcErrorReporter :: IORef Editor -> GHC.Severity -> SrcLoc.SrcSpan -> Outputable.PprStyle -> ErrUtils.Message -> IO () 
ghcErrorReporter editor severity srcSpan pprStyle message = 
    -- the following is written in very bad style.
    flip runReaderT editor $ do
      e <- readEditor id
      let [b] = findBufferWithName e "*console*"
      lift $ savingExcursion b $ do 
        moveTo b =<< getMarkPointB b =<< getMarkB b (Just "errorInsert")
        insertN b msg
        insertN b "\n"
    where msg = case severity of
                  GHC.SevInfo -> show (message pprStyle)
                  GHC.SevFatal -> show (message pprStyle)
                  _ -> show ((ErrUtils.mkLocMessage srcSpan message) pprStyle)


