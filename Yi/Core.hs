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

        Direction (..),
        -- * Construction and destruction
        startE,         -- :: a -> Editor.Config -> Int -> Maybe [FilePath] -> IO ()
        emptyE,         -- :: IO ()
        runE,           -- :: IO ()
        quitE,          -- :: Action
        rebootE,        -- :: Action
        reloadE,        -- :: Action
        refreshE,       -- :: Action
        suspendE,       -- :: Action

        -- * Global editor actions
        BufferFileInfo ( .. ), 

        nopE,           -- :: Action
        msgE,           -- :: String -> Action
        errorE,         -- :: String -> Action
        msgClrE,        -- :: Action
        bufInfoE,       -- :: IO BufferFileInfo
        fileNameE,      -- :: IO (Maybe FilePath)
        bufNameE,       -- :: IO String
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

        -- * Switching to the command line
        cmdlineFocusE,  -- :: Action
        cmdlineUnFocusE,-- :: Action


        -- * File-based actions
        fnewE,          -- :: FilePath -> Action
        fwriteE,        -- :: Action
        fwriteAllE,     -- :: Action
        fwriteToE,      -- :: String -> Action
        backupE,        -- :: FilePath -> Action

        -- * Buffer only stuff
        nextBufW,       -- :: Action
        prevBufW,       -- :: Action
        newBufferE,     -- :: String -> String -> Action
        listBuffersE,   -- :: Action
        closeBufferE,   -- :: String -> Action
        isUnchangedE,   -- :: IO Bool
        setUnchangedE,  -- :: Action
        setSynE,        -- :: String -> Action

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
        getPointE,      -- :: IO Int
	getLineAndColE, -- :: IO (Int, Int)

        atSolE,         -- :: IO Bool
        atEolE,         -- :: IO Bool
        atSofE,         -- :: IO Bool
        atEofE,         -- :: IO Bool

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
        deleteRegionE,  -- :: (Int,Int) -> Action
        writeE,         -- :: Char -> Action
        undoE,          -- :: Action
        redoE,          -- :: Action

        -- * Read parts of the buffer
        readE,          -- :: IO Char
        readRegionE,    -- :: (Int,Int) -> IO String
        readLnE,        -- :: IO String
        readNM,         -- :: Int -> Int -> IO String
        readRestOfLnE,  -- :: IO String
        readAllE,       -- :: IO String

        swapE,          -- :: Action

        -- * Basic registers
        setRegE,        -- :: String -> Action
        getRegE,        -- :: IO String

        -- * Marks
        setMarkE,
        getMarkE,
        unsetMarkE,
        exchangePointAndMarkE,

        -- * Dynamically extensible state
        getDynamic,
        setDynamic,

        -- * higher level ops
        mapRangeE,              -- :: Int -> Int -> (Buffer' -> Action) -> Action

        -- * Interacting with external commands
        pipeE,                   -- :: String -> IO String
 
        -- * Minibuffer
        spawnMinibufferE,

        catchJustE
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
import qualified Yi.Editor as Editor
import qualified Yi.Style as Style
import qualified Yi.UI as UI

import Data.Maybe
import Data.Dynamic
import Data.List
import Data.Map as M        ( lookup, insert )
import Data.IORef

import System.Directory     ( doesFileExist, getHomeDirectory )

import Control.Monad.Reader
import Control.Exception
import Control.Concurrent 
import Control.Concurrent.Chan

import System.Exit	( exitWith, ExitCode(..) )

import qualified GHC
import qualified Packages
import qualified DynFlags
import qualified ObjLink

import GHC.Exts ( unsafeCoerce# )


-- | A 'Direction' is either left or right.
data Direction = GoLeft | GoRight

-- ---------------------------------------------------------------------
-- | Start up the editor, setting any state with the user preferences
-- and file names passed in, and turning on the UI
--
startE :: Maybe Editor -> [Action] -> IO ()
startE st commandLineActions = do
    logPutStrLn "Starting Core"

    -- restore the old state
    newSt <- newIORef $ maybe emptyEditor id st
    flip runReaderT newSt $ do 
      -- start GHC session
      initializeI

      UI.start      

      -- run user configuration
      cfg <- withSession getConfig
      cfg

      lift $ logPutStrLn "Initializing First Buffer"

      -- emacs-like behaviour
      newBufferE "*scratch*" $
                   "-- This buffer is for notes you don't want to save, and for haskell evaluation\n" ++
                   "-- If you want to create a file, open that file,\n" ++
                   "-- then enter the text in that file's own buffer.\n\n"

      when (isNothing st) $ do -- process options if booting for the first time
        sequence_ commandLineActions

    logPutStrLn "Starting event handler"
    let
          eventLoop :: IO ()
          eventLoop = do
              ch <- liftM input $ readIORef newSt 
              let run = mapM_ dispatch =<< getChanContents ch
              repeatM_ $ (handle handler run >> logPutStrLn "Dispatcher execution ended")
                       
              where
                handler e = flip runReaderT newSt $ errorE (show e)
                dispatch action = flip runReaderT newSt $ withBuffer $ \b -> writeChan (bufferInput b) action
      
    t <- forkIO eventLoop 
    modifyIORef newSt $ \e -> e { threads = t : threads e }

    UI.main newSt -- transfer control to UI: GTK must run in the main thread, or else it's not happy.
                





-- ---------------------------------------------------------------------
-- | @emptyE@ and @runE@ are for automated testing purposes. The former
-- initialises the editor state, and returns, enabling the tester to
-- invoke various core commands from a program. The latter runs the
-- editor with string as input, and is useful for testing keymaps.
-- emptyE takes no input -- the ui blocks on stdin.
--
emptyE :: EditorM ()
emptyE = modifyEditor_ $ const $ return $ emptyEditor
    -- need to get it into a state where we can just run core commands
    -- to make it reinitialisable, lets blank out the state
    -- make up an abitrary screen size

    -- no ui thread
    -- no eventloop

-- for testing keymaps:
runE :: String -> EditorM ()
runE = undefined

-- ---------------------------------------------------------------------
-- Meta operations

-- | Quit.
quitE :: Action
quitE = withUI UI.end

-- | Reboot (!). Reboot the entire editor, reloading the Yi core.
rebootE :: Action
rebootE = do
    cmdlineUnFocusE     -- return focus to buffer (seems natural)
    e  <- readEditor id
    fn <- readEditor reboot
    Editor.shutdown
    lift $ fn (Just e)

-- | Recompile and reload the user's config files
reloadE :: Action
reloadE = error "Reload not implemented yet"

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
            moveTo b (n-1)

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
deleteRegionE :: Region -> EditorM ()
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
setMarkE pos = withBuffer $ \b -> setMarkB b pos

-- | Unset the current buffer mark so that there is no selection
unsetMarkE :: Action
unsetMarkE = withBuffer $ \b -> unsetMarkB b

-- | Get the current buffer mark
getMarkE :: EditorM Int
getMarkE = withBuffer getMarkB

-- | Exchange point & mark.
-- Maybe this is better put in Emacs\/Mg common file
exchangePointAndMarkE :: Action
exchangePointAndMarkE = do m <- getMarkE
                           p <- getPointE
                           setMarkE p
                           gotoPointE m

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
              return e

-- | Set the cmd buffer, and draw a pretty error message
errorE :: String -> Action
errorE s = do msgE s
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


-- | Close the current window, attach the next buffer in the buffer list
-- to a new window, and open it up
nextBufW :: Action
nextBufW = do
    b <- Editor.nextBuffer
    getWindow >>= UI.setWindowBuffer b

-- | edit the previous buffer in the buffer list
prevBufW :: Action
prevBufW = do
    b <- Editor.prevBuffer
    getWindow >>= UI.setWindowBuffer b

-- | If file exists, read contents of file into a new buffer, otherwise
-- creating a new empty buffer. Replace the current window with a new
-- window onto the new buffer.
--
-- Need to clean up semantics for when buffers exist, and how to attach
-- windows to buffers.
--
fnewE  :: FilePath -> Action
fnewE f = do
    km <- readEditor defaultKeymap
    e  <- lift $ doesFileExist f
    b  <- if e then hNewBuffer f else stringToNewBuffer f [] km
    lift $ setfileB b f        -- and associate with file f
    getWindow >>= UI.setWindowBuffer b

-- | Like fnewE, create a new buffer filled with the String @s@,
-- Open up a new window onto this buffer. Doesn't associate any file
-- with the buffer (unlike fnewE) and so is good for popup internal
-- buffers (like scratch)
newBufferE :: String -> String -> Action
newBufferE f s = do
    km <- readEditor defaultKeymap
    b <- stringToNewBuffer f s km
    canSplit <- UI.hasRoomForExtraWindow
    if canSplit 
      then UI.newWindow b >>= UI.setWindow
      else return ()
    getWindow >>= UI.setWindowBuffer b
    lift $ logPutStrLn "newBufferE ended"

-- TODO:
-- add prompt
-- resize
-- hide modeline (?)

spawnMinibufferE :: String -> Keymap -> Action
spawnMinibufferE prompt km =
    do b <- stringToNewBuffer ("Minibuffer: " ++ prompt) [] km 
       w <- UI.newWindow b
       UI.setWindow w


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
  if not (canSplit)
        then errorE "Not enough room to split"
        else do
              mw <- getWindow
              case mw of
                Nothing -> nopE
                Just w  -> do b <- getBufferWith (bufkey w)
                              w' <- UI.newWindow b
                              UI.setWindow w'

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

-- | Shift focus to command line window
cmdlineFocusE :: Action
cmdlineFocusE = modifyEditor_ $ \e -> return e { cmdlinefocus = True }

-- | Return focus to normal window
cmdlineUnFocusE :: Action
cmdlineUnFocusE = modifyEditor_ $ \e -> return e { cmdlinefocus = False }

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


------------------
-- GHCi embedding

-- the path of our GHC installation
path :: FilePath
path = GHC_LIBDIR -- See Setup.hs

objectsDir :: FilePath
#ifdef YI_FLAVOUR_GTK
objectsDir = "dist/build/yi-gtk/yi-gtk-tmp/"
#else
objectsDir = "dist/build/yi/yi-tmp/"
#endif


initializeI :: EditorM ()
initializeI = modifyEditor_ $ \e -> GHC.defaultErrorHandler DynFlags.defaultDynFlags $ do
  session <- GHC.newSession GHC.Interactive (Just path)
  dflags1 <- GHC.getSessionDynFlags session

  -- TODO: currently we require user=developer; 
  -- Yi must be run in the yi repository root directory.
  home <- getHomeDirectory
  logPutStrLn $ "Home = " ++ home
  (dflags1',_otherFlags) <- GHC.parseDynamicFlags dflags1 ["-package ghc", "-fglasgow-exts", "-cpp", 
                                                           "-i" ++ home ++ "/.yi", -- FIXME
  -- DEV MODE flags
                                                           -- "-v",
                                                           "-odir" ++ objectsDir,
                                                           "-hidir" ++ objectsDir,
#ifdef YI_FLAVOUR_GTK
                                                           "-igtk"
#else
                                                           "-ivty"
#endif
                                                          ]
  (dflags2, _packageIds) <- Packages.initPackages dflags1'
  GHC.setSessionDynFlags session dflags2{GHC.hscTarget=GHC.HscInterpreted}
  
  -- DEV MODE
  ObjLink.loadObj $ "./" ++ objectsDir ++ "cbits/YiUtils.o"
  yiTarget <- GHC.guessTarget "Yi.Yi" Nothing
  GHC.addTarget session yiTarget

  -- FIXME: don't do this if no config module is there.
  configTarget <- GHC.guessTarget "YiConfig" Nothing
  GHC.addTarget session configTarget

  result <- GHC.load session GHC.LoadAllTargets
  case result of
    GHC.Failed -> exitWith (ExitFailure (-1))
    _ -> return ()
    
  return e {editorSession = session}

getConfig :: GHC.Session -> IO (EditorM ())
getConfig session = GHC.defaultErrorHandler DynFlags.defaultDynFlags $ do
  cfgModule <- GHC.findModule session (GHC.mkModuleName "YiConfig") Nothing
  GHC.setContext session [] [cfgModule]
  result <- GHC.compileExpr session "yiMain"
  case result of
    Nothing -> error "Could not compile expression"
    Just x -> do let (x' :: EditorM ()) = unsafeCoerce# x
                 return x'

