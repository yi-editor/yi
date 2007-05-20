--
-- A Simple Dired Implementation for Yi
--

-- TODO
-------
-- Support Symlinks
-- Mark operations
-- - rename
-- - delete
-- - search
-- set 'bmode' in buffer - ReadOnly
-- DO SOME DECENT SYNTAX HIGHLIGHTING FOR THE BUFFER - LIKE good 'ls' versions etc...
-- - loaded buffers
-- - .hs files
-- - directories
-- Fix old mod dates (> 6months) to show year
-- Fix the 'number of links' field to show actual values not just 1...
-- Automatic support for browsing .zip, .gz files etc...

module Yi.Dired where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.List
import System.Directory
import System.FilePath
import System.Locale
import System.Posix.Files
import System.Posix.Types
import System.Posix.User
import System.Time
import Text.Printf
import Text.Regex.Posix

import Yi.Buffer
import Yi.Core
import Yi.Editor
import Yi.Keymap.Emacs
import Yi.Region

diredKeymap :: Keymap
diredKeymap = do
    (rebind [
             ("p", write $ upE),
             ("n", write $ downE),
             ("b", write $ leftE),
             ("f", write $ rightE),
             ("m", write $ diredMarkE),
             ("d", write $ diredMarkDelE),
             ("g", write $ diredRefreshE),
             ("^", write $ diredUpDirE),
             ("+", write $ diredCreateDirE),
             ("RET", write $ diredLoadE),
             ("SPC", write $ downE),
             ("BACKSP", write $ diredUnmarkE)
                       ] keymap)

diredE :: EditorM ()
diredE = do
    msgE "Dired...."
    dir <- liftIO getCurrentDirectory
    diredDirE dir

diredDirE :: FilePath -> EditorM ()
diredDirE dir = do
    bufs <- getBuffers
    bufsWithThisFilename <- liftIO $ filterM (\b -> readMVar (file b) >>= return . (==Just dir)) bufs
    case bufsWithThisFilename of
             [] -> do
                   b <- stringToNewBuffer ("dired-"++dir) ""
                   switchToBufferE b
                   lift $ runBuffer b (setfileB dir) -- associate the buffer with the dir
                   diredLoadNewDirE dir
                   liftIO $ setBufferKeymap b (const diredKeymap)
             _ -> switchToBufferE (head bufsWithThisFilename)

diredLoadNewDirE :: FilePath -> EditorM ()
diredLoadNewDirE dir = do
    insertNE $ dir ++ ":\n"
    files <- liftIO $ getDirectoryContents dir
    let filteredFiles = filter (not . diredOmitFile) files
    linesToShow <- liftIO $ mapM (lineForFile dir) filteredFiles
    mapM_ insertNE (intersperse "\n" linesToShow)
    topE >> downE
    where
    lineForFile :: String -> String -> IO String
    lineForFile d f = do
                        let fp = (d </> f)
                        isdir <- doesDirectoryExist fp
                        isfile <- doesFileExist fp
                        if (isdir || isfile) then lineForFilePath fp isdir else return "---------- na na"
    lineForFilePath :: FilePath -> Bool -> IO String
    lineForFilePath fp isdir = do
                        fileStatus <- getFileStatus fp
                        modTimeStr <- (getModificationTime fp >>= toCalendarTime >>= return . shortCalendarTimeToString)
                        let uid = fileOwner fileStatus
                            gid = fileGroup fileStatus
                        ownerEntry <- catch (getUserEntryForID uid) (\_ -> getAllUserEntries >>= return . scanForUid uid)
                        groupEntry <- catch (getGroupEntryForID gid) (\_ -> getAllGroupEntries >>= return . scanForGid gid)
                        let prefix = if isdir then "d" else "-"
                            fmodeStr = (modeString . fileMode) fileStatus
                            sz = toInteger $ fileSize fileStatus
                            numLinks :: Int = 1
                            ownerStr = userName ownerEntry
                            groupStr = groupName groupEntry
                        return $ printf "  %s%s %4d %s %s%8d %s %s" prefix fmodeStr numLinks ownerStr groupStr sz modTimeStr (takeFileName fp)
    shortCalendarTimeToString = formatCalendarTime defaultTimeLocale "%b %d %H:%M"

-- | Needed on Mac OS X 10.4
scanForUid :: UserID -> [UserEntry] -> UserEntry
scanForUid uid entries = maybe (UserEntry "?" "" uid 0 "" "" "") id (find ((== uid) . userID) entries)

-- | Needed on Mac OS X 10.4
scanForGid :: GroupID -> [GroupEntry] -> GroupEntry
scanForGid gid entries = maybe (GroupEntry "?" "" gid []) id (find ((== gid) . groupID) entries)

-- | Extract the filename portion from a text line
-- This is ugly - The number of 'tails' below must match the number of fields in a line
fileFromLine :: String -> String
fileFromLine = unwords . tail . tail . tail . tail . tail . tail . tail . tail . words . stripMark

modeString :: FileMode -> String
modeString fm = ""
                ++ strIfSet "r" ownerReadMode
                ++ strIfSet "w" ownerWriteMode
                ++ strIfSet "x" ownerExecuteMode
                ++ strIfSet "r" groupReadMode
                ++ strIfSet "w" groupWriteMode
                ++ strIfSet "x" groupExecuteMode
                ++ strIfSet "r" otherReadMode
                ++ strIfSet "w" otherWriteMode
                ++ strIfSet "x" otherExecuteMode
    where
    strIfSet s mode = if fm == (fm `unionFileModes` mode) then s else "-"

-- | Remove leading two characters
stripMark :: String -> String
stripMark = tail . tail

-- Default Filter: omit files ending in '~' or '#' and also '.' and '..'.
diredOmitFile :: String -> Bool
diredOmitFile = (=~".*~$|.*#$|^\\.$|^\\..$")

diredMarkE :: EditorM ()
diredMarkE = diredMarkWithChar '*' downE

diredMarkDelE :: EditorM ()
diredMarkDelE = diredMarkWithChar 'D' downE

diredMarkWithChar :: Char -> EditorM () -> EditorM ()
diredMarkWithChar c mv = do
    p <- getPointE
    solE >> insertE c >> deleteE
    gotoPointE p
    mv

diredUnmarkE :: EditorM ()
diredUnmarkE = diredMarkWithChar ' ' upE

diredLoadE :: EditorM ()
diredLoadE = do
    rl <- readLnE
    (Just dir) <- withBuffer getfileB
    let selected = dir </> (fileFromLine rl)
    msgE selected
    isdir <- liftIO $ doesDirectoryExist selected
    if isdir then diredDirE selected else fnewE selected

diredUpDirE :: EditorM ()
diredUpDirE = do
    (Just dir) <- withBuffer getfileB
    diredDirE $ takeDirectory dir

diredRefreshE :: EditorM ()
diredRefreshE = do
    -- FIXME - this loses all marks...
    p <- getPointE
    end <- withBuffer sizeB
    deleteRegionE (mkRegion 0 end)
    topE
    (Just dir) <- withBuffer getfileB
    diredLoadNewDirE dir
    gotoPointE p
    return ()

diredCreateDirE :: EditorM ()
diredCreateDirE = do
    withMinibuffer "Create Dir:" return $ \nm -> do
    (Just dir) <- withBuffer getfileB
    let newdir = dir </> nm
    msgE $ "Creating "++newdir++"...."
    liftIO $ createDirectoryIfMissing True newdir
    diredRefreshE
