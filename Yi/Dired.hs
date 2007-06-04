--
-- Copyright (c) 2007 Ben Moseley
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
-- Improve the colouring to show
-- - loaded buffers
-- - .hs files
-- - marked files
-- Fix old mod dates (> 6months) to show year
-- Fix the 'number of links' field to show actual values not just 1...
-- Automatic support for browsing .zip, .gz files etc...

module Yi.Dired (
        diredE
       ,diredDirE
       ,diredDirBufferE
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.List
import qualified Data.Map as M
import Data.Typeable
import System.Directory
import System.FilePath
import System.Locale
import System.Posix.Files
import System.Posix.Types

import System.Posix.User
-- currently used to lookup the usernames for file owners. Any
-- suggestions on how to drop the dependency welcome.

import System.Time
import Text.Printf
import Text.Regex.Posix

import Yi.Buffer
import Yi.Core
import Yi.Dynamic
import Yi.Editor
import Yi.Keymap.Emacs
import Yi.Region
import Yi.Style

data DiredFileInfo = DiredFileInfo {  permString :: String
                                    , numLinks :: Integer
                                    , owner :: String
                                    , grp :: String
                                    , sizeInBytes :: Integer
                                    , modificationTimeString :: String
                                 }
                deriving (Show, Eq, Typeable)

data DiredEntry = DiredFile DiredFileInfo
                | DiredDir DiredFileInfo
                | DiredSymLink DiredFileInfo String
                | DiredNoInfo
                deriving (Show, Eq, Typeable)

data DiredState = DiredState {
                                diredDir :: FilePath -- ^ The full path to the directory being viewed
                                -- FIXME Choose better data structure for Marks...
                              , diredMarks :: M.Map Char [FilePath] -- ^ Map values are just leafnames, not full paths
                              , diredEntries :: M.Map FilePath DiredEntry -- ^ keys are just leafnames, not full paths
                              , diredFilePoints :: [(Point,Point,FilePath)] -- ^ position in the buffer where filename is
                             }
                  deriving (Show, Eq, Typeable)

instance Initializable DiredState where
    initial = DiredState "" M.empty M.empty []

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
    msgE "Dired..."
    dir <- liftIO getCurrentDirectory
    fnewE dir

diredDirE :: FilePath -> EditorM ()
diredDirE dir = diredDirBufferE dir >> return ()

diredDirBufferE :: FilePath -> EditorM FBuffer
diredDirBufferE dir = do
                b <- stringToNewBuffer ("dired-"++dir) ""
                withGivenBuffer b (setfileB dir) -- associate the buffer with the dir
                switchToBufferE b
                diredLoadNewDirE dir
                lift $ setBufferKeymap b (const diredKeymap)
                return b

diredRefreshE :: EditorM ()
diredRefreshE = do
    -- Clear buffer
    end <- withBuffer sizeB
    deleteRegionE (mkRegion 0 end)
    topE
    -- Write Header
    (Just dir) <- withBuffer getfileB
    insertNE $ dir ++ ":\n"
    p <- getPointE
    withBuffer (addOverlayB 0 (p-2) headStyle)
    -- Scan directory
    di <- lift $ diredScanDir dir
    let ds = DiredState dir M.empty di []
    withBuffer $ setDynamicB ds
    -- Display results
    lines <- linesToDisplay
    ptsList <- mapM insertDiredLine lines
    withBuffer $ setDynamicB ds{diredFilePoints=ptsList}
    gotoPointE p
    return ()
    where
    headStyle = Style yellow black

insertDiredLine :: (String, String, Style, String) -> EditorM (Point, Point, FilePath)
insertDiredLine (pre, displaynm, sty, filenm) = do
    insertNE $ (printf "%s %s\n" pre displaynm)
    p <- getPointE
    let p1 = p - length displaynm - 1
        p2 = p - 1
    when (sty /= defaultStyle) $ withBuffer (addOverlayB p1 p2 sty)
    return (p1, p2, filenm)

-- | Return a List of (prefix, fullDisplayNameIncludingSourceAndDestOfLink, style, filename)
linesToDisplay :: EditorM ([(String, String, Style, String)])
linesToDisplay = do
    (DiredState _ _ des _) <- withBuffer getDynamicB
    return $ map (uncurry lineToDisplay) (M.assocs des)
    where
    lineToDisplay k (DiredFile v) = (" -" ++ l v, k, defaultStyle, k)
    lineToDisplay k (DiredDir v) = (" d" ++ l v, k, Style purple black, k)
    lineToDisplay k (DiredSymLink v s) = (" l" ++ l v, k ++ " -> " ++ s, Style cyan black, k)
    lineToDisplay k DiredNoInfo = ("", k++" : Not a file/dir/symlink", defaultStyle, k)

    l v = printf "%s %4d %s  %s%8d %s" (permString v) (numLinks v) (owner v) (grp v) (sizeInBytes v) (modificationTimeString v)

-- | Write the contents of the supplied directory into the current buffer in dired format
diredLoadNewDirE :: FilePath -> EditorM ()
diredLoadNewDirE dir = do
    setSynE "none" -- Colours for Dired come from overlays not syntax highlighting
    diredRefreshE

-- | Return dired entries for the contents of the supplied directory
diredScanDir :: FilePath -> IO (M.Map FilePath DiredEntry)
diredScanDir dir = do
    files <- getDirectoryContents dir
    let filteredFiles = filter (not . diredOmitFile) files
    foldM (lineForFile dir) M.empty filteredFiles
    where
    lineForFile :: String -> M.Map FilePath DiredEntry -> String -> IO (M.Map FilePath DiredEntry)
    lineForFile d m f = do
                        let fp = (d </> f)
                        fileStatus <- getSymbolicLinkStatus fp
                        dfi <- lineForFilePath fp fileStatus
                        let islink = isSymbolicLink fileStatus
                        linkTarget <- if islink then readSymbolicLink fp else return ""
                        let isdir = isDirectory fileStatus
                            isfile = isRegularFile fileStatus
                            de = if isdir then (DiredDir dfi) else
                                   if isfile then (DiredFile dfi) else
                                     if islink then (DiredSymLink dfi linkTarget) else
                                       DiredNoInfo
                        return (M.insert f de m)

    lineForFilePath :: FilePath -> FileStatus -> IO DiredFileInfo
    lineForFilePath fp fileStatus = do
                        modTimeStr <- return . shortCalendarTimeToString =<< toCalendarTime (TOD (floor $ toRational $ modificationTime fileStatus) 0)
                        let uid = fileOwner fileStatus
                            gid = fileGroup fileStatus
                        filenm <- if (isSymbolicLink fileStatus) then
                                  return . ((++) (takeFileName fp ++ " -> ")) =<< readSymbolicLink fp else
                                  return $ takeFileName fp
                        ownerEntry <- catch (getUserEntryForID uid) (\_ -> getAllUserEntries >>= return . scanForUid uid)
                        groupEntry <- catch (getGroupEntryForID gid) (\_ -> getAllGroupEntries >>= return . scanForGid gid)
                        let fmodeStr = (modeString . fileMode) fileStatus
                            sz = toInteger $ fileSize fileStatus
                            ownerStr = userName ownerEntry
                            groupStr = groupName groupEntry
                            numLinks = toInteger $ linkCount fileStatus
                        return $ DiredFileInfo {permString = fmodeStr, numLinks = numLinks, owner = ownerStr, grp = groupStr, sizeInBytes = sz, modificationTimeString = modTimeStr}

    shortCalendarTimeToString :: CalendarTime -> String
    shortCalendarTimeToString = formatCalendarTime defaultTimeLocale "%b %d %H:%M"



-- | Needed on Mac OS X 10.4
scanForUid :: UserID -> [UserEntry] -> UserEntry
scanForUid uid entries = maybe (UserEntry "?" "" uid 0 "" "" "") id (find ((== uid) . userID) entries)

-- | Needed on Mac OS X 10.4
scanForGid :: GroupID -> [GroupEntry] -> GroupEntry
scanForGid gid entries = maybe (GroupEntry "?" "" gid []) id (find ((== gid) . groupID) entries)

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
    (Just dir) <- withBuffer getfileB
    (fn, de) <- fileFromPoint
    let sel = dir </> fn
    case de of
            (DiredFile dfi) -> do
                               exists <- liftIO $ doesFileExist sel
                               if exists then fnewE sel else msgE $ sel ++ " no longer exists"
            (DiredDir dfi) -> do
                              exists <- liftIO $ doesDirectoryExist sel
                              if exists then diredDirE sel else msgE $ sel ++ " no longer exists"
            (DiredSymLink dfi dest) -> do
                                       let target = if isAbsolute dest then dest else dir </> dest
                                       existsFile <- liftIO $ doesFileExist target
                                       existsDir <- liftIO $ doesDirectoryExist target
                                       msgE $ "Following link:"++target
                                       if existsFile then fnewE target else
                                          if existsDir then diredDirE target else
                                             msgE $ target ++ " does not exist"
            DiredNoInfo -> msgE $ "No File Info for:"++sel

-- | Extract the filename at point. NB this may fail if the buffer has been edited. Maybe use Markers instead.
fileFromPoint :: EditorM (FilePath, DiredEntry)
fileFromPoint = do
    p <- getPointE
    (DiredState _ _ des pl) <- withBuffer getDynamicB
    let (_,_,f) = head $ filter (\(p1,p2,_)->p<=p2) pl
    return (f, des M.! f)

diredUpDirE :: EditorM ()
diredUpDirE = do
    (Just dir) <- withBuffer getfileB
    diredDirE $ takeDirectory dir

diredCreateDirE :: EditorM ()
diredCreateDirE = do
    withMinibuffer "Create Dir:" return $ \nm -> do
    (Just dir) <- withBuffer getfileB
    let newdir = dir </> nm
    msgE $ "Creating "++newdir++"..."
    liftIO $ createDirectoryIfMissing True newdir
    diredRefreshE

