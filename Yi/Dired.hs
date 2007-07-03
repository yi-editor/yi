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

import Yi.Yi
import Yi.Keymap.Emacs

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
             ("p", write $ lineUp),
             ("n", write $ lineDown),
             ("b", write $ leftB),
             ("f", write $ rightB),
             ("m", write $ diredMark),
             ("d", write $ diredMarkDel),
             ("g", write $ diredRefreshE),
             ("^", write $ diredUpDirE),
             ("+", write $ diredCreateDirE),
             ("RET", write $ diredLoadE),
             ("SPC", write $ lineDown),
             ("BACKSP", write $ diredUnmark)
                       ] keymap)

diredE :: YiM ()
diredE = do
    msgE "Dired..."
    dir <- liftIO getCurrentDirectory
    fnewE dir

diredDirE :: FilePath -> YiM ()
diredDirE dir = diredDirBufferE dir >> return ()

diredDirBufferE :: FilePath -> YiM FBuffer
diredDirBufferE dir = do
                b <- withEditor $ stringToNewBuffer ("dired-"++dir) ""
                withGivenBuffer b (setfileB dir) -- associate the buffer with the dir
                switchToBufferE b
                diredLoadNewDirE dir
                setBufferKeymap b (const diredKeymap)
                return b

diredRefreshE :: YiM ()
diredRefreshE = do
    -- Clear buffer
    withBuffer $ do end <- sizeB
                    deleteRegionB (mkRegion 0 end)
    -- Write Header
    Just dir <- withBuffer getfileB
    withBuffer $ insertN $ dir ++ ":\n"
    p <- withBuffer pointB
    withBuffer $ (addOverlayB 0 (p-2) headStyle)
    -- Scan directory
    di <- lift $ diredScanDir dir
    let ds = DiredState dir M.empty di []
    withBuffer $ setDynamicB ds
    -- Display results
    lines <- linesToDisplay
    let (strss, stys, strs) = unzip3 lines
        strss' = transpose $ map doPadding $ transpose $ strss
    ptsList <- mapM insertDiredLine $ zip3 strss' stys strs
    withBuffer $ do setDynamicB ds{diredFilePoints=ptsList}
                    moveTo p
    return ()
    where
    headStyle = Style yellow black
    doPadding :: [DRStrings] -> [String]
    doPadding drs = map (pad ((maximum . map drlength) drs)) drs

    pad n (DRPerms s) = s
    pad n (DRLinks s) = (replicate (max 0 (n - length s)) ' ') ++ s
    pad n (DROwners s) = s ++ (replicate (max 0 (n - length s)) ' ') ++ " "
    pad n (DRGroups s) = s ++ (replicate (max 0 (n - length s)) ' ')
    pad n (DRSizes s) = (replicate (max 0 (n - length s)) ' ') ++ s
    pad n (DRDates s) = (replicate (max 0 (n - length s)) ' ') ++ s
    pad n (DRFiles s) = s       -- Don't right-justify the filename

    drlength = length . undrs

-- | Returns a tuple containing the textual region (the end of) which is used for 'click' detection
--   and the FilePath of the file represented by that textual region
insertDiredLine :: ([String], Style, String) -> YiM (Point, Point, FilePath)
insertDiredLine (fields, sty, filenm) = do
    withBuffer $ insertN $ (concat $ intersperse " " fields) ++ "\n"
    p <- withBuffer pointB
    let p1 = p - length (last fields) - 1
        p2 = p - 1
    when (sty /= defaultStyle) $ withBuffer (addOverlayB p1 p2 sty)
    return (p1, p2, filenm)

data DRStrings = DRPerms {undrs :: String}
               | DRLinks {undrs :: String}
               | DROwners {undrs :: String}
               | DRGroups {undrs :: String}
               | DRSizes {undrs :: String}
               | DRDates {undrs :: String}
               | DRFiles {undrs :: String}

-- | Return a List of (prefix, fullDisplayNameIncludingSourceAndDestOfLink, style, filename)
linesToDisplay :: YiM ([([DRStrings], Style, String)])
linesToDisplay = do
    (DiredState _ _ des _) <- withBuffer getDynamicB
    return $ map (uncurry lineToDisplay) (M.assocs des)
    where
    lineToDisplay k (DiredFile v)      = (l " -" v ++ [DRFiles k], defaultStyle, k)
    lineToDisplay k (DiredDir v)       = (l " d" v ++ [DRFiles k], Style purple black, k)
    lineToDisplay k (DiredSymLink v s) = (l " l" v ++ [DRFiles $ k ++ " -> " ++ s], Style cyan black, k)
    lineToDisplay k DiredNoInfo        = ([DRFiles $ k ++ " : Not a file/dir/symlink"], defaultStyle, k)

    l pre v = [DRPerms $ pre ++ permString v,
               DRLinks $ printf "%4d" (numLinks v),
               DROwners $ owner v,
               DRGroups $ grp v,
               DRSizes $ printf "%8d" (sizeInBytes v),
               DRDates $ modificationTimeString v]

-- | Write the contents of the supplied directory into the current buffer in dired format
diredLoadNewDirE :: FilePath -> YiM ()
diredLoadNewDirE dir = do
    withBuffer $ setSyntaxB "none" -- Colours for Dired come from overlays not syntax highlighting
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

diredMark :: BufferM ()
diredMark = diredMarkWithChar '*' lineDown

diredMarkDel :: BufferM ()
diredMarkDel = diredMarkWithChar 'D' lineDown

diredMarkWithChar :: Char -> BufferM () -> BufferM ()
diredMarkWithChar c mv = do
    p <- pointB
    moveToSol >> insertN [c] >> deleteB
    moveTo p
    mv

diredUnmark :: BufferM ()
diredUnmark = diredMarkWithChar ' ' lineUp

diredLoadE :: YiM ()
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
fileFromPoint :: YiM (FilePath, DiredEntry)
fileFromPoint = do
    p <- withBuffer pointB
    (DiredState _ _ des pl) <- withBuffer getDynamicB
    let (_,_,f) = head $ filter (\(p1,p2,_)->p<=p2) pl
    return (f, des M.! f)

diredUpDirE :: YiM ()
diredUpDirE = do
    (Just dir) <- withBuffer getfileB
    diredDirE $ takeDirectory dir

diredCreateDirE :: YiM ()
diredCreateDirE = do
    withMinibuffer "Create Dir:" return $ \nm -> do
    (Just dir) <- withBuffer getfileB
    let newdir = dir </> nm
    msgE $ "Creating "++newdir++"..."
    liftIO $ createDirectoryIfMissing True newdir
    diredRefreshE

