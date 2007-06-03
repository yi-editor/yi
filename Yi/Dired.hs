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
import Yi.Editor
import Yi.Keymap.Emacs
import Yi.Region
import Yi.Style

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

diredLoadNewDirE :: FilePath -> EditorM ()
diredLoadNewDirE dir = do
    setSynE "none" -- Colours for Dired come from overlays not syntax highlighting
    insertNE $ dir ++ ":\n"
    p <- getPointE
    withBuffer (addOverlayB 0 (p-2) headStyle)
    files <- liftIO $ getDirectoryContents dir
    let filteredFiles = filter (not . diredOmitFile) files
    linesToShow <- liftIO $ mapM (lineForFile dir) filteredFiles
    mapM_ insertDiredLine linesToShow
    topE >> downE
    where
    lineForFile :: String -> String -> IO (String, String, Bool)
    lineForFile d f = do
                        let fp = (d </> f)
                        isdir <- doesDirectoryExist fp
                        isfile <- doesFileExist fp
                        if (isdir || isfile) then lineForFilePath fp isdir else return ("---------- na na", "na", False)
    lineForFilePath :: FilePath -> Bool -> IO (String, String, Bool)
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
                            fn = takeFileName fp
                        return $ (printf "  %s%s %4d %s %s%8d %s" prefix fmodeStr numLinks ownerStr groupStr sz modTimeStr, fn, isdir)
    shortCalendarTimeToString = formatCalendarTime defaultTimeLocale "%b %d %H:%M"
    insertDiredLine :: (String, String, Bool) -> EditorM ()
    insertDiredLine (pre, fn, isdir) = do
        insertNE $ (printf "%s %s\n" pre fn)
        p <- getPointE
        let p1 = p - length fn - 1
            p2 = p - 1
        when isdir $ withBuffer (addOverlayB p1 p2 dirStyle)

    dirStyle = Style cyan black
    headStyle = Style yellow black

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
    let sel = dir </> (fileFromLine rl)
    msgE sel
    isdir <- liftIO $ doesDirectoryExist sel
    if isdir then diredDirE sel >> return () else fnewE sel

diredUpDirE :: EditorM ()
diredUpDirE = do
    (Just dir) <- withBuffer getfileB
    diredDirE $ takeDirectory dir

diredRefreshE :: EditorM ()
diredRefreshE = do
    -- FIXME - this loses all marks...
    -- This will be solved in the future by having an underlying data
    -- structure containing all the directory state.
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
    msgE $ "Creating "++newdir++"..."
    liftIO $ createDirectoryIfMissing True newdir
    diredRefreshE

