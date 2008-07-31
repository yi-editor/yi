{-# LANGUAGE DeriveDataTypeable #-}

-- Copyright (c) 2007, 2008 Ben Moseley


-- | A Simple Dired Implementation for Yi

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
        dired
       ,diredDir
       ,diredDirBuffer
       ,fnewE
    ) where

import qualified Data.ByteString.Lazy as LazyB
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Typeable
import System.Directory
import System.FilePath
import System.Locale
#ifndef mingw32_HOST_OS
import System.Posix.Files
import System.Posix.Types
import System.Posix.User
-- currently used to lookup the usernames for file owners. Any
-- suggestions on how to drop the dependency welcome.
#else
import System.IO
import Control.Exception(bracket, handle)
#endif
import Control.Monad.Reader

import System.Time
import Text.Printf
import Yi.Regex

import Yi.MiniBuffer (withMinibufferGen, noHint)
import Control.Monad
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Config
import Yi.Core
import Yi.Editor
import Yi.Buffer.Region
import Yi.Style
import Yi.Modes (defaultFundamentalMode)
import Yi.Keymap.Keys
import System.FriendlyPath
import Yi.Accessor
import Yi.File
------------------------------------------------
-- | If file exists, read contents of file into a new buffer, otherwise
-- creating a new empty buffer. Replace the current window with a new
-- window onto the new buffer.
--
-- If the file is already open, just switch to the corresponding buffer.
--
-- Need to clean up semantics for when buffers exist, and how to attach
-- windows to buffers.
--
fnewE  :: FilePath -> YiM ()
fnewE f = fnewCanonicalized =<< liftIO (expandTilda f)

fnewCanonicalized :: FilePath -> YiM ()
fnewCanonicalized f = do
    bufs                 <- withEditor getBuffers
        -- The file names associated with the list of current buffers
    bufsWithThisFilename <- liftIO $ filterM assocWithSameFile bufs
        -- The names of the existing buffers
    let currentBufferNames   = map name bufs
        -- The new name for the buffer
        bufferName           = bestNewName desiredBufferName currentBufferNames
    b <- case bufsWithThisFilename of
             [] -> do
                   fe  <- liftIO $ doesFileExist f
                   de  <- liftIO $ doesDirectoryExist f
                   newBufferForPath bufferName fe de
             (h:_)  -> return (bkey h)
    setFileName b f
    tbl <- asks (modeTable . yiConfig)
    AnyMode newMode <- withBufferMode b $ \curmode -> fromMaybe (AnyMode curmode) (runReaderT tbl f)
    -- by default stick with the current mode (eg. stick with dired if
    -- set as such)
    withEditor $ switchToBufferE b
    withBuffer $ setMode newMode
    where
    -- Determines whether or not a given buffer is associated with
    -- the given file. 
    -- If this turns out to be slow when there are a lot of buffers
    -- we could consider canonicalising the path before associating
    -- it with a file. Personally I find this more robust.
    assocWithSameFile :: FBuffer -> IO Bool
    assocWithSameFile fbuffer =
      case file fbuffer of
        Nothing -> return False
        Just f2 -> do filename1 <- canonicalizePath f
                      filename2 <- canonicalizePath f2
                      return $ equalFilePath filename1 filename2

    -- The first argument is the buffer name the second argument is
    -- whether or not the file currently exists and the third argument
    -- is whether or not the file is a directory that exists.
    newBufferForPath :: String -> Bool -> Bool -> YiM BufferRef
    newBufferForPath bufferName True _       =
      fileToNewBuffer bufferName f -- Load the file into a new buffer
    newBufferForPath _bufferName False True  = diredDirBuffer f
    newBufferForPath bufferName False False  =
      withEditor $ stringToNewBuffer bufferName (fromString "")  -- Create new empty buffer


    -- The first argument is the buffer name
    fileToNewBuffer :: String -> FilePath -> YiM BufferRef
    fileToNewBuffer bufferName path = do
      contents <- liftIO $ LazyB.readFile path
      withEditor $ stringToNewBuffer bufferName contents
    desiredBufferName  = takeFileName f

------------------------------------------------


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
                | DiredSocket DiredFileInfo
                | DiredBlockDevice DiredFileInfo
                | DiredCharacterDevice DiredFileInfo
                | DiredNamedPipe DiredFileInfo
                | DiredNoInfo
                deriving (Show, Eq, Typeable)

data DiredState = DiredState
  { diredPath :: FilePath -- ^ The full path to the directory being viewed
    -- FIXME Choose better data structure for Marks...
   , diredMarks :: M.Map Char [FilePath] -- ^ Map values are just leafnames, not full paths
   , diredEntries :: M.Map FilePath DiredEntry -- ^ keys are just leafnames, not full paths
   , diredFilePoints :: [(Point,Point,FilePath)] -- ^ position in the buffer where filename is
  }
  deriving (Show, Eq, Typeable)

instance Initializable DiredState where
    initial = DiredState { diredPath        = ""
                         , diredMarks      = M.empty
                         , diredEntries    = M.empty
                         , diredFilePoints = []
                         }

diredKeymap :: Keymap -> Keymap
diredKeymap = do
    (choice [
             char 'p'                   ?>>! lineUp,
             oneOf [char 'n', char ' ']  >>! lineDown,
             char 'b'                   ?>>! leftB,
             char 'f'                   ?>>! rightB,
             char 'm'                   ?>>! diredMark,
             char 'd'                   ?>>! diredMarkDel,
             char 'g'                   ?>>! diredRefresh,
             char '^'                   ?>>! diredUpDir,
             char '+'                   ?>>! diredCreateDir,     
             char 'q'                   ?>>! (deleteBuffer =<< getBuffer),
             oneOf [ctrl $ char 'm', spec KEnter] >>! diredLoad,
             spec KBS                   ?>>! diredUnmark]
     <||)

dired :: YiM ()
dired = do
    msgEditor "Dired..."
    dir <- liftIO getCurrentDirectory
    fnewE dir

diredDir :: FilePath -> YiM ()
diredDir dir = diredDirBuffer dir >> return ()

diredDirBuffer :: FilePath -> YiM BufferRef
diredDirBuffer dir = do
                b <- withEditor $ stringToNewBuffer ("dired-"++dir) (fromString "")
                setFileName b dir -- associate the buffer with the dir
                withEditor $ switchToBufferE b
                diredLoadNewDir dir
                withBuffer $ setMode diredMode
                return b

diredMode :: Mode ()
diredMode = defaultFundamentalMode {modeKeymap = diredKeymap}
    -- Colours for Dired come from overlays not syntax highlighting

diredRefresh :: YiM ()
diredRefresh = do
    -- Clear buffer
    withBuffer $ do end <- sizeB
                    deleteRegionB (mkRegion 0 end)
    -- Write Header
    Just dir <- withBuffer $ getA fileA
    withBuffer $ insertN $ dir ++ ":\n"
    p <- withBuffer pointB
    withBuffer $ addOverlayB $ mkOverlay UserLayer (mkRegion 0 (p-2)) headStyle
    -- Scan directory
    di <- liftIO $ diredScanDir dir
    let ds = DiredState { diredPath        = dir
                        , diredMarks      = M.empty
                        , diredEntries    = di
                        , diredFilePoints = []
                        }
    withBuffer $ setDynamicB ds
    -- Display results
    dlines <- linesToDisplay
    let (strss, stys, strs) = unzip3 dlines
        strss' = transpose $ map doPadding $ transpose $ strss
    ptsList <- mapM insertDiredLine $ zip3 strss' stys strs
    withBuffer $ do setDynamicB ds{diredFilePoints=ptsList}
                    moveTo p
    return ()
    where
    headStyle = const [Foreground grey]
    doPadding :: [DRStrings] -> [String]
    doPadding drs = map (pad ((maximum . map drlength) drs)) drs

    pad _n (DRPerms s)  = s
    pad n  (DRLinks s)  = (replicate (max 0 (n - length s)) ' ') ++ s
    pad n  (DROwners s) = s ++ (replicate (max 0 (n - length s)) ' ') ++ " "
    pad n  (DRGroups s) = s ++ (replicate (max 0 (n - length s)) ' ')
    pad n  (DRSizes s)  = (replicate (max 0 (n - length s)) ' ') ++ s
    pad n  (DRDates s)  = (replicate (max 0 (n - length s)) ' ') ++ s
    pad _n (DRFiles s)  = s       -- Don't right-justify the filename

    drlength = length . undrs

-- | Returns a tuple containing the textual region (the end of) which is used for 'click' detection
--   and the FilePath of the file represented by that textual region
insertDiredLine :: ([String], StyleName, String) -> YiM (Point, Point, FilePath)
insertDiredLine (fields, sty, filenm) = withBuffer $ do
    insertN $ (concat $ intersperse " " (init fields))
    p1 <- pointB
    insertN (" " ++ last fields)
    p2 <- pointB
    insertN "\n"
    addOverlayB (mkOverlay UserLayer (mkRegion p1 p2) sty) >> return ()
    return (p1, p2, filenm)

data DRStrings = DRPerms {undrs :: String}
               | DRLinks {undrs :: String}
               | DROwners {undrs :: String}
               | DRGroups {undrs :: String}
               | DRSizes {undrs :: String}
               | DRDates {undrs :: String}
               | DRFiles {undrs :: String}

-- | Return a List of (prefix, fullDisplayNameIncludingSourceAndDestOfLink, style, filename)
linesToDisplay :: YiM ([([DRStrings], StyleName, String)])
linesToDisplay = do
    dState <- withBuffer getDynamicB
    return $ map (uncurry lineToDisplay) (M.assocs $ diredEntries dState)
    where
    lineToDisplay k (DiredFile v)      = (l " -" v ++ [DRFiles k], defaultStyle, k)
    lineToDisplay k (DiredDir v)       = (l " d" v ++ [DRFiles k], const [Foreground blue], k)
    lineToDisplay k (DiredSymLink v s) = (l " l" v ++ [DRFiles $ k ++ " -> " ++ s], const [Foreground cyan], k)
    lineToDisplay k (DiredSocket v) = (l " s" v ++ [DRFiles $ k], const [Foreground magenta], k)
    lineToDisplay k (DiredCharacterDevice v) = (l " c" v ++ [DRFiles $ k], const [Foreground yellow], k)
    lineToDisplay k (DiredBlockDevice v) = (l " b" v ++ [DRFiles $ k], const [Foreground yellow], k)
    lineToDisplay k (DiredNamedPipe v) = (l " p" v ++ [DRFiles $ k], const [Foreground brown], k)
    lineToDisplay k DiredNoInfo        = ([DRFiles $ k ++ " : Not a file/dir/symlink"], defaultStyle, k)

    l pre v = [DRPerms $ pre ++ permString v,
               DRLinks $ printf "%4d" (numLinks v),
               DROwners $ owner v,
               DRGroups $ grp v,
               DRSizes $ printf "%8d" (sizeInBytes v),
               DRDates $ modificationTimeString v]

-- | Write the contents of the supplied directory into the current buffer in dired format
-- TODO: trash this.
diredLoadNewDir :: FilePath -> YiM ()
diredLoadNewDir _dir = do
    diredRefresh

-- | Return dired entries for the contents of the supplied directory
diredScanDir :: FilePath -> IO (M.Map FilePath DiredEntry)
diredScanDir dir = do
    files <- getDirectoryContents dir
    let filteredFiles = filter (not . diredOmitFile) files
    foldM (lineForFile dir) M.empty filteredFiles
    where
    lineForFile :: String -> M.Map FilePath DiredEntry -> String -> IO (M.Map FilePath DiredEntry)
    lineForFile d m f = do
#ifndef mingw32_HOST_OS
                        let fp = (d </> f)
                        fileStatus <- getSymbolicLinkStatus fp
                        dfi <- lineForFilePath fp fileStatus
                        let islink = isSymbolicLink fileStatus
                        linkTarget <- if islink then readSymbolicLink fp else return ""
                        let de = if (isDirectory fileStatus) then (DiredDir dfi) else
                                   if (isRegularFile fileStatus) then (DiredFile dfi) else
                                     if islink then (DiredSymLink dfi linkTarget) else
                                       if (isSocket fileStatus) then (DiredSocket dfi) else
                                         if (isCharacterDevice fileStatus) then (DiredCharacterDevice dfi) else
                                           if (isBlockDevice fileStatus) then (DiredBlockDevice dfi) else
                                             if (isNamedPipe fileStatus) then (DiredNamedPipe dfi) else DiredNoInfo
                        return (M.insert f de m)

    lineForFilePath :: FilePath -> FileStatus -> IO DiredFileInfo
    lineForFilePath fp fileStatus = do
                        modTimeStr <- return . shortCalendarTimeToString =<< toCalendarTime (TOD (floor $ toRational $ modificationTime fileStatus) 0)
                        let uid = fileOwner fileStatus
                            gid = fileGroup fileStatus
                        _filenm <- if (isSymbolicLink fileStatus) then
                                  return . ((++) (takeFileName fp ++ " -> ")) =<< readSymbolicLink fp else
                                  return $ takeFileName fp
                        ownerEntry <- catch (getUserEntryForID uid) (\_ -> getAllUserEntries >>= return . scanForUid uid)
                        groupEntry <- catch (getGroupEntryForID gid) (\_ -> getAllGroupEntries >>= return . scanForGid gid)
                        let fmodeStr   = (modeString . fileMode) fileStatus
                            sz = toInteger $ fileSize fileStatus
                            ownerStr   = userName ownerEntry
                            groupStr   = groupName groupEntry
                            numOfLinks = toInteger $ linkCount fileStatus
                        return $ DiredFileInfo { permString = fmodeStr
                                               , numLinks = numOfLinks
                                               , owner = ownerStr
                                               , grp = groupStr
                                               , sizeInBytes = sz
                                               , modificationTimeString = modTimeStr}


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
#else
                       do
                        let fp = (d </> f)
                        dfi <- lineForFilePath fp
                        isfile <- doesFileExist fp
                        isdir  <- doesDirectoryExist fp
                        let de = if isfile then DiredFile dfi else
                                   if isdir then DiredDir dfi else
                                     DiredNoInfo
                        return (M.insert f de m)

    lineForFilePath :: FilePath -> IO DiredFileInfo
    lineForFilePath fp = do
                        fileMode <- getPermissions fp
                        time <- getModificationTime fp >>= toCalendarTime                        
                        let fmodeStr = modeString fileMode
                            modTimeStr = shortCalendarTimeToString time
                        sz <- handle (\_ -> return 0) (bracket (openBinaryFile fp ReadMode) hClose hFileSize)
                        return $ DiredFileInfo { permString = fmodeStr
                                               , numLinks = 0
                                               , owner = ""
                                               , grp = ""
                                               , sizeInBytes = sz
                                               , modificationTimeString = modTimeStr}      

modeString :: Permissions -> String
modeString fm = ""
                ++ strIfSet "r" readable
                ++ strIfSet "w" writable
                ++ strIfSet "x" executable
                ++ strIfSet "r" readable
                ++ strIfSet "w" writable
                ++ strIfSet "x" executable
                ++ strIfSet "r" readable
                ++ strIfSet "w" writable
                ++ strIfSet "x" executable
    where
    strIfSet s mode = if mode fm then s else "-"
#endif

shortCalendarTimeToString :: CalendarTime -> String
shortCalendarTimeToString = formatCalendarTime defaultTimeLocale "%b %d %H:%M"

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
    moveToSol >> insertN [c] >> deleteN 1
    moveTo p
    mv

diredUnmark :: BufferM ()
diredUnmark = diredMarkWithChar ' ' lineUp

diredLoad :: YiM ()
diredLoad = do
    (Just dir) <- withBuffer $ getA fileA
    (fn, de) <- fileFromPoint
    let sel = dir </> fn
    case de of
            (DiredFile _dfi) -> do
                               exists <- liftIO $ doesFileExist sel
                               if exists then fnewE sel else msgEditor $ sel ++ " no longer exists"
            (DiredDir _dfi)  -> do
                              exists <- liftIO $ doesDirectoryExist sel
                              if exists then diredDir sel else msgEditor $ sel ++ " no longer exists"
            (DiredSymLink _dfi dest) -> do
                                       let target = if isAbsolute dest then dest else dir </> dest
                                       existsFile <- liftIO $ doesFileExist target
                                       existsDir <- liftIO $ doesDirectoryExist target
                                       msgEditor $ "Following link:"++target
                                       if existsFile then fnewE target else
                                          if existsDir then diredDir target else
                                             msgEditor $ target ++ " does not exist"
            (DiredSocket _dfi) -> do
                               exists <- liftIO $ doesFileExist sel
                               if exists then msgEditor ("Can't open Socket " ++ sel) else msgEditor $ sel ++ " no longer exists"
            (DiredBlockDevice _dfi) -> do
                               exists <- liftIO $ doesFileExist sel
                               if exists then msgEditor ("Can't open Block Device " ++ sel) else msgEditor $ sel ++ " no longer exists"
            (DiredCharacterDevice _dfi) -> do
                               exists <- liftIO $ doesFileExist sel
                               if exists then msgEditor ("Can't open Character Device " ++ sel) else msgEditor $ sel ++ " no longer exists"
            (DiredNamedPipe _dfi) -> do
                               exists <- liftIO $ doesFileExist sel
                               if exists then msgEditor ("Can't open Pipe " ++ sel) else msgEditor $ sel ++ " no longer exists"
            DiredNoInfo -> msgEditor $ "No File Info for:"++sel

-- | Extract the filename at point. NB this may fail if the buffer has been edited. Maybe use Markers instead.
fileFromPoint :: YiM (FilePath, DiredEntry)
fileFromPoint = do
    p <- withBuffer pointB
    dState <- withBuffer getDynamicB
    let (_,_,f) = head $ filter (\(_,p2,_)->p<=p2) (diredFilePoints dState)
    return (f, M.findWithDefault DiredNoInfo f $ diredEntries dState)

diredUpDir :: YiM ()
diredUpDir = do
    (Just dir) <- withBuffer $ getA fileA
    diredDir $ takeDirectory dir

diredCreateDir :: YiM ()
diredCreateDir = do
    withMinibufferGen "" noHint "Create Dir:" return $ \nm -> do
    (Just dir) <- withBuffer $ getA fileA
    let newdir = dir </> nm
    msgEditor $ "Creating "++newdir++"..."
    liftIO $ createDirectoryIfMissing True newdir
    diredRefresh
