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
-- set buffer to ReadOnly
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

import Prelude (uncurry, catch, realToFrac, sequence)

import Data.List hiding (find, maximum, concat)
import Data.Maybe
import Data.Char (toLower)
import qualified Data.Map as M
import System.Directory
import System.FilePath
import System.Locale
import System.PosixCompat.Files
import System.PosixCompat.Types
import System.PosixCompat.User
import Control.Monad.Reader hiding (mapM)

import Data.Time
import Data.Time.Clock.POSIX
import Text.Printf
import Yi.Regex

import Yi.MiniBuffer (withMinibufferGen, noHint, withMinibuffer)
import Yi.Config
import Yi.Core hiding (sequence)
import Yi.Misc (getFolder, promptFile)
import Yi.Style
import System.FriendlyPath
import qualified Data.Rope as R


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
fnewE f = fnewCanonicalized =<< io (userToCanonPath f)

fnewCanonicalized :: FilePath -> YiM ()
fnewCanonicalized f = do
    bufs <- gets bufferSet
        -- The file names associated with the list of current buffers
    let bufsWithThisFilename = filter assocWithSameFile bufs
        -- The names of the existing buffers
    let fDirectory = takeDirectory f
    de <- io $ doesDirectoryExist f
    fe <- io $ doesFileExist f
    dfe <- io $ doesDirectoryExist fDirectory
    b <- case bufsWithThisFilename of
        (h:_) -> return (bkey h)
        [] -> 
          if de then diredDirBuffer f else do
            b <- if fe then 
                   fileToNewBuffer f 
                 else -- File does not exist
                   do when (not dfe) $ do
                        userWantMkDir <- return True -- TODO
                        when userWantMkDir $ io $ createDirectoryIfMissing True fDirectory
                      withEditor $ stringToNewBuffer (Right f) (R.fromString "") -- Create new empty buffer
            -- adjust the mode
            tbl <- asks (modeTable . yiConfig)
            contents <- withGivenBuffer b $ elemsB
            let header = take 1024 contents
                hmode = case header =~ "\\-\\*\\- *([^ ]*) *\\-\\*\\-" of 
                    AllTextSubmatches [_,m] ->m
                    _ -> ""
                Just mode = (find (\(AnyMode m)->modeName m == hmode) tbl) <|>
                            (find (\(AnyMode m)->modeApplies m f contents) tbl) <|>
                            Just (AnyMode emptyMode) 
            case mode of
                AnyMode newMode -> withGivenBuffer b $ setMode newMode
            return b
    withEditor $ switchToBufferE b
    where
    -- Determines whether or not a given buffer is associated with
    -- the given file. Note that filepaths are always canonicalized.
    assocWithSameFile :: FBuffer -> Bool
    assocWithSameFile fbuffer =
      case file fbuffer of
        Nothing -> False
        Just f2 -> equalFilePath f f2



    -- The first argument is the buffer name
    fileToNewBuffer :: FilePath -> YiM BufferRef
    fileToNewBuffer path = do
      contents <- io $ R.readFile path
      now <- io getCurrentTime
      b <- withEditor $ stringToNewBuffer (Right path) contents
      withGivenBuffer b $ markSavedB now
      return b

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
   , diredMarks :: M.Map FilePath Char -- ^ Map values are just leafnames, not full paths
   , diredEntries :: M.Map FilePath DiredEntry -- ^ keys are just leafnames, not full paths
   , diredFilePoints :: [(Point,Point,FilePath)] -- ^ position in the buffer where filename is
   , diredNameCol :: Int -- ^ position on line where filename is (all pointA are this col)
   , diredCurrFile :: FilePath -- ^ keep the position of pointer (for refreshing dired buffer)
  }
  deriving (Show, Eq, Typeable)

instance Initializable DiredState where
    initial = DiredState { diredPath        = ""
                         , diredMarks      = M.empty
                         , diredEntries    = M.empty
                         , diredFilePoints = []
                         , diredNameCol    = 0
                         , diredCurrFile   = ""
                         }

bypassReadOnly :: BufferM a -> BufferM a
bypassReadOnly f = do ro <- getA readOnlyA
                      putA readOnlyA False
                      res <- f
                      putA readOnlyA ro
                      return res

filenameColOf :: BufferM () -> BufferM ()
filenameColOf f = getA bufferDynamicValueA >>= setPrefCol . Just . diredNameCol >> f

diredKeymap :: Keymap -> Keymap
diredKeymap = do
    (choice [
             char 'p'                   ?>>! filenameColOf lineUp,
             oneOf [char 'n', char ' ']  >>! filenameColOf lineDown,
             char 'd'                   ?>>! diredMarkDel,
             char 'g'                   ?>>! diredRefresh,
             char 'm'                   ?>>! diredMark,
             char '^'                   ?>>! diredUpDir,
             char '+'                   ?>>! diredCreateDir,     
             char 'q'                   ?>>! (deleteBuffer =<< gets currentBuffer),
             char 'x'                   ?>>! diredDoMarkedDel,
             oneOf [ctrl $ char 'm', spec KEnter, char 'f'] >>! diredLoad,
             oneOf [char 'u', spec KBS]  >>! diredUnmark,
             char 'D'                   ?>>! diredDoDel,
             char 'U'                   ?>>! diredUnmarkAll]
      <||)

dired :: YiM ()
dired = do
    msgEditor "Dired..."
    maybepath <- withBuffer $ gets file
    dir <- io $ getFolder maybepath
    fnewE dir

diredDir :: FilePath -> YiM ()
diredDir dir = diredDirBuffer dir >> return ()

diredDirBuffer :: FilePath -> YiM BufferRef
diredDirBuffer dir = do
    b <- withEditor $ stringToNewBuffer (Right dir) (R.fromString "")
    withEditor $ switchToBufferE b
    diredRefresh
    return b

-- | Write the contents of the supplied directory into the current buffer in dired format
diredRefresh :: YiM ()
diredRefresh = do
    -- Get directory name
    Just dir <- withBuffer $ gets file
    -- Scan directory
    di <- io $ diredScanDir dir
    dState <- withBuffer $ getA bufferDynamicValueA
    currFile <- if null (diredFilePoints dState)
                then return ""
                else do (fp, _) <- withBuffer fileFromPoint
                        return fp
    let ds = dState {diredPath = dir, diredEntries = di, diredCurrFile = currFile}
    -- Compute results
    let dlines = linesToDisplay ds
        (strss, stys, strs) = unzip3 dlines
        strss' = transpose $ map doPadding $ transpose $ strss
        namecol = if null strss' then 0 else
                  let l1details = init $ head strss' in Data.List.sum (map length l1details) + (length l1details)

    -- Set buffer contents
    withBuffer $ do -- Clear buffer
                    putA readOnlyA False
                    ---- modifications begin here
                    deleteRegionB =<< regionOfB Document
                    -- Write Header
                    insertN $ dir ++ ":\n"
                    p <-pointB
                    -- paint header
                    addOverlayB $ mkOverlay UserLayer (mkRegion 0 (p-2)) headStyle
                    ptsList <- mapM insertDiredLine $ zip3 strss' stys strs
                    putA bufferDynamicValueA ds{diredFilePoints=ptsList,
                                                diredNameCol   =namecol}
                    -- Colours for Dired come from overlays not syntax highlighting
                    modifyMode $ \m -> m {modeKeymap = topKeymapA ^: diredKeymap, modeName = "dired"}
                    diredRefreshMark
                    ---- no modifications after this line
                    putA readOnlyA True
                    filenameColOf $ moveTo $ fromMaybe (p + 1) (getRow currFile ptsList)
    where
    getRow fp recList = lookup fp (map (\(a,_b,c)->(c,a)) recList)
    headStyle = const (withFg grey)
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
insertDiredLine :: ([String], StyleName, String) -> BufferM (Point, Point, FilePath)
insertDiredLine (fields, sty, filenm) = bypassReadOnly $ do
    insertN $ (concat $ intersperse " " (init fields))
    p1 <- pointB
    insertN (" " ++ last fields)
    p2 <- pointB
    insertN "\n"
    addOverlayB (mkOverlay UserLayer (mkRegion p1 p2) sty)
    return (p1, p2, filenm)

data DRStrings = DRPerms {undrs :: String}
               | DRLinks {undrs :: String}
               | DROwners {undrs :: String}
               | DRGroups {undrs :: String}
               | DRSizes {undrs :: String}
               | DRDates {undrs :: String}
               | DRFiles {undrs :: String}

-- | Return a List of (prefix, fullDisplayNameIncludingSourceAndDestOfLink, style, filename)
linesToDisplay :: DiredState ->([([DRStrings], StyleName, String)])
linesToDisplay dState = map (uncurry lineToDisplay) (M.assocs $ diredEntries dState)
    where
    lineToDisplay k (DiredFile v)      = (l " -" v ++ [DRFiles k], defaultStyle, k)
    lineToDisplay k (DiredDir v)       = (l " d" v ++ [DRFiles k], const (withFg blue), k)
    lineToDisplay k (DiredSymLink v s) = (l " l" v ++ [DRFiles $ k ++ " -> " ++ s], const (withFg cyan), k)
    lineToDisplay k (DiredSocket v) = (l " s" v ++ [DRFiles $ k], const (withFg magenta), k)
    lineToDisplay k (DiredCharacterDevice v) = (l " c" v ++ [DRFiles $ k], const (withFg yellow), k)
    lineToDisplay k (DiredBlockDevice v) = (l " b" v ++ [DRFiles $ k], const (withFg yellow), k)
    lineToDisplay k (DiredNamedPipe v) = (l " p" v ++ [DRFiles $ k], const (withFg brown), k)
    lineToDisplay k DiredNoInfo        = ([DRFiles $ k ++ " : Not a file/dir/symlink"], defaultStyle, k)

    l pre v = [DRPerms $ pre ++ permString v,
               DRLinks $ printf "%4d" (numLinks v),
               DROwners $ owner v,
               DRGroups $ grp v,
               DRSizes $ printf "%8d" (sizeInBytes v),
               DRDates $ modificationTimeString v]

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
                        let modTimeStr = shortCalendarTimeToString $ posixSecondsToUTCTime $ realToFrac $ modificationTime fileStatus
                        let uid = fileOwner fileStatus
                            gid = fileGroup fileStatus
                        _filenm <- if (isSymbolicLink fileStatus) then
                                  return . ((++) (takeFileName fp ++ " -> ")) =<< readSymbolicLink fp else
                                  return $ takeFileName fp
                        ownerEntry <- catch (getUserEntryForID uid) (const $ getAllUserEntries >>= return . scanForUid uid)
                        groupEntry <- catch (getGroupEntryForID gid) (const $ getAllGroupEntries >>= return . scanForGid gid)
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

shortCalendarTimeToString :: UTCTime -> String
shortCalendarTimeToString = formatTime defaultTimeLocale "%b %d %H:%M"

-- Default Filter: omit files ending in '~' or '#' and also '.' and '..'.
diredOmitFile :: String -> Bool
diredOmitFile = (=~".*~$|.*#$|^\\.$|^\\..$")

diredMark :: BufferM ()
diredMark = diredMarkWithChar '*' lineDown

diredMarkDel :: BufferM ()
diredMarkDel = diredMarkWithChar 'D' lineDown

diredMarkWithChar :: Char -> BufferM () -> BufferM ()
diredMarkWithChar c mv = bypassReadOnly $ do
                           (fn, _de) <- fileFromPoint
                           -- moveToSol >> insertN [c] >> deleteN 1
                           modA bufferDynamicValueA (\ds -> ds {diredMarks = M.insert fn c $ diredMarks ds})
                           filenameColOf mv
                           diredRefreshMark

diredRefreshMark :: BufferM ()
diredRefreshMark = do b <- pointB
                      dState <- getA bufferDynamicValueA
                      let posDict = diredFilePoints dState 
                          markMap = diredMarks dState
                          draw (pos, _, fn) = case M.lookup fn markMap of
                                                Just mark -> do 
                                                  moveTo pos >> moveToSol >> insertN [mark] >> deleteN 1
                                                  e <- pointB
                                                  addOverlayB $ mkOverlay UserLayer (mkRegion (e - 1) e) (styleOfMark mark)
                                                Nothing -> do 
                                                  -- for deleted marks
                                                  moveTo pos >> moveToSol >> insertN [' '] >> deleteN 1
                      Yi.Core.mapM_ draw posDict
                      moveTo b
    where
      styleOfMark '*' = const (withFg green)
      styleOfMark 'D' = const (withFg red)
      styleOfMark  _  = defaultStyle
      

diredUnmark :: BufferM ()
diredUnmark = bypassReadOnly $ do 
                (fn, _de) <- fileFromPoint 
                modA bufferDynamicValueA (\ds -> ds {diredMarks = M.delete fn $ diredMarks ds})
                filenameColOf lineUp
                diredRefreshMark

diredUnmarkAll :: BufferM ()
diredUnmarkAll = bypassReadOnly $ do
                   modA bufferDynamicValueA (\ds -> ds {diredMarks = const M.empty $ diredMarks ds})
                   filenameColOf $ return ()
                   diredRefreshMark

diredDoDel :: YiM ()
diredDoDel = do
  (Just dir) <- withBuffer $ gets file
  (fn, de) <- withBuffer fileFromPoint
  askDelFiles dir [(fn, de)]


diredDoMarkedDel :: YiM ()
diredDoMarkedDel = do
  (Just dir) <- withBuffer $ gets file
  fs <- markedFiles (flip Data.List.elem ['D'])
  askDelFiles dir fs



diredLoad :: YiM ()
diredLoad = do
    (Just dir) <- withBuffer $ gets file
    (fn, de) <- withBuffer fileFromPoint
    let sel = dir </> fn
    case de of
            (DiredFile _dfi) -> do
                               exists <- io $ doesFileExist sel
                               if exists then fnewE sel else msgEditor $ sel ++ " no longer exists"
            (DiredDir _dfi)  -> do
                              exists <- io $ doesDirectoryExist sel
                              if exists then diredDir sel else msgEditor $ sel ++ " no longer exists"
            (DiredSymLink _dfi dest) -> do
                                       let target = if isAbsolute dest then dest else dir </> dest
                                       existsFile <- io $ doesFileExist target
                                       existsDir <- io $ doesDirectoryExist target
                                       msgEditor $ "Following link:"++target
                                       if existsFile then fnewE target else
                                          if existsDir then diredDir target else
                                             msgEditor $ target ++ " does not exist"
            (DiredSocket _dfi) -> do
                               exists <- io $ doesFileExist sel
                               if exists then msgEditor ("Can't open Socket " ++ sel) else msgEditor $ sel ++ " no longer exists"
            (DiredBlockDevice _dfi) -> do
                               exists <- io $ doesFileExist sel
                               if exists then msgEditor ("Can't open Block Device " ++ sel) else msgEditor $ sel ++ " no longer exists"
            (DiredCharacterDevice _dfi) -> do
                               exists <- io $ doesFileExist sel
                               if exists then msgEditor ("Can't open Character Device " ++ sel) else msgEditor $ sel ++ " no longer exists"
            (DiredNamedPipe _dfi) -> do
                               exists <- io $ doesFileExist sel
                               if exists then msgEditor ("Can't open Pipe " ++ sel) else msgEditor $ sel ++ " no longer exists"
            DiredNoInfo -> msgEditor $ "No File Info for:"++sel

-- | Extract the filename at point. NB this may fail if the buffer has been edited. Maybe use Markers instead.
fileFromPoint :: BufferM (FilePath, DiredEntry)
fileFromPoint = do
    p <- pointB
    dState <- getA bufferDynamicValueA
    let (_,_,f) = head $ filter (\(_,p2,_)->p<=p2) (diredFilePoints dState)
    return (f, M.findWithDefault DiredNoInfo f $ diredEntries dState)

markedFiles :: (Char -> Bool) -> YiM [(FilePath, DiredEntry)]
markedFiles cond = do
  dState <- withBuffer $ getA bufferDynamicValueA
  let fs = fst . unzip $ filter (cond . snd) (M.assocs $ diredMarks dState)
  return $ map (\f -> (f, (diredEntries dState) M.! f)) fs

diredUpDir :: YiM ()
diredUpDir = do
    (Just dir) <- withBuffer $ gets file
    diredDir $ takeDirectory dir

diredCreateDir :: YiM ()
diredCreateDir = do
    withMinibufferGen "" noHint "Create Dir:" return $ \nm -> do
    (Just dir) <- withBuffer $ gets file
    let newdir = dir </> nm
    msgEditor $ "Creating "++newdir++"..."
    io $ createDirectoryIfMissing True newdir
    diredRefresh


-- | Possible dired operations. A command can be translated into a list of DiredOps
data DiredOp = DiredRemoveFile FilePath
             | DiredRemoveDir FilePath
             | DiredRemoveBuffers FilePath
             -- ^ remove the buffers that associate with the file
             | DiredCopyFile FilePath FilePath
             | DiredCopyDir FilePath FilePath
             | DiredRenameFile FilePath FilePath
             | DiredRenameDir FilePath FilePath
             | DiredConfirm String DiredOp
             -- ^ prompt a "yes/no" question. If yes, the embedded DiredOp is applied, otherwise ignored.
             | DiredShowResult (DiredOpState -> YiM ())
             | DiredNoOp
             -- ^ confrim operation with yes or no, given the prompt string


-- Have no idea how to keep track of this state better, so here it is ...
data DiredOpState = DiredOpState
    {
      diredOpSucCnt :: !Int -- ^ keep track of the num of successful operations
    }
    deriving (Show, Eq, Typeable)

instance Initializable DiredOpState where
    initial = DiredOpState {diredOpSucCnt = 0}

incDiredOpSucCnt :: BufferRef -> YiM ()
incDiredOpSucCnt ref = withGivenBuffer ref $ modA bufferDynamicValueA (\ds -> ds { diredOpSucCnt = (diredOpSucCnt ds) + 1 })

resetDiredOpState :: BufferRef -> YiM ()
resetDiredOpState ref = withGivenBuffer ref $ modA bufferDynamicValueA (\_ds -> initial :: DiredOpState)

getDiredOpState :: BufferRef -> YiM DiredOpState
getDiredOpState ref = withGivenBuffer ref $ getA bufferDynamicValueA

-- | execute the operations
-- TODO: scrap the boilerplate
procDiredOpB :: BufferRef -> Bool -> [DiredOp] -> YiM ()
procDiredOpB ref counting ((DiredRemoveFile f):ops) = do io $ catch (removeFile f) handler
                                                         when counting (incDiredOpSucCnt ref)
                                                         procDiredOpB ref counting ops
    where handler err = fail $ concat ["Remove file ", f, " failed: ", show err]
procDiredOpB ref counting ((DiredRemoveDir d):ops) = do io $ catch (removeDirectoryRecursive d) handler
                                                        when counting (incDiredOpSucCnt ref)
                                                        procDiredOpB ref counting ops
    where handler err = fail $ concat ["Remove directory ", d, " failed: ", show err]
procDiredOpB ref counting ((DiredRemoveBuffers f):ops) = undefined -- TODO
procDiredOpB ref counting  ((DiredCopyFile o n):ops) = do io $ catch (copyFile o n) handler
                                                          when counting (incDiredOpSucCnt ref)
                                                          procDiredOpB ref counting ops
    where handler err = fail $ concat ["Copy file ", o, " to ", n, " failed: ", show err]
procDiredOpB ref counting ((DiredCopyDir o n):ops) = undefined -- TODO
procDiredOpB ref counting ((DiredRenameFile o n):ops) = do io $ catch (renameFile o n) handler
                                                           when counting (incDiredOpSucCnt ref)
                                                           procDiredOpB ref counting ops
    where handler err = fail $ concat ["Move file ", o, " to ", n, " failed: ", show err]
procDiredOpB ref counting ((DiredRenameDir o n):ops) = do io $ catch (renameDirectory o n) handler
                                                          when counting (incDiredOpSucCnt ref)
                                                          procDiredOpB ref counting ops
    where handler err = fail $ concat ["Move directory ", o, " to ", n, "failed: ", show err]
procDiredOpB ref counting r@((DiredConfirm prompt op):ops) = withMinibuffer (prompt ++ " (yes/no)") noHint act
    where act s = case map toLower s of
                    "yes" -> procDiredOpB ref counting (op:ops)
                    "no"  -> procDiredOpB ref counting ops
                    _     -> procDiredOpB ref counting r -- TODO: show an error msg
procDiredOpB ref counting ((DiredNoOp):ops) = when counting (incDiredOpSucCnt ref) >> procDiredOpB ref counting ops
procDiredOpB ref counting ((DiredShowResult op):ops) = getDiredOpState ref >>= op >> procDiredOpB ref counting ops
procDiredOpB _ _ [] = return ()

-- | move the files in a given directory to the target location
askRenameFiles :: FilePath -> [(FilePath, DiredEntry)] -> FilePath -> YiM ()
askRenameFiles = undefined

-- | delete a list of file in the given directory
askDelFiles :: FilePath -> [(FilePath, DiredEntry)] -> YiM ()
askDelFiles dir fs = do ref <- gets currentBuffer
                        case fs of
                          (_x:_) -> withMinibuffer prompt noHint (act ref)
                          []     -> msgEditor $ "(No deletions requested)"
    where prompt = concat ["Delete ", show $ length fs, " file(s)? (yes/no)"]
          act ref s = do case map toLower s of
                           "yes" -> do
                             resetDiredOpState ref
                             opList <- io $ sequence ops
                             procDiredOpB ref True (opList ++ [DiredShowResult showResult])
                             --msgEditor $ show opList
                           "no"  -> return ()
                           _     -> askDelFiles dir fs -- TODO: show an error msg
          ops = map opGenerator fs
          showResult st = diredRefresh >> (msgEditor $ concat [show $ diredOpSucCnt st, " of ", show total, " deletions done"])
          total = length fs
          opGenerator :: (FilePath, DiredEntry) -> IO DiredOp
          opGenerator (fn, de) = do exists <- do liftM2 (||) (doesFileExist path) (doesDirectoryExist path)
                                    if exists then case de of
                                                     (DiredDir _dfi) -> do isNull <- liftM nullDir $ getDirectoryContents path
                                                                           return $ if isNull then (DiredConfirm recDelPrompt (DiredRemoveDir path))
                                                                                               else (DiredRemoveDir path)
                                                     _               -> return (DiredRemoveFile path)
                                              else return DiredNoOp
              where path = dir </> fn
                    recDelPrompt = concat ["Recursive delete of ", fn, "?"]
                    nullDir :: [FilePath] -> Bool
                    nullDir contents = Data.List.any (not . flip Data.List.elem [".", ".."]) contents