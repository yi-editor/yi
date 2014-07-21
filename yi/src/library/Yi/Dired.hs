{-# LANGUAGE
  DeriveDataTypeable,
  TemplateHaskell,
  CPP,
  StandaloneDeriving,
  DeriveGeneric,
  OverloadedStrings #-}

-- Copyright (c) 2007, 2008, 2009 Ben Moseley, Wen Pu


-- | A Simple Dired Implementation for Yi

-- TODO
-------
-- add more comments

-- Support Symlinks
-- Mark operations
-- - search
-- Improve the colouring to show
-- - loaded buffers
-- - .hs files
-- - marked files
-- Fix old mod dates (> 6months) to show year
-- Fix the 'number of links' field to show actual values not just 1...
-- Automatic support for browsing .zip, .gz files etc...

module Yi.Dired
  ( dired
  , diredDir
  , diredDirBuffer
  ) where

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad.Reader hiding (mapM)
import Control.Lens hiding (act, op, pre)
import Data.Binary
import Data.List hiding (find, maximum, concat)
import Data.Char (toLower)
#if __GLASGOW_HASKELL__ < 708
import Data.DeriveTH
#else
import GHC.Generics (Generic)
#endif
import Data.Default
import Data.Typeable
import Data.Maybe
import Data.Foldable (find)
import qualified Data.Map as M
import qualified Data.Rope as R
import Data.Time
import Data.Time.Clock.POSIX
import System.Directory hiding (canonicalizePath)
import System.FilePath
import System.CanonicalizePath (canonicalizePath)
import System.Locale
import System.PosixCompat.Files
import System.PosixCompat.Types
import System.PosixCompat.User
import Control.Exc
import Text.Printf

import Yi.Core
import {-# source #-} Yi.File (editFile)
import Yi.MiniBuffer (spawnMinibufferE, withMinibufferFree, noHint, withMinibuffer)
import Yi.Misc (getFolder, promptFile)
import Yi.Style
import Yi.Utils
import Yi.Monad

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

makeLensesWithSuffix "A" ''DiredState

#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''DiredState)
#else
deriving instance Generic DiredState
instance Binary DiredState
#endif

instance Default DiredState where
    def = DiredState { diredPath        = ""
                     , diredMarks      = M.empty
                     , diredEntries    = M.empty
                     , diredFilePoints = []
                     , diredNameCol    = 0
                     , diredCurrFile   = ""
                     }

instance YiVariable DiredState

#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''DiredEntry)
$(derive makeBinary ''DiredFileInfo)
#else
deriving instance Generic DiredEntry
deriving instance Generic DiredFileInfo
instance Binary DiredEntry
instance Binary DiredFileInfo
#endif

bypassReadOnly :: BufferM a -> BufferM a
bypassReadOnly f = do ro <- use readOnlyA
                      assign readOnlyA False
                      res <- f
                      assign readOnlyA ro
                      return res

filenameColOf :: BufferM () -> BufferM ()
filenameColOf f = use bufferDynamicValueA >>= assign preferColA . Just . diredNameCol >> f

resetDiredOpState :: YiM ()
resetDiredOpState = withBuffer $ (%=) bufferDynamicValueA (\_ds -> def :: DiredOpState)

incDiredOpSucCnt :: YiM ()
incDiredOpSucCnt = withBuffer $ (%=) bufferDynamicValueA (\ds -> ds { diredOpSucCnt = diredOpSucCnt ds + 1 })

getDiredOpState :: YiM DiredOpState
getDiredOpState = withBuffer $ use bufferDynamicValueA

modDiredOpState :: (DiredOpState -> DiredOpState) -> YiM ()
modDiredOpState f = withBuffer $ (%=) bufferDynamicValueA f

-- | execute the operations
-- Pass the list of remaining operations down, insert new ops at the head if needed
procDiredOp :: Bool -> [DiredOp] -> YiM ()
procDiredOp counting (DORemoveFile f:ops) = do
  io $ printingException ("Remove file " ++ f) (removeLink f)
  when counting postproc
  procDiredOp counting ops
    where postproc = do incDiredOpSucCnt
                        withBuffer $ diredUnmarkPath (takeFileName f)
procDiredOp counting (DORemoveDir f:ops) = do
  io $ printingException ("Remove directory " ++ f) (removeDirectoryRecursive f)
  -- document suggests removeDirectoryRecursive will follow
  -- symlinks in f, but it seems not the case, at least on OS X.
  when counting postproc
  procDiredOp counting ops
    where postproc = do
            incDiredOpSucCnt
            withBuffer $ diredUnmarkPath (takeFileName f)
procDiredOp _counting (DORemoveBuffer _:_) = undefined -- TODO
procDiredOp counting  (DOCopyFile o n:ops) = do
  io $ printingException ("Copy file " ++ o) (copyFile o n)
  when counting postproc
  procDiredOp counting ops
    where postproc = do
            incDiredOpSucCnt
            withBuffer $ diredUnmarkPath (takeFileName o)
            -- TODO: mark copied files with "C" if the target dir's dired buffer exists
procDiredOp counting (DOCopyDir o n:ops) = do
  contents <- io $ printingException (concat ["Copy directory ", o, " to ", n]) doCopy
  subops <- io $ mapM builder $ filter (`notElem` [".", ".."]) contents
  procDiredOp False subops
  when counting postproc
  procDiredOp counting ops
    where postproc = do
            incDiredOpSucCnt
            withBuffer $ diredUnmarkPath (takeFileName o)
          -- perform dir copy: create new dir and create other copy ops
          doCopy :: IO [FilePath]
          doCopy = do
            exists <- doesDirectoryExist n
            when exists $ removeDirectoryRecursive n
            createDirectoryIfMissing True n
            getDirectoryContents o
          -- build actual copy operations
          builder :: FilePath -> IO DiredOp
          builder name = do
            let npath = n </> name
            let opath = o </> name
            isDir <- doesDirectoryExist opath
            return $ DOCkOverwrite npath $ getOp isDir opath npath
                where getOp isDir = if isDir then DOCopyDir else DOCopyFile


procDiredOp counting (DORename o n:ops) = do
  io $ printingException (concat ["Rename ", o, " to ", n]) (rename o n)
  when counting postproc
  procDiredOp counting ops
    where postproc = do
            incDiredOpSucCnt
            withBuffer $ diredUnmarkPath (takeFileName o)
procDiredOp counting r@(DOConfirm prompt eops enops:ops) =
    withMinibuffer (prompt ++ " (yes/no)") noHint act
    where act s = case map toLower s of
                    "yes" -> procDiredOp counting (eops ++ ops)
                    "no"  -> procDiredOp counting (enops ++ ops)
                    _     -> procDiredOp counting r
                             -- TODO: show an error msg
procDiredOp counting (DOCheck check eops enops:ops) = do
  res <- io check
  procDiredOp counting (if res then eops ++ ops else enops ++ ops)
procDiredOp counting (DOCkOverwrite fp op:ops) = do
  exists <- io $ fileExist fp
  procDiredOp counting (if exists then newOp:ops else op:ops)
      where newOp = DOChoice (concat ["Overwrite ", fp, " ?"]) op
procDiredOp counting (DOInput prompt opGen:ops) =
  promptFile prompt act
    where act s = procDiredOp counting $ opGen s ++ ops
procDiredOp counting (DONoOp:ops) = procDiredOp counting ops
procDiredOp counting (DOFeedback f:ops) =
  getDiredOpState >>= f >> procDiredOp counting ops
procDiredOp counting r@(DOChoice prompt op:ops) = do
  st <- getDiredOpState
  if diredOpForAll st then proceedYes
                      else void $ withEditor $ spawnMinibufferE msg (const askKeymap)
    where msg = prompt ++ " (y/n/!/q/h)"
          askKeymap = choice [ char 'n' ?>>! noAction
                             , char 'y' ?>>! yesAction
                             , char '!' ?>>! allAction
                             , char 'q' ?>>! quit
                             , char 'h' ?>>! help
                             ]
          noAction = cleanUp >> proceedNo
          yesAction = cleanUp >> proceedYes
          allAction = do cleanUp
                         modDiredOpState (\st -> st{diredOpForAll=True})
                         proceedYes
          quit = cleanUp >> msgEditor "Quit"
          help = do msgEditor $ concat ["y: yes, n: no, ",
                                        "!: yes on all remaining items, ",
                                        "q: quit, h: help"]
                    cleanUp
                    procDiredOp counting r -- repeat
          -- use cleanUp to get back the original buffer
          cleanUp = withEditor closeBufferAndWindowE
          proceedYes = procDiredOp counting (op:ops)
          proceedNo = procDiredOp counting ops
procDiredOp _ _ = return ()


-- | delete a list of file in the given directory
-- 1. Ask for confirmation, if yes, perform deletions, otherwise showNothing
-- 2. Confirmation is required for recursive deletion of non-empty directry, but only the top level one
-- 3. Show the number of successful deletions at the end of the excution
-- 4. TODO: ask confirmation for wether to remove the associated buffers when a file is removed
askDelFiles :: FilePath -> [(FilePath, DiredEntry)] -> YiM ()
askDelFiles dir fs =
  case fs of
    (_x:_) -> do
            resetDiredOpState
            -- TODO: show the file name list in new tmp window
            opList <- io $ sequence ops
            -- a deletion command is mapped to a list of deletions wrapped up by DOConfirm
            -- TODO: is `counting' necessary here?
            procDiredOp True [DOConfirm prompt (opList ++ [DOFeedback showResult]) [DOFeedback showNothing]]
    -- no files listed
    []     -> procDiredOp True [DOFeedback showNothing]
    where prompt = concat ["Delete ", show $ length fs, " file(s)?"]
          ops = map opGenerator fs
          showResult st = do
                       diredRefresh
                       msgEditor $ concat [show $ diredOpSucCnt st, " of ",
                                           show total, " deletions done"]
          showNothing _ = msgEditor "(No deletions requested)"
          total = length fs
          opGenerator :: (FilePath, DiredEntry) -> IO DiredOp
          opGenerator (fn, de) = do
                       exists <- fileExist path
                       if exists then case de of
                                        (DiredDir _dfi) -> do
                                                isNull <- liftM nullDir $ getDirectoryContents path
                                                return $ if isNull then DOConfirm recDelPrompt [DORemoveDir path] [DONoOp]
                                                         else DORemoveDir path
                                        _               -> return (DORemoveFile path)
                                 else return DONoOp
              where path = dir </> fn
                    recDelPrompt = concat ["Recursive delete of ", fn, "?"]
                    -- Test the emptyness of a folder
                    nullDir :: [FilePath] -> Bool
                    nullDir = Data.List.any (not . flip Data.List.elem [".", ".."])

diredDoDel :: YiM ()
diredDoDel = do
  dir <- currentDir
  maybefile <- withBuffer fileFromPoint
  case maybefile of
    Just (fn, de) -> askDelFiles dir [(fn, de)]
    Nothing       -> noFileAtThisLine

diredDoMarkedDel :: YiM ()
diredDoMarkedDel = do
  dir <- currentDir
  fs <- markedFiles (`Data.List.elem` "D")
  askDelFiles dir fs

diredKeymap :: Keymap -> Keymap
diredKeymap =
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
             char 'U'                   ?>>! diredUnmarkAll,
             char 'R'                   ?>>! diredRename,
             char 'C'                   ?>>! diredCopy]
      <||)

dired :: YiM ()
dired = do
    msgEditor "Dired..."
    maybepath <- withBuffer $ gets file
    dir <- io $ getFolder maybepath
    void $ editFile dir

diredDir :: FilePath -> YiM ()
diredDir dir = void (diredDirBuffer dir)

diredDirBuffer :: FilePath -> YiM BufferRef
diredDirBuffer d = do
    -- Emacs doesn't follow symlinks, probbably Yi shouldn't do too
    dir <- io $ canonicalizePath d
    -- XXX Don't specify the path as the filename of the buffer.
    b <- withEditor $ stringToNewBuffer (Left dir) ""
    withEditor $ switchToBufferE b
    withBuffer $ do
      bufferDynamicValueA %= (diredPathA .~ dir)
      assign directoryContentA True
    diredRefresh
    return b

-- | Write the contents of the supplied directory into the current buffer in dired format
diredRefresh :: YiM ()
diredRefresh = do
    dState <- withBuffer $ use bufferDynamicValueA
    let dir = diredPath dState
    -- Scan directory
    di <- io $ diredScanDir dir
    currFile <- if null (diredFilePoints dState)
                then return ""
                else do maybefile <- withBuffer fileFromPoint
                        case maybefile of
                          Just (fp, _) -> return fp
                          Nothing      -> return ""
    let ds = diredEntriesA .~ di $ diredCurrFileA .~ currFile $ dState
    -- Compute results
    let dlines = linesToDisplay ds
        (strss, stys, strs) = unzip3 dlines
        strss' = transpose $ map doPadding $ transpose strss
        namecol = if null strss' then 0 else
                  let l1details = init $ head strss' in Data.List.sum (map length l1details) + length l1details

    -- Set buffer contents
    withBuffer $ do -- Clear buffer
                    assign readOnlyA False
                    ---- modifications begin here
                    deleteRegionB =<< regionOfB Document
                    -- Write Header
                    insertN $ dir ++ ":\n"
                    p <-pointB
                    -- paint header
                    addOverlayB $ mkOverlay UserLayer (mkRegion 0 (p-2)) headStyle
                    ptsList <- mapM insertDiredLine $ zip3 strss' stys strs
                    assign bufferDynamicValueA $ diredFilePointsA .~ ptsList $ diredNameColA .~ namecol $ ds
                    -- Colours for Dired come from overlays not syntax highlighting
                    modifyMode $ \m -> m {modeKeymap = topKeymapA %~ diredKeymap, modeName = "dired"}
                    diredRefreshMark
                    ---- no modifications after this line
                    assign readOnlyA True
                    when (null currFile) $ moveTo (p-2)
                    case getRow currFile ptsList of
                      Just rpos -> filenameColOf $ moveTo rpos
                      Nothing   -> filenameColOf lineDown
    where
    getRow fp recList = lookup fp (map (\(a,_b,c)->(c,a)) recList)
    headStyle = const (withFg grey)
    doPadding :: [DRStrings] -> [String]
    doPadding drs = map (pad ((maximum . map drlength) drs)) drs

    pad _n (DRPerms s)  = s
    pad n  (DRLinks s)  = replicate (max 0 (n - length s)) ' ' ++ s
    pad n  (DROwners s) = s ++ replicate (max 0 (n - length s)) ' ' ++ " "
    pad n  (DRGroups s) = s ++ replicate (max 0 (n - length s)) ' '
    pad n  (DRSizes s)  = replicate (max 0 (n - length s)) ' ' ++ s
    pad n  (DRDates s)  = replicate (max 0 (n - length s)) ' ' ++ s
    pad _n (DRFiles s)  = s       -- Don't right-justify the filename

    drlength = length . undrs

-- | Returns a tuple containing the textual region (the end of) which is used for 'click' detection
--   and the FilePath of the file represented by that textual region
insertDiredLine :: ([String], StyleName, String) -> BufferM (Point, Point, FilePath)
insertDiredLine (fields, sty, filenm) = bypassReadOnly $ do
    insertN (unwords (init fields))
    p1 <- pointB
    insertN (' ' : last fields)
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
linesToDisplay :: DiredState -> [([DRStrings], StyleName, String)]
linesToDisplay dState = map (\(k, i) -> let k' = UTF8.decodeString k in lineToDisplay k' i) (M.assocs $ diredEntries dState)
    where
    lineToDisplay k (DiredFile v)      = (l " -" v ++ [DRFiles k], defaultStyle, k)
    lineToDisplay k (DiredDir v)       = (l " d" v ++ [DRFiles k], const (withFg blue), k)
    lineToDisplay k (DiredSymLink v s) = (l " l" v ++ [DRFiles $ k ++ " -> " ++ s], const (withFg cyan), k)
    lineToDisplay k (DiredSocket v) = (l " s" v ++ [DRFiles k], const (withFg magenta), k)
    lineToDisplay k (DiredCharacterDevice v) = (l " c" v ++ [DRFiles k], const (withFg yellow), k)
    lineToDisplay k (DiredBlockDevice v) = (l " b" v ++ [DRFiles k], const (withFg yellow), k)
    lineToDisplay k (DiredNamedPipe v) = (l " p" v ++ [DRFiles k], const (withFg brown), k)
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
    -- The file strings as UTF-8 encoded on linux. They need to stay this way for functions that
    -- stat these paths. However for display they need to be converted to ISO-10646 strings.
    foldM (lineForFile dir) M.empty files
    where
    lineForFile :: String -> M.Map FilePath DiredEntry -> String -> IO (M.Map FilePath DiredEntry)
    lineForFile d m f = do
                        let fp = d </> f
                        fileStatus <- getSymbolicLinkStatus fp
                        dfi <- lineForFilePath fp fileStatus
                        let islink = isSymbolicLink fileStatus
                        linkTarget <- if islink then readSymbolicLink fp else return ""
                        let de
                              | isDirectory fileStatus = DiredDir dfi
                              | isRegularFile fileStatus = DiredFile dfi
                              | islink = DiredSymLink dfi linkTarget
                              | isSocket fileStatus = DiredSocket dfi
                              | isCharacterDevice fileStatus = DiredCharacterDevice dfi
                              | isBlockDevice fileStatus = DiredBlockDevice dfi
                              | isNamedPipe fileStatus = DiredNamedPipe dfi
                              | otherwise = DiredNoInfo
                        return (M.insert f de m)

    lineForFilePath :: FilePath -> FileStatus -> IO DiredFileInfo
    lineForFilePath fp fileStatus = do
                        let modTimeStr = shortCalendarTimeToString $ posixSecondsToUTCTime $ realToFrac $ modificationTime fileStatus
                        let uid = fileOwner fileStatus
                            gid = fileGroup fileStatus
                        _filenm <- if isSymbolicLink fileStatus then
                                  return . ((takeFileName fp ++ " -> ") ++) =<< readSymbolicLink fp else
                                  return $ takeFileName fp
                        ownerEntry <- orException (getUserEntryForID uid) (liftM (scanForUid uid) getAllUserEntries)
                        groupEntry <- orException (getGroupEntryForID gid) (liftM (scanForGid gid) getAllGroupEntries)
                        let fmodeStr = (modeString . fileMode) fileStatus
                            sz = toInteger $ fileSize fileStatus
                            ownerStr   = userName ownerEntry
                            groupStr   = groupName groupEntry
                            numOfLinks = toInteger $ linkCount fileStatus
                        return DiredFileInfo { permString = fmodeStr
                                             , numLinks = numOfLinks
                                             , owner = ownerStr
                                             , grp = groupStr
                                             , sizeInBytes = sz
                                             , modificationTimeString = modTimeStr}


-- | Needed on Mac OS X 10.4
scanForUid :: UserID -> [UserEntry] -> UserEntry
scanForUid uid entries = fromMaybe (UserEntry "?" "" uid 0 "" "" "") (find ((== uid) . userID) entries)

-- | Needed on Mac OS X 10.4
scanForGid :: GroupID -> [GroupEntry] -> GroupEntry
scanForGid gid entries = fromMaybe (GroupEntry "?" "" gid []) (find ((== gid) . groupID) entries)

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
-- TODO: customizable filters?
--diredOmitFile :: String -> Bool
--diredOmitFile = undefined

diredMark :: BufferM ()
diredMark = diredMarkWithChar '*' lineDown

diredMarkDel :: BufferM ()
diredMarkDel = diredMarkWithChar 'D' lineDown

diredMarkWithChar :: Char -> BufferM () -> BufferM ()
diredMarkWithChar c mv = bypassReadOnly $ do
                           maybefile <- fileFromPoint
                           case maybefile of
                             Just (fn, _de) -> do
                                            bufferDynamicValueA %= (diredMarksA %~ M.insert fn c)
                                            filenameColOf mv
                                            diredRefreshMark
                             Nothing        -> filenameColOf mv

diredRefreshMark :: BufferM ()
diredRefreshMark = do b <- pointB
                      dState <- use bufferDynamicValueA
                      let posDict = diredFilePoints dState
                          markMap = diredMarks dState
                          draw (pos, _, fn) = case M.lookup fn markMap of
                                                Just mark -> do
                                                  moveTo pos >> moveToSol >> insertN [mark] >> deleteN 1
                                                  e <- pointB
                                                  addOverlayB $ mkOverlay UserLayer (mkRegion (e - 1) e) (styleOfMark mark)
                                                Nothing ->
                                                  -- for deleted marks
                                                  moveTo pos >> moveToSol >> insertN " " >> deleteN 1
                      mapM_ draw posDict
                      moveTo b
    where
      styleOfMark '*' = const (withFg green)
      styleOfMark 'D' = const (withFg red)
      styleOfMark  _  = defaultStyle


diredUnmark :: BufferM ()
diredUnmark = bypassReadOnly $ do
                maybefile <- fileFromPoint
                case maybefile of
                  Just (fn, _de) -> do diredUnmarkPath fn
                                       filenameColOf lineUp
                                       diredRefreshMark
                  Nothing        -> filenameColOf lineUp


diredUnmarkPath :: FilePath -> BufferM()
diredUnmarkPath fn = bufferDynamicValueA %= (diredMarksA %~ M.delete fn)

diredUnmarkAll :: BufferM ()
diredUnmarkAll = bypassReadOnly $ do
                   bufferDynamicValueA %= (diredMarksA .~ M.empty)
                   filenameColOf $ return ()
                   diredRefreshMark

currentDir :: YiM FilePath
currentDir = fmap diredPath $ withBuffer $ use bufferDynamicValueA

-- | move selected files in a given directory to the target location given
-- by user input
--
-- if multiple source
-- then if target is not a existing dir
--      then error
--      else move source files into target dir
-- else if target is dir
--      then if target exist
--           then move source file into target dir
--           else if source is dir and parent of target exists
--                then move source to target
--                else error
--      else if parent of target exist
--           then move source to target
--           else error
askRenameFiles :: FilePath -> [(FilePath, DiredEntry)] -> YiM ()
askRenameFiles dir fs =
    case fs of
      (_x:[]) -> do resetDiredOpState
                    procDiredOp True [DOInput prompt sOpIsDir]
      (_x:_)  -> do resetDiredOpState
                    procDiredOp True [DOInput prompt mOpIsDirAndExists]
      []      -> procDiredOp True [DOFeedback showNothing]
    where prompt = concat ["Move ", show total, " item(s) to:"]
          mOpIsDirAndExists t = [DOCheck (doesDirectoryExist t) posOps negOps]
              where
                posOps = map builder fs ++ [DOFeedback showResult]
                negOps = [DOFeedback (const $ errorEditor (t ++ " is not directory!"))]
                builder (fn, _de) = let old = dir </> fn
                                        new = t </> fn
                                    in DOCkOverwrite new (DORename old new)
          sOpIsDir t = [DOCheck (doesDirectoryExist t) posOps sOpDirRename]
              where (fn, _) = head fs -- the only item
                    posOps = [DOCkOverwrite new (DORename old new),
                              DOFeedback showResult]
                        where new = t </> fn
                              old = dir </> fn
                    sOpDirRename = [DOCheck ckParentDir posOps' negOps,
                                    DOFeedback showResult]
                        where posOps' = [DOCkOverwrite new (DORename old new)]
                              negOps =
                                  [DOFeedback (\_ -> errorEditor $ concat ["Cannot move ", old, " to ", new])]
                              new = t
                              old = dir </> fn
                              ckParentDir = doesDirectoryExist $ takeDirectory (dropTrailingPathSeparator t)
          showResult st = do
              diredRefresh
              msgEditor $ concat [show (diredOpSucCnt st),
                                  " of ", show total, " item(s) moved."]
          showNothing _ = msgEditor "Quit"
          total = length fs

-- | copy selected files in a given directory to the target location given
-- by user input
--
-- askCopyFiles follow the same logic as askRenameFiles,
-- except dir and file are done by different DiredOP
askCopyFiles :: FilePath -> [(FilePath, DiredEntry)] -> YiM ()
askCopyFiles dir fs =
    case fs of
      (_x:[]) -> do resetDiredOpState
                    procDiredOp True [DOInput prompt sOpIsDir]
      (_x:_)  -> do resetDiredOpState
                    procDiredOp True [DOInput prompt mOpIsDirAndExists]
      []      -> procDiredOp True [DOFeedback showNothing]
    where prompt = concat ["Copy ", show total, " item(s) to:"]
          mOpIsDirAndExists t = [DOCheck (doesDirectoryExist t) posOps negOps]
              where
                posOps = map builder fs ++ [DOFeedback showResult]
                negOps = [DOFeedback $ const $ errorEditor (t ++ " is not directory!")]
                builder (fn, de) = let old = dir </> fn
                                       new = t </> fn
                                   in DOCkOverwrite new (op4Type de old new)
          sOpIsDir t = [DOCheck (doesDirectoryExist t) posOps sOpDirCopy]
              where (fn, de) = head fs -- the only item
                    posOps = [DOCkOverwrite new (op4Type de old new),
                              DOFeedback showResult]
                        where new = t </> fn
                              old = dir </> fn
                    sOpDirCopy = [DOCheck ckParentDir posOps' negOps,
                                  DOFeedback showResult]
                        where posOps' = [DOCkOverwrite new (op4Type de old new)]
                              negOps =
                                  [DOFeedback (\_ -> errorEditor $ concat ["Cannot copy ", old, " to ", new])]
                              new = t
                              old = dir </> fn
                              ckParentDir = doesDirectoryExist $ takeDirectory (dropTrailingPathSeparator t)
          showResult st = do
                        diredRefresh
                        msgEditor $ concat [show (diredOpSucCnt st),
                                            " of ", show total, " item(s) copied."]
          showNothing _ = msgEditor "Quit"
          total = length fs
          op4Type :: DiredEntry -> FilePath -> FilePath -> DiredOp
          op4Type (DiredDir _) = DOCopyDir
          op4Type _            = DOCopyFile

diredRename :: YiM ()
diredRename = do
  dir <- currentDir
  fs <- markedFiles (`Data.List.elem` "*")
  if null fs then do maybefile <- withBuffer fileFromPoint
                     case maybefile of
                       Just (fn, de) -> askRenameFiles dir [(fn, de)]
                       Nothing       -> noFileAtThisLine
             else askRenameFiles dir fs

diredCopy :: YiM ()
diredCopy = do
  dir <- currentDir
  fs <- markedFiles (`Data.List.elem` "*")
  if null fs then do maybefile <- withBuffer fileFromPoint
                     case maybefile of
                       Just (fn, de) -> askCopyFiles dir [(fn, de)]
                       Nothing       -> noFileAtThisLine
             else askCopyFiles dir fs

diredLoad :: YiM ()
diredLoad = do
    dir <- currentDir
    maybefile <- withBuffer fileFromPoint
    case maybefile of
      Just (fn, de) -> do let sel = dir </> fn
                          case de of
                            (DiredFile _dfi) -> do
                                    exists <- io $ doesFileExist sel
                                    if exists then void $ editFile sel
                                              else msgEditor $ sel ++ " no longer exists"
                            (DiredDir _dfi)  -> do
                                    exists <- io $ doesDirectoryExist sel
                                    if exists then diredDir sel
                                              else msgEditor $ sel ++ " no longer exists"
                            (DiredSymLink _dfi dest) -> do
                                    let target = if isAbsolute dest then dest else dir </> dest
                                    existsFile <- io $ doesFileExist target
                                    existsDir <- io $ doesDirectoryExist target
                                    msgEditor $ "Following link:"++target
                                    if existsFile then void $ editFile target else
                                        if existsDir then diredDir target else
                                            msgEditor $ target ++ " does not exist"
                            (DiredSocket _dfi) -> do
                                    exists <- io $ doesFileExist sel
                                    msgEditor (if exists
                                        then "Can't open Socket " ++ sel
                                        else sel ++ " no longer exists")
                            (DiredBlockDevice _dfi) -> do
                                    exists <- io $ doesFileExist sel
                                    msgEditor (if exists
                                        then "Can't open Block Device " ++ sel
                                        else sel ++ " no longer exists")
                            (DiredCharacterDevice _dfi) -> do
                                    exists <- io $ doesFileExist sel
                                    msgEditor (if exists
                                        then "Can't open Character Device " ++ sel
                                        else sel ++ " no longer exists")
                            (DiredNamedPipe _dfi) -> do
                                    exists <- io $ doesFileExist sel
                                    msgEditor (if exists 
                                        then "Can't open Pipe " ++ sel
                                        else sel ++ " no longer exists")
                            DiredNoInfo -> msgEditor $ "No File Info for:"++sel
      Nothing        -> noFileAtThisLine


noFileAtThisLine :: YiM ()
noFileAtThisLine = msgEditor "(No file at this line)"

-- | Extract the filename at point. NB this may fail if the buffer has been edited. Maybe use Markers instead.
fileFromPoint :: BufferM (Maybe (FilePath, DiredEntry))
fileFromPoint = do
    p <- pointB
    dState <- use bufferDynamicValueA
    let candidates = filter (\(_,p2,_)->p<=p2) (diredFilePoints dState)
    case candidates of
      ((_, _, f):_) -> return $ Just (f, M.findWithDefault DiredNoInfo f $ diredEntries dState)
      _             -> return Nothing

markedFiles :: (Char -> Bool) -> YiM [(FilePath, DiredEntry)]
markedFiles cond = do
  dState <- withBuffer $ use bufferDynamicValueA
  let fs = fst . unzip $ filter (cond . snd) (M.assocs $ diredMarks dState)
  return $ map (\f -> (f, diredEntries dState M.! f)) fs

diredUpDir :: YiM ()
diredUpDir = do
    dir <- currentDir
    diredDir $ takeDirectory dir

diredCreateDir :: YiM ()
diredCreateDir =
    withMinibufferFree "Create Dir:" $ \nm -> do
        dir <- currentDir
        let newdir = dir </> nm
        msgEditor $ "Creating "++newdir++"..."
        io $ createDirectoryIfMissing True newdir
        diredRefresh


-- | Elementary operations for dired file operations
-- Map a dired mark operation (e.g. delete, rename, copy) command
-- into a list of DiredOps, and use procDiredOp to excute them.
-- Logic and implementation of each operation are packaged in procDiredOp
-- See askDelFiles for example.
-- If new elem op is added, just add corresponding procDiredOp to handle it.
data DiredOp = DORemoveFile FilePath
             | DORemoveDir FilePath
             | DOCopyFile FilePath FilePath
             | DOCopyDir FilePath FilePath
             | DORename FilePath FilePath
             | DORemoveBuffer FilePath
             -- ^ remove the buffers that associate with the file
             | DOConfirm String [DiredOp] [DiredOp]
             -- ^ prompt a "yes/no" question. If yes, execute the first list of embedded DiredOps
             -- otherwise execute the second list of embedded DiredOps
             | DOCheck (IO Bool) [DiredOp] [DiredOp]
             -- ^ similar to DOConfirm, but no user interaction. Could be used to check file existence
             | DOCkOverwrite FilePath DiredOp
             -- ^ this is a shortcut, it invokes DCChoice if file exists
             | DOInput String (String -> [DiredOp])
             -- ^ prompt a string and collect user input.
             -- the embedded list of DiredOps is generated based on input,
             -- Remember that the input should be checked with DOCheck
             | DOChoice String DiredOp
             -- ^ prompt a string, provide keybindings for 'y', 'n', '!', 'q' and optional 'h' (help)
             -- this is useful when overwriting of existing files is required to complete the op
             -- choice '!' will bypass following DOChoice prompts.
             | DOFeedback (DiredOpState -> YiM ())
             -- ^ to feedback, given the state. such as show the result.
             | DONoOp
             -- ^ no operation

-- Have no idea how to keep track of this state better, so here it is ...
data DiredOpState = DiredOpState
    {  diredOpSucCnt :: !Int -- ^ keep track of the num of successful operations
     , diredOpForAll :: Bool -- ^ if True, DOChoice will be bypassed
    }
    deriving (Show, Eq, Typeable)

instance Default DiredOpState where
    def = DiredOpState {diredOpSucCnt = 0, diredOpForAll = False}

#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''DiredOpState)
#else
deriving instance Generic DiredOpState
instance Binary DiredOpState
#endif

instance YiVariable DiredOpState
