{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      :  Yi.Dired
License     :  GPL-2
Maintainer  :  yi-devel@googlegroups.com
Stability   :  experimental
Portability :  portable

A simple
<https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html dired>
implementation for Yi.

= TODO

* add more comments
* Support symlinks
* Mark operations

    * search

* Improve the colouring to show

    * loaded buffers
    * .hs files
    * marked files

* Fix old mod dates (> 6months) to show year
* Fix the 'number of links' field to show actual values not just 1...
* Automatic support for browsing .zip, .gz files etc...
-}

module Yi.Dired
  ( dired
  , diredDir
  , diredDirBuffer
  , editFile
  ) where

import           GHC.Generics             (Generic)

import           Control.Applicative      ((<$>), (<|>))
import           Control.Category         ((>>>))
import           Control.Exc              (orException, printingException)
import           Control.Lens             (assign, makeLenses, use, (%~), (&), (.=), (.~), (^.))
import           Control.Monad.Reader     (asks, foldM, unless, void, when)
import           Data.Binary              (Binary)
import           Data.Char                (toLower)
import           Data.Default             (Default, def)
import           Data.Foldable            (find, foldl')
import           Data.List                (any, elem, sum, transpose)
import qualified Data.Map                 as M (Map, assocs, delete, empty,
                                                findWithDefault, fromList,
                                                insert, keys, lookup, map,
                                                mapKeys, union, (!))
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              (mempty, (<>))
import qualified Data.Text                as T (Text, pack, unpack)
import qualified Data.Text.ICU            as ICU (regex, find, unfold, group, MatchOption(..))
import           Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
import           Data.Typeable            (Typeable)
import           System.CanonicalizePath  (canonicalizePath)
import           System.Directory         (copyFile, createDirectoryIfMissing,
                                           doesDirectoryExist, doesFileExist,
                                           getDirectoryContents, getPermissions,
                                           removeDirectoryRecursive, writable)
import           System.FilePath          (dropTrailingPathSeparator,
                                           equalFilePath, isAbsolute,
                                           takeDirectory, takeFileName, (</>))
import           System.FriendlyPath      (userToCanonPath)
import           System.PosixCompat.Files (FileStatus, fileExist, fileGroup,
                                           fileMode, fileOwner, fileSize,
                                           getSymbolicLinkStatus,
                                           groupExecuteMode, groupReadMode,
                                           groupWriteMode, isBlockDevice,
                                           isCharacterDevice, isDirectory,
                                           isNamedPipe, isRegularFile, isSocket,
                                           isSymbolicLink, linkCount,
                                           modificationTime, otherExecuteMode,
                                           otherReadMode, otherWriteMode,
                                           ownerExecuteMode, ownerReadMode,
                                           ownerWriteMode, readSymbolicLink,
                                           readSymbolicLink, removeLink, rename,
                                           unionFileModes)
import           System.PosixCompat.Types (FileMode, GroupID, UserID)
import           System.PosixCompat.User  (GroupEntry, GroupEntry (..),
                                           UserEntry (..), getAllGroupEntries,
                                           getAllUserEntries,
                                           getGroupEntryForID,
                                           getUserEntryForID, groupID, userID)
import           Text.Printf              (printf)
import           Yi.Buffer
import           Yi.Config                (modeTable)
import           Yi.Core                  (errorEditor)
import           Yi.Editor
import           Yi.Keymap                (Keymap, YiM, topKeymapA)
import           Yi.Keymap.Keys
import           Yi.MiniBuffer            (noHint, spawnMinibufferE, withMinibuffer, withMinibufferFree)
import           Yi.Misc                  (getFolder, promptFile)
import           Yi.Monad                 (gets)
import qualified Yi.Rope                  as R
import           Yi.String                (showT)
import           Yi.Style
import           Yi.Types                 (YiVariable, yiConfig)
import           Yi.Utils                 (io, makeLensesWithSuffix)


#if __GLASGOW_HASKELL__ < 710
import System.Locale (defaultTimeLocale)
import Data.Time     (UTCTime, formatTime, getCurrentTime)
#else
import Data.Time     (UTCTime, formatTime, getCurrentTime, defaultTimeLocale)
#endif

-- Have no idea how to keep track of this state better, so here it is ...
data DiredOpState = DiredOpState
    { _diredOpSucCnt :: !Int -- ^ keep track of the num of successful operations
    , _diredOpForAll :: Bool -- ^ if True, DOChoice will be bypassed
    } deriving (Show, Eq, Typeable, Generic)

instance Default DiredOpState where
    def = DiredOpState { _diredOpSucCnt = 0, _diredOpForAll = False }

instance Binary DiredOpState

instance YiVariable DiredOpState

makeLenses ''DiredOpState

data DiredFileInfo = DiredFileInfo
    { permString             :: R.YiString
    , numLinks               :: Integer
    , owner                  :: R.YiString
    , grp                    :: R.YiString
    , sizeInBytes            :: Integer
    , modificationTimeString :: R.YiString
    } deriving (Show, Eq, Typeable, Generic)

data DiredEntry
    = DiredFile DiredFileInfo
    | DiredDir DiredFileInfo
    | DiredSymLink DiredFileInfo R.YiString
    | DiredSocket DiredFileInfo
    | DiredBlockDevice DiredFileInfo
    | DiredCharacterDevice DiredFileInfo
    | DiredNamedPipe DiredFileInfo
    | DiredNoInfo
    deriving (Show, Eq, Typeable, Generic)

-- | Alias serving as documentation of some arguments. We keep most
-- paths as 'R.YiString' for the sole reason that we'll have to render
-- them.
type DiredFilePath = R.YiString

-- | Handy alias for 'DiredEntry' map.
type DiredEntries = M.Map DiredFilePath DiredEntry

data DiredState = DiredState
    { diredPath        :: FilePath -- ^ The full path to the directory being viewed
     -- FIXME Choose better data structure for Marks...
    , diredMarks      :: M.Map FilePath Char
      -- ^ Map values are just leafnames, not full paths
    , diredEntries    :: DiredEntries
      -- ^ keys are just leafnames, not full paths
    , diredFilePoints :: [(Point,Point,FilePath)]
      -- ^ position in the buffer where filename is
    , diredNameCol    :: Int
      -- ^ position on line where filename is (all pointA are this col)
    , diredCurrFile   :: FilePath
      -- ^ keep the position of pointer (for refreshing dired buffer)
    } deriving (Show, Eq, Typeable, Generic)

makeLensesWithSuffix "A" ''DiredState

instance Binary DiredState

instance Default DiredState where
    def = DiredState { diredPath       = mempty
                     , diredMarks      = mempty
                     , diredEntries    = mempty
                     , diredFilePoints = mempty
                     , diredNameCol    = 0
                     , diredCurrFile   = mempty
                     }

instance YiVariable DiredState

instance Binary DiredEntry
instance Binary DiredFileInfo

-- | If file exists, read contents of file into a new buffer, otherwise
-- creating a new empty buffer. Replace the current window with a new
-- window onto the new buffer.
--
-- If the file is already open, just switch to the corresponding buffer.
--
-- Need to clean up semantics for when buffers exist, and how to attach
-- windows to buffers.
--
-- @Yi.File@ module re-exports this, you probably want to import that
-- instead.
--
-- In case of a decoding failure, failure message is returned instead
-- of the 'BufferRef'.
editFile :: FilePath -> YiM (Either T.Text BufferRef)
editFile filename = do
    f <- io $ userToCanonPath filename

    dupBufs <- filter (maybe False (equalFilePath f) . file) <$> gets bufferSet

    dirExists  <- io $ doesDirectoryExist f
    fileExists <- io $ doesFileExist f

    b <- case dupBufs of
      [] -> if dirExists
            then Right <$> diredDirBuffer f
            else do
              nb <- if fileExists
                    then fileToNewBuffer f
                    else Right <$> newEmptyBuffer f
              case nb of
                Left m -> return $ Left m
                Right buf -> Right <$> setupMode f buf

      (h:_) -> return . Right $ bkey h

    case b of
     Left m -> return $ Left m
     Right bf -> withEditor (switchToBufferE bf >> addJumpHereE) >> return b
  where
    fileToNewBuffer :: FilePath -> YiM (Either T.Text BufferRef)
    fileToNewBuffer f = io getCurrentTime >>= \n -> io (R.readFile f) >>= \case
      Left m -> return $ Left m
      Right (contents, conv) -> do
        permissions <- io $ getPermissions f

        b <- stringToNewBuffer (FileBuffer f) contents
        withGivenBuffer b $ do
          encodingConverterNameA .= Just conv
          markSavedB n
          unless (writable permissions) (readOnlyA .= True)

        return $ Right b

    newEmptyBuffer :: FilePath -> YiM BufferRef
    newEmptyBuffer f =
      stringToNewBuffer (FileBuffer f) mempty

    setupMode :: FilePath -> BufferRef -> YiM BufferRef
    setupMode f b = do
      tbl <- asks (modeTable . yiConfig)
      content <- withGivenBuffer b elemsB

      let header = R.take 1024 content
          rx = ICU.regex [] "\\-\\*\\- *([^ ]*) *\\-\\*\\-"
          hmode = case ICU.find rx (R.toText header) of
              Nothing -> ""
              Just m  -> case (ICU.group 1 m) of
                           Just n  -> n
                           Nothing -> ""
          Just mode = find (\(AnyMode m) -> modeName m == hmode) tbl <|>
                      find (\(AnyMode m) -> modeApplies m f header) tbl <|>
                      Just (AnyMode emptyMode)
      case mode of
          AnyMode newMode -> withGivenBuffer b $ setMode newMode

      return b


bypassReadOnly :: BufferM a -> BufferM a
bypassReadOnly f = do ro <- use readOnlyA
                      assign readOnlyA False
                      res <- f
                      assign readOnlyA ro
                      return res

filenameColOf :: BufferM () -> BufferM ()
filenameColOf f = getBufferDyn >>= assign preferColA . Just . diredNameCol >> f

resetDiredOpState :: YiM ()
resetDiredOpState = withCurrentBuffer $ putBufferDyn (def :: DiredOpState)

incDiredOpSucCnt :: YiM ()
incDiredOpSucCnt =
  withCurrentBuffer $ getBufferDyn >>= putBufferDyn . (diredOpSucCnt %~ succ)

getDiredOpState :: YiM DiredOpState
getDiredOpState = withCurrentBuffer getBufferDyn

modDiredOpState :: (DiredOpState -> DiredOpState) -> YiM ()
modDiredOpState f = withCurrentBuffer $ getBufferDyn >>= putBufferDyn . f

-- | Execute the operations
--
-- Pass the list of remaining operations down, insert new ops at the
-- head if needed
procDiredOp :: Bool -> [DiredOp] -> YiM ()
procDiredOp counting (DORemoveFile f:ops) = do
  io $ printingException ("Remove file " <> f) (removeLink f)
  when counting postproc
  procDiredOp counting ops
    where postproc = do incDiredOpSucCnt
                        withCurrentBuffer $ diredUnmarkPath (takeFileName f)
procDiredOp counting (DORemoveDir f:ops) = do
  io $ printingException ("Remove directory " <> f) (removeDirectoryRecursive f)
  -- document suggests removeDirectoryRecursive will follow
  -- symlinks in f, but it seems not the case, at least on OS X.
  when counting postproc
  procDiredOp counting ops
    where postproc = do
            incDiredOpSucCnt
            withCurrentBuffer $ diredUnmarkPath (takeFileName f)
procDiredOp _counting (DORemoveBuffer _:_) = undefined -- TODO
procDiredOp counting  (DOCopyFile o n:ops) = do
  io $ printingException ("Copy file " <> o) (copyFile o n)
  when counting postproc
  procDiredOp counting ops
    where postproc = do
            incDiredOpSucCnt
            withCurrentBuffer $ diredUnmarkPath (takeFileName o)
            -- TODO: mark copied files with "C" if the target dir's
            -- dired buffer exists
procDiredOp counting (DOCopyDir o n:ops) = do
  contents <- io $ printingException
              (concat ["Copy directory ", o, " to ", n]) doCopy
  subops <- io $ mapM builder $ filter (`notElem` [".", ".."]) contents
  procDiredOp False subops
  when counting postproc
  procDiredOp counting ops
    where postproc = do
            incDiredOpSucCnt
            withCurrentBuffer $ diredUnmarkPath (takeFileName o)
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
            withCurrentBuffer $ diredUnmarkPath (takeFileName o)
procDiredOp counting r@(DOConfirm prompt eops enops:ops) =
  withMinibuffer (R.toText $ prompt <> " (yes/no)") noHint (act . T.unpack)
  where act s = case map toLower s of
                  "yes" -> procDiredOp counting (eops <> ops)
                  "no"  -> procDiredOp counting (enops <> ops)
                  _     -> procDiredOp counting r
                             -- TODO: show an error msg
procDiredOp counting (DOCheck check eops enops:ops) = do
  res <- io check
  procDiredOp counting (if res then eops <> ops else enops <> ops)
procDiredOp counting (DOCkOverwrite fp op:ops) = do
  exists <- io $ fileExist fp
  procDiredOp counting (if exists then newOp:ops else op:ops)
      where newOp = DOChoice ("Overwrite " <> R.fromString fp <> " ?") op
procDiredOp counting (DOInput prompt opGen:ops) =
  promptFile (R.toText prompt) (act . T.unpack)
    where act s = procDiredOp counting $ opGen s <> ops
procDiredOp counting (DONoOp:ops) = procDiredOp counting ops
procDiredOp counting (DOFeedback f:ops) =
  getDiredOpState >>= f >> procDiredOp counting ops
procDiredOp counting r@(DOChoice prompt op:ops) = do
  st <- getDiredOpState
  if st ^. diredOpForAll
    then proceedYes
    else withEditor_ $ spawnMinibufferE msg (const askKeymap)
    where msg = R.toText $ prompt <> " (y/n/!/q/h)"
          askKeymap = choice [ char 'n' ?>>! noAction
                             , char 'y' ?>>! yesAction
                             , char '!' ?>>! allAction
                             , char 'q' ?>>! quit
                             , char 'h' ?>>! help
                             ]
          noAction = cleanUp >> proceedNo
          yesAction = cleanUp >> proceedYes
          allAction = do cleanUp
                         modDiredOpState (diredOpForAll .~ True)
                         proceedYes
          quit = cleanUp >> printMsg "Quit"
          help = do
            printMsg
              "y: yes, n: no, !: yes on all remaining items, q: quit, h: help"
            cleanUp
            procDiredOp counting r -- repeat
          -- use cleanUp to get back the original buffer
          cleanUp = withEditor closeBufferAndWindowE
          proceedYes = procDiredOp counting (op:ops)
          proceedNo = procDiredOp counting ops
procDiredOp _ _ = return ()


-- | Delete a list of file in the given directory
--
-- 1. Ask for confirmation, if yes, perform deletions, otherwise
-- showNothing
--
-- 2. Confirmation is required for recursive deletion of non-empty
-- directry, but only the top level one
--
-- 3. Show the number of successful deletions at the end of the excution
--
-- 4. TODO: ask confirmation for wether to remove the associated
-- buffers when a file is removed
askDelFiles :: FilePath -> [(FilePath, DiredEntry)] -> YiM ()
askDelFiles dir fs =
  case fs of
    (_x:_) -> do
            resetDiredOpState
            -- TODO: show the file name list in new tmp window
            opList <- io $ sequence ops
            -- a deletion command is mapped to a list of deletions
            -- wrapped up by DOConfirm
            -- TODO: is `counting' necessary here?
            let ops' = opList <> [DOFeedback showResult]
            procDiredOp True [DOConfirm prompt ops' [DOFeedback showNothing]]
    -- no files listed
    []     -> procDiredOp True [DOFeedback showNothing]
    where
      prompt = R.concat [ "Delete "
                        , R.fromString . show $ length fs
                        , " file(s)?"
                        ]
      ops = map opGenerator fs
      showResult st = do
        diredRefresh
        printMsg $ showT (st ^. diredOpSucCnt) <> " of "
                    <> showT total <> " deletions done"
      showNothing _ = printMsg "(No deletions requested)"
      total = length fs
      opGenerator :: (FilePath, DiredEntry) -> IO DiredOp
      opGenerator (fn, de) = do
                   exists <- fileExist path
                   if exists then case de of
                     (DiredDir _dfi) -> do
                       isNull <- fmap nullDir $ getDirectoryContents path
                       return $ if isNull then DOConfirm recDelPrompt
                                               [DORemoveDir path] [DONoOp]
                                else DORemoveDir path
                     _               -> return (DORemoveFile path)
                     else return DONoOp
          where path = dir </> fn
                recDelPrompt = "Recursive delete of " <> R.fromString fn <> "?"
                -- Test the emptyness of a folder
                nullDir :: [FilePath] -> Bool
                nullDir = Data.List.any (not . flip Data.List.elem [".", ".."])

diredDoDel :: YiM ()
diredDoDel = do
  dir <- currentDir
  maybefile <- withCurrentBuffer fileFromPoint
  case maybefile of
    Just (fn, de) -> askDelFiles dir [(fn, de)]
    Nothing       -> noFileAtThisLine

diredDoMarkedDel :: YiM ()
diredDoMarkedDel = do
  dir <- currentDir
  fs <- markedFiles (== 'D')
  askDelFiles dir fs

diredKeymap :: Keymap -> Keymap
diredKeymap = important $ withArg mainMap
  where
    -- produces a copy of the map allowing for C-u
    withArg :: (Maybe Int -> Keymap) -> Keymap
    withArg k = choice [ ctrlCh 'u' ?>> k (Just 1) , k Nothing ]

    mainMap :: Maybe Int -> Keymap
    mainMap univArg = choice
      [ char 'p'                   ?>>! filenameColOf lineUp
      , oneOf [char 'n', char ' ']  >>! filenameColOf lineDown
      , char 'd'                   ?>>! diredMarkDel
      , char 'g'                   ?>>! diredRefresh
      , char 'm'                   ?>>! diredMark
      , char '^'                   ?>>! diredUpDir
      , char '+'                   ?>>! diredCreateDir
      , char 'q'                   ?>>!
          ((deleteBuffer =<< gets currentBuffer) :: EditorM ())
      , char 'x'                   ?>>! diredDoMarkedDel
      , oneOf [ctrl $ char 'm', spec KEnter, char 'f', char 'e'] >>! diredLoad
        -- Currently ‘o’ misbehaves, seems this naive method loses
        -- track of buffers.
      , char 'o'                   ?>>! withOtherWindow diredLoad
      , char 'u'                   ?>>! diredUnmark Forward
      , spec KBS                   ?>>! diredUnmark Backward
      , char 'D'                   ?>>! diredDoDel
      , char 'U'                   ?>>! diredUnmarkAll
      , char 'R'                   ?>>! diredRename
      , char 'C'                   ?>>! diredCopy
      , char '*'                   ?>>  multiMarks univArg
      ]

    multiMarks :: Maybe Int -> Keymap
    multiMarks univArg = choice
      [ char '!' ?>>! diredUnmarkAll
      , char '@' ?>>! diredMarkSymlinks univArg
      , char '/' ?>>! diredMarkDirectories univArg
      , char 't' ?>>! diredToggleAllMarks
      ]

dired :: YiM ()
dired = do
    printMsg "Dired..."
    maybepath <- withCurrentBuffer $ gets file
    dir <- io $ getFolder maybepath
    void $ editFile dir

diredDir :: FilePath -> YiM ()
diredDir dir = void (diredDirBuffer dir)

diredDirBuffer :: FilePath -> YiM BufferRef
diredDirBuffer d = do
    -- Emacs doesn't follow symlinks, probably Yi shouldn't do too
    dir <- io $ canonicalizePath d
    b <- stringToNewBuffer (FileBuffer dir) mempty
    withEditor $ switchToBufferE b
    withCurrentBuffer $ do
      state <- getBufferDyn
      putBufferDyn (state & diredPathA .~ dir)
      directoryContentA .= True
    diredRefresh
    return b

-- | Write the contents of the supplied directory into the current
-- buffer in dired format
diredRefresh :: YiM ()
diredRefresh = do
    dState <- withCurrentBuffer getBufferDyn
    let dir = diredPath dState
    -- Scan directory
    di <- io $ diredScanDir dir
    currFile <- if null (diredFilePoints dState)
                then return ""
                else do maybefile <- withCurrentBuffer fileFromPoint
                        case maybefile of
                          Just (fp, _) -> return fp
                          Nothing      -> return ""
    let ds = diredEntriesA .~ di $ diredCurrFileA .~ currFile $ dState
    -- Compute results
    let dlines = linesToDisplay ds
        (strss, stys, strs) = unzip3 dlines
        strss' = transpose $ map doPadding $ transpose strss
        namecol = if null strss' then 0 else
                  let l1details = init $ head strss'
                  in Data.List.sum (map R.length l1details) + length l1details

    -- Set buffer contents
    withCurrentBuffer $ do -- Clear buffer
      assign readOnlyA False
      ---- modifications begin here
      deleteRegionB =<< regionOfB Document
      -- Write Header
      insertN $ R.fromString dir <> ":\n"
      p <- pointB
      -- paint header
      addOverlayB $ mkOverlay "dired" (mkRegion 0 (p-2)) headStyle ""
      ptsList <- mapM insertDiredLine $ zip3 strss' stys strs
      putBufferDyn $ diredFilePointsA .~ ptsList $ diredNameColA .~ namecol $ ds

      -- Colours for Dired come from overlays not syntax highlighting
      modifyMode $ modeKeymapA .~  topKeymapA %~ diredKeymap
                   >>> modeNameA .~ "dired"
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
    doPadding :: [DRStrings] -> [R.YiString]
    doPadding drs = map (pad ((maximum . map drlength) drs)) drs

    pad _n (DRPerms s)  = s
    pad n  (DRLinks s)  = R.replicate (max 0 (n - R.length s)) " " <> s
    pad n  (DROwners s) = s <> R.replicate (max 0 (n - R.length s)) " " <> " "
    pad n  (DRGroups s) = s <> R.replicate (max 0 (n - R.length s)) " "
    pad n  (DRSizes s)  = R.replicate (max 0 (n - R.length s)) " " <> s
    pad n  (DRDates s)  = R.replicate (max 0 (n - R.length s)) " " <> s
    pad _n (DRFiles s)  = s       -- Don't right-justify the filename

    drlength = R.length . undrs

-- | Returns a tuple containing the textual region (the end of) which
-- is used for 'click' detection and the FilePath of the file
-- represented by that textual region
insertDiredLine :: ([R.YiString], StyleName, R.YiString)
                -> BufferM (Point, Point, FilePath)
insertDiredLine (fields, sty, filenm) = bypassReadOnly $ do
  insertN . R.unwords $ init fields
  p1 <- pointB
  insertN  $ ' ' `R.cons` last fields
  p2 <- pointB
  newlineB
  addOverlayB (mkOverlay "dired" (mkRegion p1 p2) sty "")
  return (p1, p2, R.toString filenm)

data DRStrings = DRPerms {undrs :: R.YiString}
               | DRLinks {undrs :: R.YiString}
               | DROwners {undrs :: R.YiString}
               | DRGroups {undrs :: R.YiString}
               | DRSizes {undrs :: R.YiString}
               | DRDates {undrs :: R.YiString}
               | DRFiles {undrs :: R.YiString}

-- | Return a List of (prefix,
-- fullDisplayNameIncludingSourceAndDestOfLink, style, filename)
linesToDisplay :: DiredState -> [([DRStrings], StyleName, R.YiString)]
linesToDisplay dState = map (uncurry lineToDisplay) (M.assocs entries)
  where
    entries = diredEntries dState

    lineToDisplay k (DiredFile v)      =
      (l " -" v <> [DRFiles k], defaultStyle, k)
    lineToDisplay k (DiredDir v)       =
      (l " d" v <> [DRFiles k], const (withFg blue), k)
    lineToDisplay k (DiredSymLink v s) =
      (l " l" v <> [DRFiles $ k <> " -> " <> s], const (withFg cyan), k)
    lineToDisplay k (DiredSocket v) =
      (l " s" v <> [DRFiles k], const (withFg magenta), k)
    lineToDisplay k (DiredCharacterDevice v) =
      (l " c" v <> [DRFiles k], const (withFg yellow), k)
    lineToDisplay k (DiredBlockDevice v) =
      (l " b" v <> [DRFiles k], const (withFg yellow), k)
    lineToDisplay k (DiredNamedPipe v) =
      (l " p" v <> [DRFiles k], const (withFg brown), k)
    lineToDisplay k DiredNoInfo        =
      ([DRFiles $ k <> " : Not a file/dir/symlink"], defaultStyle, k)

    l pre v = [DRPerms $ pre <> permString v,
               DRLinks . R.fromString $ printf "%4d" (numLinks v),
               DROwners $ owner v,
               DRGroups $ grp v,
               DRSizes . R.fromString $ printf "%8d" (sizeInBytes v),
               DRDates $ modificationTimeString v]

-- | Return dired entries for the contents of the supplied directory
diredScanDir :: FilePath -> IO DiredEntries
diredScanDir dir = do
    files <- getDirectoryContents dir
    foldM (lineForFile dir) M.empty files
    where
    lineForFile :: FilePath
                -> DiredEntries
                -> FilePath
                -> IO DiredEntries
    lineForFile d m f = do
      let fp = d </> f
      fileStatus <- getSymbolicLinkStatus fp
      dfi <- lineForFilePath fp fileStatus
      let islink = isSymbolicLink fileStatus
      linkTarget <- if islink then readSymbolicLink fp else return mempty
      let de
            | isDirectory fileStatus = DiredDir dfi
            | isRegularFile fileStatus = DiredFile dfi
            | islink = DiredSymLink dfi (R.fromString linkTarget)
            | isSocket fileStatus = DiredSocket dfi
            | isCharacterDevice fileStatus = DiredCharacterDevice dfi
            | isBlockDevice fileStatus = DiredBlockDevice dfi
            | isNamedPipe fileStatus = DiredNamedPipe dfi
            | otherwise = DiredNoInfo
      return $ M.insert (R.fromString f) de m

    lineForFilePath :: FilePath -> FileStatus -> IO DiredFileInfo
    lineForFilePath fp fileStatus = do
      let modTimeStr = R.fromString . shortCalendarTimeToString
                       . posixSecondsToUTCTime . realToFrac
                       $ modificationTime fileStatus
      let uid = fileOwner fileStatus
          gid = fileGroup fileStatus
          fn = takeFileName fp
      _filenm <- if isSymbolicLink fileStatus
                 then return . ((fn <> " -> ") <>) =<< readSymbolicLink fp
                 else return fn
      ownerEntry <- orException (getUserEntryForID uid)
                    (fmap (scanForUid uid) getAllUserEntries)
      groupEntry <- orException (getGroupEntryForID gid)
                    (fmap (scanForGid gid) getAllGroupEntries)
      let fmodeStr = (modeString . fileMode) fileStatus
          sz = toInteger $ fileSize fileStatus
          ownerStr   = R.fromString $ userName ownerEntry
          groupStr   = R.fromString $ groupName groupEntry
          numOfLinks = toInteger $ linkCount fileStatus
      return DiredFileInfo { permString = fmodeStr
                           , numLinks = numOfLinks
                           , owner = ownerStr
                           , grp = groupStr
                           , sizeInBytes = sz
                           , modificationTimeString = modTimeStr}


-- | Needed on Mac OS X 10.4
scanForUid :: UserID -> [UserEntry] -> UserEntry
scanForUid uid entries = fromMaybe missingEntry $
                                   find ((uid ==) . userID) entries
  where
    missingEntry = UserEntry "?" mempty uid 0 mempty mempty mempty

-- | Needed on Mac OS X 10.4
scanForGid :: GroupID -> [GroupEntry] -> GroupEntry
scanForGid gid entries = fromMaybe missingEntry $
                                   find ((gid ==) . groupID) entries
  where
    missingEntry = GroupEntry "?" mempty gid mempty


modeString :: FileMode -> R.YiString
modeString fm = ""
                <> strIfSet "r" ownerReadMode
                <> strIfSet "w" ownerWriteMode
                <> strIfSet "x" ownerExecuteMode
                <> strIfSet "r" groupReadMode
                <> strIfSet "w" groupWriteMode
                <> strIfSet "x" groupExecuteMode
                <> strIfSet "r" otherReadMode
                <> strIfSet "w" otherWriteMode
                <> strIfSet "x" otherExecuteMode
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

-- | Generic mark toggler.
diredMarkKind :: Maybe Int
                 -- ^ universal argument, usually indicating whether
                 -- to mark or unmark. Here ‘Just …’ is taken as
                 -- unmark.
              -> (DiredFilePath -> DiredEntry -> Bool)
                 -- ^ Picks which entries to consider
              -> Char
                 -- ^ Character used for marking. Pass garbage if
                 -- unmarking.
              -> BufferM ()
diredMarkKind m p c = bypassReadOnly $ do
  dState <- getBufferDyn
  let es = M.assocs $ diredEntries dState
      ms = M.fromList [ (R.toString fp, c) | (fp, e) <- es, p fp e ]
  putBufferDyn (dState & diredMarksA %~ run ms)
  diredRefreshMark
  where
    run :: M.Map FilePath Char -> M.Map FilePath Char -> M.Map FilePath Char
    run ms cms = case m of
      Nothing -> M.union ms cms
      Just _ -> deleteKeys cms (M.keys ms)

diredMarkSymlinks :: Maybe Int -> BufferM ()
diredMarkSymlinks m = diredMarkKind m p '*'
  where
    p _ DiredSymLink {} = True
    p _ _ = False

diredMarkDirectories :: Maybe Int -> BufferM ()
diredMarkDirectories m = diredMarkKind m p '*'
  where
    p "." DiredDir {} = False
    p ".." DiredDir {} = False
    p _ DiredDir {} = True
    p _ _ = False

diredToggleAllMarks :: BufferM ()
diredToggleAllMarks = bypassReadOnly $ do
  dState <- getBufferDyn
  let es = diredEntries dState
  putBufferDyn (dState & diredMarksA %~ tm es)
  diredRefreshMark
  where
    -- Get all entries, filter out the ones that are marked already,
    -- then mark everything that remains, in effect toggling the
    -- marks.
    tm :: DiredEntries -> M.Map FilePath Char -> M.Map FilePath Char
    tm de ms = let unmarked = deleteKeys (M.mapKeys R.toString de) (M.keys ms)
               in M.map (const '*') unmarked

-- | Delete all the keys from the map.
deleteKeys :: Ord k => M.Map k v -> [k] -> M.Map k v
deleteKeys = foldl' (flip M.delete)

diredMarkWithChar :: Char -> BufferM () -> BufferM ()
diredMarkWithChar c mv = bypassReadOnly $
  fileFromPoint >>= \case
    Just (fn, _de) -> do
      state <- getBufferDyn
      putBufferDyn (state & diredMarksA %~ M.insert fn c)
      filenameColOf mv
      diredRefreshMark
    Nothing -> filenameColOf mv

diredRefreshMark :: BufferM ()
diredRefreshMark = do
  b <- pointB
  dState <- getBufferDyn
  let posDict = diredFilePoints dState
      markMap = diredMarks dState
      draw (pos, _, fn) = case M.lookup fn markMap of
        Just mark -> do
          moveTo pos >> moveToSol >> insertB mark >> deleteN 1
          e <- pointB
          addOverlayB $
            mkOverlay "dired" (mkRegion (e - 1) e) (styleOfMark mark) ""
        Nothing ->
          -- for deleted marks
          moveTo pos >> moveToSol >> insertN " " >> deleteN 1
  mapM_ draw posDict
  moveTo b
    where
      styleOfMark '*' = const (withFg green)
      styleOfMark 'D' = const (withFg red)
      styleOfMark  _  = defaultStyle

-- | Removes mark from current file (if any) and moves in the
-- specified direction.
diredUnmark :: Direction -- ^ Direction to move in after unmarking
            -> BufferM ()
diredUnmark d = bypassReadOnly $ do
  let lineDir = case d of { Forward -> lineDown; Backward -> lineUp; }
  fileFromPoint >>= \case
    Just (fn, _de) -> do
      diredUnmarkPath fn
      filenameColOf lineDir
      diredRefreshMark
    Nothing        -> filenameColOf lineDir


diredUnmarkPath :: FilePath -> BufferM()
diredUnmarkPath fn = getBufferDyn >>= putBufferDyn.(diredMarksA %~ M.delete fn)

diredUnmarkAll :: BufferM ()
diredUnmarkAll = bypassReadOnly $ do
                   getBufferDyn >>= putBufferDyn.(diredMarksA .~ M.empty)
                   filenameColOf $ return ()
                   diredRefreshMark

currentDir :: YiM FilePath
currentDir = diredPath <$> withCurrentBuffer getBufferDyn

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
askRenameFiles dir fs = case fs of
  [_x] -> do resetDiredOpState
             procDiredOp True [DOInput prompt sOpIsDir]
  _x:_ -> do resetDiredOpState
             procDiredOp True [DOInput prompt mOpIsDirAndExists]
  []   ->    procDiredOp True [DOFeedback showNothing]
  where
    mkErr t = return . DOFeedback . const $ errorEditor t
    prompt = "Move " <> R.fromString (show total) <> " item(s) to:"
    mOpIsDirAndExists t = [DOCheck (doesDirectoryExist t) posOps negOps]
      where
        posOps = map builder fs <> [DOFeedback showResult]
        negOps = mkErr $ T.pack t <> " is not directory!"
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
                        p = "Cannot move " <> T.pack old
                            <> " to " <> T.pack new
                        negOps = mkErr p
                        new = t
                        old = dir </> fn
                        ps = dropTrailingPathSeparator t
                        ckParentDir = doesDirectoryExist $ takeDirectory ps
    showResult st = do
      diredRefresh
      printMsg $ showT (st ^. diredOpSucCnt) <> " of "
                  <> showT total <> " item(s) moved."
    showNothing _ = printMsg "Quit"
    total = length fs

-- | copy selected files in a given directory to the target location given
-- by user input
--
-- askCopyFiles follow the same logic as askRenameFiles,
-- except dir and file are done by different DiredOP
askCopyFiles :: FilePath -> [(FilePath, DiredEntry)] -> YiM ()
askCopyFiles dir fs =
  case fs of
    [_x] -> do resetDiredOpState
               procDiredOp True [DOInput prompt sOpIsDir]
    _x:_ -> do resetDiredOpState
               procDiredOp True [DOInput prompt mOpIsDirAndExists]
    []   ->    procDiredOp True [DOFeedback showNothing]
  where
    prompt = "Copy " <> R.fromString (show total) <> " item(s) to:"
    mOpIsDirAndExists t = [DOCheck (doesDirectoryExist t) posOps negOps]
      where
        posOps = map builder fs <> [DOFeedback showResult]
        negOps = [DOFeedback . const $
                  errorEditor (T.pack t <> " is not directory!")]
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
                      p = "Cannot copy " <> T.pack old <> " to " <> T.pack new
                      negOps =
                          [DOFeedback . const $ errorEditor p]
                      new = t
                      old = dir </> fn
                      ckParentDir = doesDirectoryExist $
                                    takeDirectory (dropTrailingPathSeparator t)
    showResult st = do
      diredRefresh
      printMsg $ showT (st ^. diredOpSucCnt) <> " of "
                  <> showT total <> " item(s) copied."
    showNothing _ = printMsg "Quit"
    total = length fs
    op4Type :: DiredEntry -> FilePath -> FilePath -> DiredOp
    op4Type (DiredDir _) = DOCopyDir
    op4Type _            = DOCopyFile

diredRename :: YiM ()
diredRename = do
  dir <- currentDir
  fs <- markedFiles (== '*')
  if null fs then do maybefile <- withCurrentBuffer fileFromPoint
                     case maybefile of
                       Just (fn, de) -> askRenameFiles dir [(fn, de)]
                       Nothing       -> noFileAtThisLine
             else askRenameFiles dir fs

diredCopy :: YiM ()
diredCopy = do
  dir <- currentDir
  fs <- markedFiles (== '*')
  if null fs then do maybefile <- withCurrentBuffer fileFromPoint
                     case maybefile of
                       Just (fn, de) -> askCopyFiles dir [(fn, de)]
                       Nothing       -> noFileAtThisLine
             else askCopyFiles dir fs

diredLoad :: YiM ()
diredLoad = do
  dir <- currentDir
  withCurrentBuffer fileFromPoint >>= \case
    Just (fn, de) -> do
      let sel = dir </> fn
          sel' = T.pack sel
      case de of
        (DiredFile _dfi) -> do
          exists <- io $ doesFileExist sel
          if exists
            then void $ editFile sel
            else printMsg $ sel' <> " no longer exists"
        (DiredDir _dfi)  -> do
          exists <- io $ doesDirectoryExist sel
          if exists
            then diredDir sel
            else printMsg $ sel' <> " no longer exists"
        (DiredSymLink _dfi dest') -> do
          let dest = R.toString dest'
              target = if isAbsolute dest then dest else dir </> dest
          existsFile <- io $ doesFileExist target
          existsDir <- io $ doesDirectoryExist target
          printMsg $ "Following link:" <> T.pack target
          if existsFile then void $ editFile target else
            if existsDir then diredDir target else
              printMsg $ T.pack target <> " does not exist"
        (DiredSocket _dfi) -> do
          exists <- io $ doesFileExist sel
          printMsg (if exists
                    then "Can't open Socket " <> sel'
                    else sel' <> " no longer exists")
        (DiredBlockDevice _dfi) -> do
          exists <- io $ doesFileExist sel
          printMsg (if exists
                    then "Can't open Block Device " <> sel'
                    else sel' <> " no longer exists")
        (DiredCharacterDevice _dfi) -> do
          exists <- io $ doesFileExist sel
          printMsg (if exists
                    then "Can't open Character Device " <> sel'
                    else sel' <> " no longer exists")
        (DiredNamedPipe _dfi) -> do
          exists <- io $ doesFileExist sel
          printMsg (if exists
                    then "Can't open Pipe " <> sel'
                    else sel' <> " no longer exists")
        DiredNoInfo -> printMsg $ "No File Info for:" <> sel'
    Nothing        -> noFileAtThisLine


noFileAtThisLine :: YiM ()
noFileAtThisLine = printMsg "(No file at this line)"

-- | Extract the filename at point. NB this may fail if the buffer has
-- been edited. Maybe use Markers instead.
fileFromPoint :: BufferM (Maybe (FilePath, DiredEntry))
fileFromPoint = do
    p <- pointB
    dState <- getBufferDyn
    let candidates = filter (\(_,p2,_)->p <= p2) (diredFilePoints dState)
        finddef f = M.findWithDefault DiredNoInfo (R.fromString f)
    return $ case candidates of
      ((_, _, f):_) -> Just (f, finddef f $ diredEntries dState)
      _             -> Nothing

markedFiles :: (Char -> Bool) -> YiM [(FilePath, DiredEntry)]
markedFiles cond = do
  dState <- withCurrentBuffer getBufferDyn
  let fs = fst . unzip $ filter (cond . snd) (M.assocs $ diredMarks dState)
  return $ map (\f -> (f, diredEntries dState M.! R.fromString f)) fs

diredUpDir :: YiM ()
diredUpDir = do
    dir <- currentDir
    diredDir $ takeDirectory dir

diredCreateDir :: YiM ()
diredCreateDir =
  withMinibufferFree "Create Dir:" $ \nm -> do
    dir <- currentDir
    let newdir = dir </> T.unpack nm
    printMsg $ "Creating " <> T.pack newdir <> "..."
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
             | DOConfirm R.YiString [DiredOp] [DiredOp]
             -- ^ prompt a "yes/no" question. If yes, execute the
             -- first list of embedded DiredOps otherwise execute the
             -- second list of embedded DiredOps
             | DOCheck (IO Bool) [DiredOp] [DiredOp]
             -- ^ similar to DOConfirm, but no user interaction. Could
             -- be used to check file existence
             | DOCkOverwrite FilePath DiredOp
             -- ^ this is a shortcut, it invokes DCChoice if file exists
             | DOInput R.YiString (String -> [DiredOp])
             -- ^ prompt a string and collect user input.
             -- the embedded list of DiredOps is generated based on input,
             -- Remember that the input should be checked with DOCheck
             | DOChoice R.YiString DiredOp
             -- ^ prompt a string, provide keybindings for 'y', 'n',
             -- '!', 'q' and optional 'h' (help) this is useful when
             -- overwriting of existing files is required to complete
             -- the op choice '!' will bypass following DOChoice
             -- prompts.
             | DOFeedback (DiredOpState -> YiM ())
             -- ^ to feedback, given the state. such as show the result.
             | DONoOp
             -- ^ no operation
