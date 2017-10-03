{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Fuzzy
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

--   TODO if need arises: factor out generic part that captures a pattern of
--   having an interactive minibuffer and a window that just renders some state.

module Yi.Fuzzy (fuzzyOpen, fuzzyOpenWithDepth, defaultDepth) where

import Control.Monad (void)
import Control.Monad.Base (liftBase)
import Control.Monad.State (gets)
import Data.Binary (Binary(..), Word8)
import Data.Default (Default(..))
import Data.List (isSuffixOf)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO.Error (tryIOError)
import GHC.Exts (IsList(..))

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as M

import Yi
import Yi.Completion
import Yi.MiniBuffer
import Yi.Types
import Yi.Utils ()
import qualified Yi.Rope as R

import Yi.Fuzzy.Zipper (Zipper(..))
import qualified Yi.Fuzzy.Zipper as Z

-- FuzzyState is stored in minibuffer's dynamic state
data FuzzyState = FuzzyState
  { items :: !(Zipper FuzzyItem)
  , search :: !Text
  } deriving (Show, Generic, Typeable)

data FuzzyItem
  = FileItem !Text
  | BufferItem !BufferId
  deriving (Typeable)

instance Show FuzzyItem where
  -- TODO: make subsequenceMatch work on Text
  show :: FuzzyItem -> String
  show = \case
    i@(FileItem   _) -> "File  "   <> itemAsStr i
    i@(BufferItem _) -> "Buffer  " <> itemAsStr i

itemAsTxt :: FuzzyItem -> Text
itemAsTxt = \case
  FileItem   x              -> x
  BufferItem (MemBuffer  x) -> x
  BufferItem (FileBuffer x) -> T.pack x

itemAsStr :: FuzzyItem -> String
itemAsStr = T.unpack . itemAsTxt

-- | The depth 'fuzzyOpen' should traverse by default. Currently __5__.
defaultDepth :: Natural
defaultDepth = 5

-- | Fuzzy open the current directory. The depth searched is
-- 'defaultDepth', use fuzzyOpenWithDepth if you want to customise
-- this.
fuzzyOpen :: YiM ()
fuzzyOpen = fuzzyOpenWithDepth defaultDepth

-- | Fuzzy-opens the directory to the specified depth. The depth needs
-- to be at least @1@ for it to do anything meaningful.
fuzzyOpenWithDepth :: Natural -> YiM ()
fuzzyOpenWithDepth (fromIntegral->d) = do
  fileList <- (fmap.fmap) (FileItem . T.pack) (liftBase $ getRecursiveContents d ".")
  bufList  <- (fmap.fmap) (BufferItem . ident . attributes) (withEditor (gets (M.elems . buffers)))
  promptRef <- withEditor (spawnMinibufferE "" (const localKeymap))

  let initialState = FuzzyState (fromList (filterNotCommon bufList<>fileList)) ""
  withGivenBuffer promptRef $ putBufferDyn initialState
  withEditor (renderE initialState)
  where
    filterNotCommon :: [FuzzyItem] -> [FuzzyItem]
    filterNotCommon = filter ((/="console") . itemAsTxt)


-- shamelessly stolen from Chapter 9 of Real World Haskell
-- takes about 3 seconds to traverse linux kernel, which is not too outrageous
-- TODO: check if it works at all with cyclic links
-- TODO: perform in background, limit file count or directory depth
getRecursiveContents :: Natural -> FilePath -> IO [FilePath]
getRecursiveContents d t
  | d == 0    = return mempty
  | otherwise = tryIOError (getDirectoryContents t) >>= \case
    Left  _     -> return mempty
    Right names -> do
      paths <- mapM withName (filter isProperName names)
      return $ mconcat paths

  where
    isProperName :: FilePath -> Bool
    isProperName fileName
      = fileName `notElem` [".", "..", ".git", ".svn"]
      && not (".hi" `isSuffixOf` fileName)
      && not ("-boot" `isSuffixOf` fileName)

    withName :: FilePath -> IO [FilePath]
    withName name = do
      let path = t </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory
      then getRecursiveContents (d - 1) path
      else pure [path]

localKeymap :: Keymap
localKeymap =
  choice
      [ spec KEnter ?>>! openInThisWindow
      , ctrlCh 't'  ?>>! openInNewTab
      , ctrlCh 's'  ?>>! openInSplit
      , spec KEsc   ?>>! cleanupE
      , ctrlCh 'g'  ?>>! cleanupE
      , ctrlCh 'h'  ?>>! updatingB (deleteB Character Backward)
      , spec KBS    ?>>! updatingB (deleteB Character Backward)
      , spec KDel   ?>>! updatingB (deleteB Character Backward)
      , ctrlCh 'a'  ?>>! moveToSol
      , ctrlCh 'e'  ?>>! moveToEol
      , spec KLeft  ?>>! moveXorSol 1
      , spec KRight ?>>! moveXorEol 1
      , ctrlCh 'p'  ?>>! modifyE goLeft
      , ctrlCh 'n'  ?>>! modifyE goRight
      , spec KDown  ?>>! modifyE goRight

      , Event KTab [MShift] ?>>! modifyE goLeft
      , Event KTab []       ?>>! modifyE goRight

      , ctrlCh 'w'  ?>>! updatingB (deleteB unitWord Backward)
      , ctrlCh 'u'  ?>>! updatingB (moveToSol >> deleteToEol)
      , ctrlCh 'k'  ?>>! updatingB deleteToEol
      ]
    <|| (insertChar >>! (withCurrentBuffer updateNeedleB >>= renderE))
 where
  updatingB :: BufferM () -> EditorM ()
  updatingB bufAction =
    withCurrentBuffer (bufAction >> updateNeedleB) >>= renderE

updateNeedleB :: BufferM FuzzyState
updateNeedleB = do
  s <- R.toText <$> readLnB
  oldState <- getBufferDyn
  let newState = oldState `filterState` s
  putBufferDyn newState
  return newState
  where
    filterState :: FuzzyState -> Text -> FuzzyState
    filterState old s = old
      { search = s
      , items = Z.toStart (filterItems (items old) s)
      }

filterItems :: Zipper FuzzyItem -> Text -> Zipper FuzzyItem
filterItems zipper s = Z.filter matchesSearch zipper
  where
    matchesSearch :: FuzzyItem -> Bool
    matchesSearch = subsequenceTextMatch s . itemAsTxt

modifyE :: (FuzzyState -> FuzzyState) -> EditorM ()
modifyE f = do
  prevState <- withCurrentBuffer getBufferDyn
  let newState = f prevState
  withCurrentBuffer (putBufferDyn newState)
  renderE newState

goRight :: FuzzyState -> FuzzyState
goRight = changeIndex Z.goRight

goLeft :: FuzzyState -> FuzzyState
goLeft = changeIndex Z.goLeft

changeIndex :: (Zipper FuzzyItem -> Zipper FuzzyItem) -> FuzzyState -> FuzzyState
changeIndex dir fs = fs { items = dir (items fs) }

renderE :: FuzzyState -> EditorM ()
renderE (FuzzyState z s) = setStatus (content, defaultStyle)
  where
    content :: [Text]
    content = toList $ Z.mapWithCurrent ("  "<>) ("* "<>) (renderItem <$> filterItems z s)

    -- TODO justify to actual screen width
    renderItem :: FuzzyItem -> Text
    renderItem = T.justifyLeft 79 ' ' . T.pack . show

openInThisWindow :: YiM ()
openInThisWindow = openRoutine (return ())

openInSplit :: YiM ()
openInSplit = openRoutine splitE

openInNewTab :: YiM ()
openInNewTab = openRoutine newTabE

openRoutine :: EditorM () -> YiM ()
openRoutine preOpenAction = do
  FuzzyState zipper _ <- withCurrentBuffer getBufferDyn
  case Z.current zipper of
    Nothing -> printMsg "Nothing selected"
    Just i -> do
      withEditor $ do
        cleanupE
        preOpenAction
      action i
  where
    action :: FuzzyItem  -> YiM ()
    action = \case
      FileItem   x -> void (editFile (T.unpack x))
      BufferItem x -> withEditor $ do
        bufs <- gets (M.assocs . buffers)
        case filter ((==x) . ident . attributes . snd) bufs of
          []            -> error ("Couldn't find " <> show x)
          (bufRef, _):_ -> switchToBufferE bufRef


insertChar :: Keymap
insertChar = textChar >>= write . insertB

cleanupE :: EditorM ()
cleanupE = clrStatus >> closeBufferAndWindowE

instance Binary FuzzyItem where
  put (FileItem x) = put (0 :: Word8) >> put x
  put (BufferItem x) = put (1 :: Word8) >> put x
  get = do
    tag :: Word8 <- get
    case tag of
      0 -> FileItem <$> get
      1 -> BufferItem <$> get
      _ -> error "Unexpected FuzzyItem Binary."

instance Binary FuzzyState where
  put (FuzzyState zipper s) = do
    put zipper
    put (T.encodeUtf8 s)

  get = FuzzyState <$> get <*> fmap T.decodeUtf8 get

instance Default FuzzyState where
  def = FuzzyState mempty mempty

instance YiVariable FuzzyState
