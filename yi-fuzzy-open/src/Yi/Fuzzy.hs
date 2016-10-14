{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.State (gets)
import           Data.Binary
import           Data.Default
import           Data.List (isSuffixOf)
import qualified Data.Map.Strict as M
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Typeable
import qualified Data.Vector as V
import           GHC.Generics
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath ((</>))
import           System.IO.Error
import           Yi
import           Yi.Completion
import           Yi.MiniBuffer
import qualified Yi.Rope as R
import           Yi.Types
import           Yi.Utils ()

-- The following import is a hack which silences redundant import
-- warnings on recent (4.8.0.0) base
import          Prelude

-- FuzzyState is stored in minibuffer's dynamic state
data FuzzyState = FuzzyState
    { _fsItems :: !(V.Vector FuzzyItem)
    , fsSelectedIndex :: !(Maybe Int)
    , fsNeedle :: !T.Text
    } deriving (Show, Generic, Typeable)

data FuzzyItem
    = FileItem { _filePath :: !FilePath }
    | BufferItem { _bufferIdent :: !BufferId }
    deriving (Show, Typeable)

-- TODO: make subsequenceMatch work on Text
itemToString :: FuzzyItem -> String
itemToString (FileItem x) = x
itemToString (BufferItem (MemBuffer x))  = T.unpack x
itemToString (BufferItem (FileBuffer x))  = x

-- | The depth 'fuzzyOpen' should traverse by default. Currently
-- __5__.
defaultDepth :: Int
defaultDepth = 5

-- | Fuzzy open the current directory. The depth searched is
-- 'defaultDepth', use fuzzyOpenWithDepth if you want to customise
-- this.
fuzzyOpen :: YiM ()
fuzzyOpen = fuzzyOpenWithDepth defaultDepth

-- | Fuzzy-opens the directory to the specified depth. The depth needs
-- to be at least @1@ for it to do anything meaningful.
fuzzyOpenWithDepth :: Int -> YiM ()
fuzzyOpenWithDepth d = case () of
  _ | d <= 0 -> printMsg "You need at least depth of 1 for fuzzyOpenWithDepth"
    | otherwise -> do
    fileList <- fmap (fmap FileItem)
                     (liftBase (getRecursiveContents d "."))
    bufList <- fmap (fmap (BufferItem . ident . attributes))
                    (withEditor (gets (M.elems . buffers)))
    promptRef <- withEditor (spawnMinibufferE "" (const localKeymap))
    let initialState =
            FuzzyState (fileList <> V.fromList bufList)
                       (Just 0)
                       ""
    withGivenBuffer promptRef $ do
        putBufferDyn initialState
    withEditor (renderE initialState)

-- shamelessly stolen from Chapter 9 of Real World Haskell
-- takes about 3 seconds to traverse linux kernel, which is not too outrageous
-- TODO: check if it works at all with cyclic links
-- TODO: perform in background, limit file count or directory depth
getRecursiveContents :: Int -> FilePath -> IO (V.Vector FilePath)
getRecursiveContents d _ | d <= 0 = return mempty
getRecursiveContents d t = tryIOError (getDirectoryContents t) >>= \case
  Left _ -> return mempty
  Right names -> do
    let properNames = filter predicate names

        predicate :: FilePath -> Bool
        predicate fileName = and
            [ fileName `notElem` [".", "..", ".git", ".svn"]
            , not (".hi" `isSuffixOf` fileName)
            , not ("-boot" `isSuffixOf` fileName)
            ]
    paths <- forM properNames $ \name -> do
        let path = t </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents (d - 1) path
            else return $ V.singleton path
    return $ mconcat paths

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
        , ctrlCh 'p'  ?>>! modifyE decrementIndex
        , spec KUp    ?>>! modifyE decrementIndex
        , ctrlCh 'n'  ?>>! modifyE incrementIndex
        , spec KDown  ?>>! modifyE incrementIndex
        , ctrlCh 'w'  ?>>! updatingB (deleteB unitWord Backward)
        , ctrlCh 'u'  ?>>! updatingB (moveToSol >> deleteToEol)
        , ctrlCh 'k'  ?>>! updatingB deleteToEol
        ]
    <|| (insertChar >>! ((withCurrentBuffer updateNeedleB) >>= renderE))
    where updatingB :: BufferM () -> EditorM ()
          updatingB bufAction = withCurrentBuffer (bufAction >> updateNeedleB) >>= renderE

updateNeedleB :: BufferM FuzzyState
updateNeedleB = do
   needle <- R.toText <$> readLnB
   oldState <- getBufferDyn
   let intermediateState = oldState { fsNeedle = needle }
       newState = intermediateState
           { fsSelectedIndex =
               case V.toList (filteredItems intermediateState) of
                   [] -> Nothing
                   (_, index) : _ -> Just index
           }
   putBufferDyn newState
   return newState

filteredItems :: FuzzyState -> (V.Vector (FuzzyItem, Int))
filteredItems (FuzzyState items _ needle) =
    V.filter (subsequenceMatch (T.unpack needle) . itemToString . fst)
             (V.zip items (V.enumFromTo 0 (V.length items)))

modifyE :: (FuzzyState -> FuzzyState) -> EditorM ()
modifyE f = do
    prevState <- withCurrentBuffer getBufferDyn
    let newState = f prevState
    withCurrentBuffer (putBufferDyn newState)
    renderE newState

incrementIndex :: FuzzyState -> FuzzyState
incrementIndex fs@(FuzzyState _ Nothing _) = fs
incrementIndex fs@(FuzzyState _ (Just index) _) =
    let fitems = filteredItems fs
        steps = V.zipWith (\x y -> (snd x, snd y)) fitems (V.tail fitems)
        newIndex =  case V.find ((== index) . fst) steps of
            Nothing -> Just index
            Just (_, nextIndex) -> Just nextIndex
    in fs { fsSelectedIndex = newIndex }

decrementIndex :: FuzzyState -> FuzzyState
decrementIndex fs@(FuzzyState _ Nothing _) = fs
decrementIndex fs@(FuzzyState _ (Just index) _) =
    let fitems = filteredItems fs
        steps = V.zipWith (\x y -> (snd x, snd y)) (V.tail fitems) fitems
        newIndex = case V.find ((== index) . fst) steps of
            Nothing -> Just index
            Just (_, prevIndex) -> Just prevIndex
    in fs { fsSelectedIndex = newIndex }

renderE :: FuzzyState -> EditorM ()
renderE fs@(FuzzyState _ selIndex _) = do
    let content = V.toList (fmap renderItem (filteredItems fs))
        -- TODO justify to actual screen width
        renderItem (item, itemIndex) = (T.justifyLeft 79 ' ' . mconcat)
            [ (if Just itemIndex == selIndex then "* " else "  ")
            , renderItem' item
            ]
        renderItem' (FileItem x) = "File  " <> T.pack x
        renderItem' (BufferItem (MemBuffer x)) = "Buffer  " <> x
        renderItem' (BufferItem (FileBuffer x)) = "Buffer  " <> T.pack x
    setStatus (content, defaultStyle)

openInThisWindow :: YiM ()
openInThisWindow = openRoutine (return ())

openInSplit :: YiM ()
openInSplit = openRoutine splitE

openInNewTab :: YiM ()
openInNewTab = openRoutine newTabE

openRoutine :: EditorM () -> YiM ()
openRoutine preOpenAction = do
    FuzzyState items mselIndex _ <- withCurrentBuffer getBufferDyn
    case mselIndex of
        Nothing -> printMsg "Nothing selected"
        Just selIndex -> do
            let action = case items V.! selIndex of
                    FileItem x -> void (editFile x)
                    BufferItem x -> withEditor $ do
                        bufs <- gets (M.assocs . buffers)
                        case filter ((== x) . ident . attributes . snd) bufs of
                            [] -> error ("Couldn't find buffer" <> show x)
                            (bufRef, _) : _ -> switchToBufferE bufRef
            withEditor $ do
                cleanupE
                preOpenAction
            action

insertChar :: Keymap
insertChar = textChar >>= write . insertB

cleanupE :: EditorM ()
cleanupE = clrStatus >> closeBufferAndWindowE

instance Binary FuzzyItem where
    put (FileItem x) = put (0 :: Int) >> put x
    put (BufferItem x) = put (1 :: Int) >> put x
    get = do
        tag :: Int <- get
        case tag of
            0 -> liftM FileItem get
            1 -> liftM BufferItem get
            _ -> error "Unexpected FuzzyItem Binary."

instance Binary FuzzyState where
    put (FuzzyState items index needle) = do
        put (V.length items)
        V.mapM_ put items
        put index
        put (T.encodeUtf8 needle)
    get = do
        itemCount <- get
        items <- liftM V.fromList (replicateM itemCount get)
        liftM2 (FuzzyState items) get (liftM T.decodeUtf8 get)

instance Default FuzzyState where
    def = FuzzyState mempty Nothing mempty

instance YiVariable FuzzyState
