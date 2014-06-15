{-# LANGUAGE
  CPP,
  DeriveDataTypeable,
  DeriveGeneric,
  StandaloneDeriving,
  TemplateHaskell #-}
module Yi.Keymap.Vim.Tag
    ( gotoTag
    , visitTagTable
    ) where

import Data.Binary
import Data.Default
import Data.Maybe
import Data.Typeable
import Control.Applicative
import Control.Monad
import System.Directory (doesFileExist)
import System.FilePath
import System.FriendlyPath

#if __GLASGOW_HASKELL__ < 708
import Data.DeriveTH
#else
import GHC.Generics (Generic)
#endif

import Yi.Core
import Yi.File
import Yi.Tag
import Yi.Utils

-- | List of tags and the file/line/char that they originate from.
-- (the location that :tag or Ctrl-[ was called from).
data VimTagStack = VimTagStack {
        tagStackList :: [(Tag, FilePath, Int, Int)]
      , tagStackIndex :: Int }
    deriving Typeable

instance Default VimTagStack where
    def = VimTagStack [] 0

instance YiVariable VimTagStack

#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''VimTagStack)
#else
deriving instance Generic VimTagStack
instance Binary VimTagStack
#endif

getTagList :: EditorM [(Tag, FilePath, Int, Int)]
getTagList = do
    VimTagStack ts _ <- getDynamic
    return ts

getTagIndex :: EditorM Int
getTagIndex = do
    VimTagStack _ ti <- getDynamic
    return ti

setTagList :: [(Tag, FilePath, Int, Int)] -> EditorM ()
setTagList tl =  do
    t@(VimTagStack _ _) <- getDynamic
    setDynamic $ t { tagStackList = tl }

setTagIndex :: Int -> EditorM ()
setTagIndex ti = do
    t@(VimTagStack _ _) <- getDynamic
    setDynamic $ t { tagStackIndex = ti }

-- | Push tag at index.
pushTagStack :: Tag -> FilePath -> Int -> Int -> EditorM ()
pushTagStack tag fp ln cn = do
    tl <- getTagList
    ti <- getTagIndex
    setTagList $ (take ti tl) ++ [(tag, fp, ln, cn)]
    setTagIndex $ ti + 1

-- | Get tag and decrement index (so that when a new push is done, the current
-- tag is popped)
popTagStack :: EditorM (Maybe (Tag, FilePath, Int, Int))
popTagStack = do
    tl <- getTagList
    ti <- getTagIndex
    case tl of
        [] -> return Nothing
        _  -> case ti of
                0 -> return Nothing
                _ -> setTagIndex (ti - 1) >> return (Just $ tl !! (ti - 1))

-- | Opens the file that contains @tag@. Uses the global tag table or uses
-- the first valid tag file in @TagsFileList@.
gotoTag :: Tag -> YiM ()
gotoTag tag =
    void . visitTagTable $ \tagTable ->
        case lookupTag tag tagTable of
          Nothing -> errorEditor $ "tag not found: " ++ tag
          Just (filename, line) -> do
            bufinf <- withBuffer bufInfoB
            let ln = bufInfoLineNo bufinf
                cn = bufInfoColNo bufinf
                fn = bufInfoFileName bufinf
            withEditor $ pushTagStack tag fn ln cn
            void $ editFile filename
            void . withBuffer $ gotoLn line

-- | Gets the first valid tags file in @TagsFileList@, if such a valid
-- file exists.
tagsFile :: YiM (Maybe FilePath)
tagsFile = do
    fs <- withEditor getTagsFileList
    let g f' f = do
        case f' of
            Just _ -> return f'
            Nothing -> tagsFileLocation f
    foldM g Nothing fs

-- | Handles paths of the form ./[path], which represents a tags file relative
-- to the path of the current directory of a file rather than the directory
-- of the process.
tagsFileLocation :: String -> YiM (Maybe FilePath)
tagsFileLocation s
    | length s < 2 || take 2 s /= "./" = check s
    | otherwise = do
       let s' = drop 2 s
       dir <- takeDirectory <$>
                 (withBuffer $ bufInfoB >>= return . bufInfoFileName)
       check $ dir </> s'
    where check f = do
            f' <- io $ userToCanonPath f
            fileExists <- io $ doesFileExist f'
            if fileExists
                then return $ Just f'
                else return Nothing

-- | Call continuation @act@ with the TagTable. Uses the global table
-- or, if it doesn't exist, uses the first valid tag file in
-- @TagsFileList@.
visitTagTable :: (TagTable -> YiM a) -> YiM (Maybe a)
visitTagTable act = do
    posTagTable <- withEditor getTags
    case posTagTable of
        Just tagTable -> Just <$> act tagTable
        Nothing -> do
            f <- tagsFile
            case f of
                Nothing -> errorEditor "No tags file" >> return Nothing
                Just f' -> do
                    tagTable <- io $ importTagTable f'
                    withEditor $ setTags tagTable
                    Just <$> act tagTable
