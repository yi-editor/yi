{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Tag
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Tag
    ( completeVimTag
    , gotoTag
    , popTag
    , unpopTag
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Default
#if __GLASGOW_HASKELL__ < 708
import           Data.DeriveTH
#else
import           GHC.Generics (Generic)
#endif
import           Data.Maybe
import qualified Data.Text as T
import           Data.Typeable
import           System.Directory (doesFileExist)
import           System.FilePath
import           System.FriendlyPath
import           Yi.Buffer
import           Yi.Core (errorEditor)
import           Yi.Dynamic
import           Yi.Editor
import           Yi.File
import           Yi.Keymap
import           Yi.Tag
import           Yi.Utils

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
          Nothing -> errorEditor $ "tag not found: " `T.append` _unTag tag
          Just (filename, line) -> do
            bufinf <- withCurrentBuffer bufInfoB
            let ln = bufInfoLineNo bufinf
                cn = bufInfoColNo bufinf
                fn = bufInfoFileName bufinf
            withEditor $ pushTagStack tag fn ln cn
            void $ editFile filename
            void . withCurrentBuffer $ gotoLn line

-- | Return to location from before last tag jump.
popTag :: YiM ()
popTag = do
    tl <- withEditor getTagList
    case tl of
        [] -> errorEditor "tag stack empty"
        _ -> do
            posloc <- withEditor popTagStack
            case posloc of
                Nothing -> errorEditor "at bottom of tag stack"
                Just (_, fn, ln, cn) -> do
                    void $ editFile fn
                    void . withCurrentBuffer $ moveToLineColB ln cn

-- | Go to next tag in the tag stack. Represents :tag without any
-- specified tag.
unpopTag :: YiM ()
unpopTag = do
  tl <- withEditor getTagList
  ti <- withEditor getTagIndex
  if ti >= length tl
    then case tl of
            [] -> errorEditor "at top of tag stack"
            _ -> errorEditor "tag stack empty"
    else let (tag, _, _, _) = tl !! ti
         in void . visitTagTable $ \tagTable ->
             case lookupTag tag tagTable of
               Nothing -> errorEditor $ "tag not found: " `T.append` _unTag tag
               Just (filename, line) -> do
                   bufinf <- withCurrentBuffer bufInfoB
                   let ln = bufInfoLineNo bufinf
                       cn = bufInfoColNo bufinf
                       fn = bufInfoFileName bufinf
                       tl' = take ti tl
                               ++ (tag, fn, ln, cn):(drop (ti + 1) tl)
                   withEditor $ setTagList tl'
                   void $ editFile filename
                   void . withCurrentBuffer $ gotoLn line

completeVimTag :: T.Text -> YiM [T.Text]
completeVimTag s =
  fmap maybeToList . visitTagTable $ return . flip completeTag s

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
                 (withCurrentBuffer $ bufInfoB >>= return . bufInfoFileName)
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
