{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
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
    , nextTag
    , popTag
    , unpopTag
    ) where

import           GHC.Generics (Generic)

import           Control.Applicative ((<$>))
import           Control.Lens        (view)
import           Control.Monad       (foldM, void)
import           Data.Binary         (Binary (..))
import           Data.Default        (Default (..))
import           Data.Maybe          (maybeToList)
import           Data.Monoid         ((<>))
import qualified Data.Text           as T (Text)
import           Data.Typeable       (Typeable)
import           System.Directory    (doesFileExist)
import           System.FilePath     (takeDirectory, (</>))
import           System.FriendlyPath (userToCanonPath)
import           Yi.Buffer
import           Yi.Core             (errorEditor)
import           Yi.Editor
import           Yi.File             (openingNewFile)
import           Yi.Keymap           (YiM)
import           Yi.Tag
import           Yi.Types            (YiVariable)
import           Yi.Utils            (io)

-- | List of tags and the file/line/char that they originate from.
-- (the location that :tag or Ctrl-[ was called from).
data VimTagStack = VimTagStack
    { tagStackList :: [(Tag, Int, FilePath, Int, Int)]
    , tagStackIndex :: Int
    } deriving (Typeable, Generic)

instance Default VimTagStack where
    def = VimTagStack [] 0

instance YiVariable VimTagStack

instance Binary VimTagStack

-- | Returns tag, tag index, filepath, line number, char number
getTagList :: EditorM [(Tag, Int, FilePath, Int, Int)]
getTagList = do
    VimTagStack ts _ <- getEditorDyn
    return ts

getTagIndex :: EditorM Int
getTagIndex = do
    VimTagStack _ ti <- getEditorDyn
    return ti

setTagList :: [(Tag, Int, FilePath, Int, Int)] -> EditorM ()
setTagList tl =  do
    t@(VimTagStack _ _) <- getEditorDyn
    putEditorDyn $ t { tagStackList = tl }

setTagIndex :: Int -> EditorM ()
setTagIndex ti = do
    t@(VimTagStack _ _) <- getEditorDyn
    putEditorDyn $ t { tagStackIndex = ti }

-- | Push tag at index.
pushTagStack :: Tag -> Int -> FilePath -> Int -> Int -> EditorM ()
pushTagStack tag ind fp ln cn = do
    tl <- getTagList
    ti <- getTagIndex
    setTagList $ (take ti tl) ++ [(tag, ind, fp, ln, cn)]
    setTagIndex $ ti + 1

-- | Get tag and decrement index (so that when a new push is done, the current
-- tag is popped)
popTagStack :: EditorM (Maybe (Tag, Int, FilePath, Int, Int))
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
gotoTag :: Tag -> Int -> Maybe (FilePath, Int, Int) -> YiM ()
gotoTag tag ind ret =
    void . visitTagTable $ \tagTable -> do
        let lis = lookupTag tag tagTable
        if (length lis) <= ind
          then errorEditor $ "tag not found: " <> _unTag tag
          else do
            bufinf <- withCurrentBuffer bufInfoB

            let (filename, line) = lis !! ind
                (fn, ln, cn) = case ret of
                   Just ret' -> ret'
                   Nothing -> (bufInfoFileName bufinf, 
                               bufInfoLineNo bufinf, 
                               bufInfoColNo bufinf)
            withEditor $ pushTagStack tag ind fn ln cn
            openingNewFile filename $ gotoLn line

-- | Goes to the next tag. (:tnext)
nextTag :: YiM ()
nextTag = do
    prev <- withEditor popTagStack 
    case prev of
        Nothing -> errorEditor $ "tag stack empty"
        Just (tag, ind, fn, ln, cn) -> gotoTag tag (ind + 1) (Just (fn, ln, cn))

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
                Just (_, _, fn, ln, cn) -> openingNewFile fn $ moveToLineColB ln cn

-- | Go to next tag in the tag stack. Represents :tag without any
-- specified tag.
unpopTag :: YiM ()
unpopTag = do
  tl <- withEditor getTagList
  ti <- withEditor getTagIndex
  if ti >= length tl
    then case tl of
            [] -> errorEditor "tag stack empty"
            _ -> errorEditor "at top of tag stack"
    else let (tag, ind, _, _, _) = tl !! ti
         in void . visitTagTable $ \tagTable -> do
             let lis =  lookupTag tag tagTable
             if (length lis) <= ind
               then errorEditor $ "tag not found: " <> _unTag tag
               else do
                   bufinf <- withCurrentBuffer bufInfoB
                   let (filename, line) = lis !! ind
                       ln = bufInfoLineNo bufinf
                       cn = bufInfoColNo bufinf
                       fn = bufInfoFileName bufinf
                       tl' = take ti tl
                               ++ (tag, ind, fn, ln, cn):(drop (ti + 1) tl)
                   withEditor $ setTagList tl'
                   openingNewFile filename $ gotoLn line

completeVimTag :: T.Text -> YiM [T.Text]
completeVimTag s =
  fmap maybeToList . visitTagTable $ return . flip completeTag s

-- | Gets the first valid tags file in @TagsFileList@, if such a valid
-- file exists.
tagsFile :: YiM (Maybe FilePath)
tagsFile = do
    fs <- view tagsFileList <$> askCfg
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
