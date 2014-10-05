{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Tag
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A module for CTags integration. Note that this reads the ‘tags’
-- file produced by @hasktags@, not the ‘TAGS’ file which uses a
-- different format (etags).

module Yi.Tag ( lookupTag
              , importTagTable
              , hintTags
              , completeTag
              , Tag(..)
              , mkTag
              , unTag'
              , TagTable(..)
              , getTags
              , setTags
              , resetTags
              , getTagsFileList
              , setTagsFileList
              ) where

import           Control.Applicative
import           Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS8
#if __GLASGOW_HASKELL__ < 708
import           Data.DeriveTH
#else
import           GHC.Generics (Generic)
#endif
import           Data.Default
import           Data.List (isPrefixOf)
import           Data.List.Split (splitOn)
import           Data.Map (Map, fromList, lookup, keys)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Trie as Trie
import           Data.Typeable
import           System.FilePath (takeFileName, takeDirectory, (</>))
import           System.FriendlyPath
import           Yi.Editor
import           Yi.Types (YiVariable)


newtype Tags  = Tags (Maybe TagTable) deriving Typeable
instance Default Tags where
    def = Tags Nothing

newtype TagsFileList  = TagsFileList [FilePath] deriving Typeable
instance Default TagsFileList where
    def = TagsFileList ["tags"]

newtype Tag = Tag { _unTag :: T.Text } deriving (Show, Eq, Ord)

-- | Helper
mkTag :: String -> Tag
mkTag = Tag . T.pack

unTag' :: Tag -> String
unTag' = T.unpack . _unTag

instance Binary Tag where
  put (Tag t) = put (E.encodeUtf8 t)
  get = Tag . E.decodeUtf8 <$> get

data TagTable = TagTable { tagFileName :: FilePath
                           -- ^ local name of the tag file
                           -- TODO: reload if this file is changed
                           , tagBaseDir :: FilePath
                           -- ^ path to the tag file directory
                           -- tags are relative to this path
                           , tagFileMap :: Map Tag (FilePath, Int)
                           -- ^ map from tags to files
                           , tagTrie :: Trie.Trie
                           -- ^ trie to speed up tag hinting
                         } deriving Typeable

-- | Find the location of a tag using the tag table.
-- Returns a full path and line number
lookupTag :: Tag -> TagTable -> Maybe (FilePath, Int)
lookupTag tag tagTable = do
  (file, line) <- Data.Map.lookup tag $ tagFileMap tagTable
  return (tagBaseDir tagTable </> file, line)

-- | Super simple parsing CTag format 1 parsing algorithm
-- TODO: support search patterns in addition to lineno
readCTags :: String -> Map Tag (FilePath, Int)
readCTags =
    fromList . mapMaybe (parseTagLine . words) . lines
    where parseTagLine (tag:tagfile:lineno:_) =
              -- remove ctag control lines
              if "!_TAG_" `isPrefixOf` tag then Nothing
              else Just (mkTag tag, (tagfile, fst . head . reads $ lineno))
          parseTagLine _ = Nothing

-- | Read in a tag file from the system
importTagTable :: FilePath -> IO TagTable
importTagTable filename = do
  friendlyName <-  expandTilda filename
  tagStr <- fmap BS8.toString $ BS.readFile friendlyName
  let cts = readCTags tagStr
  return TagTable { tagFileName = takeFileName filename
                  , tagBaseDir  = takeDirectory filename
                  , tagFileMap  = cts
                    -- TODO either change word-trie to use Text or
                    -- figure out a better way all together for this
                  , tagTrie = Trie.fromList . map (T.unpack . _unTag) $ keys cts
                  }

-- | Gives all the possible expanded tags that could match a given @prefix@
hintTags :: TagTable -> T.Text -> [T.Text]
hintTags tags prefix = map (T.append prefix . T.pack) sufs
  where
    sufs = Trie.possibleSuffixes (T.unpack prefix) $ tagTrie tags

-- | Extends the string to the longest certain length
completeTag :: TagTable -> T.Text -> T.Text
completeTag tags prefix =
  prefix `T.append` T.pack (Trie.certainSuffix (T.unpack prefix) (tagTrie tags))


-- ---------------------------------------------------------------------
-- Direct access interface to TagTable.

-- | Set a new TagTable
setTags :: TagTable -> EditorM ()
setTags = putEditorDyn . Tags . Just

-- | Reset the TagTable
resetTags :: EditorM ()
resetTags = putEditorDyn $ Tags Nothing

-- | Get the currently registered tag table
getTags :: EditorM (Maybe TagTable)
getTags = do
  Tags t <- getEditorDyn
  return t

setTagsFileList :: String -> EditorM ()
setTagsFileList fps = do
  resetTags
  putEditorDyn $ TagsFileList (splitOn "," fps)

getTagsFileList :: EditorM [FilePath]
getTagsFileList = do
  TagsFileList fps <- getEditorDyn
  return fps

#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''Tags)
$(derive makeBinary ''TagTable)
$(derive makeBinary ''TagsFileList)
#else
deriving instance Generic Tags
deriving instance Generic TagTable
deriving instance Generic TagsFileList
instance Binary Tags
instance Binary TagTable
instance Binary TagsFileList
#endif

instance YiVariable Tags

instance YiVariable TagsFileList
