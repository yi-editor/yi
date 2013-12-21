{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, TemplateHaskell #-}

-- | A module for CTags integration

module Yi.Tag
  (
   lookupTag,
   importTagTable,
   hintTags,
   completeTag,
   Tag,
   TagTable(..),
   getTags,
   setTags,
   resetTags,
   getTagsFileList,
   setTagsFileList
  )
where

{- Standard Library Module Imports -}
import Prelude (map, words, lines, readFile, reads)
import Yi.Prelude
import Yi.Editor
import Yi.Dynamic

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS8
import Data.Maybe (mapMaybe)
import Data.List (isPrefixOf)
import System.FilePath (takeFileName, takeDirectory, FilePath, (</>))
import System.FriendlyPath
import Data.Map (Map, fromList, lookup, keys)
import Data.List.Split (splitOn)

import qualified Data.Trie as Trie
import Data.Binary
import Data.DeriveTH
import Data.Default

newtype Tags  = Tags (Maybe TagTable) deriving Typeable
instance Default Tags where
    def = Tags Nothing

newtype TagsFileList  = TagsFileList [FilePath] deriving Typeable
instance Default TagsFileList where
    def = TagsFileList ["tags"]

type Tag = String

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
  return $ (tagBaseDir tagTable </> file, line)

-- | Super simple parsing CTag format 1 parsing algorithm
-- TODO: support search patterns in addition to lineno
readCTags :: String -> Map Tag (FilePath, Int)
readCTags =
    fromList . mapMaybe (parseTagLine . words) . lines
    where parseTagLine (tag:tagfile:lineno:_) =
              -- remove ctag control lines
              if "!_TAG_" `isPrefixOf` tag then Nothing
              else Just (tag, (tagfile, fst . head . reads $ lineno))
          parseTagLine _ = Nothing

-- | Read in a tag file from the system
importTagTable :: FilePath -> IO TagTable
importTagTable filename = do
  friendlyName <-  expandTilda filename
  tagStr <- fmap BS8.toString $ BS.readFile friendlyName
  let ctags = readCTags tagStr
  return $ TagTable { tagFileName = takeFileName filename,
                      tagBaseDir  = takeDirectory filename,
                      tagFileMap  = ctags,
                      tagTrie     = Trie.fromList $ keys ctags
                    }

-- | Gives all the possible expanded tags that could match a given @prefix@
hintTags :: TagTable -> String -> [String]
hintTags tags prefix = map (prefix ++) $ Trie.possibleSuffixes prefix $ tagTrie tags

-- | Extends the string to the longest certain length
completeTag :: TagTable -> String -> String
completeTag tags prefix = prefix ++ (Trie.certainSuffix prefix $ tagTrie tags)


-- ---------------------------------------------------------------------
-- Direct access interface to TagTable.

-- | Set a new TagTable
setTags :: TagTable -> EditorM ()
setTags = setDynamic . Tags . Just

-- | Reset the TagTable
resetTags :: EditorM ()
resetTags = setDynamic $ Tags Nothing

-- | Get the currently registered tag table
getTags :: EditorM (Maybe TagTable)
getTags = do
  Tags t <- getDynamic
  return t

setTagsFileList :: String -> EditorM ()
setTagsFileList fps = do
  resetTags
  setDynamic $ TagsFileList (splitOn "," fps)

getTagsFileList :: EditorM [FilePath]
getTagsFileList = do
  TagsFileList fps <- getDynamic
  return fps

-- template haskell at the end to avoid 'not found' compilation errors
$(derives [makeBinary] [''Tags, ''TagTable, ''TagsFileList])

-- For GHC 7.0 with template-haskell 2.5 (at least on my computer - coconnor) the Binary instance
-- needs to be defined before the YiVariable instance.
--
-- GHC 7.1 does not appear to have this issue.
instance YiVariable Tags

instance YiVariable TagsFileList

