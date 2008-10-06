
-- | A module for CTags integration

module Yi.Tag
  (
   lookupTag,
   importTagTable,
   hintTags,
   completeTag,
   Tag,
   TagTable(..)
  )
where

{- Standard Library Module Imports -}

import Data.Maybe (mapMaybe)
import Data.List (isPrefixOf)
import System.FilePath (takeFileName, takeDirectory, FilePath, (</>))
import System.FriendlyPath
import Data.Map (Map, fromList, lookup, keys)
import Control.Monad (liftM)


import qualified Data.Trie as Trie


type Tag = String

data TagTable = TagTable { tagFileName:: FilePath
                           -- ^ local name of the tag file
                           -- TODO: reload if this file is change
                           , tagBaseDir :: FilePath
                           -- ^ path to the tag file directory
                           -- tags are relative to this path
                           , tagFileMap :: Map Tag (FilePath, Int)
                           -- ^ map from tags to files
                           , tagTrie :: Trie.Trie
                           -- ^ trie to speed up tag hinting
                         }

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
    where parseTagLine [tag, tagfile, lineno] =
              -- remove ctag control lines
              if "!_TAG_" `isPrefixOf` tag then Nothing
              else Just (tag, (tagfile, read lineno))
          parseTagLine _ = Nothing

-- | Read in a tag file from the system
importTagTable :: FilePath -> IO TagTable
importTagTable filename = do
  friendlyName <-  expandTilda filename
  tagStr <- readFile friendlyName
  let ctags = readCTags tagStr
  return $ TagTable { tagFileName = takeFileName filename,
                      tagBaseDir  = takeDirectory filename,
                      tagFileMap  = ctags,
                      tagTrie     = Trie.fromList $ keys ctags
                    }

-- | Gives all the possible expanded tags that could match a given @prefix
hintTags :: TagTable -> String -> [String]
hintTags tags prefix = map (prefix ++) $ Trie.possibleSuffixes prefix $ tagTrie tags

-- | Extends the string to the longest certain length
completeTag :: TagTable -> String -> String
completeTag tags prefix = prefix ++ (Trie.certainSuffix prefix $ tagTrie tags)
