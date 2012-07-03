{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}
-- | This module defines a list type and operations on it; it further
-- provides functions which write in and out the list.
-- The goal is to make it easy for the user to store a large number of text buffers
-- and cycle among them, making edits as she goes. The idea is inspired by
-- \"incremental reading\", see <http://en.wikipedia.org/wiki/Incremental_reading>.
module Yi.IReader where

import Control.Monad.State (join)
import Control.Exception
import Prelude hiding (catch)
import Data.Binary (Binary, decode, encodeFile)
import Data.Sequence as S
import Data.Typeable (Typeable)
import System.Directory (getHomeDirectory)
import qualified Data.ByteString.Char8 as B (pack, unpack, readFile, ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL (fromChunks)

import Yi.Buffer.HighLevel (replaceBufferContent, topB)
import Yi.Buffer.Misc (bufferDynamicValueA, BufferM)
import Yi.Buffer.Normal (regionOfB, TextUnit(Document))
import Yi.Buffer.Region (readRegionB)
import Yi.Dynamic
import Yi.Keymap (withBuffer, YiM)
import Yi.Prelude (getA, putA, io, discard, Initializable(..))

type Article = B.ByteString
newtype ArticleDB = ADB { unADB :: Seq Article }
  deriving(Typeable, Binary)

instance Initializable ArticleDB where
    initial = ADB S.empty
instance YiVariable ArticleDB

-- | Take an 'ArticleDB', and return the first 'Article' and an ArticleDB - *without* that article.
split :: ArticleDB -> (Article, ArticleDB)
split (ADB adb) = case viewl adb of
               EmptyL -> (B.pack "", initial)
               (a :< b) -> (a, ADB b)

-- | Get the first article in the list. We use the list to express relative priority;
-- the first is the most, the last least. We then just cycle through - every article gets equal time.
getLatestArticle :: ArticleDB -> Article
getLatestArticle = fst . split -- we only want the article

-- | We remove the old first article, and we stick it on the end of the
-- list using the presumably modified version.
removeSetLast :: ArticleDB -> Article -> ArticleDB
removeSetLast adb old = ADB (unADB (snd (split adb)) |> old)

-- we move the last entry to  the entry 'length `div` n'from the beginning; so 'shift 1' would do nothing
-- (eg. the last index is 50, 50 `div` 1 == 50, so the item would be moved to where it is)
--  'shift 2' will move it to the middle of the list, though; last index = 50, then 50 `div` 2 will shift
-- the item to index 25, and so on down to 50 `div` 50 - the head of the list/Seq.
shift :: Int ->ArticleDB -> ArticleDB
shift n adb = if n < 2 || lst < 2 then adb else ADB $ (r |> lastentry) >< s'
              where lst = S.length (unADB adb) - 1
                    (r,s) = S.splitAt (lst `div` n) (unADB adb)
                    (s' :> lastentry) = S.viewr s

-- | Insert a new article with top priority (that is, at the front of the list).
insertArticle :: ArticleDB -> Article -> ArticleDB
insertArticle (ADB adb) new = ADB (new <| adb)

-- | Serialize given 'ArticleDB' out.
writeDB :: ArticleDB -> YiM ()
writeDB adb = discard $ io . join . fmap (flip encodeFile adb) $ dbLocation

-- | Read in database from 'dbLocation' and then parse it into an 'ArticleDB'.
readDB :: YiM ArticleDB
readDB = io $ (dbLocation >>= r) `catch` returnDefault
          where r = fmap (decode . BL.fromChunks . return) . B.readFile
                -- We read in with strict bytestrings to guarantee the file is closed,
                -- and then we convert it to the lazy bytestring data.binary expects.
                -- This is inefficient, but alas...
                returnDefault (_ :: SomeException) = return initial

-- | The canonical location. We assume \~\/.yi has been set up already.
dbLocation :: IO FilePath
dbLocation = getHomeDirectory >>= \home -> return (home ++ "/.yi/articles.db")

-- | Returns the database as it exists on the disk, and the current Yi buffer contents.
--   Note that the Initializable typeclass gives us an empty Seq. So first we try the buffer
--   state in the hope we can avoid a very expensive read from disk, and if we find nothing
--   (that is, if we get an empty Seq), only then do we call 'readDB'.
oldDbNewArticle :: YiM (ArticleDB, Article)
oldDbNewArticle = do saveddb <- withBuffer $ getA bufferDynamicValueA
                     newarticle <-fmap B.pack $ withBuffer getBufferContents
                     if not $ S.null (unADB saveddb)
                      then return (saveddb, newarticle)
                      else do olddb <- readDB
                              return (olddb, newarticle)

-- TODO: move to somewhere more sensible. Buffer.Misc?
getBufferContents :: BufferM String
getBufferContents = readRegionB =<< regionOfB Document

-- | Given an 'ArticleDB', dump the scheduled article into the buffer (replacing previous contents).
setDisplayedArticle :: ArticleDB -> YiM ()
setDisplayedArticle newdb = do let next = getLatestArticle newdb
                               withBuffer $ do replaceBufferContent $ B.unpack next
                                               topB -- replaceBufferContents moves us
                                                    -- to bottom?
                                               putA bufferDynamicValueA newdb

-- | Go to next one. This ignores the buffer, but it doesn't remove anything from the database.
-- However, the ordering does change.
nextArticle :: YiM ()
nextArticle = do (oldb,_) <- oldDbNewArticle
                 -- Ignore buffer, just set the first article last
                 let newdb = removeSetLast oldb (getLatestArticle oldb)
                 writeDB newdb
                 setDisplayedArticle newdb

-- | Delete current article (the article as in the database), and go to next one.
deleteAndNextArticle :: YiM ()
deleteAndNextArticle = do (oldb,_) <- oldDbNewArticle -- throw away changes,
                          let ndb = ADB $ case viewl (unADB oldb) of     -- drop 1st article
                                EmptyL -> empty
                                (_ :< b) -> b
                          writeDB ndb
                          setDisplayedArticle ndb

-- | The main action. We fetch the old database, we fetch the modified article from the buffer,
-- then we call the function 'updateSetLast' which removes the first article and pushes our modified article
-- to the end of the list.
saveAndNextArticle :: Int -> YiM ()
saveAndNextArticle n = do (oldb,newa) <- oldDbNewArticle
                          let newdb = shift n $ removeSetLast oldb newa
                          writeDB newdb
                          setDisplayedArticle newdb

-- | Assume the buffer is an entirely new article just imported this second, and save it.
-- We don't want to use 'updateSetLast' since that will erase an article.
saveAsNewArticle :: YiM ()
saveAsNewArticle = do oldb <- readDB -- make sure we read from disk - we aren't in iread-mode!
                      (_,newa) <- oldDbNewArticle -- we ignore the fst - the Initializable is 'empty'
                      let newdb = insertArticle oldb newa
                      writeDB newdb
