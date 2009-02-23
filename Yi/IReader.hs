-- | This module defines a list type and operations on it; it further
-- provides functions which write in and out the list.
-- The goal is to make it easy for the user to store a large number of text buffers
-- and cycle among them, making edits as she goes. The idea is inspired by
-- \"incremental reading\", see <http://en.wikipedia.org/wiki/Incremental_reading>.
module Yi.IReader where

import Control.Monad.State (join)
import Data.Sequence
import System.Directory (getHomeDirectory)
import Control.Concurrent

import Yi.Keymap (withBuffer, YiM)
import Yi.Prelude (io)
import Yi.Buffer.Misc (BufferM)
import Yi.Buffer.Region (readRegionB)
import Yi.Buffer.Normal (regionOfB, TextUnit(Document))
import Yi.Buffer.HighLevel (replaceBufferContent, topB)
import qualified Data.ByteString.Char8 as B (pack, unpack, readFile, writeFile)

type Article = String
type ArticleDB = Seq Article

-- | Take an 'ArticleDB', and return the first 'Article' and an ArticleDB - *without* that article.
split :: ArticleDB -> (Article, ArticleDB)
split adb = case viewl adb of
               EmptyL -> ("", empty)
               (a :< b) -> (a, b)

-- | Get the first article in the list. We use the list to express relative priority;
-- the first is the most, the last least. We then just cycle through - every article gets equal time.
getLatestArticle :: ArticleDB -> Article
getLatestArticle = fst . split -- we only want the article

-- | We remove the old first article, and we stick it on the end of the
-- list using the presumably modified version.
removeSetLast :: ArticleDB -> Article -> ArticleDB
removeSetLast adb old = (snd $ split adb) |> old

-- | Insert a new article with top priority (that is, at the front of the list).
insertArticle :: ArticleDB -> Article -> ArticleDB
insertArticle adb new = new <| adb

-- | In the background, serialize given 'ArticleDB' out.
writeDB :: ArticleDB -> YiM ()
writeDB adb = do io . forkIO . join . fmap (flip B.writeFile $ B.pack $ show adb) $ dbLocation
                 return ()

-- | Read in database from 'dbLocation' and then parse it into an 'ArticleDB'.
readDB :: YiM ArticleDB
readDB = io $ rddb `catch` (const $ return empty) -- May seem silly to read in as bytestring
         where rddb = do db <- fmap B.readFile dbLocation -- and then unpack it, but - is strict
                         fmap (read . B.unpack) db 

-- | The canonical location. We assume \~\/.yi has been set up already.
dbLocation :: IO FilePath
dbLocation = getHomeDirectory >>= \home -> return (home ++ "/.yi/articles.db")

-- | Returns the database as it exists on the disk, and the current Yi buffer contents.
oldDbNewArticle :: YiM (ArticleDB, Article)
oldDbNewArticle = do olddb <- readDB
                     newarticle <- withBuffer getBufferContents
                     return (olddb, newarticle)

getBufferContents :: BufferM String
getBufferContents = readRegionB =<< regionOfB Document

-- | Given an 'ArticleDB', dump the scheduled article into the buffer (replacing previous contents).
setDisplayedArticle :: ArticleDB -> YiM ()
setDisplayedArticle newdb = do let nextarticle = getLatestArticle newdb
                               withBuffer (replaceBufferContent nextarticle)
                               withBuffer topB -- replaceBufferContents moves us
                                               -- to bottom?

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
                          let ndb = case viewl oldb of     -- drop 1st article
                                EmptyL -> empty
                                (_ :< b) -> b
                          writeDB ndb
                          setDisplayedArticle ndb

-- | The main action. We fetch the old database, we fetch the modified article from the buffer,
-- then we call the function 'updateSetLast' which removes the first article and pushes our modified article
-- to the end of the list.
saveAndNextArticle :: YiM ()
saveAndNextArticle = do (oldb,newa) <- oldDbNewArticle
                        let newdb = removeSetLast oldb newa
                        writeDB newdb
                        setDisplayedArticle newdb

-- | Assume the buffer is an entirely new article just imported this second, and save it.
-- We don't want to use 'updateSetLast' since that will erase an article.
saveAsNewArticle :: YiM ()
saveAsNewArticle = do (oldb,newa) <- oldDbNewArticle
                      let newdb = insertArticle oldb newa
                      writeDB newdb
