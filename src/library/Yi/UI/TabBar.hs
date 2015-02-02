{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.UI.TabBar
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Tabs.

module Yi.UI.TabBar where

import           Control.Applicative            ((<$>))
import           Control.Lens                   ((^.))
import qualified Data.List.PointedList.Circular as PL (PointedList (_focus), withFocus)
import qualified Data.Text                      as T (Text, pack, unpack)
import           System.FilePath                (isValid, splitPath)
import           Yi.Buffer                      (shortIdentString)
import           Yi.Editor                      (Editor (..), commonNamePrefix, findBufferWith, tabsA)
import           Yi.Tab                         (tabWindowsA)
import           Yi.Window                      (Window (bufkey))

-- | A TabDescr describes the properties of a UI tab independent of
-- the particular GUI in use.
data TabDescr = TabDescr
    { tabText :: T.Text
    , tabInFocus :: Bool
    } deriving (Show, Eq)

type TabBarDescr = PL.PointedList TabDescr

tabBarDescr :: Editor -> TabBarDescr
tabBarDescr editor = tabDescr <$> PL.withFocus (editor ^. tabsA)
  where
    prefix = commonNamePrefix editor
    shorten = tabAbbrevTitle . shortIdentString (length prefix)
    mkHintWith f = shorten $ findBufferWith f editor
    hintForTab tab = mkHintWith (bufkey $ PL._focus (tab ^. tabWindowsA))

    tabDescr (tab, True)  = TabDescr (hintForTab tab) True
    tabDescr (tab, False) = TabDescr (hintForTab tab) False


-- FIXME: it seems that using splitDirectories can abstract the '/'
-- handling away. (Making it win32 friendly and simpler)
tabAbbrevTitle :: T.Text -> T.Text
tabAbbrevTitle title = if isValid fp
                       then T.pack $ concatMap abbrev (splitPath fp)
                       else title
  where
    fp = T.unpack title
    abbrev "/" = "/"
    abbrev path | head path == '.' && last path == '/' = take 2 path ++ "/"
                | last path == '/' = head path : "/"
                | otherwise        = path
