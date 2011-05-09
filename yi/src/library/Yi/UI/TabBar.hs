module Yi.UI.TabBar where

import Data.Accessor
import qualified Data.List.PointedList.Circular as PL
import System.FilePath

import Yi.Buffer (shortIdentString)
import Yi.Tab
import Yi.Window
import Yi.Editor (Editor(..)
                 ,commonNamePrefix
                 ,findBufferWith, tabsA)

-- | A TabDescr describes the properties of a UI tab independent of the particular GUI in use. 
data TabDescr = TabDescr
    { tabText :: String
    , tabInFocus :: Bool
    }

type TabBarDescr = PL.PointedList TabDescr

tabBarDescr :: Editor -> TabBarDescr
tabBarDescr editor = 
    let prefix = commonNamePrefix editor
        hintForTab tab = tabAbbrevTitle $ shortIdentString prefix $ findBufferWith (bufkey $ PL.focus (tabWindows tab)) editor
        tabDescr (tab,True) = TabDescr (hintForTab tab) True
        tabDescr (tab,False) = TabDescr (hintForTab tab) False
    in fmap tabDescr (PL.withFocus $ editor ^. tabsA)

-- FIXME: it seems that using splitDirectories can abstract the '/' handling away.
-- (Making it win32 friendly and simpler)
tabAbbrevTitle :: String -> String
tabAbbrevTitle title = if isValid title
                           then concatMap abbrev (splitPath title)
                           else title
  where abbrev "/" = "/"
        abbrev path | head path == '.' && last path == '/' = take 2 path ++ "/"
                    | last path == '/' = head path : "/"
                    | otherwise        = path

