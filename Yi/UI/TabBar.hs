module Yi.UI.TabBar where
import Data.Accessor
import Yi.Buffer (identString)
import Yi.Window
import Yi.WindowSet
import Yi.Editor (Editor(..)
                 ,findBufferWith, tabsA)

-- | A TabDescr describes the properties of a UI tab independent of the particular GUI in use. 
data TabDescr = TabDescr
    {
        tabText :: String,
        tabInFocus :: Bool
    }

type TabBarDescr = WindowSet TabDescr

tabBarDescr :: Editor -> TabBarDescr
tabBarDescr editor = 
    let hintForTab tab = tabAbbrevTitle $ identString $ findBufferWith (bufkey $ current tab) editor 
        tabDescr (tab,True) = TabDescr (hintForTab tab) True
        tabDescr (tab,False) = TabDescr (hintForTab tab) False
    in fmap tabDescr (withFocus $ editor ^. tabsA)

-- FIXME: * use System.FilePath functions;
--        * handle the empty String
tabAbbrevTitle :: String -> String
tabAbbrevTitle title = case break (=='/') title of
                         ([],     ('/':suffix)) -> '/' : tabAbbrevTitle suffix
                         (prefix, ('/':suffix)) -> head prefix : '/' : tabAbbrevTitle (suffix)
                         (prefix, [])           -> prefix
