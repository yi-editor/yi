module Yi.UI.TabBar where
import Data.Accessor
import Yi.Buffer (identString)
import Yi.Window
import Yi.WindowSet
import Yi.Editor (Editor(..)
                 ,findBufferWith, tabsA)

data TabDescr = TabDescr
    {
        tabText :: String,
        tabInFocus :: Bool
    }

type TabBarDescr = WindowSet TabDescr

tabBarDescr :: Editor -> TabBarDescr
tabBarDescr editor = 
    let hintForTab tab = identString $ findBufferWith (bufkey $ current tab) editor 
        tabDescr (tab,True) = TabDescr (hintForTab tab) True
        tabDescr (tab,False) = TabDescr (hintForTab tab) False
    in fmap tabDescr (withFocus $ editor ^. tabsA)

tabAbbrevTitle :: String -> String
tabAbbrevTitle title = case break (=='/') title of
                         ([],     ('/':suffix)) -> '/' : tabAbbrevTitle suffix
                         (prefix, ('/':suffix)) -> head prefix : '/' : tabAbbrevTitle (suffix)
                         (prefix, [])           -> prefix
