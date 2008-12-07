module Yi.UI.TabBar where
import Data.Accessor
import Yi.Buffer (FBuffer(..), nameA)
import Yi.Window
import Yi.WindowSet
import Yi.Editor (Editor(..)
                 ,findBufferWith)

data TabDescr = TabDescr
    {
        tabText :: String,
        tabInFocus :: Bool
    }

type TabBarDescr = WindowSet TabDescr

tabBarDescr :: Editor -> TabBarDescr
tabBarDescr editor = 
    let hintForTab tab = findBufferWith (bufkey $ current tab) editor ^. nameA
        tabDescr (tab,True) = TabDescr (hintForTab tab) True
        tabDescr (tab,False) = TabDescr (hintForTab tab) False
    in fmap tabDescr (withFocus $ tabs editor)
