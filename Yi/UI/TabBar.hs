module Yi.UI.TabBar where
import Yi.Buffer (FBuffer(..))
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
    let hintForTab tab = name $ findBufferWith (bufkey $ current tab) editor
        tabDescr (tab,True) = TabDescr (hintForTab tab) True
        tabDescr (tab,False) = TabDescr (hintForTab tab) False
    in fmap tabDescr (withFocus $ tabs editor)
