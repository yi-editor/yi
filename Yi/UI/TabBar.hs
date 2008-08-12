module Yi.UI.TabBar where
import Yi.Buffer (FBuffer(..))
import Yi.Window
import Yi.WindowSet
import Yi.Editor (Editor(..)
                 ,findBufferWith)
import Yi.Style

import qualified Data.Map as Map

data TabDescr = TabDescr
    {
        tabText :: String,
        tabStyle :: Style,
        tabInFocus :: Bool
    }

type TabBarDescr = WindowSet TabDescr

tabBarDescr :: Editor -> Int -> UIStyle -> TabBarDescr
tabBarDescr editor maxWidth uiStyle = 
    -- TODO: Use different styles for oofTabStyle and ifTabStyle. 
    -- I tried using modeline and modelineFocused but this had an undesired 
    -- effect on the mode line. - CROC
    let oofTabStyle = modeline uiStyle
        ifTabStyle = modeline uiStyle
        hintForTab tab = hint
            where currentBufferName = name $ findBufferWith (bufkey $ current tab) editor
                  hint = if length currentBufferName > maxWidth
                            then take (maxWidth - 3) currentBufferName ++ "..."
                            else currentBufferName
        tabDescr (tab,True) = TabDescr (hintForTab tab) ifTabStyle True
        tabDescr (tab,False) = TabDescr (hintForTab tab) oofTabStyle False
    in fmap tabDescr (withFocus $ tabs editor)
    
