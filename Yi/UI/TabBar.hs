module Yi.UI.TabBar where
import Yi.Buffer (FBuffer(..))
import Yi.Window
import Yi.WindowSet
import Yi.Editor (Editor(..)
                 ,findBufferWith)
import Yi.Style

import qualified Data.Map as Map

type TabBarDescr = WindowSet (String,Style)

tabBarDescr :: Editor -> Int -> UIStyle -> TabBarDescr
tabBarDescr editor maxWidth uiStyle = 
    let oofTabStyle = modeline uiStyle
        ifTabStyle = modelineFocused uiStyle
        outOfFocusTab hint = 
            hint ++ replicate (maxWidth - length hint) '#' ++ "|"
        inFocusTab hint = 
            hint ++ replicate (maxWidth - length hint) ' ' ++ "|"
        hintForTab tab = hint
            where currentBufferName = name $ findBufferWith (bufkey $ current tab) editor
                  hint = if length currentBufferName > maxWidth
                            then take (maxWidth - 3) currentBufferName ++ "..."
                            else currentBufferName
        tabDescr (tab,True) = (inFocusTab $ hintForTab tab, oofTabStyle)
        tabDescr (tab,False) = (outOfFocusTab $ hintForTab tab, oofTabStyle)
    in fmap tabDescr (withFocus $ tabs editor) 
    
