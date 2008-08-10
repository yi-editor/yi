module Yi.UI.TabBar where
import Yi.Window
import Yi.WindowSet
import Yi.Style

type TabBarDescr = WindowSet (String,Style)

tabBarDescr :: WindowSet (WindowSet Window) -> Int -> UIStyle -> TabBarDescr
tabBarDescr tabs maxWidth uiStyle = 
    let oofTabStyle = modeline uiStyle
        ifTabStyle = modelineFocused uiStyle
        outOfFocusTab = replicate maxWidth '#' ++ "|"
        inFocusTab = replicate maxWidth ' ' ++ "|"
        tabDescr (_,True) = (inFocusTab, oofTabStyle)
        tabDescr (_,False) = (outOfFocusTab, oofTabStyle)
    in fmap tabDescr (withFocus tabs) 
    
