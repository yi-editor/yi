module Yi.Keymap.Vim2.StyledRegion
    ( StyledRegion(..)
    ) where

import Yi.Buffer.Normal
import Yi.Region

data StyledRegion = StyledRegion !RegionStyle !Region
