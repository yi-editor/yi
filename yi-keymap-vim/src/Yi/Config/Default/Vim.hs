module Yi.Config.Default.Vim (configureVim) where

import Lens.Micro.Platform ((.=), (%=), (.~))
import Yi.Buffer.Normal    (RegionStyle (..))
import Yi.Keymap.Vim       (keymapSet)
import Yi.Config.Misc      (ScrollStyle (..))
import Yi.Config.Lens
import Yi.Config.Simple    (ConfigM)

configureVim :: ConfigM ()
configureVim = do
  defaultKmA         .= keymapSet
  configUIA          %= (configScrollStyleA .~ Just SingleLine)
  configRegionStyleA .= Inclusive
