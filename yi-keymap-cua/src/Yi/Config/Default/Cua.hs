module Yi.Config.Default.Cua (configureCua) where

import Lens.Micro.Platform ((.=))
import Yi.Config.Simple    (ConfigM)
import Yi.Config.Lens      (defaultKmA)
import Yi.Keymap.Cua

configureCua :: ConfigM ()
configureCua = defaultKmA .= keymap