module Yi.Config.Default.Pango (configurePango) where

import Lens.Micro.Platform ((.=))
import Yi.Frontend.Pango   (start)
import Yi.Config.Lens      (startFrontEndA)
import Yi.Config.Simple    (ConfigM)

configurePango :: ConfigM ()
configurePango = startFrontEndA .= start