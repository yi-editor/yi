module Yi.Config.Default.Vty (configureVty) where

import Lens.Micro.Platform ((.=))
import Yi.Frontend.Vty     (start)
import Yi.Config.Lens      (startFrontEndA)
import Yi.Config.Simple    (ConfigM)

configureVty :: ConfigM ()
configureVty = startFrontEndA .= start
