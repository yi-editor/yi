module Yi.Config.Default.JavaScriptMode (configureJavaScriptMode) where

import Lens.Micro.Platform ((%=))
import Yi.Mode.JavaScript
import Yi.Config.Simple (ConfigM)
import Yi.Config.Lens   (modeTableA)
import Yi.Types         (AnyMode (..))

configureJavaScriptMode :: ConfigM ()
configureJavaScriptMode = modeTableA %= (AnyMode (hooks javaScriptMode) :)