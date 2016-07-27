module Yi.Config.Default.HaskellMode (configureHaskellMode) where

import Lens.Micro.Platform ((%=))
import Yi.Config.Simple    (ConfigM, addMode)
import Yi.Config.Lens      (modeTableA)
import Yi.Mode.Haskell
import Yi.Types            (AnyMode (..))

configureHaskellMode :: ConfigM ()
configureHaskellMode = do
  addMode literateMode
  addMode preciseMode
  addMode cleverMode
