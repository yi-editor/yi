module Yi.Config.Default.HaskellMode (configureHaskellMode) where

import Yi.Config.Simple    (ConfigM, addMode)
import Yi.Mode.Haskell

configureHaskellMode :: ConfigM ()
configureHaskellMode = do
  addMode literateMode
  addMode preciseMode
  addMode cleverMode
