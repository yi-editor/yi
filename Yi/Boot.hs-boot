module Yi.Boot where

import Yi.Config (Config)
import Yi.Editor (Editor)
import Yi.Keymap
import qualified HConf

reloadEditor :: YiM ()
projectName :: String
defaultHConfParams :: HConf.HConfParams Config (Maybe Editor)
