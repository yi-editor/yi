-- | Boot process of Yi, as an instanciation of HConf
module Yi.Boot (driver, yi, reloadEditor, defaultHConfParams, projectName) where

import Control.Monad.State
import qualified Data.Binary
import HConf (HConf(HConf), getHConf)
import qualified HConf
import Yi.Buffer.Basic
import Yi.Config
import Yi.Debug
import Yi.Editor (newBufferE, Editor, withEditor)
import Yi.Keymap (makeAction, YiM)
import qualified Yi.Main
import qualified Yi.Config.Default

recoverState :: FilePath -> IO (Maybe Editor)
recoverState = Data.Binary.decodeFile

saveState :: FilePath -> Maybe Editor -> IO ()
saveState = Data.Binary.encodeFile

realMain :: Config -> Maybe Editor -> IO ()
realMain staticConfig state = do
          when (debugMode staticConfig) $ initDebug ".yi.dbg" 
          -- initialize here so we can see debug messages early, if
          -- the flag is set in the static configuration.
          Yi.Main.main staticConfig state

initState :: Maybe Editor
initState = Nothing

-- FIXME: Reloading works now, but the terminal is messed up when we reload,
-- then quit (in vty).  I attempted to quit the UI (via withUI UI.end) from
-- here, as a cleanup before restarting, but then the program would exit
-- immediately.
reloadEditor :: YiM ()
reloadEditor = do
    editor <- withEditor get
    liftIO $ restart (Just editor)


driver :: IO ()
yi :: Config -> IO ()
restart :: Maybe Editor -> IO ()
HConf driver yi restart = getHConf defaultHConfParams Yi.Config.Default.defaultConfig initState

showErrorsInConf :: String -> Config -> Config
showErrorsInConf errs conf 
    = conf {startActions = [makeAction $ newBufferE (Left "errors") (fromString errs)]}

projectName :: String
projectName = "yi"

defaultHConfParams :: HConf.HConfParams Config (Maybe Editor)
defaultHConfParams = HConf.HConfParams
    { HConf.projectName      = projectName
    , HConf.recoverState     = recoverState
    , HConf.saveState        = saveState
    , HConf.showErrorsInConf = showErrorsInConf
    , HConf.realMain         = realMain
    , HConf.ghcFlags         = []
    }

