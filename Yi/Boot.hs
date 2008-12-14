-- | Boot process of Yi, as an instanciation of HConf
module Yi.Boot (driver, yi, reloadEditor) where

import Control.Monad.State
import qualified Data.Binary
import HConf
import Yi.Buffer.Basic
import Yi.Config
import Yi.Debug
import Yi.Editor (newBufferE, Editor, withEditor)
import Yi.Keymap (makeAction, YiM)
import qualified Yi.Main

recoverState :: FilePath -> IO (Maybe Editor)
recoverState path = do
    Data.Binary.decodeFile path

saveState :: FilePath -> Maybe Editor -> IO ()
saveState path ed = do
    Data.Binary.encodeFile path ed

realMain :: Config -> Maybe Editor -> IO ()
realMain staticConfig state = do
          when (debugMode staticConfig) $ initDebug ".yi.dbg" 
          -- initialize here so we can see debug messages early, if
          -- the flag is set in the static configuration.
          Yi.Main.main staticConfig state

initState :: Maybe Editor
initState = Nothing

reloadEditor :: YiM ()
reloadEditor = do
    editor <- withEditor get
    liftIO $ restart (Just editor)


driver :: IO ()
yi :: Config -> IO ()
restart :: Maybe Editor -> IO ()
HConf driver yi restart = getHConf Yi.Main.projectName initState recoverState saveState Yi.Main.defaultConfig showErrorsInConf realMain

showErrorsInConf :: String -> Config -> Config
showErrorsInConf errs conf 
    = conf {startActions = [makeAction $ newBufferE (Left "errors") (fromString errs)]}

