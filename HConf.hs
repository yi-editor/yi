module HConf where

import Prelude hiding ( catch )
import Control.Exception (handle, catch, bracket, throw, Exception(ExitException))
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import System.IO
import System.Info
#ifndef mingw32_HOST_OS
import System.Posix.Process (executeFile, forkProcess, getProcessStatus, createSession)
#endif
import System.Process
import System.Directory
import System.Exit
import System.Environment
import Data.Typeable
import Data.Monoid
import System.FilePath


{-

This module provides functions to manage Xmonad style configuration.

It takes as "parameters":
  * project name

  * Main function of the program. This is will take a configuration, and a state as
    parameter.

  * Default configuration

  * A way to change the configuration to make user aware of slave-compilation errors.


This will provide:

  * master main:
    This is the "driver" program, that just attemps to compile ~/.project/project.hs, and exec. it.
    If that fails, the default main is called.

  * slave main:
    Function that has to be called in the ~/.project/project.hs (provides a configurable entry point)

  * restart function:
    (TBD) save the state; recompile and execute the new version with the saved state.

Standard scenario:

* 

-}

data HConf state configuration = HConf {
      mainMaster :: IO (),
      mainSlave :: configuration -> IO ()
}


getHConf :: String -> state -> (String -> IO state) ->
            configuration -> (String -> configuration -> configuration) ->
            (configuration -> state -> IO ()) -> 
            HConf state configuration

getHConf projectName initialState recoverState defaultConfiguration showErrorsInConf realMain = HConf
 {
    -- | The entry point into Project. Attempts to compile any custom main
    -- for Project, and if it doesn't find one, just launches the default.
    mainMaster = do
     args <- getArgs
     let launch = do maybeErrors <- buildLaunch projectName
                     case maybeErrors of 
                       Nothing ->     realMain defaultConfiguration initialState
                       Just errors -> realMain (showErrorsInConf errors defaultConfiguration) initialState
     case args of
        ["--resume", _]       -> launch
        ["--recompile"]       -> recompile projectName False >> return ()
        ["--recompile-force"] -> recompile projectName True >> return ()
        _                     -> launch,

    -- | The entry point into Project. Attempts to compile any custom main
    -- for Project, and if it doesn't find one, just launches the default.
    -- mainSlave :: IO ()
    mainSlave = \userConfig -> do
        args <- getArgs
        state <- case args of
            ["--resume", s]   -> recoverState s
            _                 -> return initialState
        realMain userConfig state

 }

-- Lift an IO action
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Return the path to @~\/.Project@.
getProjectDir :: MonadIO m => String -> m String
getProjectDir projectName = io $ getAppUserDataDirectory projectName


getErrorsFile projectName = (++ ".errors") <$> getProjectDir projectName
 
-- | @restart name resume@. Attempt to restart Project by executing the program
-- @name@.  If @resume@ is 'True', restart with the current window state.
-- When executing another window manager, @resume@ should be 'False'.
--
{-
restart :: String -> IO String -> IO ()
restart projectName saveState = do
  s <- saveState
  let args = ["--resume", s]
  executeFile projectName True args Nothing -- TODO: catch exceptions
  -- run the master, who will take care of recompiling.
-}


-- | 'recompile projectName force' recompiles ~\/.Project\/Project.hs when any of the
-- following apply:
--      * force is True
--      * the Project executable does not exist
--      * the Project executable is older than Project.hs
--
-- The -i flag is used to restrict recompilation to the Project.hs file only.
--
-- Compilation errors (if any) are logged to ~\/.Project\/Project.errors.  If
-- GHC indicates failure with a non-zero exit code, an xmessage displaying
-- that file is spawned.
--
-- Returns the errors if there were any; otherwise Nothing
--
recompile :: MonadIO m => String -> Bool -> m (Maybe String)
recompile projectName force = io $ do
    dir <- getProjectDir projectName
    let binn = projectName ++ "-"++arch++"-"++os
        bin  = dir ++ "/" ++ binn
        base = dir ++ "/" ++ projectName
        err  = base ++ ".errors"
        src  = base ++ ".hs"
    srcT <- getModTime src
    binT <- getModTime bin
    if (force || srcT > binT)
      then do
        status <- bracket (openFile err WriteMode) hClose $ \h -> do
            waitForProcess =<< runProcess "ghc" ["--make", projectName ++ ".hs", "-i", "-no-recomp", "-v0", "-o",binn] (Just dir)
                                    Nothing Nothing Nothing (Just h)

        -- now, if it fails, run xmessage to let the user know:
        if status /= ExitSuccess
          then do
            ghcErr <- readFile err
            let msg = unlines $
                    ["Error detected while loading " ++ projectName ++ " configuration file: " ++ src]
                    ++ lines ghcErr ++ ["","Please check the file for errors."]
            return $ Just msg
          else return Nothing
        else return Nothing
 where getModTime f = catch (Just <$> getModificationTime f) (const $ return Nothing)







-- | Build "~\/.Project\/Project.hs" with ghc, then execute it.  If there are no
-- errors, this function does not return.  An exception is raised in any of
-- these cases:
--   * ghc missing
--   * ~\/.Project\/Project.hs missing
--   * Project.hs fails to compile
--      ** wrong ghc in path (fails to compile)
--      ** type error, syntax error, ..
--   * Missing Project dependency packages
--
buildLaunch ::  String -> IO (Maybe String)
buildLaunch projectName = do
#ifndef mingw32_HOST_OS
    errMsg <- recompile projectName False
    dir  <- getProjectDir projectName
    args <- getArgs
    args' <- case errMsg of
               Nothing -> return args
               Just msg -> do errFile <- getErrorsFile projectName
                              return (args ++ [errFile])
    handle (\err -> return ()) 
       (executeFile (dir </> projectName ++ "-"++arch++"-"++os) False args' Nothing)
    return (Just "Custom yi could not be launched!\n" `mappend` errMsg)
#else
    return Nothing
#endif
