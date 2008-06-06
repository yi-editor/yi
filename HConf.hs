module HConf where

import Prelude hiding ( catch )
import Control.Exception (handle, catch, bracket)
import Control.Applicative
import Control.Monad.Reader
import System.IO
import System.Info
#ifndef mingw32_HOST_OS
#ifdef darwin_HOST_OS
import System.Posix.Process 
            (executeFile, 
             getProcessStatus, 
             forkProcess,
             exitImmediately)
#else
import System.Posix.Process (executeFile)
#endif
#endif
import System.Process
import System.Directory
import System.Exit
import System.Environment
import Data.Monoid
import System.FilePath ((</>))


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
      mainSlave :: configuration -> IO (),
      restart :: state -> IO ()
 }


getHConf :: String -> state -> (String -> IO state) -> (state -> IO String) ->
            configuration -> (String -> configuration -> configuration) ->
            (configuration -> state -> IO ()) -> 
            HConf state configuration

getHConf projectName initialState recoverState saveState defaultConfiguration showErrorsInConf realMain = HConf
 {
    -- The entry point into Project. Attempts to compile any custom main
    -- for Project, and if it doesn't find one, just launches the default.
    mainMaster = do
     args <- getArgs
     let launch = do maybeErrors <- buildLaunch projectName
                     case maybeErrors of 
                       Nothing     -> realMain defaultConfiguration initialState
                       Just errors -> realMain (showErrorsInConf errors defaultConfiguration) initialState
     case args of
        ["--resume", _]       -> launch
        ["--recompile"]       -> recompile projectName False >> return ()
        ["--recompile-force"] -> recompile projectName True >> return ()
        _                     -> launch

     -- @restart name resume@. Attempt to restart Project by executing the program
     -- @name@.  If @resume@ is 'True', restart with the current window state.
     -- When executing another window manager, @resume@ should be 'False'.
     -- this function will never return.
    , restart = \state -> do
#ifndef mingw32_HOST_OS
        s <- saveState state
        let args = ["--resume", s]
        executeFile projectName True args Nothing -- TODO: catch exceptions
        -- run the master, who will take care of recompiling; getting the new state, etc.
#else
        return ()
#endif

    -- The configurable main, callable from ~/.project/project.hs
    , mainSlave = \userConfig -> do
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

-- | Return the full path to the errors file
getErrorsFile :: (MonadIO m, Functor m) => String -> m String
getErrorsFile projectName = do dir <- getProjectDir projectName
                               return $ dir </> projectName ++ ".errors"
 
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
            waitForProcess =<< runProcess "ghc" ["--make", projectName ++ ".hs", "-i", "-no-recomp", "-v0", "-o",binn,"-threaded"] (Just dir)
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
buildLaunch :: String -> IO (Maybe String)
buildLaunch projectName = do
#ifndef mingw32_HOST_OS
    errMsg <- recompile projectName False
    dir  <- getProjectDir projectName
    args <- getArgs
    args' <- case errMsg of
               Nothing -> return args
               Just _ -> do errFile <- getErrorsFile projectName
                            return (args ++ [errFile])
    let executable_file = projectName ++ "-" ++ arch ++ "-" ++ os
        executable_path = dir </> executable_file
    putStrLn $ "Launching custom yi: " ++ show executable_path
#ifndef darwin_HOST_OS
    handle (\_exception -> return ())
       (executeFile executable_path False args' Nothing)
    return $ Just ("Custom yi (" ++ show executable_path ++ ") could not be launched!\n") `mappend` errMsg
#else
    -- Darwin is odd or broken; Take your pick. According to:
    --      http://uninformed.org/index.cgi?v=1&a=1&p=16
    -- and
    --      http://www.cherrypy.org/ticket/581
    -- In order to get around a "Operation not supported" error on execv[e] it's
    -- required to fork THEN execv[e]. - coconnor
    child_pid <- forkProcess $ executeFile executable_path False args' Nothing
    child_status <- getProcessStatus True False child_pid
    case child_status of
        Nothing -> return $ Just 
            ("Custom yi (" ++ show executable_path ++ ") could not be launched!\n") `mappend` errMsg
        Just _ -> do 
            exitImmediately ExitSuccess
            return Nothing
#endif
#else
    return Nothing
#endif

