{-# LANGUAGE CPP #-}
module HConf (getHConf, HConf(HConf), hconfOptions) where

import Prelude hiding ( catch )
import Control.Exception (catch, bracket)
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
import Control.Exception (handle)
#endif
#endif
import System.Process
import System.Directory
import System.Exit
import System.Environment
import System.Console.GetOpt 
import System.FilePath ((</>), takeDirectory)
import qualified GHC.Paths

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
    (TBD) save the state; then call master to recompile and execute the new version with the saved state.

Standard scenario:

TDB

-}

data HConf state configuration = HConf {
      mainMaster :: IO (),
      mainSlave :: configuration -> IO (),
      restart :: state -> IO ()
 }

hconfOptions :: String -> [(String, String, IO () -> IO ())]
hconfOptions projectName = 
    [
        ("recompile-force", 
         "Force recompile of custom " ++ projectName ++ " before starting.", 
         \_ -> recompile projectName True >> return ()
        ),
        ("resume",
         "Resume execution of " ++ projectName ++ " from previous state",
         id -- this option is actually handled by the Slave.
        ),
        ("recompile",
         "Recompile custom " ++ projectName ++ " if required then exit.",
         \_ -> do
            recompile projectName False
            exitWith ExitSuccess
        )
    ]

getHConf :: String -> state -> (FilePath -> IO state) -> (FilePath -> state -> IO ()) ->
            configuration -> (String -> configuration -> configuration) ->
            (configuration -> state -> IO ()) -> 
            HConf state configuration

getHConf projectName initialState recoverState saveState defaultConfiguration showErrorsInConf realMain = HConf
 {
    -- The entry point into Project. Attempts to compile any custom main
    -- for Project, and if it doesn't find one, just launches the default.
    mainMaster = do
        let launch = do
                maybeErrors <- buildLaunch projectName
                case maybeErrors of 
                    Nothing     -> realMain defaultConfiguration initialState
                    Just errors -> realMain (showErrorsInConf errors defaultConfiguration) initialState
        let optDescriptions = 
                (flip fmap) (hconfOptions projectName) $ \(name,desc,f) -> 
                    let apply_f_descr = NoArg (f launch)
                    in Option [] [name] apply_f_descr desc
        args <- getArgs
        let (opt_actions, _, _) = getOpt Permute optDescriptions args 
        sequence_  opt_actions
        launch

     -- @restart state@. Attempt to restart Project by executing the
     -- program @projectName@.
     -- This function will never return.
    , restart = \state -> do
#ifndef mingw32_HOST_OS
        f <- getStateFile
        createDirectoryIfMissing True (takeDirectory f)
        saveState f state
        let args = ["--resume"]
        progName <- getProgName
        executeFile progName True args Nothing -- TODO: catch exceptions
        -- run the master, who will take care of recompiling; handle errors, etc.
#else
        return ()
#endif

    -- The configurable main, callable from ~/.project/project.hs
    , mainSlave = \userConfig -> do
        args <- getArgs
        state <- case args of
            ["--resume"]   -> recoverState =<< getStateFile
            _              -> return initialState
        realMain userConfig state

 } where
     getStateFile = (</> "status") <$> getProjectDir projectName

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
-- Compilation errors (if any) are logged to
-- ~\/.Project\/Project.errors.  If GHC indicates failure with a
-- non-zero exit code; we read the errors and return them.
--
-- Returns the errors if there were any; otherwise Nothing
--
-- Errors can be returned in any of
-- these cases:
--   * ghc missing
--   * ~\/.Project\/Project.hs missing
--   * Project.hs fails to compile
--      ** wrong ghc in path (fails to compile)
--      ** type error, syntax error, ..
--   * Missing Project dependency packages
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
        if force
            then putStrLn $ "Forcing recompile of custom " ++ projectName
            else putStrLn $ "Recompiling custom " ++ projectName
        status <- bracket (openFile err WriteMode) hClose $ \h -> do
            -- note that since we checked for recompilation ourselves,
            -- we disable ghc recompilaton checks.
            waitForProcess =<< runProcess GHC.Paths.ghc ["--make", projectName ++ ".hs", "-i", "-fforce-recomp", "-v0", "-o",binn,"-threaded"] (Just dir)
                                    Nothing Nothing Nothing (Just h)
            -- note that we use the ghc executable used to build Yi (through GHC.Paths).

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


(+++) :: Maybe [a] -> Maybe [a] -> Maybe [a]
Nothing +++ x = x
x +++ Nothing = x
(Just x) +++ (Just y) = Just (x ++ y)



-- | Launch the custom (slave) program.

-- Call @recompile False@

-- If there is a slave to run, this function does not return. 

-- If there are errors and the function returns, they are returned in a string;
-- If there are errors and the slave is run, we pass the error file as an argument to it. 

buildLaunch :: String -> IO (Maybe String)
buildLaunch projectName = do
    haveConfigFile <- doesFileExist =<< (</> (projectName ++ ".hs")) <$> getProjectDir projectName 
    -- if there is no config file, then we return immediately /with no error/. This is 
    -- a normal situation: the user has not produced a config file.
    if not haveConfigFile then return Nothing else do
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
    putStrLn $ "Launching custom " ++ projectName ++ ": " ++ show executable_path
    let launchFailed = return $ Just 
         ("Custom " ++ projectName 
          ++ " (" ++ show executable_path ++ ") "
          ++ "could not be launched!\n") +++ errMsg

# ifndef darwin_HOST_OS
    handle (\_exception -> return ())
       (executeFile executable_path False args' Nothing)
    -- if we reach this point then exec failed.
    launchFailed
# else
    -- Darwin is odd or broken; Take your pick. According to:
    --      http://uninformed.org/index.cgi?v=1&a=1&p=16
    -- and
    --      http://www.cherrypy.org/ticket/581
    -- In order to get around a "Operation not supported" error on execv[e] it's
    -- required to fork THEN execv[e]. - coconnor
    child_pid <- forkProcess $ executeFile executable_path False args' Nothing
    child_status <- getProcessStatus True False child_pid
    case child_status of
        Nothing -> launchFailed
        Just _ -> do 
            exitImmediately ExitSuccess
            return Nothing
# endif
#else
    return Nothing
#endif

