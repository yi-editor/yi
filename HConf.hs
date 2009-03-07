{- |

HConf provides functions to manage an Xmonad-style configuration. By
Xmonad-style configuration we mean the following scheme:

When somebody installs our application, they install the executable, say,
/usr/bin/app, as well as a library, say, libapp, that our application is based
on. We will refer to that executable as the "default" executable, because it
realizes a default, non-customized behaviour, referred to as the default
configuration.

If the user whishes to customize their installation, they create a configuration
file ~/.app/app.hs, which is just a Haskell source file that imports our
library and effectively implements a custom application. Thus it will typically
look something like this:

    module Main where

    import App

    main = defaultConfig { ... }

(You may prefer to call this file a customization rather than a configuration,
but this is only a technical distinction.)

Now we want to save the user from the trouble to compile their configuration by
hand and placing the resulting executable somewhere in their $PATH. That is why
on every start of the default executable, we check if the user has put a
configuration in place. If there is none, then we just continue the default
application. If there is one, we compile it (unless no recompilation is needed)
and launch the resulting "custom" executable. (Edge case: If there are errors
on recompilation, we must take care that the user gets at the error message.
For the case that they do miss it, we log it to ~/.app/app.errors.)

Finally, we also provide a way to reload the configuration without restarting
the application. This is useful if the user will keep adjusting their
configuration, or if the application is designed to never exit (think of
xmonad, a window manager). Restarting is realized by saving the application
state to a file (~/.app/status), then calling the new custom executable with
the --resume option. (FIXME: What if two instances of app try to access
~/.app/status? Also, above, what if to instances of app try and access
~/.app/app.errors?)

-}

{-# LANGUAGE CPP #-}
module HConf (getHConf, HConf(HConf), HConfParams(..), hconfOptions) where

import Prelude hiding ( catch )
import Control.OldException (catch, bracket)
import Control.Applicative
import Control.Monad.Reader
import Data.List (intercalate)
import GHC.Environment (getFullArgs)
import System.IO
import System.Info
#ifndef mingw32_HOST_OS
#ifdef darwin_HOST_OS
import System.Posix.Process
            (getProcessStatus,
             forkProcess,
             exitImmediately,
             ProcessStatus(..))
import qualified System.Posix.Process as SPP (executeFile)
import System.Posix.Signals (raiseSignal, sigTSTP)
#else
import System.Posix.Process (executeFile)
#endif
import Control.OldException (handle)
#endif
import System.Process
import System.Directory
import System.Exit
import System.Environment
import System.Console.GetOpt
import System.FilePath ((</>), takeDirectory)
import qualified GHC.Paths
import HConf.Paths
import HConf.Utils

-- | Input to getHConf

data HConfParams config state = HConfParams
 { projectName          :: String
    -- ^ the project name ("app" in the example). The default executable
    --   must be named the same as this, and be in the $PATH.
    --   FIXME: This is true for a specific interpretation of @restart@,
    --   cf. there.
 , ghcFlags             :: [String]
    -- ^ additional options to pass to GHC on recompilation
 , recoverState         :: FilePath -> IO state
    -- ^ how to recover state from a status file
 , saveState            :: FilePath -> state -> IO ()
    -- ^ how to write state to a status file
 , showErrorsInConf     :: (String -> config -> config)
    -- ^ how to report compilation errors to the user (the resulting
    --   configuration will be passed to the main function)
 , realMain             :: config -> state -> IO ()
    -- ^ The main function, used on resume, on compilation error, and
    --   if the user didn't provide a customization. It takes a
    --   configuration and an initial state.
 }

-- | Output of getHConf

data HConf config state = HConf {
      hConfMainMaster :: IO (),
        -- ^ Attempts to compile the user configuration and launch it, and if
        --   there is no user configuration, just continues with the default
        --   configuration.
      hConfMainSlave :: config -> IO (),
        -- ^ Launches the application with a given configuration. Designed to be
        --   used in the user configuration file.
      hConfRestart :: state -> IO ()
 }

-- | Command-line options

data HConfOption
    = Action (IO () -> IO ())  -- ^ the argument IO () is the slave
    | GhcFlags String          -- ^ options to pass to GHC on recompilation

actionDescriptions :: HConfParams config state -> [OptDescr HConfOption]
actionDescriptions params@HConfParams {projectName = app} =
  [ Option [] ["recompile-force"]
           (NoArg . Action . const $ recompile params True >> return ())
           ("Force recompile of custom " ++ app ++ " before starting")
  , Option [] ["resume"]
           (NoArg $ Action id)
           ("Resume execution of " ++ app ++ " from previous state")
  , Option [] ["recompile"]
           (NoArg . Action . const $ recompileExit params)
           ("Recompile custom " ++ app ++ " if required then exit")
  ]

ghcFlagsDescription :: OptDescr HConfOption
ghcFlagsDescription = Option [] ["ghc-options"] (ReqArg GhcFlags "[flags]") "Flags to pass to GHC"

-- | Descriptions of the command-line options that are processed by HConf
hconfOptions :: HConfParams config state -> [OptDescr HConfOption]
hconfOptions params = actionDescriptions params ++ [ghcFlagsDescription]

-- | Find out what to do from the command-line arguments.
getActions :: HConfParams config state -> IO [IO () -> IO ()]
getActions params = do
    args <- getArgs
    let (opt_actions, _, _) = getOpt Permute (actionDescriptions params) args
    return $ map (\(Action x) -> x) opt_actions

-- | Find out what additional options to pass to GHC (parse command-line
-- arguments for --ghc-options).
getGhcFlags :: IO [String]
getGhcFlags = do
        args <- getArgs
        let (opt_flags, _, _) = getOpt Permute [ghcFlagsDescription] args
        return . shellWords . intercalate " " . map (\(GhcFlags x) -> x)
               $ opt_flags

getHConf :: HConfParams config state -- ^ general parameters
        -> config                    -- ^ the (default) configuration
        -> state                     -- ^ the initial/current application state
        -> HConf config state
getHConf params defaultConfig initialState = HConf
 { hConfMainMaster = mainMaster params defaultConfig initialState
 , hConfRestart    = \state -> restart params state
 , hConfMainSlave  = \config -> mainSlave params config initialState
 }

-------------------------------------------------------------------------
-- Procedures that do the real work.
-------------------------------------------------------------------------

-- | Recompile the user configuration if needed, print any eventual errors to
-- stderr, then exit.
recompileExit :: HConfParams config state -> IO ()
recompileExit params = recompile params False >>=
    maybe exitSuccess (\err -> hPutStrLn stderr err >> exitFailure)

-- | The entry point into the application. Attempts to compile any custom main
-- for Project, and if it doesn't find one, just launches the default.
mainMaster :: HConfParams config state -> config -> state -> IO ()
mainMaster params@HConfParams { showErrorsInConf = showErrors, realMain = main }
 defaultConfig initialState = do
    actions <- getActions params
    let launch = do
        maybeErrors <- buildLaunch params
        case maybeErrors of
            Nothing     -> main defaultConfig initialState
            Just errors -> main (showErrors errors defaultConfig) initialState
    mapM_ ($ launch) actions
    launch

-- | Attempt to restart the application by executing the
-- program @projectName@. This function will never return.
restart :: HConfParams config state -> state -> IO ()
restart HConfParams { projectName = app, saveState = save } state = do
#ifndef mingw32_HOST_OS
# ifdef darwin_HOST_OS
        -- disable restart for the time being
        -- executeFile is custom-defined for darwin, and it causes
        -- significant issues when trying to restart as it leaves
        -- the UI of both the old and the new processes alive.
        -- Unfortunately UI.end cannot be used to fix this as that
        -- terminates the program.
        error "restart: this operation is not supported under darwin"
# else
        f <- getStateFile app
        createDirectoryIfMissing True (takeDirectory f)
        save f state
        let args = ["--resume"]
        executeFile app True args Nothing -- TODO: catch exceptions
        -- run the master, who will take care of recompiling; handle errors, etc.
# endif
#else
        return ()
#endif

-- | The configurable main, to be called from ~/.project/project.hs.
mainSlave :: HConfParams config state -> config -> state -> IO ()
mainSlave params userConfig initialState = do
        args <- getArgs
        state <- case args of
            ["--resume"]   -> recover =<< getStateFile app
            _              -> return initialState
        main userConfig state
 where
        HConfParams { projectName  = app
                    , recoverState = recover
                    , realMain     = main
                    } = params

-- | 'recompile params force' recompiles ~\/.Project\/Project.hs when any of the
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
recompile :: HConfParams config state -> Bool -> IO (Maybe String)
recompile HConfParams {projectName = app, ghcFlags = flags} force = do
    dir <- getProjectDir app
    err <- getErrorsFile app
    let binn = app ++ "-"++arch++"-"++os
        bin  = dir </> binn
        base = dir </> app
        src  = base ++ ".hs"
    srcT <- getModTime src
    binT <- getModTime bin
    if (force || srcT > binT)
      then do
        if force
            then putStrLn $ "Forcing recompile of custom " ++ app
            else putStrLn $ "Recompiling custom " ++ app
        status <- bracket (openFile err WriteMode) hClose $ \h -> do
            -- note that since we checked for recompilation ourselves,
            -- we disable ghc recompilaton checks.
            flags' <- getGhcFlags
            let allFlags = ["--make", app ++ ".hs", "-i", "-optl-s",
                             "-fforce-recomp", "-v0", "-o",binn,"-threaded"]
                            ++ flags ++ flags'
            waitForProcess =<< runProcess GHC.Paths.ghc allFlags (Just dir)
                                          Nothing Nothing Nothing (Just h)
            -- note that we use the ghc executable used to build Yi (through GHC.Paths).

        -- now, if GHC fails, return the error message that was written to 'err':
        if status /= ExitSuccess
          then do
            ghcErr <- readFile err
            let msg = unlines $
                    ["Error detected while loading " ++ app ++ " configuration file: " ++ src]
                    ++ lines ghcErr ++ ["","Please check the file for errors."]
            return $ Just msg
          else return Nothing
        else return Nothing
 where getModTime f = catch (Just <$> getModificationTime f) (const $ return Nothing)


-- | Launch the custom (slave) program.

-- Call @recompile False@

-- If there is a slave to run, this function does not return.

-- If there are errors and the function returns, they are returned in a string;
-- If there are errors and the slave is run, we pass the error file as an argument to it.

buildLaunch :: HConfParams config state -> IO (Maybe String)
buildLaunch params@HConfParams{ projectName = app } = do
    haveConfigFile <- doesFileExist =<< getConfigFile app
    -- if there is no config file, then we return immediately /with no error/. This is
    -- a normal situation: the user has not produced a config file.
    if not haveConfigFile then return Nothing else do
#ifndef mingw32_HOST_OS
    errMsg <- recompile params False
    executable_path <- getCustomExecutable app
    args <- getFullArgs
    args' <- case errMsg of
               Nothing -> return args
               Just _ -> do errFile <- getErrorsFile app
                            return (args ++ [errFile])
    putStrLn $ "Launching custom " ++ app ++ ": " ++ show executable_path
    let launchFailed = return $ Just
         ("Custom " ++ app
          ++ " (" ++ show executable_path ++ ") "
          ++ "could not be launched!\n") +++ errMsg

    handle (\_exception -> return ())
       (executeFile executable_path False args' Nothing)
    -- if we reach this point then exec failed.
    launchFailed

#else
    return Nothing
#endif

#ifdef darwin_HOST_OS
-- Darwin is odd or broken; Take your pick. According to:
--      http://uninformed.org/index.cgi?v=1&a=1&p=16
-- and
--      http://www.cherrypy.org/ticket/581
-- In order to get around a "Operation not supported" error on execv[e] it's
-- required to fork THEN execv[e]. - coconnor
executeFile :: FilePath -> Bool -> [String] -> Maybe [(String,String)] -> IO ()
executeFile cmd usePath args cmdEnv = do
    child_pid <- forkProcess $ SPP.executeFile cmd usePath args cmdEnv
    forever $ do
        child_status <- getProcessStatus True True child_pid
        case child_status of
            Nothing -> error "executeFile: could not get child process status"
            Just (Exited code) -> exitImmediately code
            Just (Terminated _) -> exitImmediately ExitSuccess
            Just (Stopped _) -> raiseSignal sigTSTP
#endif
