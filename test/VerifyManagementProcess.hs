{-# LANGUAGE FlexibleContexts #-}
module VerifyManagementProcess where

import Prelude hiding ( concat )

import Distribution.TestSuite

import Main

import Paths_yi

import Control.Applicative
import Control.Concurrent
import Control.Monad ( when )
import Control.Monad.Error hiding ( forM_ )

import Data.Either
import Data.Foldable
import Data.Maybe

import System.Exit
import System.FilePath
import System.IO
import System.Process

import Text.Printf
import Text.Regex.Posix

info f = do
    hPrintf stderr (if last f == '\n' then f else f ++ "\n")
    hFlush stderr

-- TODO(corey): I'm suspect of the plan here.
-- # create new handles for stdout and stderr
-- # spawn the yi process
-- # spawn threads to write to stdin from a string
-- # get the contents lazily of stdout and stderr
-- # wait on the process to end
str_interact_yi :: [String] -> String -> IO (String, String, ExitCode)
str_interact_yi args stdin_str = do
    let yi_exe_path = "dist/build/yi/yi"
    let cmd = RawCommand yi_exe_path args
        process_spec = CreateProcess 
                        { cmdspec = cmd
                        , cwd = Nothing
                        , env = Nothing
                        , std_in = CreatePipe
                        , std_out = CreatePipe
                        , std_err = CreatePipe
                        , close_fds = False
                        , create_group = True
                        }
    (Just stdin_h, Just stdout_h, Just stderr_h, process) <- createProcess process_spec
    forM_ [stdin_h, stdout_h, stderr_h] (flip hSetBuffering NoBuffering)
    forkIO $ hPutStr stdin_h stdin_str >> hClose stdin_h
    stdout_str <- hGetContents stdout_h
    stderr_str <- hGetContents stderr_h
    exit_code <- waitForProcess process
    info $ printf "%s - yi %s" (show exit_code) (concat args)
    return (stdout_str, stderr_str, exit_code)

tests :: IO [Test]
tests = return $ concat [ verify_v_opts ]

assert :: (MonadError String m, Monad m) => Bool -> String -> m ()
assert b f_str = when (not b) (throwError f_str)

verify_stdout :: String -> [String] -> (String -> Either String ()) -> Test
verify_stdout name args check = 
    Test $ TestInstance
        { run = do
            (stdout_str, stderr_str, exit_code) <- str_interact_yi args ""
            info $ "stdout -\n" ++ stdout_str
            info $ "stderr -\n" ++ stderr_str
            let r = do
                    assert (exit_code == ExitSuccess)
                        $ "yi failed - " ++ show exit_code
                    check stdout_str
            info $ show r
            return $ Finished $ case r of
                Right () -> Pass
                Left f_msg -> Fail f_msg
        , name = name
        , tags = []
        , options = []
        , setOption = const (const $ Left "no options") 
        }

verify_v_opts :: [Test]
verify_v_opts = foldMap tests_for_version_opt ["-v", "--version"] 
    where 
        tests_for_version_opt v_opt = 
            [ verify_stdout v_opt [v_opt] $ \stdout_str -> do
                let expected = "^version: (.*)$"
                assert (stdout_str =~ expected) "output did not include version" 
            ] 

