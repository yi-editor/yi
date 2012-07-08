module Yi.Test.BatchInteract where

import Prelude hiding ( concat )

import Yi.Test.Prelude

import Control.Concurrent

import System.Exit
import System.FilePath
import System.IO
import System.Process

-- TODO(corey): I'm suspect of the plan here.
-- Lazily interact with Yi master using string input. Assumes string output
--
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

verify_str_interact :: String -- name
                    -> [String]  -- yi args
                    --  stdout    stderr    exit_code
                    -> (String -> String -> ExitCode -> Either String ()) 
                    -> Test
verify_str_interact name args check = 
    Test $ TestInstance
        { run = do
            (stdout_str, stderr_str, exit_code) <- str_interact_yi args ""
            info $ "stdout -\n" ++ stdout_str
            info $ "stderr -\n" ++ stderr_str
            let r = check stdout_str stderr_str exit_code
            info $ show r
            return $ Finished $ case r of
                Right () -> Pass
                Left f_msg -> Fail f_msg
        , name = name
        , tags = []
        , options = []
        , setOption = const (const $ Left "no options") 
        }

verify_stdout :: String -> [String] -> (String -> Either String ()) -> Test
verify_stdout name args check = verify_str_interact name args $ \stdout_str _ exit_code -> do
    assert (exit_code == ExitSuccess)
        $ "yi failed - " ++ show exit_code
    check stdout_str

