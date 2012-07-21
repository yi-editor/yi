-- Copyright (C) 2012 Corey O'Connor
--
-- | This process manages the compilation and execution of yi.
--
-- The management process is modeled as a state machine. The initial state is process start and the
-- final states are process termination.
--

module Main (main, main_) where

import Prelude hiding ( catch )

import Paths_yi

import Control.Applicative
import Control.Exception

import Data.Monoid

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO

import Text.Printf

appDir :: IO FilePath
appDir = getAppUserDataDirectory "yi"

info :: HPrintfType r => String -> r
info format = hPrintf stderr format

data YiSystem where
    -- Initial state. Parameterized by command line arguments. 
    Init :: [String] -> YiSystem
    Die  :: Int -> String -> YiSystem
    Exit :: YiSystem

main :: IO ()
main = do
    exit_code <- main_ =<< getArgs
    exitWith exit_code

main_ :: [String] -> IO ExitCode
main_ args = catch (manageYiSystem $ Init args) dieFromException

dieFromException :: SomeException -> IO ExitCode
dieFromException e = manageYiSystem (Die 1 $ show e)

data MonitorOpts
    = ReportVersion
    | NoMonitorOpts
    deriving (Show, Eq)

instance Monoid MonitorOpts where
    mempty = NoMonitorOpts
    NoMonitorOpts `mappend` x             = x
    x             `mappend` NoMonitorOpts = x
    ReportVersion `mappend` _             = ReportVersion
    _             `mappend` ReportVersion = ReportVersion

opts_spec :: [OptDescr MonitorOpts] 
opts_spec =
    [ Option "v" ["version"]
             (NoArg ReportVersion) 
             "report the version of the yi monitor and custom yi"
    ]

manageYiSystem :: YiSystem -> IO ExitCode
manageYiSystem (Die n reason) = do
    info "E%d - %s" n reason
    return $ ExitFailure n
manageYiSystem Exit = do
    return ExitSuccess
manageYiSystem (Init args) = do
    -- Examine the command line arguments to determine if any actions need to be taken before
    -- booting yi proper.
    let (opts, unknown_non, unknown_opts) = getOpt RequireOrder opts_spec args
    case mconcat opts of
        NoMonitorOpts -> do
            fail "TODO: NoMonitorOpts"
        ReportVersion -> do
            putStrLn $ "version: " ++ show version
            manageYiSystem Exit

