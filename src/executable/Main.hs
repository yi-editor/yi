{-# LANGUAGE RecordWildCards #-}
-- Copyright (C) 2012 Corey O'Connor
--
-- | This process manages the compilation and execution of yi.
--
-- The management process is modeled as a state machine. The initial state is process start and the
-- final states are process termination.
--

module Main (main, main_) where

import Prelude hiding ( catch )

import Yi.System.Info

import Paths_yi

import Control.Applicative
import Control.Exception

import Data.Monoid

import Distribution.Text ( display )

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

data MonitorCommand
    = ReportVersion
    | ReportSystemInfo Bool
    | StartDelegate [String]
    deriving (Show, Eq)

instance Monoid MonitorCommand where
    mempty = StartDelegate []
    (StartDelegate _) `mappend` x                 = x
    x                 `mappend` (StartDelegate _) = x
    x                 `mappend` _                 = x

opts_spec :: [OptDescr MonitorCommand] 
opts_spec =
    [ Option "v" ["version"]
             (NoArg ReportVersion) 
             "Output the version of the yi monitor and custom yi"
    , Option "" ["info"]
             (NoArg $ ReportSystemInfo False)
             "Output some of the system information in a human-friendly format."
    , Option "" ["raw-info"]
             (NoArg $ ReportSystemInfo True)
             "Output the system information in a machine-friendly format."
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
    let (opts, non_opts, unknown_opts, _) = getOpt' RequireOrder opts_spec args
    case mconcat opts of
        ReportVersion -> do
            putStrLn $ "version: " ++ display version
            manageYiSystem Exit
        ReportSystemInfo True -> do
            putStr $! show yiSystemInfo
            manageYiSystem Exit
        ReportSystemInfo False -> do
            let SystemInfo {..} = yiSystemInfo
                PackageDescription {..} = yiPackageDescription
                LocalBuildInfo {..} = yiLocalBuildInfo
            _ <- printf "base-version: %s\n" $ display $ pkgVersion package
            manageYiSystem Exit
        StartDelegate opts -> do
            fail "TODO: Start delegate"

