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

manageYiSystem :: YiSystem -> IO ExitCode
manageYiSystem (Die n reason) = do
    info "E%d - %s" n reason
    return $ ExitFailure n
manageYiSystem Exit = do
    return ExitSuccess
manageYiSystem (Init args) = do
    -- Examine the command line arguments to determine if any actions need to be taken before
    -- booting yi proper.
    user_dir_exists <- doesDirectoryExist <$> appDir
    manageYiSystem Exit

